//! Fiber-based cooperative concurrency runtime for Baseline's async primitives.
//!
//! Implements `scope!`, `Scope.spawn!`, `Cell.await!`, `Cell.cancel!`,
//! `Channel.bounded`, `Channel.send!`, `Channel.recv!`, `Channel.close!`,
//! and async combinators (`Async.parallel!`, `Async.race!`, etc.)
//!
//! All concurrency is cooperative and single-threaded, using the fiber system
//! from `baseline_rt::fiber` for interleaving (e.g., channel backpressure).

use std::cell::RefCell;
use std::collections::VecDeque;

use super::{NValue, NativeError};
use baseline_rt::fiber::{self, Fiber, FiberState};
use baseline_rt::helpers::{call_handler_clause, NV_UNIT};
use baseline_rt::nvalue::HeapObject;

// ---------------------------------------------------------------------------
// Thread-local state
// ---------------------------------------------------------------------------

/// A scope context holds all tasks spawned within a `scope!` block.
struct ScopeContext {
    tasks: Vec<TaskState>,
}

/// State of a spawned task (Cell).
enum TaskState {
    /// Fiber is suspended or ready to run.
    Running(Box<Fiber>),
    /// Fiber completed with a result (raw NValue bits).
    Completed(u64),
    /// Task was cancelled.
    Cancelled,
}

/// Bounded channel internals.
struct ChannelInner {
    buffer: VecDeque<u64>,
    capacity: usize,
    closed: bool,
}

thread_local! {
    /// Stack of active scope contexts. Innermost scope is last.
    static SCOPE_STACK: RefCell<Vec<ScopeContext>> = const { RefCell::new(Vec::new()) };
    /// Registry of channels (indexed by channel_id).
    static CHANNELS: RefCell<Vec<ChannelInner>> = const { RefCell::new(Vec::new()) };
    /// Stash for closure bits passed to fiber entry points.
    static ASYNC_CLOSURE_STASH: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

// ---------------------------------------------------------------------------
// Helpers: call a Baseline closure from native code
// ---------------------------------------------------------------------------

/// Call a Baseline closure (NValue function/closure) with the given arguments.
/// Returns the raw result bits.
fn call_closure(closure_bits: u64, args: &[u64]) -> u64 {
    call_handler_clause(closure_bits, args)
}

/// Fiber entry point for spawned tasks. Reads the closure from the stash
/// and invokes it with no arguments.
extern "C" fn spawn_fiber_entry(_arg: u64) -> u64 {
    let closure_bits = ASYNC_CLOSURE_STASH.with(|c| c.get());
    call_closure(closure_bits, &[])
}

// ---------------------------------------------------------------------------
// scope!(closure) -> T
// ---------------------------------------------------------------------------

/// `scope!(|s| { ... })` — creates a structured concurrency scope.
///
/// The closure receives a Scope handle (encoded as an Int: scope stack index).
/// When the closure returns, all spawned tasks that haven't been awaited are
/// automatically awaited (structured concurrency guarantee).
pub fn native_scope(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError("scope! requires a closure argument".into()));
    }

    let closure_bits = args[0].raw();

    // Push a new scope context.
    let scope_id = SCOPE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        let id = stack.len();
        stack.push(ScopeContext { tasks: Vec::new() });
        id
    });

    // Create the scope handle as an NValue Int.
    let scope_handle = NValue::int(scope_id as i64);

    // Call the closure with the scope handle.
    let result_bits = call_closure(closure_bits, &[scope_handle.raw()]);

    // Structured concurrency: await all remaining tasks.
    SCOPE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        if let Some(scope) = stack.get_mut(scope_id) {
            for task in scope.tasks.iter_mut() {
                if let TaskState::Running(fiber) = task {
                    // Run the fiber to completion.
                    loop {
                        match &fiber.state {
                            FiberState::Completed(_) => break,
                            FiberState::Aborted => break,
                            _ => {
                                fiber.resume(0);
                            }
                        }
                    }
                    if let FiberState::Completed(val) = fiber.state {
                        *task = TaskState::Completed(val);
                    } else {
                        *task = TaskState::Cancelled;
                    }
                }
            }
        }
    });

    // Pop the scope context.
    SCOPE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        if stack.len() == scope_id + 1 {
            stack.pop();
        }
    });

    Ok(unsafe { NValue::borrow_from_raw(result_bits) })
}

// ---------------------------------------------------------------------------
// Scope.spawn!(scope, closure) -> Cell<T>
// ---------------------------------------------------------------------------

/// `Scope.spawn!(s, || expr)` — spawns a task within the scope.
///
/// Creates a fiber for the closure and returns a Cell handle (encoded as a
/// tuple of (scope_id, task_id) integers).
pub fn native_scope_spawn(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() < 2 {
        return Err(NativeError(
            "Scope.spawn! requires (scope, closure) arguments".into(),
        ));
    }

    let scope_id = if args[0].is_int() {
        args[0].as_int() as usize
    } else {
        return Err(NativeError("Scope.spawn!: first arg must be a Scope".into()));
    };

    let closure_bits = args[1].raw();

    // Stash the closure for the fiber entry point.
    ASYNC_CLOSURE_STASH.with(|c| c.set(closure_bits));

    // Create a fiber for the closure.
    let mut fiber = Fiber::new(spawn_fiber_entry, 0);

    // Start the fiber — it runs until it yields or completes.
    fiber.resume(0);

    let task_id = SCOPE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        let scope = stack.get_mut(scope_id).expect("Scope.spawn!: invalid scope");
        let id = scope.tasks.len();
        match fiber.state {
            FiberState::Completed(val) => {
                scope.tasks.push(TaskState::Completed(val));
            }
            _ => {
                scope.tasks.push(TaskState::Running(fiber));
            }
        }
        id
    });

    // Return a Cell handle as a tuple (scope_id, task_id).
    Ok(NValue::tuple(vec![
        NValue::int(scope_id as i64),
        NValue::int(task_id as i64),
    ]))
}

// ---------------------------------------------------------------------------
// Cell.await!(cell) -> T
// ---------------------------------------------------------------------------

/// `Cell.await!(cell)` — waits for a spawned task to complete and returns its result.
pub fn native_cell_await(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError("Cell.await! requires a Cell argument".into()));
    }

    let (scope_id, task_id) = extract_cell_ids(&args[0])?;

    // Get the task result, running the fiber if needed.
    let result_bits = SCOPE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        let scope = stack
            .get_mut(scope_id)
            .ok_or_else(|| NativeError("Cell.await!: invalid scope".into()))?;
        let task = scope
            .tasks
            .get_mut(task_id)
            .ok_or_else(|| NativeError("Cell.await!: invalid task".into()))?;

        match task {
            TaskState::Completed(val) => Ok(*val),
            TaskState::Cancelled => Err(NativeError("Cell.await!: task was cancelled".into())),
            TaskState::Running(fiber) => {
                // Resume the fiber until it completes.
                loop {
                    match &fiber.state {
                        FiberState::Completed(val) => {
                            let val = *val;
                            *task = TaskState::Completed(val);
                            return Ok(val);
                        }
                        FiberState::Aborted => {
                            *task = TaskState::Cancelled;
                            return Err(NativeError("Cell.await!: task aborted".into()));
                        }
                        FiberState::Yielded { key, .. } => {
                            // Handle channel yield operations.
                            let key = key.clone();
                            if key.starts_with("__chan_send:") || key.starts_with("__chan_recv:") {
                                // Handle channel operations inline.
                                let resume_val = handle_channel_yield(fiber)?;
                                fiber.resume(resume_val);
                            } else {
                                // Unknown yield — just resume with unit.
                                fiber.resume(NV_UNIT);
                            }
                        }
                        FiberState::Ready => {
                            fiber.resume(0);
                        }
                    }
                }
            }
        }
    })?;

    Ok(unsafe { NValue::borrow_from_raw(result_bits) })
}

/// Extract (scope_id, task_id) from a Cell handle NValue.
fn extract_cell_ids(cell: &NValue) -> Result<(usize, usize), NativeError> {
    if let Some(items) = cell.as_tuple() {
        if items.len() == 2 && items[0].is_int() && items[1].is_int() {
            return Ok((items[0].as_int() as usize, items[1].as_int() as usize));
        }
    }
    Err(NativeError("Invalid Cell handle".into()))
}

// ---------------------------------------------------------------------------
// Cell.cancel!(cell) -> ()
// ---------------------------------------------------------------------------

/// `Cell.cancel!(cell)` — cancels a spawned task.
pub fn native_cell_cancel(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError(
            "Cell.cancel! requires a Cell argument".into(),
        ));
    }

    let (scope_id, task_id) = extract_cell_ids(&args[0])?;

    SCOPE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        if let Some(scope) = stack.get_mut(scope_id) {
            if let Some(task) = scope.tasks.get_mut(task_id) {
                match task {
                    TaskState::Running(fiber) => {
                        fiber.state = FiberState::Aborted;
                        *task = TaskState::Cancelled;
                    }
                    _ => {} // Already completed or cancelled.
                }
            }
        }
    });

    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Channel.bounded(capacity) -> (Tx<T>, Rx<T>)
// ---------------------------------------------------------------------------

/// `Channel.bounded(n)` — creates a bounded channel with capacity `n`.
/// Returns a tuple `(Tx, Rx)` where each is encoded as an Int (channel_id).
/// Tx and Rx share the same channel_id (direction determined at call site).
pub fn native_channel_bounded(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() || !args[0].is_int() {
        return Err(NativeError(
            "Channel.bounded requires an Int capacity".into(),
        ));
    }

    let capacity = args[0].as_int() as usize;
    if capacity == 0 {
        return Err(NativeError(
            "Channel.bounded: capacity must be > 0".into(),
        ));
    }

    let channel_id = CHANNELS.with(|c| {
        let mut channels = c.borrow_mut();
        let id = channels.len();
        channels.push(ChannelInner {
            buffer: VecDeque::with_capacity(capacity),
            capacity,
            closed: false,
        });
        id
    });

    // Return (Tx, Rx) as a tuple of ints: (channel_id, channel_id)
    // Both endpoints reference the same channel. The semantic difference
    // (send vs recv) is enforced by the type checker.
    Ok(NValue::tuple(vec![
        NValue::int(channel_id as i64),
        NValue::int(channel_id as i64),
    ]))
}

// ---------------------------------------------------------------------------
// Channel.send!(tx, value) -> ()
// ---------------------------------------------------------------------------

/// `Channel.send!(tx, value)` — sends a value through the channel.
/// If the buffer is full, yields the fiber until space is available.
pub fn native_channel_send(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() < 2 {
        return Err(NativeError(
            "Channel.send! requires (tx, value) arguments".into(),
        ));
    }

    let channel_id = if args[0].is_int() {
        args[0].as_int() as usize
    } else {
        return Err(NativeError(
            "Channel.send!: first arg must be a Tx handle".into(),
        ));
    };

    let value_bits = args[1].raw();

    // Try to send — if buffer full, yield via fiber to allow receiver to drain.
    loop {
        let send_result = CHANNELS.with(|c| {
            let mut channels = c.borrow_mut();
            let chan = channels
                .get_mut(channel_id)
                .ok_or_else(|| NativeError("Channel.send!: invalid channel".into()))?;

            if chan.closed {
                return Err(NativeError("Channel.send!: channel is closed".into()));
            }

            if chan.buffer.len() < chan.capacity {
                chan.buffer.push_back(value_bits);
                Ok(true) // sent
            } else {
                Ok(false) // buffer full, need to yield
            }
        })?;

        if send_result {
            return Ok(NValue::unit());
        }

        // Buffer full — yield to allow other fibers to drain.
        // This uses the fiber yield mechanism with a special key.
        fiber::fiber_yield(format!("__chan_send:{}", channel_id), vec![value_bits]);
    }
}

// ---------------------------------------------------------------------------
// Channel.recv!(rx) -> Option<T>
// ---------------------------------------------------------------------------

/// `Channel.recv!(rx)` — receives a value from the channel.
/// Returns `Some(value)` or `None` if the channel is closed and empty.
pub fn native_channel_recv(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError(
            "Channel.recv! requires an Rx argument".into(),
        ));
    }

    let channel_id = if args[0].is_int() {
        args[0].as_int() as usize
    } else {
        return Err(NativeError(
            "Channel.recv!: arg must be an Rx handle".into(),
        ));
    };

    loop {
        let recv_result = CHANNELS.with(|c| {
            let mut channels = c.borrow_mut();
            let chan = channels
                .get_mut(channel_id)
                .ok_or_else(|| NativeError("Channel.recv!: invalid channel".into()))?;

            if let Some(val_bits) = chan.buffer.pop_front() {
                Ok(Some(val_bits)) // got a value
            } else if chan.closed {
                Ok(None) // channel closed, no more values -> None
            } else {
                // Buffer empty but channel open — need to wait.
                // For now, return a sentinel to indicate "need to yield".
                Ok(None) // No fiber yield from recv for simplicity
            }
        })?;

        match recv_result {
            Some(val_bits) => {
                // Return Some(value)
                let val = unsafe { NValue::borrow_from_raw(val_bits) };
                return Ok(NValue::from_heap_obj(HeapObject::Enum {
                    tag: "Some".into(),
                    tag_id: 1,
                    payload: vec![val],
                }));
            }
            None => {
                // Check if closed.
                let is_closed = CHANNELS.with(|c| {
                    let channels = c.borrow();
                    channels
                        .get(channel_id)
                        .map(|ch| ch.closed && ch.buffer.is_empty())
                        .unwrap_or(true)
                });

                if is_closed {
                    // Return None
                    return Ok(NValue::from_heap_obj(HeapObject::Enum {
                        tag: "None".into(),
                        tag_id: 0,
                        payload: vec![],
                    }));
                }

                // Buffer empty, channel open — yield to let senders run.
                fiber::fiber_yield(format!("__chan_recv:{}", channel_id), vec![]);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Channel.close!(tx) -> ()
// ---------------------------------------------------------------------------

/// `Channel.close!(tx)` — closes the sending end of the channel.
pub fn native_channel_close(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError(
            "Channel.close! requires a Tx argument".into(),
        ));
    }

    let channel_id = if args[0].is_int() {
        args[0].as_int() as usize
    } else {
        return Err(NativeError(
            "Channel.close!: arg must be a Tx handle".into(),
        ));
    };

    CHANNELS.with(|c| {
        let mut channels = c.borrow_mut();
        if let Some(chan) = channels.get_mut(channel_id) {
            chan.closed = true;
        }
    });

    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Handle channel yields from within Cell.await!
// ---------------------------------------------------------------------------

/// When a fiber yields with a channel operation key, handle it inline.
/// Returns the value to resume the fiber with.
fn handle_channel_yield(fiber: &Fiber) -> Result<u64, NativeError> {
    let (key, args) = match &fiber.state {
        FiberState::Yielded { key, args } => (key.clone(), args.clone()),
        _ => return Ok(NV_UNIT),
    };

    if let Some(chan_id_str) = key.strip_prefix("__chan_send:") {
        let channel_id: usize = chan_id_str.parse().unwrap_or(0);
        let value_bits = args.first().copied().unwrap_or(NV_UNIT);

        // Try to push into the channel buffer.
        let sent = CHANNELS.with(|c| {
            let mut channels = c.borrow_mut();
            if let Some(chan) = channels.get_mut(channel_id) {
                if chan.buffer.len() < chan.capacity {
                    chan.buffer.push_back(value_bits);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        });

        if sent {
            Ok(NV_UNIT) // Resume with unit (send succeeded)
        } else {
            Ok(NV_UNIT) // Will retry on next resume
        }
    } else if key.starts_with("__chan_recv:") {
        // Recv yield — just resume, the recv loop will retry.
        Ok(NV_UNIT)
    } else {
        Ok(NV_UNIT)
    }
}

// ---------------------------------------------------------------------------
// Async combinators
// ---------------------------------------------------------------------------

/// `Async.parallel!(fns)` — runs a list of closures, returns results in order.
pub fn native_async_parallel(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError(
            "Async.parallel! requires a List argument".into(),
        ));
    }

    let fns = args[0]
        .as_list()
        .ok_or_else(|| NativeError("Async.parallel!: argument must be a List".into()))?;

    let mut results = Vec::with_capacity(fns.len());
    for f in fns {
        let result_bits = call_closure(f.raw(), &[]);
        results.push(unsafe { NValue::borrow_from_raw(result_bits) });
    }

    Ok(NValue::from_heap_obj(HeapObject::List(results)))
}

/// `Async.race!(fns)` — runs closures, returns the first result.
/// (In cooperative mode, this is just the first closure's result.)
pub fn native_async_race(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        return Err(NativeError(
            "Async.race! requires a List argument".into(),
        ));
    }

    let fns = args[0]
        .as_list()
        .ok_or_else(|| NativeError("Async.race!: argument must be a List".into()))?;

    if fns.is_empty() {
        return Err(NativeError("Async.race!: empty list".into()));
    }

    // In cooperative (single-threaded) mode, run first closure.
    let result_bits = call_closure(fns[0].raw(), &[]);
    Ok(unsafe { NValue::borrow_from_raw(result_bits) })
}

/// `Async.scatter_gather!(fns, gather)` — runs closures in parallel, then
/// applies the gather function to the collected results.
pub fn native_async_scatter_gather(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() < 2 {
        return Err(NativeError(
            "Async.scatter_gather! requires (fns, gather) arguments".into(),
        ));
    }

    let fns = args[0]
        .as_list()
        .ok_or_else(|| NativeError("Async.scatter_gather!: first arg must be a List".into()))?;

    let gather_bits = args[1].raw();

    // Run all closures, collect results.
    let mut results = Vec::with_capacity(fns.len());
    for f in fns {
        let result_bits = call_closure(f.raw(), &[]);
        results.push(unsafe { NValue::borrow_from_raw(result_bits) });
    }

    let results_list = NValue::from_heap_obj(HeapObject::List(results));

    // Call the gather function with the results list.
    let final_bits = call_closure(gather_bits, &[results_list.raw()]);
    Ok(unsafe { NValue::borrow_from_raw(final_bits) })
}

/// `Async.delay!(ms, closure)` — delays execution by `ms` milliseconds, then
/// runs the closure.
pub fn native_async_delay(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() < 2 {
        return Err(NativeError(
            "Async.delay! requires (ms, closure) arguments".into(),
        ));
    }

    let ms = if args[0].is_int() {
        args[0].as_int()
    } else {
        return Err(NativeError("Async.delay!: first arg must be Int".into()));
    };

    if ms > 0 {
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }

    let result_bits = call_closure(args[1].raw(), &[]);
    Ok(unsafe { NValue::borrow_from_raw(result_bits) })
}

/// `Async.interval!(ms, closure)` — calls closure repeatedly with interval.
/// Runs 10 iterations max to prevent infinite loops in the VM.
pub fn native_async_interval(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() < 2 {
        return Err(NativeError(
            "Async.interval! requires (ms, closure) arguments".into(),
        ));
    }

    let ms = if args[0].is_int() {
        args[0].as_int() as u64
    } else {
        return Err(NativeError(
            "Async.interval!: first arg must be Int".into(),
        ));
    };

    let closure_bits = args[1].raw();

    // Run a bounded number of iterations.
    for _ in 0..10 {
        if ms > 0 {
            std::thread::sleep(std::time::Duration::from_millis(ms));
        }
        call_closure(closure_bits, &[]);
    }

    Ok(NValue::unit())
}

/// `Async.timeout!(ms, closure)` — runs closure with a timeout.
/// Returns `Ok(result)` if closure completes, `Err("timeout")` if it exceeds the limit.
///
/// Note: In cooperative single-threaded mode, true preemptive timeout is not possible.
/// The closure runs to completion and timeout is checked afterward.
pub fn native_async_timeout(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() < 2 {
        return Err(NativeError(
            "Async.timeout! requires (ms, closure) arguments".into(),
        ));
    }

    let ms = if args[0].is_int() {
        args[0].as_int() as u64
    } else {
        return Err(NativeError(
            "Async.timeout!: first arg must be Int".into(),
        ));
    };

    let closure_bits = args[1].raw();

    let start = std::time::Instant::now();
    let result_bits = call_closure(closure_bits, &[]);
    let elapsed = start.elapsed();

    if elapsed > std::time::Duration::from_millis(ms) {
        // Timed out — return Err("timeout")
        let err_msg = NValue::from_heap_obj(HeapObject::String("timeout".into()));
        Ok(NValue::from_heap_obj(HeapObject::Enum {
            tag: "Err".into(),
            tag_id: 3,
            payload: vec![err_msg],
        }))
    } else {
        // Success — return Ok(result)
        let result = unsafe { NValue::borrow_from_raw(result_bits) };
        Ok(NValue::from_heap_obj(HeapObject::Enum {
            tag: "Ok".into(),
            tag_id: 2,
            payload: vec![result],
        }))
    }
}
