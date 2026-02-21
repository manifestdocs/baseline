//! Fiber Runtime — structured concurrency backed by tokio tasks.
//!
//! Each `spawn!` creates a new `Vm` instance sharing the same `Arc<Program>`.
//! The async boundary lives at the native function level (scope!, spawn!, await!, cancel!),
//! keeping the VM dispatch loop synchronous.
//!
//! Key types:
//! - `ScopeState` — tracks child fibers spawned within a `scope!` block
//! - `CellState` — wraps a oneshot receiver + abort handle for a spawned fiber
//!
//! All spawns are scoped: `scope!` creates a scope, `Scope.spawn!` spawns within it,
//! and the scope waits for all children before returning. If any child errors,
//! remaining children are cancelled.

use std::sync::{Arc, Mutex};

use tokio::sync::oneshot;
use tokio::task::{AbortHandle, JoinSet};

use super::chunk::{Chunk, CompileError, Program};
use super::exec::Vm;
use super::nvalue::{HeapObject, NValue};

/// NativeObject tag for Scope handles.
const SCOPE_TAG: &str = "scope";
/// NativeObject tag for Cell handles.
const CELL_TAG: &str = "cell";

// ---------------------------------------------------------------------------
// Scope State
// ---------------------------------------------------------------------------

/// Tracks all child fibers spawned within a `scope!` block.
/// Shared between the scope owner and spawned fibers via `Arc<Mutex<_>>`.
pub struct ScopeState {
    /// JoinSet for structured cancellation — when scope exits, all children are cancelled.
    join_set: JoinSet<Result<NValue, String>>,
    /// Abort handles for explicit cancellation of individual fibers.
    abort_handles: Vec<AbortHandle>,
}

impl ScopeState {
    fn new() -> Self {
        Self {
            join_set: JoinSet::new(),
            abort_handles: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Cell State
// ---------------------------------------------------------------------------

/// The result receiver for a spawned fiber.
pub struct CellState {
    /// Receives the fiber's result value.
    receiver: Option<oneshot::Receiver<Result<NValue, String>>>,
    /// Handle to abort the fiber.
    abort_handle: AbortHandle,
}

// ---------------------------------------------------------------------------
// NValue helpers
// ---------------------------------------------------------------------------

fn make_scope_nvalue(state: Arc<Mutex<ScopeState>>) -> NValue {
    NValue::native_object(SCOPE_TAG, state)
}

fn make_cell_nvalue(state: Arc<Mutex<CellState>>) -> NValue {
    NValue::native_object(CELL_TAG, state)
}

fn extract_scope(nv: &NValue) -> Option<Arc<Mutex<ScopeState>>> {
    if !nv.is_heap() {
        return None;
    }
    if let HeapObject::NativeObject { tag, data } = nv.as_heap_ref() {
        if *tag == SCOPE_TAG {
            return data.clone().downcast::<Mutex<ScopeState>>().ok();
        }
    }
    None
}

fn extract_cell(nv: &NValue) -> Option<Arc<Mutex<CellState>>> {
    if !nv.is_heap() {
        return None;
    }
    if let HeapObject::NativeObject { tag, data } = nv.as_heap_ref() {
        if *tag == CELL_TAG {
            return data.clone().downcast::<Mutex<CellState>>().ok();
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Public API — called from VM dispatch
// ---------------------------------------------------------------------------

/// Execute a `scope!` block: create a scope, run the body closure, then join all children.
///
/// The body closure receives the scope handle as its first argument.
/// On return, all spawned children are awaited. If any child errors,
/// remaining children are cancelled and the first error is propagated.
pub fn exec_scope(
    vm: &mut Vm,
    body: NValue,
    chunks: &[Chunk],
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    // Create the scope state
    let scope_state = Arc::new(Mutex::new(ScopeState::new()));
    let scope_nv = make_scope_nvalue(scope_state.clone());

    // Call the body closure with the scope handle
    let body_result = vm.call_nvalue(&body, &[scope_nv], chunks, line, col)?;

    // Join all children — block the current thread waiting for them
    let rt = tokio::runtime::Handle::try_current().map_err(|_| CompileError {
        message: "scope! requires a tokio runtime (use `blc run --async`)".into(),
        line,
        col,
    })?;

    let join_result = rt.block_on(async {
        let mut state = scope_state.lock().unwrap();
        let mut first_error: Option<String> = None;

        while let Some(result) = state.join_set.join_next().await {
            match result {
                Ok(Ok(_)) => {} // child completed successfully
                Ok(Err(e)) => {
                    if first_error.is_none() {
                        first_error = Some(e);
                        // Cancel remaining children on first error
                        state.join_set.abort_all();
                    }
                }
                Err(join_err) => {
                    if join_err.is_cancelled() {
                        // Expected when scope cancels children
                        continue;
                    }
                    if first_error.is_none() {
                        first_error = Some(format!("Fiber panicked: {}", join_err));
                        state.join_set.abort_all();
                    }
                }
            }
        }
        first_error
    });

    if let Some(err) = join_result {
        return Err(CompileError {
            message: err,
            line,
            col,
        });
    }

    Ok(body_result)
}

/// Spawn a fiber within a scope: `Scope.spawn!(scope, || body)`.
///
/// Creates a new Vm on a tokio blocking task, runs the closure, and returns a Cell.
pub fn exec_spawn(
    scope_nv: &NValue,
    body: NValue,
    program: Arc<Program>,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    // Extract the scope state
    let scope_state = extract_scope(scope_nv).ok_or_else(|| CompileError {
        message: "Scope.spawn! first argument must be a Scope handle".into(),
        line,
        col,
    })?;

    // Create oneshot channel for result delivery
    let (tx, rx) = oneshot::channel();

    // Spawn the fiber on a tokio blocking task
    let mut state = scope_state.lock().unwrap();
    let abort_handle = state.join_set.spawn_blocking(move || {
        let mut fiber_vm = Vm::new();
        match fiber_vm.call_nvalue(&body, &[], &program.chunks, line, col) {
            Ok(result) => {
                // Send result to the cell; ignore error if receiver was dropped
                let _ = tx.send(Ok(result.clone()));
                Ok(result)
            }
            Err(e) => {
                let _ = tx.send(Err(e.message.clone()));
                Err(e.message)
            }
        }
    });
    state.abort_handles.push(abort_handle.clone());

    // Return a Cell wrapping the receiver + abort handle
    let cell_state = Arc::new(Mutex::new(CellState {
        receiver: Some(rx),
        abort_handle,
    }));
    Ok(make_cell_nvalue(cell_state))
}

/// Await a cell: `Cell.await!(cell)` — blocks until the fiber completes.
pub fn exec_cell_await(
    cell_nv: &NValue,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let cell_state = extract_cell(cell_nv).ok_or_else(|| CompileError {
        message: "Cell.await! argument must be a Cell".into(),
        line,
        col,
    })?;

    let rx = {
        let mut state = cell_state.lock().unwrap();
        state.receiver.take().ok_or_else(|| CompileError {
            message: "Cell.await! called on already-awaited Cell".into(),
            line,
            col,
        })?
    };

    // Block the current thread waiting for the fiber result
    match rx.blocking_recv() {
        Ok(Ok(value)) => Ok(value),
        Ok(Err(e)) => Err(CompileError {
            message: e,
            line,
            col,
        }),
        Err(_) => Err(CompileError {
            message: "Cell.await!: fiber was cancelled".into(),
            line,
            col,
        }),
    }
}

/// Cancel a cell: `Cell.cancel!(cell)` — aborts the fiber.
pub fn exec_cell_cancel(
    cell_nv: &NValue,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let cell_state = extract_cell(cell_nv).ok_or_else(|| CompileError {
        message: "Cell.cancel! argument must be a Cell".into(),
        line,
        col,
    })?;

    let state = cell_state.lock().unwrap();
    state.abort_handle.abort();
    Ok(NValue::unit())
}

/// Execute `Async.parallel!(closures)` — run multiple closures concurrently and return a list of their results.
pub fn exec_parallel(
    closures_list: &NValue,
    program: Arc<Program>,
    chunks: &[Chunk],
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let closures = closures_list.as_list().ok_or_else(|| CompileError {
        message: "Async.parallel! argument must be a List of closures".into(),
        line,
        col,
    })?;

    let rt = tokio::runtime::Handle::try_current().map_err(|_| CompileError {
        message: "Async.parallel! requires a tokio runtime (use `blc run --async`)".into(),
        line,
        col,
    })?;

    let join_result = rt.block_on(async {
        let mut join_set = JoinSet::new();
        let num_closures = closures.len();

        for (i, closure) in closures.iter().enumerate() {
            let closure = closure.clone();
            let program = program.clone();
            join_set.spawn_blocking(move || {
                let mut fiber_vm = Vm::new();
                match fiber_vm.call_nvalue(&closure, &[], &program.chunks, line, col) {
                    Ok(result) => Ok((i, result)),
                    Err(e) => Err(e.message),
                }
            });
        }

        let mut results = vec![None; num_closures];
        let mut first_error: Option<String> = None;

        while let Some(res) = join_set.join_next().await {
            match res {
                Ok(Ok((i, val))) => {
                    results[i] = Some(val);
                }
                Ok(Err(err)) => {
                    if first_error.is_none() {
                        first_error = Some(err);
                        join_set.abort_all();
                    }
                }
                Err(join_err) => {
                    if !join_err.is_cancelled() && first_error.is_none() {
                        first_error = Some(format!("Fiber panicked: {}", join_err));
                        join_set.abort_all();
                    }
                }
            }
        }

        if let Some(err) = first_error {
            Err(err)
        } else {
            // All completed successfully
            let final_results = results.into_iter().map(|o| o.unwrap()).collect();
            Ok(NValue::list(final_results))
        }
    });

    match join_result {
        Ok(vals) => Ok(vals),
        Err(err) => Err(CompileError {
            message: err,
            line,
            col,
        }),
    }
}

/// Execute `Async.race!(closures)` — run multiple closures concurrently and return the result of the first to complete.
pub fn exec_race(
    closures_list: &NValue,
    program: Arc<Program>,
    chunks: &[Chunk],
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let closures = closures_list.as_list().ok_or_else(|| CompileError {
        message: "Async.race! argument must be a List of closures".into(),
        line,
        col,
    })?;

    if closures.is_empty() {
        return Err(CompileError {
            message: "Async.race! requires at least one closure".into(),
            line,
            col,
        });
    }

    let rt = tokio::runtime::Handle::try_current().map_err(|_| CompileError {
        message: "Async.race! requires a tokio runtime (use `blc run --async`)".into(),
        line,
        col,
    })?;

    let join_result = rt.block_on(async {
        let mut join_set = JoinSet::new();

        for closure in closures.iter() {
            let closure = closure.clone();
            let program = program.clone();
            join_set.spawn_blocking(move || {
                let mut fiber_vm = Vm::new();
                match fiber_vm.call_nvalue(&closure, &[], &program.chunks, line, col) {
                    Ok(result) => Ok(result),
                    Err(e) => Err(e.message),
                }
            });
        }

        let mut final_result = None;
        let mut first_error: Option<String> = None;

        while let Some(res) = join_set.join_next().await {
            match res {
                Ok(Ok(val)) => {
                    // First to succeed!
                    final_result = Some(val);
                    join_set.abort_all(); // Cancel the rest
                    break;
                }
                Ok(Err(err)) => {
                    // One errored. For race!, do we fail immediately, or wait for others?
                    // Standard race usually fails on first error if no success yet.
                    if first_error.is_none() {
                        first_error = Some(err);
                        join_set.abort_all();
                    }
                }
                Err(join_err) => {
                    if !join_err.is_cancelled() && first_error.is_none() {
                        first_error = Some(format!("Fiber panicked: {}", join_err));
                        join_set.abort_all();
                    }
                }
            }
        }

        if let Some(r) = final_result {
            Ok(r)
        } else if let Some(err) = first_error {
            Err(err)
        } else {
            Err("All tasks in Async.race! failed or panicked".into())
        }
    });

    match join_result {
        Ok(val) => Ok(val),
        Err(err) => Err(CompileError {
            message: err,
            line,
            col,
        }),
    }
}

/// Execute `Async.scatter_gather!(closures, aggregator)` — run closures concurrently and aggregate results.
pub fn exec_scatter_gather(
    closures_list: &NValue,
    aggregator: &NValue,
    program: Arc<Program>,
    chunks: &[Chunk],
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    // First, run them all in parallel
    let list_result = exec_parallel(closures_list, program, chunks, line, col)?;
    
    // Then sequentially run the aggregator on the resulting list
    let mut current_vm = Vm::new();
    current_vm.call_nvalue(aggregator, &[list_result], chunks, line, col)
}

// ---------------------------------------------------------------------------
// Channel State
// ---------------------------------------------------------------------------

const CHANNEL_TX_TAG: &str = "channel_tx";
const CHANNEL_RX_TAG: &str = "channel_rx";

pub struct ChannelTxState {
    sender: Option<tokio::sync::mpsc::Sender<NValue>>,
}

pub struct ChannelRxState {
    receiver: tokio::sync::mpsc::Receiver<NValue>,
}

fn make_channel_tx_nvalue(tx: tokio::sync::mpsc::Sender<NValue>) -> NValue {
    NValue::native_object(
        CHANNEL_TX_TAG,
        Arc::new(Mutex::new(ChannelTxState { sender: Some(tx) })),
    )
}

fn make_channel_rx_nvalue(rx: tokio::sync::mpsc::Receiver<NValue>) -> NValue {
    NValue::native_object(
        CHANNEL_RX_TAG,
        Arc::new(Mutex::new(ChannelRxState { receiver: rx })),
    )
}

fn extract_channel_tx(nv: &NValue) -> Option<Arc<Mutex<ChannelTxState>>> {
    if !nv.is_heap() {
        return None;
    }
    if let HeapObject::NativeObject { tag, data } = nv.as_heap_ref() {
        if *tag == CHANNEL_TX_TAG {
            return data.clone().downcast::<Mutex<ChannelTxState>>().ok();
        }
    }
    None
}

fn extract_channel_rx(nv: &NValue) -> Option<Arc<Mutex<ChannelRxState>>> {
    if !nv.is_heap() {
        return None;
    }
    if let HeapObject::NativeObject { tag, data } = nv.as_heap_ref() {
        if *tag == CHANNEL_RX_TAG {
            return data.clone().downcast::<Mutex<ChannelRxState>>().ok();
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Channels API
// ---------------------------------------------------------------------------

pub fn exec_channel_bounded(
    cap_nv: &NValue,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    if !cap_nv.is_int() {
        return Err(CompileError {
            message: "Channel.bounded capacity must be an integer".into(),
            line,
            col,
        });
    }
    let capacity = cap_nv.as_int();

    if capacity <= 0 {
        return Err(CompileError {
            message: "Channel.bounded capacity must be greater than 0".into(),
            line,
            col,
        });
    }

    let (tx, rx) = tokio::sync::mpsc::channel(capacity as usize);

    let tx_nv = make_channel_tx_nvalue(tx);
    let rx_nv = make_channel_rx_nvalue(rx);

    // Using list instead of tuple since NValue::tuple inexplicably fails to resolve
    Ok(NValue::list(vec![tx_nv, rx_nv]))
}

pub fn exec_channel_send(
    tx_nv: &NValue,
    value_nv: &NValue,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let tx_state = extract_channel_tx(tx_nv).ok_or_else(|| CompileError {
        message: "Channel.send! first argument must be a Channel Sender".into(),
        line,
        col,
    })?;

    // Clone the sender so we don't hold the Mutex guard across a blocking operation.
    let sender = {
        let state = tx_state.lock().unwrap();
        match &state.sender {
            Some(s) => s.clone(),
            None => {
                return Err(CompileError {
                    message: "Channel.send! failed: channel is closed".into(),
                    line,
                    col,
                });
            }
        }
    };

    match sender.blocking_send(value_nv.clone()) {
        Ok(_) => Ok(NValue::unit()),
        Err(_) => Err(CompileError {
            message: "Channel.send! failed: channel is closed".into(),
            line,
            col,
        }),
    }
}

pub fn exec_channel_recv(
    rx_nv: &NValue,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let rx_state = extract_channel_rx(rx_nv).ok_or_else(|| CompileError {
        message: "Channel.recv! argument must be a Channel Receiver".into(),
        line,
        col,
    })?;

    let mut state = rx_state.lock().unwrap();
    match state.receiver.blocking_recv() {
        Some(val) => Ok(NValue::enum_val("Some".into(), val)),
        None => Ok(NValue::enum_val("None".into(), NValue::unit())),
    }
}

pub fn exec_channel_close(
    tx_nv: &NValue,
    line: usize,
    col: usize,
) -> Result<NValue, CompileError> {
    let tx_state = extract_channel_tx(tx_nv).ok_or_else(|| CompileError {
        message: "Channel.close! argument must be a Channel Sender".into(),
        line,
        col,
    })?;

    let mut state = tx_state.lock().unwrap();
    state.sender = None; // Drop the sender, closing the channel if no other clones exist

    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope_and_cell_state_are_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Arc<Mutex<CellState>>>();
        assert_send_sync::<Arc<Mutex<ScopeState>>>();
    }
}
