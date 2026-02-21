use std::collections::HashMap;
use std::rc::Rc;

use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::fiber;
use crate::vm::nvalue::{HeapObject, NValue};

use super::frame::{CallFrame, PackedBase};
use super::{DispatchResult, MAX_CALL_DEPTH};

/// Function call opcodes: Call, CallDirect, CallNative, TailCall,
/// GetUpvalue, MakeClosure, and superinstructions.
impl super::Vm {
    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn dispatch_call(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        chunks: &[Chunk],
        ip: usize,
        chunk_idx: usize,
        base_slot: usize,
        _base_depth: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::Call(arg_count) => {
                if self.frames.len() >= MAX_CALL_DEPTH {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        "Stack overflow: maximum call depth exceeded".into(),
                        line,
                        col,
                    ));
                }
                let n = *arg_count as usize;
                let func_pos = self.stack.len() - n - 1;
                {
                    let func = unsafe { self.stack.get_unchecked(func_pos) };
                    if func.is_continuation() {
                        let func = self.stack[func_pos].clone();
                        let caller = self.frames.last_mut();
                        caller.ip = ip as u32;
                        caller.chunk_idx = chunk_idx as u32;
                        return Ok(self.dispatch_continuation(&func, n));
                    }
                }

                let func = &self.stack[func_pos];
                let (new_chunk_idx, new_upvalue_idx) = if func.is_function() {
                    (func.as_function(), u32::MAX)
                } else if func.is_heap() {
                    if let HeapObject::Closure {
                        chunk_idx: idx,
                        upvalues,
                    } = func.as_heap_ref()
                    {
                        let uv_idx = self.upvalue_stack.len() as u32;
                        self.upvalue_stack.push(Rc::new(upvalues.clone()));
                        (*idx, uv_idx)
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(format!("Cannot call {}", func), line, col));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot call {}", func), line, col));
                };

                let caller = self.frames.last_mut();
                caller.ip = ip as u32;
                caller.chunk_idx = chunk_idx as u32;

                let new_base = PackedBase::from_slot(func_pos + 1).with_has_func();
                self.frames.push(CallFrame {
                    chunk_idx: new_chunk_idx as u32,
                    ip: 0,
                    base_slot: new_base,
                    upvalue_idx: new_upvalue_idx,
                });
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx: new_chunk_idx,
                    base_slot: new_base.slot(),
                });
            }

            Op::CallDirect(target_chunk, arg_count) => {
                if self.frames.len() >= MAX_CALL_DEPTH {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        "Stack overflow: maximum call depth exceeded".into(),
                        line,
                        col,
                    ));
                }
                let n = *arg_count as usize;
                let new_base = PackedBase::from_slot(self.stack.len() - n);

                let caller = self.frames.last_mut();
                caller.ip = ip as u32;
                caller.chunk_idx = chunk_idx as u32;

                self.frames.push(CallFrame {
                    chunk_idx: *target_chunk as u32,
                    ip: 0,
                    base_slot: new_base,
                    upvalue_idx: u32::MAX,
                });
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx: *target_chunk as usize,
                    base_slot: new_base.slot(),
                });
            }

            Op::GetUpvalue(idx) => {
                let frame = self.frames.last();
                let uv_idx = frame.upvalue_idx as usize;
                let val = self.upvalue_stack[uv_idx][*idx as usize].clone();
                self.stack.push(val);
            }

            Op::MakeClosure(new_chunk_idx, upvalue_count) => {
                let n = *upvalue_count as usize;
                let start = self.stack.len() - n;
                let upvalues: Vec<NValue> = self.stack.drain(start..).collect();
                self.stack
                    .push(NValue::closure(*new_chunk_idx as usize, upvalues));
            }

            Op::CallNative(fn_id, arg_count) => {
                let n = *arg_count as usize;

                if !self.handler_stack.is_empty() {
                    let name = self.natives.name(*fn_id).to_string();
                    if let Some(dot) = name.find('.') {
                        let module = &name[..dot];
                        let method = name[dot + 1..]
                            .strip_suffix('!')
                            .unwrap_or(&name[dot + 1..]);
                        if let Some(restart) = self.dispatch_handler_interception(module, method, n, ip, chunk_idx, chunks)? {
                            return Ok(restart);
                        }
                    }
                }

                if self.natives.is_hof(*fn_id) {
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    let (line, col) = chunk.source_map[ip - 1];
                    self.dispatch_hof(*fn_id, n, chunks, line, col)?;
                } else if self.natives.is_server_listen(*fn_id) {
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    let (line, col) = chunk.source_map[ip - 1];

                    #[cfg(feature = "async-server")]
                    {
                        self.dispatch_server_listen_async(n, chunks, line, col)?;
                    }
                    #[cfg(not(feature = "async-server"))]
                    {
                        self.dispatch_server_listen(n, chunks, line, col)?;
                    }
                } else if self.natives.is_async(*fn_id) {
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    let (line, col) = chunk.source_map[ip - 1];
                    self.dispatch_async(*fn_id, n, chunks, line, col)?;
                } else {
                    let start = self.stack.len() - n;
                    let result = self.natives.call(*fn_id, &self.stack[start..]);
                    let result = result.map_err(|e| {
                        let (line, col) = chunk.source_map[ip - 1];
                        self.error(format!("{}: {}", self.natives.name(*fn_id), e.0), line, col)
                    })?;
                    self.stack.truncate(start);
                    self.stack.push(result);
                }
            }

            // -- Superinstructions --
            Op::GetLocalSubInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::int(val.wrapping_sub(*k as i64)));
            }

            Op::GetLocalLeInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::bool(val <= *k as i64));
            }

            Op::GetLocalAddInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::int(val.wrapping_add(*k as i64)));
            }

            Op::GetLocalLtInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::bool(val < *k as i64));
            }

            Op::GetLocalLeIntJumpIfFalse(slot, k, offset) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                if val > *k as i64 {
                    return Ok(DispatchResult::Restart {
                        ip: ip + *offset as usize,
                        chunk_idx,
                        base_slot,
                    });
                }
            }

            Op::GetLocalLtIntJumpIfFalse(slot, k, offset) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                if val >= *k as i64 {
                    return Ok(DispatchResult::Restart {
                        ip: ip + *offset as usize,
                        chunk_idx,
                        base_slot,
                    });
                }
            }

            Op::TailCall(arg_count) => {
                let n = *arg_count as usize;
                let args_start = self.stack.len() - n;
                for i in 0..n {
                    let val = unsafe { self.stack.get_unchecked(args_start + i) }.clone();
                    unsafe {
                        *self.stack.get_unchecked_mut(base_slot + i) = val;
                    }
                }
                self.stack.truncate(base_slot + n);
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx,
                    base_slot,
                });
            }

            _ => unreachable!("dispatch_call called with non-call op"),
        }
        Ok(DispatchResult::Continue)
    }

    #[inline(always)]
    fn dispatch_continuation(
        &mut self,
        func: &NValue,
        n: usize,
    ) -> DispatchResult {
        let (ss, fs, us, hs, original_base_slot, hbs, r_ip, r_ci) = match func.as_heap_ref() {
            HeapObject::Continuation {
                stack_segment,
                frame_segment,
                upvalue_segment,
                handler_stack_segment,
                base_stack_depth,
                handler_boundary_segment,
                resume_ip,
                resume_chunk_idx,
                ..
            } => (
                stack_segment.clone(),
                frame_segment.clone(),
                upvalue_segment.clone(),
                handler_stack_segment.clone(),
                *base_stack_depth,
                handler_boundary_segment.clone(),
                *resume_ip,
                *resume_chunk_idx,
            ),
            _ => unreachable!(),
        };
        let resume_value = if n == 1 {
            self.stack.pop().unwrap()
        } else {
            NValue::unit()
        };

        // Instead of replacing the caller's frame, we append the continuation's
        // captured frames ON TOP of the caller. This ensures that when the continuation
        // finishes (returns), execution naturally falls back to the caller (the handler).
        let caller_frame_idx = self.frames.len();
        let stack_offset = self.stack.len() - original_base_slot;

        self.stack.extend(ss);

        for (i, &(ci, fip, fbs, fuv)) in fs.iter().enumerate() {
            let fbs_packed = PackedBase(fbs);
            let new_slot = fbs_packed.slot() + stack_offset;
            let mut new_base = PackedBase::from_slot(new_slot);
            if fbs_packed.has_func() {
                new_base = new_base.with_has_func();
            }
            if i == 0 {
                // Mark as continuation boundary and ensure has-func flag is set
                // so the stack cleanup on return removes the slot below args.
                new_base = new_base.with_is_cont().with_has_func();
            }
            self.frames.push(CallFrame {
                chunk_idx: ci,
                ip: fip,
                base_slot: new_base,
                upvalue_idx: fuv,
            });
        }

        for uvs in us {
            self.upvalue_stack.push(Rc::new(uvs));
        }

        for h in hs {
            self.handler_stack.push(h);
        }
        for (sd, fd, ud, hsi, rip, obs_rel) in hbs {
            self.handler_boundaries.push(super::HandlerBoundary {
                stack_depth: sd + stack_offset,
                frame_depth: fd + caller_frame_idx,
                upvalue_depth: ud,
                handler_stack_idx: hsi,
                return_ip: rip,
                original_base_slot: obs_rel + stack_offset + original_base_slot,
            });
        }

        self.stack.push(resume_value);

        let new_ip = r_ip as usize;
        let new_ci = r_ci as usize;
        let top = self.frames.last();
        let new_bs = top.base_slot.slot();

        DispatchResult::Restart {
            ip: new_ip,
            chunk_idx: new_ci,
            base_slot: new_bs,
        }
    }

    pub(crate) fn dispatch_handler_interception(
        &mut self,
        module: &str,
        method: &str,
        n: usize,
        ip: usize,
        chunk_idx: usize,
        chunks: &[Chunk],
    ) -> Result<Option<DispatchResult>, CompileError> {
        if let Some((handler_fn, boundary_idx)) = self.find_handler(module, method) {
            if let Some(bi) = boundary_idx {
                // Resumable handler: capture continuation
                let boundary = &self.handler_boundaries[bi];
                let bd_stack = boundary.stack_depth;
                let bd_frame = boundary.frame_depth;
                let bd_upvalue = boundary.upvalue_depth;
                let handler_depth = boundary.handler_stack_idx;
                let return_ip = boundary.return_ip;
                let original_base_slot_usize = boundary.original_base_slot;

                let resume_ip = ip as u32;
                let resume_chunk_idx = chunk_idx as u32;

                let args_start = self.stack.len() - n;

                // The frame that installed the handler is at index bd_frame - 1.
                let frame_idx_start = bd_frame.saturating_sub(1);

                // Sync current IP before capturing
                self.frames.last_mut().ip = ip as u32;
                self.frames.last_mut().chunk_idx = chunk_idx as u32;

                let stack_segment: Vec<NValue> =
                    self.stack[original_base_slot_usize..args_start].to_vec();

                let frame_segment: Vec<(u32, u32, u32, u32)> = (frame_idx_start
                    ..self.frames.len())
                    .map(|i| {
                        let f =
                            unsafe { *self.frames.frames.get_unchecked(i) };
                        (f.chunk_idx, f.ip, f.base_slot.raw(), f.upvalue_idx)
                    })
                    .collect();

                // NOW set the abort target on the frame that holds the handle block
                let abort_frame = unsafe { self.frames.frames.get_unchecked_mut(frame_idx_start) };
                if abort_frame.base_slot.is_cont() {
                    // This is a continuation frame! It's not allowed to execute past the handle block.
                    // If a sub-handler aborts, the continuation itself must abort and return the value.
                    // We point it to the chunk's guaranteed terminal Return opcode.
                    let chunk_len = chunks[abort_frame.chunk_idx as usize].code.len();
                    abort_frame.ip = (chunk_len - 1) as u32;
                } else {
                    abort_frame.ip = return_ip as u32;
                }
                // If there were nested frames, we abort all the way back to the boundary frame
                self.frames.len = bd_frame;

                let upvalue_segment: Vec<Vec<NValue>> = self.upvalue_stack
                    [bd_upvalue..]
                    .iter()
                    .map(|rc| (**rc).clone())
                    .collect();

                let args: Vec<NValue> =
                    self.stack.drain(args_start..).collect();

                let handler_stack_segment: Vec<HashMap<String, NValue>> =
                    self.handler_stack[handler_depth..].to_vec();
                let handler_boundary_segment: Vec<(usize, usize, usize, usize, usize, usize)> =
                    self.handler_boundaries[bi..]
                        .iter()
                        .map(|b| (
                            b.stack_depth - original_base_slot_usize,
                            b.frame_depth - frame_idx_start,
                            b.upvalue_depth,
                            b.handler_stack_idx,
                            b.return_ip,
                            b.original_base_slot - original_base_slot_usize,
                        ))
                        .collect();

                self.stack.truncate(bd_stack);
                // self.frames.len is already handled above
                self.upvalue_stack.truncate(bd_upvalue);
                self.handler_stack.truncate(handler_depth);
                self.handler_boundaries.truncate(bi);

                let cont = NValue::continuation(
                    stack_segment,
                    frame_segment,
                    upvalue_segment,
                    handler_depth,
                    original_base_slot_usize,
                    handler_stack_segment,
                    handler_boundary_segment,
                    resume_ip,
                    resume_chunk_idx,
                );

                for arg in args {
                    self.stack.push(arg);
                }
                self.stack.push(cont);

                let total_args = n + 1;
                if handler_fn.is_function() {
                    let fn_idx = handler_fn.as_function();
                    let new_base = PackedBase::from_slot(self.stack.len() - total_args);
                    self.frames.push(CallFrame {
                        chunk_idx: fn_idx as u32,
                        ip: 0,
                        base_slot: new_base,
                        upvalue_idx: u32::MAX,
                    });
                    return Ok(Some(DispatchResult::Restart {
                        ip: 0,
                        chunk_idx: fn_idx,
                        base_slot: new_base.slot(),
                    }));
                } else if let HeapObject::Closure {
                    chunk_idx: cidx,
                    upvalues,
                } = handler_fn.as_heap_ref()
                {
                    let fn_idx = *cidx;
                    let new_base = PackedBase::from_slot(self.stack.len() - total_args);
                    self.upvalue_stack
                        .push(Rc::new(upvalues.clone()));
                    let uv_idx =
                        (self.upvalue_stack.len() - 1) as u32;
                    self.frames.push(CallFrame {
                        chunk_idx: fn_idx as u32,
                        ip: 0,
                        base_slot: new_base,
                        upvalue_idx: uv_idx,
                    });
                    return Ok(Some(DispatchResult::Restart {
                        ip: 0,
                        chunk_idx: fn_idx,
                        base_slot: new_base.slot(),
                    }));
                }
            } else {
                // Tail-resumptive (no boundary): call handler inline
                let start = self.stack.len() - n;
                if handler_fn.is_function() {
                    let fn_idx = handler_fn.as_function();
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    let new_base = PackedBase::from_slot(start);
                    self.frames.push(CallFrame {
                        chunk_idx: fn_idx as u32,
                        ip: 0,
                        base_slot: new_base,
                        upvalue_idx: u32::MAX,
                    });
                    return Ok(Some(DispatchResult::Restart {
                        ip: 0,
                        chunk_idx: fn_idx,
                        base_slot: new_base.slot(),
                    }));
                } else if handler_fn.is_heap()
                    && let HeapObject::Closure {
                        chunk_idx: cidx,
                        upvalues,
                    } = handler_fn.as_heap_ref()
                {
                    let fn_idx = *cidx;
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    self.upvalue_stack
                        .push(Rc::new(upvalues.clone()));
                    let uv_idx = self.upvalue_stack.len() - 1;
                    let new_base = PackedBase::from_slot(start);
                    self.frames.push(CallFrame {
                        chunk_idx: fn_idx as u32,
                        ip: 0,
                        base_slot: new_base,
                        upvalue_idx: uv_idx as u32,
                    });
                    return Ok(Some(DispatchResult::Restart {
                        ip: 0,
                        chunk_idx: fn_idx,
                        base_slot: new_base.slot(),
                    }));
                }
            }
        }
        Ok(None)
    }

    /// Dispatch async fiber primitives: scope!, Scope.spawn!, Cell.await!, Cell.cancel!
    fn dispatch_async(
        &mut self,
        fn_id: u16,
        n: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        let name = self.natives.name(fn_id);
        let start = self.stack.len() - n;
        let args: Vec<NValue> = self.stack[start..].to_vec();
        self.stack.truncate(start);

        let result = match name {
            "scope!" | "scope" => {
                // scope!(body_closure)
                if args.is_empty() {
                    return Err(self.error("scope! requires a closure argument".into(), line, col));
                }
                fiber::exec_scope(self, args[0].clone(), chunks, line, col)?
            }
            "Scope.spawn!" | "Scope.spawn" => {
                // Scope.spawn!(scope_handle, body_closure)
                if args.len() < 2 {
                    return Err(self.error(
                        "Scope.spawn! requires scope handle and closure arguments".into(),
                        line,
                        col,
                    ));
                }
                let program = self
                    .program
                    .as_ref()
                    .ok_or_else(|| {
                        self.error(
                            "Scope.spawn! requires a program context (use execute_program_arc)"
                                .into(),
                            line,
                            col,
                        )
                    })?
                    .clone();
                fiber::exec_spawn(&args[0], args[1].clone(), program, line, col)?
            }
            "Cell.await!" | "Cell.await" => {
                // Cell.await!(cell)
                if args.is_empty() {
                    return Err(self.error("Cell.await! requires a Cell argument".into(), line, col));
                }
                fiber::exec_cell_await(&args[0], line, col)?
            }
            "Cell.cancel!" | "Cell.cancel" => {
                // Cell.cancel!(cell)
                if args.is_empty() {
                    return Err(
                        self.error("Cell.cancel! requires a Cell argument".into(), line, col)
                    );
                }
                fiber::exec_cell_cancel(&args[0], line, col)?
            }
            "Async.parallel!" | "Async.parallel" => {
                if args.is_empty() {
                    return Err(self.error("Async.parallel! requires a list of closures".into(), line, col));
                }
                let program = self.program.as_ref().ok_or_else(|| {
                    self.error("Async.parallel! requires a program context".into(), line, col)
                })?.clone();
                fiber::exec_parallel(&args[0], program, chunks, line, col)?
            }
            "Async.race!" | "Async.race" => {
                if args.is_empty() {
                    return Err(self.error("Async.race! requires a list of closures".into(), line, col));
                }
                let program = self.program.as_ref().ok_or_else(|| {
                    self.error("Async.race! requires a program context".into(), line, col)
                })?.clone();
                fiber::exec_race(&args[0], program, chunks, line, col)?
            }
            "Async.scatter_gather!" | "Async.scatter_gather" => {
                if args.len() < 2 {
                    return Err(self.error("Async.scatter_gather! requires a list of closures and an aggregator closure".into(), line, col));
                }
                let program = self.program.as_ref().ok_or_else(|| {
                    self.error("Async.scatter_gather! requires a program context".into(), line, col)
                })?.clone();
                fiber::exec_scatter_gather(&args[0], &args[1], program, chunks, line, col)?
            }
            "Channel.bounded" => {
                if args.is_empty() {
                    return Err(self.error("Channel.bounded requires a capacity argument".into(), line, col));
                }
                fiber::exec_channel_bounded(&args[0], line, col)?
            }
            "Channel.send!" | "Channel.send" => {
                if args.len() < 2 {
                    return Err(self.error("Channel.send! requires sender and value arguments".into(), line, col));
                }
                fiber::exec_channel_send(&args[0], &args[1], line, col)?
            }
            "Channel.recv!" | "Channel.recv" => {
                if args.is_empty() {
                    return Err(self.error("Channel.recv! requires a receiver argument".into(), line, col));
                }
                fiber::exec_channel_recv(&args[0], line, col)?
            }
            "Channel.close!" | "Channel.close" => {
                if args.is_empty() {
                    return Err(self.error("Channel.close! requires a sender argument".into(), line, col));
                }
                fiber::exec_channel_close(&args[0], line, col)?
            }
            _ => {
                return Err(self.error(format!("Unknown async primitive: {}", name), line, col));
            }
        };

        self.stack.push(result);
        Ok(())
    }
}
