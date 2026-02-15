use std::collections::HashMap;
use std::rc::Rc;

use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::fiber;
use crate::vm::nvalue::{HeapObject, NValue};

use super::frame::{CallFrame, FRAME_HAS_FUNC};
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
                let func = unsafe { self.stack.get_unchecked(func_pos) };

                // Check for continuation call first
                if func.is_continuation() {
                    let (ss, fs, us, hs, hbs, r_ip, r_ci) = match func.as_heap_ref() {
                        HeapObject::Continuation {
                            stack_segment,
                            frame_segment,
                            upvalue_segment,
                            handler_stack_segment,
                            handler_boundary_segment,
                            resume_ip,
                            resume_chunk_idx,
                            ..
                        } => (
                            stack_segment.clone(),
                            frame_segment.clone(),
                            upvalue_segment.clone(),
                            handler_stack_segment.clone(),
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

                    let handler_frame = self.frames.pop();
                    let raw_base = handler_frame.base_slot;
                    let has_func = raw_base & FRAME_HAS_FUNC != 0;
                    let handler_base = (raw_base & !FRAME_HAS_FUNC) as usize;
                    if has_func {
                        self.stack.truncate(handler_base - 1);
                    } else {
                        self.stack.truncate(handler_base);
                    }

                    if handler_frame.upvalue_idx != u32::MAX
                        && handler_frame.upvalue_idx as usize == self.upvalue_stack.len() - 1
                    {
                        self.upvalue_stack.pop();
                    }

                    self.stack.extend(ss);

                    for &(ci, fip, fbs, fuv) in &fs {
                        self.frames.push(CallFrame {
                            chunk_idx: ci,
                            ip: fip,
                            base_slot: fbs,
                            upvalue_idx: fuv,
                        });
                    }

                    for uvs in us {
                        self.upvalue_stack.push(Rc::new(uvs));
                    }

                    for h in hs {
                        self.handler_stack.push(h);
                    }
                    for (sd, fd, ud, hsi, rip) in hbs {
                        self.handler_boundaries.push(super::HandlerBoundary {
                            stack_depth: sd,
                            frame_depth: fd,
                            upvalue_depth: ud,
                            handler_stack_idx: hsi,
                            return_ip: rip,
                        });
                    }

                    self.stack.push(resume_value);

                    let new_ip = r_ip as usize;
                    let new_ci = r_ci as usize;
                    let top = self.frames.last();
                    let new_bs = (top.base_slot & !FRAME_HAS_FUNC) as usize;
                    return Ok(DispatchResult::Restart {
                        ip: new_ip,
                        chunk_idx: new_ci,
                        base_slot: new_bs,
                    });
                }

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

                let new_base = (func_pos + 1) as u32;
                self.frames.push(CallFrame {
                    chunk_idx: new_chunk_idx as u32,
                    ip: 0,
                    base_slot: new_base | FRAME_HAS_FUNC,
                    upvalue_idx: new_upvalue_idx,
                });
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx: new_chunk_idx,
                    base_slot: new_base as usize,
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
                let new_base = (self.stack.len() - n) as u32;

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
                    base_slot: new_base as usize,
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

                // Effect handler interception
                if !self.handler_stack.is_empty() {
                    let name = self.natives.name(*fn_id);
                    if let Some(dot) = name.find('.') {
                        let module = &name[..dot];
                        let method = name[dot + 1..]
                            .strip_suffix('!')
                            .unwrap_or(&name[dot + 1..]);
                        if let Some((handler_fn, boundary_idx)) =
                            self.find_handler(module, method)
                        {
                            if let Some(bi) = boundary_idx {
                                // Resumable handler: capture continuation
                                let boundary = &self.handler_boundaries[bi];
                                let bd_stack = boundary.stack_depth;
                                let bd_frame = boundary.frame_depth;
                                let bd_upvalue = boundary.upvalue_depth;
                                let handler_depth = boundary.handler_stack_idx;
                                let return_ip = boundary.return_ip;

                                let resume_ip = ip as u32;
                                let resume_chunk_idx = chunk_idx as u32;

                                let caller = self.frames.last_mut();
                                caller.ip = return_ip as u32;
                                caller.chunk_idx = chunk_idx as u32;

                                let args_start = self.stack.len() - n;
                                let stack_segment: Vec<NValue> =
                                    self.stack[bd_stack..args_start].to_vec();

                                let frame_segment: Vec<(u32, u32, u32, u32)> = (bd_frame
                                    ..self.frames.len())
                                    .map(|i| {
                                        let f =
                                            unsafe { *self.frames.frames.get_unchecked(i) };
                                        (f.chunk_idx, f.ip, f.base_slot, f.upvalue_idx)
                                    })
                                    .collect();

                                let upvalue_segment: Vec<Vec<NValue>> = self.upvalue_stack
                                    [bd_upvalue..]
                                    .iter()
                                    .map(|rc| (**rc).clone())
                                    .collect();

                                let args: Vec<NValue> =
                                    self.stack.drain(args_start..).collect();

                                let handler_stack_segment: Vec<HashMap<String, NValue>> =
                                    self.handler_stack[handler_depth..].to_vec();
                                let handler_boundary_segment: Vec<(usize, usize, usize, usize, usize)> =
                                    self.handler_boundaries[bi..]
                                        .iter()
                                        .map(|b| (b.stack_depth, b.frame_depth, b.upvalue_depth, b.handler_stack_idx, b.return_ip))
                                        .collect();

                                self.stack.truncate(bd_stack);
                                self.frames.len = bd_frame;
                                self.upvalue_stack.truncate(bd_upvalue);
                                self.handler_stack.truncate(handler_depth);
                                self.handler_boundaries.truncate(bi);

                                let cont = NValue::continuation(
                                    stack_segment,
                                    frame_segment,
                                    upvalue_segment,
                                    handler_depth,
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
                                    let new_base =
                                        (self.stack.len() - total_args) as u32;
                                    self.frames.push(CallFrame {
                                        chunk_idx: fn_idx as u32,
                                        ip: 0,
                                        base_slot: new_base,
                                        upvalue_idx: u32::MAX,
                                    });
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: new_base as usize,
                                    });
                                } else if let HeapObject::Closure {
                                    chunk_idx: cidx,
                                    upvalues,
                                } = handler_fn.as_heap_ref()
                                {
                                    let fn_idx = *cidx;
                                    let new_base =
                                        (self.stack.len() - total_args) as u32;
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
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: new_base as usize,
                                    });
                                }
                            } else {
                                // Tail-resumptive (no boundary): call handler inline
                                let start = self.stack.len() - n;
                                if handler_fn.is_function() {
                                    let fn_idx = handler_fn.as_function();
                                    let caller = self.frames.last_mut();
                                    caller.ip = ip as u32;
                                    caller.chunk_idx = chunk_idx as u32;
                                    self.frames.push(CallFrame {
                                        chunk_idx: fn_idx as u32,
                                        ip: 0,
                                        base_slot: start as u32,
                                        upvalue_idx: u32::MAX,
                                    });
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: start,
                                    });
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
                                    self.frames.push(CallFrame {
                                        chunk_idx: fn_idx as u32,
                                        ip: 0,
                                        base_slot: start as u32,
                                        upvalue_idx: uv_idx as u32,
                                    });
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: start,
                                    });
                                }
                            }
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
            _ => {
                return Err(self.error(format!("Unknown async primitive: {}", name), line, col));
            }
        };

        self.stack.push(result);
        Ok(())
    }
}
