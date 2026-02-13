use std::collections::HashMap;
use std::rc::Rc;

use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::nvalue::{HeapObject, NValue};

use super::frame::{CallFrame, FRAME_HAS_FUNC};
use super::DispatchResult;

/// Effect handler and control flow opcodes: PushHandler, PushResumableHandler,
/// PerformEffect, PopHandler, Halt, Return.
impl super::Vm {
    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn dispatch_effects(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        _chunks: &[Chunk],
        ip: usize,
        chunk_idx: usize,
        _base_slot: usize,
        base_depth: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::PushHandler => {
                let handler_record = self.stack.pop().unwrap();
                match handler_record.as_heap_ref() {
                    HeapObject::Record(fields) => {
                        let mut map = HashMap::new();
                        for (k, v) in fields {
                            map.insert(k.to_string(), v.clone());
                        }
                        self.handler_stack.push(map);
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "PushHandler: expected Record".to_string(),
                            line,
                            col,
                        ));
                    }
                }
            }

            Op::PushResumableHandler(skip_offset) => {
                let handler_record = self.stack.pop().unwrap();
                match handler_record.as_heap_ref() {
                    HeapObject::Record(fields) => {
                        let mut map = HashMap::new();
                        for (k, v) in fields {
                            map.insert(k.to_string(), v.clone());
                        }
                        let handler_stack_idx = self.handler_stack.len();
                        self.handler_stack.push(map);
                        let return_ip = (ip - 1) + 1 + *skip_offset as usize;
                        self.handler_boundaries.push(super::HandlerBoundary {
                            stack_depth: self.stack.len(),
                            frame_depth: self.frames.len(),
                            upvalue_depth: self.upvalue_stack.len(),
                            handler_stack_idx,
                            return_ip,
                        });
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "PushResumableHandler: expected Record".to_string(),
                            line,
                            col,
                        ));
                    }
                }
            }

            Op::PerformEffect(name_idx, arg_count) => {
                let key = match chunk.constants[*name_idx as usize].as_string() {
                    Some(s) => s.to_string(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "PerformEffect: expected string key".to_string(),
                            line,
                            col,
                        ));
                    }
                };
                let n = *arg_count as usize;
                eprintln!("[DEBUG] PerformEffect: key={}, n={}", key, n);

                let (effect_name, method_name) = if let Some(dot) = key.find('.') {
                    (&key[..dot], &key[dot + 1..])
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("PerformEffect: invalid key '{}'", key),
                        line,
                        col,
                    ));
                };

                // Search handler_stack in reverse for matching effect
                let mut found_handler = None;
                let mut found_boundary_idx = None;
                for (hs_idx, frame) in self.handler_stack.iter().enumerate().rev() {
                    if let Some(handler_record) = frame.get(effect_name) {
                        if let HeapObject::Record(fields) = handler_record.as_heap_ref() {
                            for (k, v) in fields {
                                if k.as_ref() == method_name {
                                    found_handler = Some(v.clone());
                                    for (bi, b) in self.handler_boundaries.iter().enumerate() {
                                        if b.handler_stack_idx == hs_idx {
                                            found_boundary_idx = Some(bi);
                                            break;
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                        break;
                    }
                }

                eprintln!("[DEBUG] PerformEffect: handler_stack.len={}, boundaries.len={}", self.handler_stack.len(), self.handler_boundaries.len());
                for (i, b) in self.handler_boundaries.iter().enumerate() {
                    eprintln!("[DEBUG]   boundary[{}]: handler_stack_idx={}", i, b.handler_stack_idx);
                }
                eprintln!("[DEBUG]   found_handler={}, found_boundary_idx={:?}", found_handler.is_some(), found_boundary_idx);

                if let Some(handler_fn) = found_handler {
                    if let Some(bi) = found_boundary_idx {
                        // Resumable handler: capture continuation
                        eprintln!("[DEBUG] PerformEffect: resumable handler found, boundary={bi}");
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
                                let f = unsafe { *self.frames.frames.get_unchecked(i) };
                                (f.chunk_idx, f.ip, f.base_slot, f.upvalue_idx)
                            })
                            .collect();

                        let upvalue_segment: Vec<Vec<NValue>> = self.upvalue_stack
                            [bd_upvalue..]
                            .iter()
                            .map(|rc| (**rc).clone())
                            .collect();

                        let args: Vec<NValue> = self.stack.drain(args_start..).collect();

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
                        eprintln!("[DEBUG] PerformEffect: calling handler with {} total args, stack.len={}", total_args, self.stack.len());
                        if handler_fn.is_function() {
                            let fn_idx = handler_fn.as_function();
                            let new_base = (self.stack.len() - total_args) as u32;
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
                            let new_base = (self.stack.len() - total_args) as u32;
                            self.upvalue_stack.push(Rc::new(upvalues.clone()));
                            let uv_idx = (self.upvalue_stack.len() - 1) as u32;
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
                        } else if let HeapObject::Closure {
                            chunk_idx: cidx,
                            upvalues,
                        } = handler_fn.as_heap_ref()
                        {
                            let fn_idx = *cidx;
                            let caller = self.frames.last_mut();
                            caller.ip = ip as u32;
                            caller.chunk_idx = chunk_idx as u32;
                            self.upvalue_stack.push(Rc::new(upvalues.clone()));
                            let uv_idx = (self.upvalue_stack.len() - 1) as u32;
                            self.frames.push(CallFrame {
                                chunk_idx: fn_idx as u32,
                                ip: 0,
                                base_slot: start as u32,
                                upvalue_idx: uv_idx,
                            });
                            return Ok(DispatchResult::Restart {
                                ip: 0,
                                chunk_idx: fn_idx,
                                base_slot: start,
                            });
                        }
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Unhandled effect: {}", key),
                        line,
                        col,
                    ));
                }
            }

            Op::PopHandler => {
                if self.handler_stack.is_empty() {
                    // Handler was already consumed — nothing to pop.
                } else {
                    let hs_idx = self.handler_stack.len() - 1;
                    if let Some(bi) = self.handler_boundaries.last()
                        && bi.handler_stack_idx == hs_idx
                    {
                        self.handler_boundaries.pop();
                    }
                    self.handler_stack.pop();
                }
            }

            Op::Halt(idx) => {
                let msg = match chunk.constants[*idx as usize].as_string() {
                    Some(s) => s.to_string(),
                    None => "Runtime error".to_string(),
                };
                let (line, col) = chunk.source_map[ip - 1];
                return Err(self.error(msg, line, col));
            }

            Op::Return => {
                let result = self.stack.pop().unwrap_or_else(NValue::unit);
                let frame = self.frames.pop();

                if frame.upvalue_idx != u32::MAX
                    && frame.upvalue_idx as usize == self.upvalue_stack.len() - 1
                {
                    self.upvalue_stack.pop();
                }

                if self.frames.len() <= base_depth {
                    return Ok(DispatchResult::Return(result));
                }

                let raw_base = frame.base_slot;
                let has_func = raw_base & FRAME_HAS_FUNC != 0;
                let frame_base = (raw_base & !FRAME_HAS_FUNC) as usize;
                if has_func {
                    self.stack.truncate(frame_base - 1);
                } else {
                    self.stack.truncate(frame_base);
                }
                self.stack.push(result);

                let caller = self.frames.last();
                return Ok(DispatchResult::Restart {
                    ip: caller.ip as usize,
                    chunk_idx: caller.chunk_idx as usize,
                    base_slot: (caller.base_slot & !FRAME_HAS_FUNC) as usize,
                });
            }

            _ => unreachable!("dispatch_effects called with non-effect op"),
        }
        Ok(DispatchResult::Continue)
    }
}
