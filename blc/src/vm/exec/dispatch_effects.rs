use std::collections::HashMap;

use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::nvalue::{HeapObject, NValue};
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
        chunks: &[Chunk],
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
                            original_base_slot: self.frames.last().base_slot.slot(),
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

                if let Some(restart) = self.dispatch_handler_interception(effect_name, method_name, n, ip, chunk_idx, chunks)? {
                    return Ok(restart);
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

                let top_frame = self.frames.last();
                if top_frame.base_slot.is_cont() {
                    // This frame was the bottom-most frame of a resumed continuation.
                    // The handle block has finished — return the value to the resume() caller.
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

                    let base = frame.base_slot;
                    if base.has_func() {
                        self.stack.truncate(base.slot().saturating_sub(1));
                    } else {
                        self.stack.truncate(base.slot());
                    }
                    self.stack.push(result);

                    let caller = self.frames.last();
                    return Ok(DispatchResult::Restart {
                        ip: caller.ip as usize,
                        chunk_idx: caller.chunk_idx as usize,
                        base_slot: caller.base_slot.slot(),
                    });
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

                let base = frame.base_slot;
                if base.has_func() {
                    self.stack.truncate(base.slot().saturating_sub(1));
                } else {
                    self.stack.truncate(base.slot());
                }
                self.stack.push(result);

                let caller = self.frames.last();
                return Ok(DispatchResult::Restart {
                    ip: caller.ip as usize,
                    chunk_idx: caller.chunk_idx as usize,
                    base_slot: caller.base_slot.slot(),
                });
            }

            _ => unreachable!("dispatch_effects called with non-effect op"),
        }
        Ok(DispatchResult::Continue)
    }
}
