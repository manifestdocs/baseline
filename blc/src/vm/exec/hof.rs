use std::rc::Rc;

use crate::vm::chunk::{Chunk, CompileError};
use crate::vm::nvalue::{HeapObject, NValue};

use super::frame::{CallFrame, FRAME_HAS_FUNC};
use super::MAX_CALL_DEPTH;

impl super::Vm {
    #[inline(always)]
    pub(crate) fn compare_lt(&self, a: &NValue, b: &NValue) -> Option<bool> {
        if a.is_int() && b.is_int() {
            Some(a.as_int() < b.as_int())
        } else if a.is_number() && b.is_number() {
            Some(a.as_f64() < b.as_f64())
        } else if a.is_heap() && b.is_heap() {
            match (a.as_heap_ref(), b.as_heap_ref()) {
                (HeapObject::String(x), HeapObject::String(y)) => Some(x < y),
                _ => None,
            }
        } else {
            None
        }
    }

    pub(crate) fn pop(&mut self, line: usize, col: usize) -> Result<NValue, CompileError> {
        self.stack
            .pop()
            .ok_or_else(|| self.error("Stack underflow".into(), line, col))
    }

    /// Execute a HOF native function by calling bytecode closures from within the VM loop.
    pub(crate) fn dispatch_hof(
        &mut self,
        fn_id: u16,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        let name = self.natives.name(fn_id);
        match name {
            "List.map" => {
                if arg_count != 2 {
                    return Err(self.error("List.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error("List.map: first arg must be List".into(), line, col));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.map: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut results = Vec::with_capacity(items.len());
                for item in &items {
                    let result =
                        self.call_nvalue(&func, std::slice::from_ref(item), chunks, line, col)?;
                    results.push(result);
                }
                self.stack.push(NValue::list(results));
            }
            "List.filter" => {
                if arg_count != 2 {
                    return Err(self.error("List.filter: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error(
                        "List.filter: first arg must be List".into(),
                        line,
                        col,
                    ));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.filter: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut results = Vec::new();
                for item in &items {
                    let result =
                        self.call_nvalue(&func, std::slice::from_ref(item), chunks, line, col)?;
                    if result.is_truthy() {
                        results.push(item.clone());
                    }
                }
                self.stack.push(NValue::list(results));
            }
            "List.fold" => {
                if arg_count != 3 {
                    return Err(self.error("List.fold: expected 3 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let initial = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error("List.fold: first arg must be List".into(), line, col));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.fold: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut acc = initial;
                for item in &items {
                    acc = self.call_nvalue(&func, &[acc, item.clone()], chunks, line, col)?;
                }
                self.stack.push(acc);
            }
            "List.find" => {
                if arg_count != 2 {
                    return Err(self.error("List.find: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error("List.find: first arg must be List".into(), line, col));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.find: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut found = NValue::enum_val("None".into(), NValue::unit());
                for item in &items {
                    let result =
                        self.call_nvalue(&func, std::slice::from_ref(item), chunks, line, col)?;
                    if result.is_truthy() {
                        found = NValue::enum_val("Some".into(), item.clone());
                        break;
                    }
                }
                self.stack.push(found);
            }
            "Option.map" => {
                if arg_count != 2 {
                    return Err(self.error("Option.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let opt = self.pop(line, col)?;
                if !opt.is_heap() {
                    return Err(self.error(
                        "Option.map: first arg must be Option".into(),
                        line,
                        col,
                    ));
                }
                match opt.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Some" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        self.stack.push(NValue::enum_val("Some".into(), result));
                    }
                    HeapObject::Enum { tag, .. } if &**tag == "None" => {
                        self.stack
                            .push(NValue::enum_val("None".into(), NValue::unit()));
                    }
                    _ => {
                        return Err(self.error(
                            "Option.map: first arg must be Option".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            "Result.map" => {
                if arg_count != 2 {
                    return Err(self.error("Result.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let res = self.pop(line, col)?;
                if !res.is_heap() {
                    return Err(self.error(
                        "Result.map: first arg must be Result".into(),
                        line,
                        col,
                    ));
                }
                match res.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        self.stack.push(NValue::enum_val("Ok".into(), result));
                    }
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                        self.stack
                            .push(NValue::enum_val("Err".into(), payload.clone()));
                    }
                    _ => {
                        return Err(self.error(
                            "Result.map: first arg must be Result".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            "Option.flat_map" => {
                if arg_count != 2 {
                    return Err(self.error(
                        "Option.flat_map: expected 2 arguments".into(),
                        line,
                        col,
                    ));
                }
                let func = self.pop(line, col)?;
                let opt = self.pop(line, col)?;
                if !opt.is_heap() {
                    return Err(self.error(
                        "Option.flat_map: first arg must be Option".into(),
                        line,
                        col,
                    ));
                }
                match opt.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Some" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        // flat_map: function returns Option directly, no wrapping
                        self.stack.push(result);
                    }
                    HeapObject::Enum { tag, .. } if &**tag == "None" => {
                        self.stack
                            .push(NValue::enum_val("None".into(), NValue::unit()));
                    }
                    _ => {
                        return Err(self.error(
                            "Option.flat_map: first arg must be Option".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            "Result.and_then" => {
                if arg_count != 2 {
                    return Err(self.error(
                        "Result.and_then: expected 2 arguments".into(),
                        line,
                        col,
                    ));
                }
                let func = self.pop(line, col)?;
                let res = self.pop(line, col)?;
                if !res.is_heap() {
                    return Err(self.error(
                        "Result.and_then: first arg must be Result".into(),
                        line,
                        col,
                    ));
                }
                match res.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        // and_then: function returns Result directly, no wrapping
                        self.stack.push(result);
                    }
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                        self.stack
                            .push(NValue::enum_val("Err".into(), payload.clone()));
                    }
                    _ => {
                        return Err(self.error(
                            "Result.and_then: first arg must be Result".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            "Result.map_err" => {
                if arg_count != 2 {
                    return Err(self.error(
                        "Result.map_err: expected 2 arguments".into(),
                        line,
                        col,
                    ));
                }
                let func = self.pop(line, col)?;
                let res = self.pop(line, col)?;
                if !res.is_heap() {
                    return Err(self.error(
                        "Result.map_err: first arg must be Result".into(),
                        line,
                        col,
                    ));
                }
                match res.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        self.stack.push(NValue::enum_val("Err".into(), result));
                    }
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                        self.stack
                            .push(NValue::enum_val("Ok".into(), payload.clone()));
                    }
                    _ => {
                        return Err(self.error(
                            "Result.map_err: first arg must be Result".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            "Fs.with_file!" | "Fs.with_file" => {
                if arg_count != 2 {
                    return Err(self.error(
                        "Fs.with_file!: expected 2 arguments".into(),
                        line,
                        col,
                    ));
                }
                let func = self.pop(line, col)?;
                let path_val = self.pop(line, col)?;
                let path_str = if path_val.is_heap() {
                    match path_val.as_heap_ref() {
                        HeapObject::String(s) => s.clone(),
                        _ => {
                            return Err(self.error(
                                "Fs.with_file!: first arg must be String".into(),
                                line,
                                col,
                            ));
                        }
                    }
                } else {
                    return Err(self.error(
                        "Fs.with_file!: first arg must be String".into(),
                        line,
                        col,
                    ));
                };

                // Open file for the duration of the scope
                let _file = std::fs::File::open(path_str.as_ref()).map_err(|e| {
                    self.error(
                        format!("Fs.with_file!: failed to open file: {}", e),
                        line,
                        col,
                    )
                })?;

                // Call the closure with the path as the scoped handle
                let result = self.call_nvalue(&func, &[path_val], chunks, line, col)?;

                // File is dropped here (Rust's Drop guarantees cleanup)
                self.stack.push(result);
            }
            _ => {
                return Err(self.error(format!("Unknown HOF: {}", name), line, col));
            }
        }
        Ok(())
    }

    /// Call a bytecode function/closure with given arguments (NValue version).
    ///
    /// This is the main entry point for invoking Baseline functions from Rust.
    pub fn call_nvalue(
        &mut self,
        func: &NValue,
        args: &[NValue],
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, CompileError> {
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(self.error(
                "Stack overflow: maximum call depth exceeded".into(),
                line,
                col,
            ));
        }
        let base_depth = self.frames.len();
        let stack_base = self.stack.len();

        // Intercept NativeMwNext: when middleware calls next(req), dispatch
        // to the remaining middleware chain instead of bytecode execution.
        if func.is_heap()
            && let HeapObject::NativeMwNext {
                handler,
                remaining_mw,
            } = func.as_heap_ref()
        {
            let handler = handler.clone();
            let remaining_mw = remaining_mw.clone();
            return self.call_mw_next(&handler, &remaining_mw, args, chunks, line, col);
        }

        let (ci, uv_idx) = if func.is_function() {
            (func.as_function(), u32::MAX)
        } else if func.is_heap() {
            if let HeapObject::Closure {
                chunk_idx,
                upvalues,
            } = func.as_heap_ref()
            {
                let idx = self.upvalue_stack.len() as u32;
                self.upvalue_stack.push(Rc::new(upvalues.clone()));
                (*chunk_idx, idx)
            } else {
                return Err(self.error(format!("Cannot call {}", func), line, col));
            }
        } else {
            return Err(self.error(format!("Cannot call {}", func), line, col));
        };

        self.stack.push(func.clone());
        for arg in args {
            self.stack.push(arg.clone());
        }
        let bs = self.stack.len() - args.len();
        self.frames.push(CallFrame {
            chunk_idx: ci as u32,
            ip: 0,
            // Bit 31 set: function value sits at base_slot - 1
            base_slot: bs as u32 | FRAME_HAS_FUNC,
            upvalue_idx: uv_idx,
        });

        let result = self.run_frames(chunks, base_depth)?;
        self.stack.truncate(stack_base);
        Ok(result)
    }
}
