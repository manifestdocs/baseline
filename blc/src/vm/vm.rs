use std::rc::Rc;

use super::chunk::{Chunk, Op, Program};
use super::natives::NativeRegistry;
use super::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

// ---------------------------------------------------------------------------
// VM Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct VmError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.col, self.message)
    }
}

// ---------------------------------------------------------------------------
// Call Frame
// ---------------------------------------------------------------------------

struct CallFrame {
    chunk_idx: usize,
    ip: usize,
    /// Where this frame's locals start on the value stack.
    base_slot: usize,
    /// Captured upvalues (for closures). Shared via Rc for O(1) clone.
    upvalues: Rc<Vec<Value>>,
}

// ---------------------------------------------------------------------------
// VM
// ---------------------------------------------------------------------------

pub struct Vm {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    natives: NativeRegistry,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(16),
            natives: NativeRegistry::new(),
        }
    }

    /// Get a reference to the native function registry (for compiler lookups).
    pub fn natives(&self) -> &NativeRegistry {
        &self.natives
    }

    /// Execute a single chunk of bytecode (backward-compatible API).
    pub fn execute(&mut self, chunk: &Chunk) -> Result<Value, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.frames.push(CallFrame {
            chunk_idx: 0,
            ip: 0,
            base_slot: 0,
            upvalues: Rc::new(Vec::new()),
        });
        self.run(std::slice::from_ref(chunk))
    }

    /// Execute a multi-chunk program starting at the entry point.
    pub fn execute_program(&mut self, program: &Program) -> Result<Value, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.frames.push(CallFrame {
            chunk_idx: program.entry,
            ip: 0,
            base_slot: 0,
            upvalues: Rc::new(Vec::new()),
        });
        self.run(&program.chunks)
    }

    /// Execute a specific chunk within a multi-chunk program.
    /// Used by the VM test runner to execute individual test expression chunks.
    pub fn execute_chunk_at(&mut self, chunks: &[Chunk], chunk_idx: usize) -> Result<Value, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.frames.push(CallFrame {
            chunk_idx,
            ip: 0,
            base_slot: 0,
            upvalues: Rc::new(Vec::new()),
        });
        self.run(chunks)
    }

    /// Main dispatch loop — shared by execute and execute_program.
    fn run(&mut self, chunks: &[Chunk]) -> Result<Value, VmError> {
        self.run_frames(chunks, 0)
    }

    /// Dispatch loop that stops when frame count drops to `base_depth`.
    /// Used by call_value to run a single function call without consuming outer frames.
    fn run_frames(&mut self, chunks: &[Chunk], base_depth: usize) -> Result<Value, VmError> {
        loop {
            let frame = self.frames.last().unwrap();
            let chunk = &chunks[frame.chunk_idx];
            let ip = frame.ip;

            if ip >= chunk.code.len() {
                return Ok(self.stack.pop().unwrap_or(Value::Unit));
            }

            let (line, col) = chunk.source_map[ip];
            let op = chunk.code[ip];

            // Advance IP
            self.frames.last_mut().unwrap().ip += 1;

            match op {
                Op::LoadConst(idx) => {
                    let chunk_idx = self.frames.last().unwrap().chunk_idx;
                    self.stack.push(chunks[chunk_idx].constants[idx as usize].clone());
                }

                Op::Add => {
                    let (b, a) = self.pop2(line, col)?;
                    let result = match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(*x as f64 + y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x + *y as f64),
                        _ => return Err(self.error(
                            format!("Cannot add {} and {}", a, b), line, col,
                        )),
                    };
                    self.stack.push(result);
                }

                Op::Sub => {
                    let (b, a) = self.pop2(line, col)?;
                    let result = match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(*x as f64 - y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x - *y as f64),
                        _ => return Err(self.error(
                            format!("Cannot subtract {} from {}", b, a), line, col,
                        )),
                    };
                    self.stack.push(result);
                }

                Op::Mul => {
                    let (b, a) = self.pop2(line, col)?;
                    let result = match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(*x as f64 * y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x * *y as f64),
                        _ => return Err(self.error(
                            format!("Cannot multiply {} and {}", a, b), line, col,
                        )),
                    };
                    self.stack.push(result);
                }

                Op::Div => {
                    let (b, a) = self.pop2(line, col)?;
                    let result = match (&a, &b) {
                        (Value::Int(_), Value::Int(0)) => {
                            return Err(self.error("Division by zero".into(), line, col));
                        }
                        (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(*x as f64 / y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x / *y as f64),
                        _ => return Err(self.error(
                            format!("Cannot divide {} by {}", a, b), line, col,
                        )),
                    };
                    self.stack.push(result);
                }

                Op::Mod => {
                    let (b, a) = self.pop2(line, col)?;
                    let result = match (&a, &b) {
                        (Value::Int(_), Value::Int(0)) => {
                            return Err(self.error("Modulo by zero".into(), line, col));
                        }
                        (Value::Int(x), Value::Int(y)) => Value::Int(x % y),
                        _ => return Err(self.error(
                            format!("Cannot modulo {} by {}", a, b), line, col,
                        )),
                    };
                    self.stack.push(result);
                }

                Op::Negate => {
                    let v = self.pop(line, col)?;
                    let result = match &v {
                        Value::Int(x) => Value::Int(-x),
                        Value::Float(x) => Value::Float(-x),
                        _ => return Err(self.error(
                            format!("Cannot negate {}", v), line, col,
                        )),
                    };
                    self.stack.push(result);
                }

                Op::Not => {
                    let v = self.pop(line, col)?;
                    self.stack.push(Value::Bool(!v.is_truthy()));
                }

                Op::Eq => {
                    let (b, a) = self.pop2(line, col)?;
                    self.stack.push(Value::Bool(a == b));
                }

                Op::Ne => {
                    let (b, a) = self.pop2(line, col)?;
                    self.stack.push(Value::Bool(a != b));
                }

                Op::Lt => {
                    let (b, a) = self.pop2(line, col)?;
                    self.stack.push(Value::Bool(self.compare_lt(&a, &b, line, col)?));
                }

                Op::Gt => {
                    let (b, a) = self.pop2(line, col)?;
                    self.stack.push(Value::Bool(self.compare_lt(&b, &a, line, col)?));
                }

                Op::Le => {
                    let (b, a) = self.pop2(line, col)?;
                    let lt = self.compare_lt(&a, &b, line, col)?;
                    self.stack.push(Value::Bool(lt || a == b));
                }

                Op::Ge => {
                    let (b, a) = self.pop2(line, col)?;
                    let lt = self.compare_lt(&b, &a, line, col)?;
                    self.stack.push(Value::Bool(lt || a == b));
                }

                Op::Concat => {
                    let (b, a) = self.pop2(line, col)?;
                    let result = Value::String(format!("{}{}", a, b).into());
                    self.stack.push(result);
                }

                Op::GetLocal(slot) => {
                    let base = self.frames.last().unwrap().base_slot;
                    let idx = base + slot as usize;
                    if idx >= self.stack.len() {
                        return Err(self.error(
                            format!("Invalid local slot {}", slot), line, col,
                        ));
                    }
                    self.stack.push(self.stack[idx].clone());
                }

                Op::SetLocal(slot) => {
                    let base = self.frames.last().unwrap().base_slot;
                    let idx = base + slot as usize;
                    let val = self.stack.last().ok_or_else(|| {
                        self.error("Stack underflow on SetLocal".into(), line, col)
                    })?.clone();
                    if idx >= self.stack.len() {
                        return Err(self.error(
                            format!("Invalid local slot {}", slot), line, col,
                        ));
                    }
                    self.stack[idx] = val;
                }

                Op::PopN(n) => {
                    let count = n as usize;
                    let new_len = self.stack.len().saturating_sub(count);
                    self.stack.truncate(new_len);
                }

                Op::CloseScope(n) => {
                    let count = n as usize;
                    if count > 0 && !self.stack.is_empty() {
                        let top = self.stack.pop().unwrap();
                        let new_len = self.stack.len().saturating_sub(count);
                        self.stack.truncate(new_len);
                        self.stack.push(top);
                    }
                }

                Op::Pop => {
                    self.pop(line, col)?;
                }

                Op::Jump(offset) => {
                    self.frames.last_mut().unwrap().ip += offset as usize;
                }

                Op::JumpIfFalse(offset) => {
                    let v = self.pop(line, col)?;
                    if !v.is_truthy() {
                        self.frames.last_mut().unwrap().ip += offset as usize;
                    }
                }

                Op::JumpIfTrue(offset) => {
                    let v = self.stack.last().ok_or_else(|| {
                        self.error("Stack underflow".into(), line, col)
                    })?;
                    if v.is_truthy() {
                        self.frames.last_mut().unwrap().ip += offset as usize;
                    }
                }

                Op::JumpBack(offset) => {
                    self.frames.last_mut().unwrap().ip -= offset as usize;
                }

                Op::MakeRange => {
                    let (end_val, start_val) = self.pop2(line, col)?;
                    match (&start_val, &end_val) {
                        (Value::Int(start), Value::Int(end)) => {
                            let list: Vec<Value> = (*start..*end)
                                .map(Value::Int)
                                .collect();
                            self.stack.push(Value::List(Rc::new(list)));
                        }
                        _ => return Err(self.error(
                            format!("Range requires integers, got {} and {}", start_val, end_val),
                            line, col,
                        )),
                    }
                }

                Op::ListGet => {
                    let (idx_val, list_val) = self.pop2(line, col)?;
                    match (&list_val, &idx_val) {
                        (Value::List(items), Value::Int(i)) => {
                            let idx = *i as usize;
                            if idx >= items.len() {
                                return Err(self.error(
                                    format!("Index {} out of bounds (len {})", idx, items.len()),
                                    line, col,
                                ));
                            }
                            self.stack.push(items[idx].clone());
                        }
                        _ => return Err(self.error(
                            format!("ListGet requires List and Int, got {} and {}", list_val, idx_val),
                            line, col,
                        )),
                    }
                }

                Op::ListLen => {
                    let list_val = self.pop(line, col)?;
                    match &list_val {
                        Value::List(items) => {
                            self.stack.push(Value::Int(items.len() as i64));
                        }
                        _ => return Err(self.error(
                            format!("ListLen requires List, got {}", list_val),
                            line, col,
                        )),
                    }
                }

                Op::MakeList(n) => {
                    let count = n as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<Value> = self.stack.drain(start..).collect();
                    self.stack.push(Value::List(Rc::new(items)));
                }

                Op::MakeRecord(n) => {
                    let count = n as usize;
                    let start = self.stack.len() - count * 2;
                    let pairs: Vec<Value> = self.stack.drain(start..).collect();
                    let mut fields = Vec::with_capacity(count);
                    for pair in pairs.chunks(2) {
                        if let Value::String(key) = &pair[0] {
                            fields.push((key.clone(), pair[1].clone()));
                        } else {
                            return Err(self.error(
                                format!("Record key must be String, got {}", pair[0]),
                                line, col,
                            ));
                        }
                    }
                    self.stack.push(Value::Record(Rc::new(fields)));
                }

                Op::GetField(name_idx) => {
                    let chunk_idx = self.frames.last().unwrap().chunk_idx;
                    let field_name = match &chunks[chunk_idx].constants[name_idx as usize] {
                        Value::String(s) => s.clone(),
                        _ => return Err(self.error("GetField constant must be String".into(), line, col)),
                    };
                    let record = self.pop(line, col)?;
                    match record {
                        Value::Record(fields) => {
                            match fields.iter().find(|(k, _)| *k == field_name) {
                                Some((_, v)) => self.stack.push(v.clone()),
                                None => return Err(self.error(
                                    format!("Record has no field '{}'", field_name),
                                    line, col,
                                )),
                            }
                        }
                        _ => return Err(self.error(
                            format!("Cannot access field '{}' on {}", field_name, record),
                            line, col,
                        )),
                    }
                }

                Op::MakeTuple(n) => {
                    let count = n as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<Value> = self.stack.drain(start..).collect();
                    self.stack.push(Value::Tuple(Rc::new(items)));
                }

                Op::MakeEnum(tag_idx) => {
                    let chunk_idx = self.frames.last().unwrap().chunk_idx;
                    let tag = match &chunks[chunk_idx].constants[tag_idx as usize] {
                        Value::String(s) => s.clone(),
                        _ => return Err(self.error("MakeEnum constant must be String".into(), line, col)),
                    };
                    let payload = self.pop(line, col)?;
                    self.stack.push(Value::Enum(tag, Rc::new(payload)));
                }

                Op::EnumTag => {
                    let val = self.pop(line, col)?;
                    match val {
                        Value::Enum(tag, _) => self.stack.push(Value::String(tag)),
                        _ => return Err(self.error(
                            format!("EnumTag requires Enum, got {}", val), line, col,
                        )),
                    }
                }

                Op::EnumPayload => {
                    let val = self.pop(line, col)?;
                    match val {
                        Value::Enum(_, payload) => self.stack.push((*payload).clone()),
                        _ => return Err(self.error(
                            format!("EnumPayload requires Enum, got {}", val), line, col,
                        )),
                    }
                }

                Op::UpdateRecord(n) => {
                    let count = n as usize;
                    // Pop N key-value pairs
                    let start = self.stack.len() - count * 2;
                    let updates: Vec<Value> = self.stack.drain(start..).collect();
                    // Pop base record
                    let base = self.pop(line, col)?;
                    match base {
                        Value::Record(fields_rc) => {
                            let mut fields = Rc::try_unwrap(fields_rc)
                                .unwrap_or_else(|rc| (*rc).clone());
                            for pair in updates.chunks(2) {
                                if let Value::String(key) = &pair[0] {
                                    if let Some(existing) = fields.iter_mut().find(|(k, _)| *k == *key) {
                                        existing.1 = pair[1].clone();
                                    } else {
                                        return Err(self.error(
                                            format!("Record has no field '{}'", key), line, col,
                                        ));
                                    }
                                }
                            }
                            self.stack.push(Value::Record(Rc::new(fields)));
                        }
                        _ => return Err(self.error(
                            format!("UpdateRecord requires Record, got {}", base), line, col,
                        )),
                    }
                }

                // -- Functions --

                Op::Call(arg_count) => {
                    if self.frames.len() >= MAX_CALL_DEPTH {
                        return Err(self.error(
                            "Stack overflow: maximum call depth exceeded".into(),
                            line, col,
                        ));
                    }
                    let n = arg_count as usize;
                    let func_pos = self.stack.len() - n - 1;
                    let func_val = self.stack[func_pos].clone();

                    match func_val {
                        Value::Function(chunk_idx) => {
                            self.frames.push(CallFrame {
                                chunk_idx,
                                ip: 0,
                                base_slot: func_pos + 1,
                                upvalues: Rc::new(Vec::new()),
                            });
                        }
                        Value::Closure { chunk_idx, upvalues } => {
                            self.frames.push(CallFrame {
                                chunk_idx,
                                ip: 0,
                                base_slot: func_pos + 1,
                                upvalues,
                            });
                        }
                        _ => return Err(self.error(
                            format!("Cannot call {}", func_val), line, col,
                        )),
                    }
                }

                Op::GetUpvalue(idx) => {
                    let frame = self.frames.last().unwrap();
                    let val = frame.upvalues[idx as usize].clone();
                    self.stack.push(val);
                }

                Op::MakeClosure(chunk_idx, upvalue_count) => {
                    let n = upvalue_count as usize;
                    let start = self.stack.len() - n;
                    let upvalues: Vec<Value> = self.stack.drain(start..).collect();
                    self.stack.push(Value::Closure {
                        chunk_idx: chunk_idx as usize,
                        upvalues: Rc::new(upvalues),
                    });
                }

                Op::CallNative(fn_id, arg_count) => {
                    let n = arg_count as usize;
                    if self.natives.is_hof(fn_id) {
                        self.dispatch_hof(fn_id, n, chunks, line, col)?;
                    } else {
                        let start = self.stack.len() - n;
                        let args: Vec<Value> = self.stack.drain(start..).collect();
                        let result = self.natives.call(fn_id, &args).map_err(|e| {
                            self.error(format!("{}: {}", self.natives.name(fn_id), e.0), line, col)
                        })?;
                        self.stack.push(result);
                    }
                }

                Op::Return => {
                    let result = self.stack.pop().unwrap_or(Value::Unit);
                    let frame = self.frames.pop().unwrap();

                    if self.frames.len() <= base_depth {
                        // Returned to base depth — nested call complete (or program done)
                        return Ok(result);
                    }

                    // Remove callee's locals + args + the function value
                    // base_slot points to first arg; func_value is at base_slot - 1
                    self.stack.truncate(frame.base_slot - 1);
                    self.stack.push(result);
                }
            }
        }
    }

    // -- Helpers --

    fn pop(&mut self, line: usize, col: usize) -> Result<Value, VmError> {
        self.stack.pop().ok_or_else(|| self.error("Stack underflow".into(), line, col))
    }

    fn pop2(&mut self, line: usize, col: usize) -> Result<(Value, Value), VmError> {
        let b = self.pop(line, col)?;
        let a = self.pop(line, col)?;
        Ok((b, a))
    }

    fn compare_lt(&self, a: &Value, b: &Value, line: usize, col: usize) -> Result<bool, VmError> {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => Ok(x < y),
            (Value::Float(x), Value::Float(y)) => Ok(x < y),
            (Value::Int(x), Value::Float(y)) => Ok((*x as f64) < *y),
            (Value::Float(x), Value::Int(y)) => Ok(*x < (*y as f64)),
            (Value::String(x), Value::String(y)) => Ok(x < y),
            _ => Err(self.error(
                format!("Cannot compare {} and {}", a, b), line, col,
            )),
        }
    }

    /// Execute a HOF native function (List.map, List.filter, etc.) by calling
    /// bytecode closures from within the VM loop.
    fn dispatch_hof(
        &mut self,
        fn_id: u16,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), VmError> {
        let name = self.natives.name(fn_id);
        match name {
            "List.map" => {
                // args: list, func
                if arg_count != 2 {
                    return Err(self.error("List.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                let items = match list {
                    Value::List(v) => v,
                    _ => return Err(self.error("List.map: first arg must be List".into(), line, col)),
                };
                let mut results = Vec::with_capacity(items.len());
                for item in items.iter() {
                    let result = self.call_value(&func, &[item.clone()], chunks, line, col)?;
                    results.push(result);
                }
                self.stack.push(Value::List(Rc::new(results)));
            }
            "List.filter" => {
                if arg_count != 2 {
                    return Err(self.error("List.filter: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                let items = match list {
                    Value::List(v) => v,
                    _ => return Err(self.error("List.filter: first arg must be List".into(), line, col)),
                };
                let mut results = Vec::new();
                for item in items.iter() {
                    let result = self.call_value(&func, &[item.clone()], chunks, line, col)?;
                    if result.is_truthy() {
                        results.push(item.clone());
                    }
                }
                self.stack.push(Value::List(Rc::new(results)));
            }
            "List.fold" => {
                // args: list, initial, func
                if arg_count != 3 {
                    return Err(self.error("List.fold: expected 3 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let initial = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                let items = match list {
                    Value::List(v) => v,
                    _ => return Err(self.error("List.fold: first arg must be List".into(), line, col)),
                };
                let mut acc = initial;
                for item in items.iter() {
                    acc = self.call_value(&func, &[acc, item.clone()], chunks, line, col)?;
                }
                self.stack.push(acc);
            }
            "List.find" => {
                if arg_count != 2 {
                    return Err(self.error("List.find: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                let items = match list {
                    Value::List(v) => v,
                    _ => return Err(self.error("List.find: first arg must be List".into(), line, col)),
                };
                let mut found = Value::Enum("None".into(), Rc::new(Value::Unit));
                for item in items.iter() {
                    let result = self.call_value(&func, &[item.clone()], chunks, line, col)?;
                    if result.is_truthy() {
                        found = Value::Enum("Some".into(), Rc::new(item.clone()));
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
                match opt {
                    Value::Enum(tag, payload) if *tag == *"Some" => {
                        let result = self.call_value(&func, &[(*payload).clone()], chunks, line, col)?;
                        self.stack.push(Value::Enum("Some".into(), Rc::new(result)));
                    }
                    Value::Enum(tag, _) if *tag == *"None" => {
                        self.stack.push(Value::Enum("None".into(), Rc::new(Value::Unit)));
                    }
                    _ => return Err(self.error("Option.map: first arg must be Option".into(), line, col)),
                }
            }
            "Result.map" => {
                if arg_count != 2 {
                    return Err(self.error("Result.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let res = self.pop(line, col)?;
                match res {
                    Value::Enum(tag, payload) if *tag == *"Ok" => {
                        let result = self.call_value(&func, &[(*payload).clone()], chunks, line, col)?;
                        self.stack.push(Value::Enum("Ok".into(), Rc::new(result)));
                    }
                    Value::Enum(tag, payload) if *tag == *"Err" => {
                        self.stack.push(Value::Enum("Err".into(), payload));
                    }
                    _ => return Err(self.error("Result.map: first arg must be Result".into(), line, col)),
                }
            }
            _ => {
                return Err(self.error(format!("Unknown HOF: {}", name), line, col));
            }
        }
        Ok(())
    }

    /// Call a bytecode function/closure with the given arguments.
    /// Uses run_frames with base_depth to only execute the callee without
    /// consuming outer frames. Cleans up the stack after execution.
    fn call_value(
        &mut self,
        func: &Value,
        args: &[Value],
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<Value, VmError> {
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(self.error(
                "Stack overflow: maximum call depth exceeded".into(),
                line, col,
            ));
        }
        let base_depth = self.frames.len();
        let stack_base = self.stack.len();

        let (chunk_idx, upvals) = match func {
            Value::Function(idx) => (*idx, Rc::new(Vec::new())),
            Value::Closure { chunk_idx, upvalues } => (*chunk_idx, upvalues.clone()),
            _ => return Err(self.error(format!("Cannot call {}", func), line, col)),
        };

        self.stack.push(func.clone());
        for arg in args {
            self.stack.push(arg.clone());
        }
        let base_slot = self.stack.len() - args.len();
        self.frames.push(CallFrame {
            chunk_idx,
            ip: 0,
            base_slot,
            upvalues: upvals,
        });

        let result = self.run_frames(chunks, base_depth)?;

        // Clean up: run_frames returns early at base_depth without stack cleanup,
        // so we restore the stack to its state before we pushed func+args.
        self.stack.truncate(stack_base);

        Ok(result)
    }

    fn error(&self, message: String, line: usize, col: usize) -> VmError {
        VmError { message, line, col }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn run_chunk(chunk: Chunk) -> Value {
        let mut vm = Vm::new();
        vm.execute(&chunk).expect("VM error")
    }

    fn run_chunk_err(chunk: Chunk) -> VmError {
        let mut vm = Vm::new();
        vm.execute(&chunk).expect_err("Expected VM error")
    }

    // -- Arithmetic --

    #[test]
    fn add_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(3));
        let b = c.add_constant(Value::Int(4));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Add, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    #[test]
    fn sub_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(10));
        let b = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Sub, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    #[test]
    fn mul_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(6));
        let b = c.add_constant(Value::Int(7));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Mul, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(42));
    }

    #[test]
    fn div_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(15));
        let b = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Div, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(5));
    }

    #[test]
    fn div_by_zero() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Int(0));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Div, 1, 0);
        let err = run_chunk_err(c);
        assert!(err.message.contains("Division by zero"));
    }

    #[test]
    fn modulo() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(10));
        let b = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Mod, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(1));
    }

    #[test]
    fn negate_int() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Negate, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(-5));
    }

    #[test]
    fn negate_float() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Float(3.14));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Negate, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Float(-3.14));
    }

    // -- Mixed int/float --

    #[test]
    fn add_int_float() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Float(2.5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Add, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Float(3.5));
    }

    // -- Comparisons --

    #[test]
    fn eq_same() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(5));
        let b = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Eq, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn lt_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(3));
        let b = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Lt, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn not_true() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Bool(true));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Not, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    // -- String concat --

    #[test]
    fn concat_strings() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::String("hello ".into()));
        let b = c.add_constant(Value::String("world".into()));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Concat, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::String("hello world".into()));
    }

    #[test]
    fn concat_int_to_string() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::String("count: ".into()));
        let b = c.add_constant(Value::Int(42));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Concat, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::String("count: 42".into()));
    }

    // -- Jumps --

    #[test]
    fn jump_unconditional() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Int(99));
        c.emit(Op::Jump(1), 1, 0);         // skip LoadConst(a)
        c.emit(Op::LoadConst(a), 1, 0);     // skipped
        c.emit(Op::LoadConst(b), 1, 0);     // executed
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(99));
    }

    #[test]
    fn jump_if_false_taken() {
        let mut c = Chunk::new();
        let f = c.add_constant(Value::Bool(false));
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Int(2));
        c.emit(Op::LoadConst(f), 1, 0);
        c.emit(Op::JumpIfFalse(1), 1, 0);   // condition false → jump
        c.emit(Op::LoadConst(a), 1, 0);      // skipped
        c.emit(Op::LoadConst(b), 1, 0);      // executed
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(2));
    }

    #[test]
    fn jump_if_false_not_taken() {
        let mut c = Chunk::new();
        let t = c.add_constant(Value::Bool(true));
        let a = c.add_constant(Value::Int(1));
        c.emit(Op::LoadConst(t), 1, 0);
        c.emit(Op::JumpIfFalse(1), 1, 0);   // condition true → no jump
        c.emit(Op::LoadConst(a), 1, 0);      // executed
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(1));
    }

    // -- Compound expression: 1 + 2 * 3 = 7 --

    #[test]
    fn order_of_operations() {
        let mut c = Chunk::new();
        let one = c.add_constant(Value::Int(1));
        let two = c.add_constant(Value::Int(2));
        let three = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(one), 1, 0);
        c.emit(Op::LoadConst(two), 1, 0);
        c.emit(Op::LoadConst(three), 1, 0);
        c.emit(Op::Mul, 1, 0);  // 2 * 3 = 6
        c.emit(Op::Add, 1, 0);  // 1 + 6 = 7
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    // -- Function calls --

    #[test]
    fn call_simple_function() {
        // Chunk 0 (caller): push Function(1), push 5, Call(1)
        let mut caller = Chunk::new();
        let func = caller.add_constant(Value::Function(1));
        let arg = caller.add_constant(Value::Int(5));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::Call(1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        // Chunk 1 (callee): GetLocal(0) * 2
        let mut callee = Chunk::new();
        let two = callee.add_constant(Value::Int(2));
        callee.emit(Op::GetLocal(0), 1, 0); // param x
        callee.emit(Op::LoadConst(two), 1, 0);
        callee.emit(Op::Mul, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
    }

    #[test]
    fn call_two_arg_function() {
        // Chunk 0: push Function(1), push 3, push 4, Call(2)
        let mut caller = Chunk::new();
        let func = caller.add_constant(Value::Function(1));
        let a = caller.add_constant(Value::Int(3));
        let b = caller.add_constant(Value::Int(4));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(a), 1, 0);
        caller.emit(Op::LoadConst(b), 1, 0);
        caller.emit(Op::Call(2), 1, 0);
        caller.emit(Op::Return, 1, 0);

        // Chunk 1: GetLocal(0) + GetLocal(1)
        let mut callee = Chunk::new();
        callee.emit(Op::GetLocal(0), 1, 0);
        callee.emit(Op::GetLocal(1), 1, 0);
        callee.emit(Op::Add, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(7));
    }

    #[test]
    fn nested_calls() {
        // Chunk 0 (entry): call add(double(3), 4)
        //   push add, push double, push 3, Call(1), push 4, Call(2)
        let mut entry = Chunk::new();
        let add_fn = entry.add_constant(Value::Function(1));
        let dbl_fn = entry.add_constant(Value::Function(2));
        let three = entry.add_constant(Value::Int(3));
        let four = entry.add_constant(Value::Int(4));
        entry.emit(Op::LoadConst(add_fn), 1, 0);  // push add
        entry.emit(Op::LoadConst(dbl_fn), 1, 0);  // push double
        entry.emit(Op::LoadConst(three), 1, 0);    // push 3
        entry.emit(Op::Call(1), 1, 0);              // double(3) = 6
        entry.emit(Op::LoadConst(four), 1, 0);     // push 4
        entry.emit(Op::Call(2), 1, 0);              // add(6, 4) = 10
        entry.emit(Op::Return, 1, 0);

        // Chunk 1 (add): a + b
        let mut add = Chunk::new();
        add.emit(Op::GetLocal(0), 1, 0);
        add.emit(Op::GetLocal(1), 1, 0);
        add.emit(Op::Add, 1, 0);
        add.emit(Op::Return, 1, 0);

        // Chunk 2 (double): x * 2
        let mut dbl = Chunk::new();
        let two = dbl.add_constant(Value::Int(2));
        dbl.emit(Op::GetLocal(0), 1, 0);
        dbl.emit(Op::LoadConst(two), 1, 0);
        dbl.emit(Op::Mul, 1, 0);
        dbl.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, add, dbl],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
    }

    #[test]
    fn get_field_missing_returns_error() {
        let mut c = Chunk::new();
        let key = c.add_constant(Value::String("x".into()));
        let val = c.add_constant(Value::Int(1));
        let bad_field = c.add_constant(Value::String("y".into()));
        c.emit(Op::LoadConst(key), 1, 0);
        c.emit(Op::LoadConst(val), 1, 0);
        c.emit(Op::MakeRecord(1), 1, 0);
        c.emit(Op::GetField(bad_field), 1, 0);
        c.emit(Op::Return, 1, 0);
        let err = run_chunk_err(c);
        assert!(err.message.contains("no field 'y'"), "got: {}", err.message);
    }

    #[test]
    fn stack_overflow_detected() {
        // Chunk 0 (entry): call chunk 1
        let mut entry = Chunk::new();
        let func = entry.add_constant(Value::Function(1));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::Call(0), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // Chunk 1: calls itself unconditionally (infinite recursion)
        let mut recurse = Chunk::new();
        let self_func = recurse.add_constant(Value::Function(1));
        recurse.emit(Op::LoadConst(self_func), 1, 0);
        recurse.emit(Op::Call(0), 1, 0);
        recurse.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, recurse],
            entry: 0,
        };
        let mut vm = Vm::new();
        let err = vm.execute_program(&program).expect_err("Expected stack overflow");
        assert!(err.message.contains("Stack overflow"), "got: {}", err.message);
    }
}
