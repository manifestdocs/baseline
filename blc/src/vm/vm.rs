use super::chunk::{Chunk, Op};
use super::value::Value;

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
// VM
// ---------------------------------------------------------------------------

pub struct Vm {
    stack: Vec<Value>,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: Vec::with_capacity(256),
        }
    }

    /// Execute a chunk of bytecode, returning the final value.
    pub fn execute(&mut self, chunk: &Chunk) -> Result<Value, VmError> {
        let mut ip = 0;

        while ip < chunk.code.len() {
            let (line, col) = chunk.source_map[ip];
            let op = &chunk.code[ip];
            ip += 1;

            match op {
                Op::LoadConst(idx) => {
                    self.stack.push(chunk.constants[*idx as usize].clone());
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
                    let result = Value::String(format!("{}{}", a, b));
                    self.stack.push(result);
                }

                Op::Pop => {
                    self.pop(line, col)?;
                }

                Op::Jump(offset) => {
                    ip += *offset as usize;
                }

                Op::JumpIfFalse(offset) => {
                    let v = self.pop(line, col)?;
                    if !v.is_truthy() {
                        ip += *offset as usize;
                    }
                }

                Op::JumpIfTrue(offset) => {
                    // Peek — don't pop (for short-circuit: leave value on stack)
                    let v = self.stack.last().ok_or_else(|| {
                        self.error("Stack underflow".into(), line, col)
                    })?;
                    if v.is_truthy() {
                        ip += *offset as usize;
                    }
                }

                Op::Return => {
                    return Ok(self.stack.pop().unwrap_or(Value::Unit));
                }
            }
        }

        // Implicit return of whatever's on the stack
        Ok(self.stack.pop().unwrap_or(Value::Unit))
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
        // Bytecode for 1 + (2 * 3) — compiler respects precedence
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
}
