use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::nvalue::{HeapObject, NValue};

use super::DispatchResult;

/// Arithmetic and logic opcodes: Add, Sub, Mul, Div, Mod, Negate, Not,
/// AddInt, SubInt, MulInt, DivInt, ModInt, GtInt, GeInt, Concat.
impl super::Vm {
    #[inline(always)]
    pub(crate) fn dispatch_arithmetic(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        ip: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::Add => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    self.stack
                        .push(NValue::int(a.as_int().wrapping_add(b.as_int())));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() + b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot add {} and {}", a, b), line, col));
                }
            }
            Op::Sub => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    self.stack
                        .push(NValue::int(a.as_int().wrapping_sub(b.as_int())));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() - b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot subtract {} from {}", b, a), line, col));
                }
            }
            Op::Mul => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    self.stack
                        .push(NValue::int(a.as_int().wrapping_mul(b.as_int())));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() * b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot multiply {} and {}", a, b), line, col));
                }
            }
            Op::Div => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    if b.as_int() == 0 {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("Division by zero".into(), line, col));
                    }
                    self.stack.push(NValue::int(a.as_int() / b.as_int()));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() / b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot divide {} by {}", a, b), line, col));
                }
            }
            Op::Mod => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    if b.as_int() == 0 {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("Modulo by zero".into(), line, col));
                    }
                    self.stack.push(NValue::int(a.as_int() % b.as_int()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot modulo {} by {}", a, b), line, col));
                }
            }
            Op::Negate => {
                let v = self.pop_fast();
                if v.is_int() {
                    self.stack.push(NValue::int(-v.as_int()));
                } else if v.is_float() {
                    self.stack.push(NValue::float(-v.as_float()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot negate {}", v), line, col));
                }
            }
            Op::AddInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::int(a.as_any_int().wrapping_add(b.as_any_int())));
            }
            Op::SubInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::int(a.as_any_int().wrapping_sub(b.as_any_int())));
            }
            Op::MulInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::int(a.as_any_int().wrapping_mul(b.as_any_int())));
            }
            Op::DivInt => {
                let (b, a) = self.pop2_fast();
                let bi = b.as_any_int();
                if bi == 0 {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error("Division by zero".into(), line, col));
                }
                self.stack.push(NValue::int(a.as_any_int() / bi));
            }
            Op::ModInt => {
                let (b, a) = self.pop2_fast();
                let bi = b.as_any_int();
                if bi == 0 {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error("Modulo by zero".into(), line, col));
                }
                self.stack.push(NValue::int(a.as_any_int() % bi));
            }
            Op::GtInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::bool(a.as_any_int() > b.as_any_int()));
            }
            Op::GeInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::bool(a.as_any_int() >= b.as_any_int()));
            }
            Op::Not => {
                let v = self.pop_fast();
                self.stack.push(NValue::bool(!v.is_truthy()));
            }
            Op::Concat => {
                let (b, a) = self.pop2_fast();
                // Fast path: both strings — avoid Display overhead
                if a.is_heap()
                    && b.is_heap()
                    && let (HeapObject::String(sa), HeapObject::String(sb)) =
                        (a.as_heap_ref(), b.as_heap_ref())
                {
                    let mut s = String::with_capacity(sa.len() + sb.len());
                    s.push_str(sa);
                    s.push_str(sb);
                    self.stack.push(NValue::string(s.into()));
                    return Ok(DispatchResult::Continue);
                }
                // Slow path: mixed types via Display
                let result = NValue::string(format!("{}{}", a, b).into());
                self.stack.push(result);
            }
            _ => unreachable!("dispatch_arithmetic called with non-arithmetic op"),
        }
        Ok(DispatchResult::Continue)
    }
}
