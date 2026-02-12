use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::nvalue::NValue;

use super::DispatchResult;

/// Comparison opcodes: Eq, Ne, Lt, Gt, Le, Ge, LtInt, LeInt.
impl super::Vm {
    #[inline(always)]
    pub(crate) fn dispatch_comparison(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        ip: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::Eq => {
                let (b, a) = self.pop2_fast();
                self.stack.push(NValue::bool(a == b));
            }
            Op::Ne => {
                let (b, a) = self.pop2_fast();
                self.stack.push(NValue::bool(a != b));
            }
            Op::Lt => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&a, &b) {
                    Some(result) => self.stack.push(NValue::bool(result)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::Gt => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&b, &a) {
                    Some(result) => self.stack.push(NValue::bool(result)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::Le => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&a, &b) {
                    Some(lt) => self.stack.push(NValue::bool(lt || a == b)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::Ge => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&b, &a) {
                    Some(lt) => self.stack.push(NValue::bool(lt || a == b)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::LtInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::bool(a.as_any_int() < b.as_any_int()));
            }
            Op::LeInt => {
                let (b, a) = self.pop2_fast();
                self.stack
                    .push(NValue::bool(a.as_any_int() <= b.as_any_int()));
            }
            _ => unreachable!("dispatch_comparison called with non-comparison op"),
        }
        Ok(DispatchResult::Continue)
    }
}
