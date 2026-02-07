use std::fmt;
use std::rc::Rc;

/// Shared string type for cheap cloning.
pub type RcStr = Rc<str>;

// ---------------------------------------------------------------------------
// Value
// ---------------------------------------------------------------------------

/// Lifetime-free value type for the bytecode VM.
/// Functions and closures are represented as bytecode offsets, not tree-sitter nodes.
/// Heap-allocated variants use Rc for O(1) clone on stack operations.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(RcStr),
    Bool(bool),
    Unit,
    List(Rc<Vec<Value>>),
    /// Ordered key-value pairs. Keys are shared strings (field names).
    Record(Rc<Vec<(RcStr, Value)>>),
    /// Positional tuple.
    Tuple(Rc<Vec<Value>>),
    /// Tagged enum variant: (tag_name, optional payload).
    Enum(RcStr, Rc<Value>),
    /// Named struct: type name + ordered key-value fields (e.g., Point { x: 1, y: 2 }).
    Struct(RcStr, Rc<Vec<(RcStr, Value)>>),
    /// A compiled function — index into Program.chunks.
    Function(usize),
    /// A closure — function + captured upvalues.
    Closure {
        chunk_idx: usize,
        upvalues: Rc<Vec<Value>>,
    },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
            Value::List(vals) => {
                let s = vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "[{}]", s)
            }
            Value::Record(fields) => {
                let s = fields.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<_>>().join(", ");
                write!(f, "{{ {} }}", s)
            }
            Value::Tuple(vals) => {
                let s = vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({})", s)
            }
            Value::Enum(tag, payload) => {
                if **payload == Value::Unit {
                    write!(f, "{}", tag)
                } else {
                    write!(f, "{}({})", tag, payload)
                }
            }
            Value::Struct(name, fields) => {
                let s = fields.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<_>>().join(", ");
                write!(f, "{} {{ {} }}", name, s)
            }
            Value::Function(idx) => write!(f, "<fn:{}>", idx),
            Value::Closure { chunk_idx, .. } => write!(f, "<closure:{}>", chunk_idx),
        }
    }
}

impl Value {
    /// Returns true if the value is truthy (for conditional jumps).
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Unit => false,
            _ => true,
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_display() {
        assert_eq!(Value::Int(42).to_string(), "42");
        assert_eq!(Value::Float(3.14).to_string(), "3.14");
        assert_eq!(Value::String("hi".into()).to_string(), "hi");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::Unit.to_string(), "()");
    }

    #[test]
    fn value_truthiness() {
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(Value::Int(1).is_truthy());
        assert!(!Value::Int(0).is_truthy());
        assert!(!Value::Unit.is_truthy());
        assert!(Value::String("hi".into()).is_truthy());
    }

    #[test]
    fn value_equality() {
        assert_eq!(Value::Int(1), Value::Int(1));
        assert_ne!(Value::Int(1), Value::Int(2));
        assert_eq!(Value::String("a".into()), Value::String("a".into()));
        assert_ne!(Value::Bool(true), Value::Bool(false));
    }
}
