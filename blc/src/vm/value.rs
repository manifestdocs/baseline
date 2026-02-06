use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------------
// String Interner
// ---------------------------------------------------------------------------

/// Interned string identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrId(pub u32);

/// Interns strings so field names, identifiers, etc. are cheap u32 comparisons.
#[derive(Debug, Default)]
pub struct StringInterner {
    map: HashMap<String, StrId>,
    strings: Vec<String>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self::default()
    }

    /// Intern a string, returning its id. If already interned, returns existing id.
    pub fn intern(&mut self, s: &str) -> StrId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = StrId(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }

    /// Resolve an interned id back to its string.
    pub fn resolve(&self, id: StrId) -> &str {
        &self.strings[id.0 as usize]
    }
}

// ---------------------------------------------------------------------------
// Value
// ---------------------------------------------------------------------------

/// Lifetime-free value type for the bytecode VM.
/// Functions and closures are represented as bytecode offsets, not tree-sitter nodes.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
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

    // -- StringInterner tests --

    #[test]
    fn intern_returns_same_id_for_same_string() {
        let mut interner = StringInterner::new();
        let a = interner.intern("hello");
        let b = interner.intern("hello");
        assert_eq!(a, b);
    }

    #[test]
    fn intern_returns_different_ids_for_different_strings() {
        let mut interner = StringInterner::new();
        let a = interner.intern("hello");
        let b = interner.intern("world");
        assert_ne!(a, b);
    }

    #[test]
    fn resolve_returns_original_string() {
        let mut interner = StringInterner::new();
        let id = interner.intern("baseline");
        assert_eq!(interner.resolve(id), "baseline");
    }

    #[test]
    fn intern_ids_are_sequential() {
        let mut interner = StringInterner::new();
        let a = interner.intern("a");
        let b = interner.intern("b");
        let c = interner.intern("c");
        assert_eq!(a.0, 0);
        assert_eq!(b.0, 1);
        assert_eq!(c.0, 2);
    }

    // -- Value tests --

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
