//! NaN-boxed value type for the VM stack.
//!
//! Encodes common value types (Int, Float, Bool, Unit, Function) inline in 8 bytes,
//! with heap-allocated variants (String, List, Record, etc.) behind an Rc pointer.
//!
//! Encoding scheme (top 16 bits determine type):
//!   Float:    any f64 whose top 16 bits < 0xFFFA (includes normal NaN = 0x7FF8...)
//!   Int:      0xFFFA_PPPP_PPPP_PPPP  (P = 48-bit signed integer, sign-extended)
//!   Bool:     0xFFFB_0000_0000_000V  (V = 0 or 1)
//!   Unit:     0xFFFC_0000_0000_0000
//!   Function: 0xFFFD_PPPP_PPPP_PPPP  (P = chunk index)
//!   Heap:     0xFFFE_PPPP_PPPP_PPPP  (P = pointer to Rc<HeapObject> inner data)

use std::fmt;
use std::rc::Rc;

use super::value::{RcStr, Value};

// ---------------------------------------------------------------------------
// Tag constants
// ---------------------------------------------------------------------------

const TAG_MASK: u64 = 0xFFFF_0000_0000_0000;
const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;
const TAG_THRESHOLD: u64 = 0xFFFA_0000_0000_0000; // values >= this are tagged

const TAG_INT: u64 = 0xFFFA_0000_0000_0000;
const TAG_BOOL: u64 = 0xFFFB_0000_0000_0000;
const TAG_UNIT: u64 = 0xFFFC_0000_0000_0000;
const TAG_FUNC: u64 = 0xFFFD_0000_0000_0000;
const TAG_HEAP: u64 = 0xFFFE_0000_0000_0000;

// ---------------------------------------------------------------------------
// HeapObject — heap-allocated value variants
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub enum HeapObject {
    String(RcStr),
    List(Vec<NValue>),
    Record(Vec<(RcStr, NValue)>),
    Tuple(Vec<NValue>),
    Enum {
        tag: RcStr,
        payload: NValue,
    },
    Struct {
        name: RcStr,
        fields: Vec<(RcStr, NValue)>,
    },
    Closure {
        chunk_idx: usize,
        upvalues: Vec<NValue>,
    },
    /// Integers that don't fit in 48-bit signed range.
    BigInt(i64),
}

// ---------------------------------------------------------------------------
// NValue — 8-byte NaN-boxed value
// ---------------------------------------------------------------------------

pub struct NValue(u64);

// -- Construction --

impl NValue {
    #[inline(always)]
    pub fn int(i: i64) -> Self {
        // 48-bit signed range: -(1<<47) to (1<<47)-1
        const MIN_INLINE: i64 = -(1i64 << 47);
        const MAX_INLINE: i64 = (1i64 << 47) - 1;
        if (MIN_INLINE..=MAX_INLINE).contains(&i) {
            NValue(TAG_INT | (i as u64 & PAYLOAD_MASK))
        } else {
            Self::from_heap(HeapObject::BigInt(i))
        }
    }

    #[inline(always)]
    pub fn float(f: f64) -> Self {
        let bits = f.to_bits();
        // Canonicalize NaN values that would collide with our tags
        if bits >= TAG_THRESHOLD {
            NValue(f64::NAN.to_bits())
        } else {
            NValue(bits)
        }
    }

    #[inline(always)]
    pub fn bool(b: bool) -> Self {
        NValue(TAG_BOOL | b as u64)
    }

    #[inline(always)]
    pub fn unit() -> Self {
        NValue(TAG_UNIT)
    }

    #[inline(always)]
    pub fn function(chunk_idx: usize) -> Self {
        debug_assert!(chunk_idx as u64 <= PAYLOAD_MASK);
        NValue(TAG_FUNC | chunk_idx as u64)
    }

    pub fn string(s: RcStr) -> Self {
        Self::from_heap(HeapObject::String(s))
    }

    pub fn list(items: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::List(items))
    }

    pub fn record(fields: Vec<(RcStr, NValue)>) -> Self {
        Self::from_heap(HeapObject::Record(fields))
    }

    pub fn tuple(items: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::Tuple(items))
    }

    pub fn enum_val(tag: RcStr, payload: NValue) -> Self {
        Self::from_heap(HeapObject::Enum { tag, payload })
    }

    pub fn struct_val(name: RcStr, fields: Vec<(RcStr, NValue)>) -> Self {
        Self::from_heap(HeapObject::Struct { name, fields })
    }

    pub fn closure(chunk_idx: usize, upvalues: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::Closure {
            chunk_idx,
            upvalues,
        })
    }

    fn from_heap(obj: HeapObject) -> Self {
        let rc = Rc::new(obj);
        let ptr = Rc::into_raw(rc) as u64;
        debug_assert!(ptr & TAG_MASK == 0, "heap pointer exceeds 48 bits");
        NValue(TAG_HEAP | ptr)
    }
}

// -- Type checking --

impl NValue {
    #[inline(always)]
    pub fn is_float(&self) -> bool {
        self.0 < TAG_THRESHOLD
    }

    #[inline(always)]
    pub fn is_int(&self) -> bool {
        self.0 & TAG_MASK == TAG_INT
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        self.0 & TAG_MASK == TAG_BOOL
    }

    #[inline(always)]
    pub fn is_unit(&self) -> bool {
        self.0 == TAG_UNIT
    }

    #[inline(always)]
    pub fn is_function(&self) -> bool {
        self.0 & TAG_MASK == TAG_FUNC
    }

    #[inline(always)]
    pub fn is_heap(&self) -> bool {
        self.0 & TAG_MASK == TAG_HEAP
    }

    /// Check if this is a number (int or float) — used for mixed arithmetic.
    #[inline(always)]
    pub fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    #[inline(always)]
    pub fn is_truthy(&self) -> bool {
        if self.is_bool() {
            self.0 & 1 != 0
        } else if self.is_int() {
            self.0 & PAYLOAD_MASK != 0
        } else if self.is_unit() {
            false
        } else {
            // Float, heap objects, functions are truthy
            true
        }
    }

    /// Returns true if this is a BigInt (heap-allocated large integer).
    #[inline(always)]
    fn is_bigint(&self) -> bool {
        if !self.is_heap() {
            return false;
        }
        matches!(self.as_heap_ref(), HeapObject::BigInt(_))
    }

    /// Returns true if this is any kind of integer (inline or BigInt).
    #[inline(always)]
    pub fn is_any_int(&self) -> bool {
        self.is_int() || self.is_bigint()
    }
}

// -- Accessors --

impl NValue {
    /// Extract i64 from an inline Int. Caller must ensure is_int().
    #[inline(always)]
    pub fn as_int(&self) -> i64 {
        debug_assert!(self.is_int());
        // Sign-extend from 48 bits: shift left 16, arithmetic shift right 16
        ((self.0 << 16) as i64) >> 16
    }

    /// Extract i64 from any integer (inline or BigInt).
    #[inline(always)]
    pub fn as_any_int(&self) -> i64 {
        if self.is_int() {
            self.as_int()
        } else {
            match self.as_heap_ref() {
                HeapObject::BigInt(i) => *i,
                _ => panic!("as_any_int on non-integer"),
            }
        }
    }

    #[inline(always)]
    pub fn as_float(&self) -> f64 {
        debug_assert!(self.is_float());
        f64::from_bits(self.0)
    }

    /// Get f64 from either Float or Int (for mixed arithmetic).
    #[inline(always)]
    pub fn as_f64(&self) -> f64 {
        if self.is_float() {
            self.as_float()
        } else {
            self.as_any_int() as f64
        }
    }

    #[inline(always)]
    pub fn as_bool(&self) -> bool {
        debug_assert!(self.is_bool());
        self.0 & 1 != 0
    }

    #[inline(always)]
    pub fn as_function(&self) -> usize {
        debug_assert!(self.is_function());
        (self.0 & PAYLOAD_MASK) as usize
    }

    #[inline(always)]
    pub fn as_heap_ref(&self) -> &HeapObject {
        debug_assert!(self.is_heap());
        let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
        unsafe { &*ptr }
    }

    /// Extract string reference from a heap String.
    #[inline]
    pub fn as_string(&self) -> Option<&RcStr> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::String(s) => Some(s),
            _ => None,
        }
    }

    /// Extract list reference from a heap List.
    #[inline]
    pub fn as_list(&self) -> Option<&Vec<NValue>> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::List(items) => Some(items),
            _ => None,
        }
    }

    /// Extract record fields reference from a heap Record.
    #[inline]
    pub fn as_record(&self) -> Option<&Vec<(RcStr, NValue)>> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::Record(fields) => Some(fields),
            _ => None,
        }
    }

    /// Extract enum tag and payload from a heap Enum.
    #[inline]
    pub fn as_enum(&self) -> Option<(&RcStr, &NValue)> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::Enum { tag, payload } => Some((tag, payload)),
            _ => None,
        }
    }
}

// -- Clone: bump Rc for heap, copy bits for inline --

impl Clone for NValue {
    #[inline(always)]
    fn clone(&self) -> Self {
        if self.is_heap() {
            let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
            unsafe {
                Rc::increment_strong_count(ptr);
            }
        }
        NValue(self.0)
    }
}

// -- Drop: decrement Rc for heap --

impl Drop for NValue {
    #[inline(always)]
    fn drop(&mut self) {
        if self.is_heap() {
            let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
            unsafe {
                Rc::decrement_strong_count(ptr);
            }
        }
    }
}

// -- PartialEq --

impl PartialEq for NValue {
    fn eq(&self, other: &Self) -> bool {
        // Fast path: identical bits (works for Int, Bool, Unit, Function)
        if self.0 == other.0 {
            // But NaN != NaN per IEEE 754
            if self.is_float() {
                return !f64::from_bits(self.0).is_nan();
            }
            return true;
        }
        // Different bits — could still be equal for floats or heap objects
        if self.is_float() && other.is_float() {
            return self.as_float() == other.as_float();
        }
        if self.is_heap() && other.is_heap() {
            return self.as_heap_ref() == other.as_heap_ref();
        }
        false
    }
}

// -- Display --

impl fmt::Display for NValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_int() {
            write!(f, "{}", self.as_int())
        } else if self.is_float() {
            write!(f, "{}", self.as_float())
        } else if self.is_bool() {
            write!(f, "{}", self.as_bool())
        } else if self.is_unit() {
            write!(f, "()")
        } else if self.is_function() {
            write!(f, "<fn:{}>", self.as_function())
        } else {
            match self.as_heap_ref() {
                HeapObject::String(s) => write!(f, "{}", s),
                HeapObject::List(items) => {
                    let s: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                    write!(f, "[{}]", s.join(", "))
                }
                HeapObject::Record(fields) => {
                    let s: Vec<String> = fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "{{ {} }}", s.join(", "))
                }
                HeapObject::Tuple(items) => {
                    let s: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
                HeapObject::Enum { tag, payload } => {
                    if payload.is_unit() {
                        write!(f, "{}", tag)
                    } else {
                        write!(f, "{}({})", tag, payload)
                    }
                }
                HeapObject::Struct { name, fields } => {
                    let s: Vec<String> = fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "{} {{ {} }}", name, s.join(", "))
                }
                HeapObject::Closure { chunk_idx, .. } => {
                    write!(f, "<closure:{}>", chunk_idx)
                }
                HeapObject::BigInt(i) => write!(f, "{}", i),
            }
        }
    }
}

impl fmt::Debug for NValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NValue({})", self)
    }
}

// -- Conversion to/from Value --

impl NValue {
    pub fn from_value(v: &Value) -> NValue {
        match v {
            Value::Int(i) => NValue::int(*i),
            Value::Float(f) => NValue::float(*f),
            Value::Bool(b) => NValue::bool(*b),
            Value::Unit => NValue::unit(),
            Value::Function(idx) => NValue::function(*idx),
            Value::String(s) => NValue::string(s.clone()),
            Value::List(items) => {
                let nv: Vec<NValue> = items.iter().map(NValue::from_value).collect();
                NValue::list(nv)
            }
            Value::Record(fields) => {
                let nf: Vec<(RcStr, NValue)> = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), NValue::from_value(v)))
                    .collect();
                NValue::record(nf)
            }
            Value::Tuple(items) => {
                let nv: Vec<NValue> = items.iter().map(NValue::from_value).collect();
                NValue::tuple(nv)
            }
            Value::Enum(tag, payload) => NValue::enum_val(tag.clone(), NValue::from_value(payload)),
            Value::Struct(name, fields) => {
                let nf: Vec<(RcStr, NValue)> = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), NValue::from_value(v)))
                    .collect();
                NValue::struct_val(name.clone(), nf)
            }
            Value::Closure {
                chunk_idx,
                upvalues,
            } => {
                let nv: Vec<NValue> = upvalues.iter().map(NValue::from_value).collect();
                NValue::closure(*chunk_idx, nv)
            }
        }
    }

    pub fn to_value(&self) -> Value {
        if self.is_int() {
            return Value::Int(self.as_int());
        }
        if self.is_float() {
            return Value::Float(self.as_float());
        }
        if self.is_bool() {
            return Value::Bool(self.as_bool());
        }
        if self.is_unit() {
            return Value::Unit;
        }
        if self.is_function() {
            return Value::Function(self.as_function());
        }
        match self.as_heap_ref() {
            HeapObject::String(s) => Value::String(s.clone()),
            HeapObject::List(items) => {
                Value::List(Rc::new(items.iter().map(|v| v.to_value()).collect()))
            }
            HeapObject::Record(fields) => Value::Record(Rc::new(
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_value()))
                    .collect(),
            )),
            HeapObject::Tuple(items) => {
                Value::Tuple(Rc::new(items.iter().map(|v| v.to_value()).collect()))
            }
            HeapObject::Enum { tag, payload } => {
                Value::Enum(tag.clone(), Rc::new(payload.to_value()))
            }
            HeapObject::Struct { name, fields } => Value::Struct(
                name.clone(),
                Rc::new(
                    fields
                        .iter()
                        .map(|(k, v)| (k.clone(), v.to_value()))
                        .collect(),
                ),
            ),
            HeapObject::Closure {
                chunk_idx,
                upvalues,
            } => Value::Closure {
                chunk_idx: *chunk_idx,
                upvalues: Rc::new(upvalues.iter().map(|v| v.to_value()).collect()),
            },
            HeapObject::BigInt(i) => Value::Int(*i),
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
    fn int_roundtrip() {
        for i in [
            -1i64,
            0,
            1,
            42,
            -42,
            1000000,
            -1000000,
            (1 << 47) - 1,
            -(1 << 47),
        ] {
            let v = NValue::int(i);
            assert!(v.is_int() || v.is_heap(), "failed for {}", i);
            assert_eq!(v.as_any_int(), i, "roundtrip failed for {}", i);
        }
    }

    #[test]
    fn big_int_roundtrip() {
        let big = i64::MAX;
        let v = NValue::int(big);
        assert!(v.is_heap(), "big int should be heap-allocated");
        assert_eq!(v.as_any_int(), big);
    }

    #[test]
    fn float_roundtrip() {
        for f in [0.0, 1.0, -1.0, 3.125, f64::INFINITY, f64::NEG_INFINITY] {
            let v = NValue::float(f);
            assert!(v.is_float());
            assert_eq!(v.as_float(), f);
        }
    }

    #[test]
    fn nan_is_float() {
        let v = NValue::float(f64::NAN);
        assert!(v.is_float());
        assert!(v.as_float().is_nan());
    }

    #[test]
    fn bool_roundtrip() {
        assert!(NValue::bool(true).as_bool());
        assert!(!NValue::bool(false).as_bool());
        assert!(NValue::bool(true).is_truthy());
        assert!(!NValue::bool(false).is_truthy());
    }

    #[test]
    fn unit() {
        let v = NValue::unit();
        assert!(v.is_unit());
        assert!(!v.is_truthy());
    }

    #[test]
    fn function_roundtrip() {
        let v = NValue::function(42);
        assert!(v.is_function());
        assert_eq!(v.as_function(), 42);
    }

    #[test]
    fn string_heap() {
        let v = NValue::string("hello".into());
        assert!(v.is_heap());
        match v.as_heap_ref() {
            HeapObject::String(s) => assert_eq!(&**s, "hello"),
            _ => panic!("expected String"),
        }
    }

    #[test]
    fn clone_heap_bumps_refcount() {
        let v1 = NValue::string("test".into());
        let v2 = v1.clone();
        // Both should display the same
        assert_eq!(v1.to_string(), "test");
        assert_eq!(v2.to_string(), "test");
        // Dropping one should not affect the other
        drop(v1);
        assert_eq!(v2.to_string(), "test");
    }

    #[test]
    fn equality() {
        assert_eq!(NValue::int(1), NValue::int(1));
        assert_ne!(NValue::int(1), NValue::int(2));
        assert_eq!(NValue::bool(true), NValue::bool(true));
        assert_ne!(NValue::bool(true), NValue::bool(false));
        assert_eq!(NValue::unit(), NValue::unit());
        assert_eq!(NValue::float(1.0), NValue::float(1.0));
        let nan = NValue::float(f64::NAN);
        assert_ne!(nan, nan.clone());
    }

    #[test]
    fn value_conversion_roundtrip() {
        let values = vec![
            Value::Int(42),
            Value::Float(3.125),
            Value::Bool(true),
            Value::Unit,
            Value::Function(5),
            Value::String("hello".into()),
        ];
        for v in &values {
            let nv = NValue::from_value(v);
            let back = nv.to_value();
            assert_eq!(&back, v, "roundtrip failed for {:?}", v);
        }
    }

    #[test]
    fn size_is_8_bytes() {
        assert_eq!(std::mem::size_of::<NValue>(), 8);
    }
}
