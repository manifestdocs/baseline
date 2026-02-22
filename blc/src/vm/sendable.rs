//! Thread-safe value and handler types for cross-thread VM communication.
//!
//! These types use owned data (String, Vec, usize) instead of Rc-based NValue,
//! making them safe to send across thread boundaries.

use super::nvalue::{HeapObject, NValue};
use super::value::RcStr;

#[cfg(feature = "async-server")]
use bytes::Bytes as BytesType;

/// Thread-safe value representation using owned Strings instead of Rc<str>.
#[derive(Clone)]
pub enum SendableValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
    List(Vec<SendableValue>),
    Record(Vec<(String, SendableValue)>),
    Tuple(Vec<SendableValue>),
    Enum {
        tag: String,
        payload: Box<SendableValue>,
    },
    Function(usize),
    /// Zero-copy body bytes (Bytes::clone is O(1) refcount bump).
    #[cfg(feature = "async-server")]
    Bytes(BytesType),
}

impl SendableValue {
    /// Convert from NValue to SendableValue.
    pub fn from_nvalue(nv: &NValue) -> Self {
        if nv.is_int() {
            return SendableValue::Int(nv.as_int());
        }
        if nv.is_any_int() {
            return SendableValue::Int(nv.as_any_int());
        }
        if nv.is_float() {
            return SendableValue::Float(nv.as_float());
        }
        if nv.is_bool() {
            return SendableValue::Bool(nv.as_bool());
        }
        if nv.is_unit() {
            return SendableValue::Unit;
        }
        if nv.is_function() {
            return SendableValue::Function(nv.as_function());
        }
        if nv.is_heap() {
            match nv.as_heap_ref() {
                HeapObject::String(s) => SendableValue::String(s.to_string()),
                HeapObject::List(items) => {
                    SendableValue::List(items.iter().map(SendableValue::from_nvalue).collect())
                }
                HeapObject::Record(fields) => SendableValue::Record(
                    fields
                        .iter()
                        .map(|(k, v)| (k.to_string(), SendableValue::from_nvalue(v)))
                        .collect(),
                ),
                HeapObject::Tuple(items) => {
                    SendableValue::Tuple(items.iter().map(SendableValue::from_nvalue).collect())
                }
                HeapObject::Enum { tag, payload, .. } => {
                    let sv = if payload.is_empty() {
                        SendableValue::Unit
                    } else if payload.len() == 1 {
                        SendableValue::from_nvalue(&payload[0])
                    } else {
                        SendableValue::Tuple(
                            payload.iter().map(SendableValue::from_nvalue).collect(),
                        )
                    };
                    SendableValue::Enum {
                        tag: tag.to_string(),
                        payload: Box::new(sv),
                    }
                }
                HeapObject::Closure { chunk_idx, .. } => SendableValue::Function(*chunk_idx),
                HeapObject::BigInt(i) => SendableValue::Int(*i),
                _ => SendableValue::Unit,
            }
        } else {
            SendableValue::Unit
        }
    }

    /// Convert from SendableValue to NValue.
    pub fn to_nvalue(&self) -> NValue {
        match self {
            SendableValue::Int(i) => NValue::int(*i),
            SendableValue::Float(f) => NValue::float(*f),
            SendableValue::String(s) => NValue::string(RcStr::from(s.as_str())),
            SendableValue::Bool(b) => NValue::bool(*b),
            SendableValue::Unit => NValue::unit(),
            SendableValue::List(items) => {
                NValue::list(items.iter().map(|v| v.to_nvalue()).collect())
            }
            SendableValue::Record(fields) => NValue::record(
                fields
                    .iter()
                    .map(|(k, v)| (RcStr::from(k.as_str()), v.to_nvalue()))
                    .collect(),
            ),
            SendableValue::Tuple(items) => {
                NValue::tuple(items.iter().map(|v| v.to_nvalue()).collect())
            }
            SendableValue::Enum { tag, payload } => {
                NValue::enum_val(RcStr::from(tag.as_str()), payload.to_nvalue())
            }
            SendableValue::Function(idx) => NValue::function(*idx),
            #[cfg(feature = "async-server")]
            SendableValue::Bytes(b) => {
                let s = std::str::from_utf8(b).unwrap_or("");
                NValue::string(RcStr::from(s))
            }
        }
    }
}

/// Thread-safe handler representation. Handlers are either plain functions
/// (chunk index) or closures (chunk index + upvalue data).
#[derive(Clone)]
pub enum SendableHandler {
    /// Plain function (chunk index)
    Function(usize),
    /// Closure (chunk index + captured upvalues)
    Closure {
        chunk_idx: usize,
        upvalues: Vec<SendableValue>,
    },
}

impl SendableHandler {
    /// Convert from NValue to SendableHandler.
    pub fn from_nvalue(nv: &NValue) -> Option<Self> {
        if nv.is_function() {
            return Some(SendableHandler::Function(nv.as_function()));
        }
        if nv.is_heap()
            && let HeapObject::Closure {
                chunk_idx,
                upvalues,
            } = nv.as_heap_ref()
        {
            return Some(SendableHandler::Closure {
                chunk_idx: *chunk_idx,
                upvalues: upvalues.iter().map(SendableValue::from_nvalue).collect(),
            });
        }
        None
    }

    /// Convert to NValue for VM invocation.
    pub fn to_nvalue(&self) -> NValue {
        match self {
            SendableHandler::Function(idx) => NValue::function(*idx),
            SendableHandler::Closure {
                chunk_idx,
                upvalues,
            } => NValue::closure(*chunk_idx, upvalues.iter().map(|v| v.to_nvalue()).collect()),
        }
    }
}

// SAFETY: SendableHandler/SendableValue use only owned data (String, Vec, usize).
unsafe impl Send for SendableHandler {}
unsafe impl Sync for SendableHandler {}
