//! Async VM Executor - Bridge between async HTTP server and synchronous VM
//!
//! This module provides a way to execute VM handlers from async contexts
//! by using a thread pool with tokio::task::spawn_blocking.
//!
//! Since NValue uses Rc (not Send), we convert to SendableValue for cross-thread transfer.

use std::sync::Arc;

use tokio::sync::Semaphore;

use crate::vm::chunk::Chunk;
use crate::vm::nvalue::{HeapObject, NValue};
use crate::vm::value::RcStr;
use crate::vm::vm::{Vm, VmError};

/// Configuration for the async executor.
#[derive(Clone)]
pub struct AsyncExecutorConfig {
    /// Maximum concurrent VM executions
    pub max_concurrent: usize,
}

impl Default for AsyncExecutorConfig {
    fn default() -> Self {
        Self {
            max_concurrent: std::thread::available_parallelism()
                .map(|n| n.get() * 2)
                .unwrap_or(8),
        }
    }
}

// ---------------------------------------------------------------------------
// Sendable Types (thread-safe representations)
// ---------------------------------------------------------------------------

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
                HeapObject::Enum { tag, payload } => SendableValue::Enum {
                    tag: tag.to_string(),
                    payload: Box::new(SendableValue::from_nvalue(payload)),
                },
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
        }
    }
}

/// Thread-safe handler representation.
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
        if nv.is_heap() {
            if let HeapObject::Closure {
                chunk_idx,
                upvalues,
            } = nv.as_heap_ref()
            {
                return Some(SendableHandler::Closure {
                    chunk_idx: *chunk_idx,
                    upvalues: upvalues.iter().map(SendableValue::from_nvalue).collect(),
                });
            }
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

// ---------------------------------------------------------------------------
// Async Executor
// ---------------------------------------------------------------------------

/// Async executor that runs VM handlers in a controlled thread pool.
///
/// Uses tokio's spawn_blocking with a semaphore to limit concurrent executions.
pub struct AsyncVmExecutor {
    /// Semaphore to limit concurrent VM executions
    semaphore: Arc<Semaphore>,
    /// Shared chunks (bytecode) for all handlers
    chunks: Arc<Vec<Chunk>>,
}

impl AsyncVmExecutor {
    /// Create a new executor with the given chunks and configuration.
    pub fn new(chunks: Vec<Chunk>, config: AsyncExecutorConfig) -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(config.max_concurrent)),
            chunks: Arc::new(chunks),
        }
    }

    /// Execute a VM handler asynchronously.
    ///
    /// This spawns the VM execution on a blocking thread to avoid blocking
    /// the async runtime, while using a semaphore to limit concurrency.
    pub async fn execute_handler(
        &self,
        handler: SendableHandler,
        request: SendableValue,
    ) -> Result<SendableValue, VmError> {
        // Acquire semaphore permit (limits concurrent VM executions)
        let _permit = self.semaphore.acquire().await.unwrap();

        let chunks = Arc::clone(&self.chunks);

        // Run VM execution on a blocking thread
        tokio::task::spawn_blocking(move || {
            let mut vm = Vm::new();
            let handler_nv = handler.to_nvalue();
            let request_nv = request.to_nvalue();

            let result = vm.call_nvalue(&handler_nv, &[request_nv], &chunks, 0, 0)?;
            Ok(SendableValue::from_nvalue(&result))
        })
        .await
        .map_err(|e| VmError {
            message: format!("VM execution panicked: {}", e),
            line: 0,
            col: 0,
        })?
    }

    /// Execute with middleware chain.
    pub async fn execute_with_middleware(
        &self,
        middleware: Vec<SendableHandler>,
        handler: SendableHandler,
        request: SendableValue,
    ) -> Result<SendableValue, VmError> {
        let _permit = self.semaphore.acquire().await.unwrap();

        let chunks = Arc::clone(&self.chunks);

        tokio::task::spawn_blocking(move || {
            let mut vm = Vm::new();
            let handler_nv = handler.to_nvalue();
            let request_nv = request.to_nvalue();
            let mw_nvalues: Vec<NValue> = middleware.iter().map(|h| h.to_nvalue()).collect();

            let result = vm.apply_mw_chain(&mw_nvalues, &handler_nv, &request_nv, &chunks, 0, 0)?;
            Ok(SendableValue::from_nvalue(&result))
        })
        .await
        .map_err(|e| VmError {
            message: format!("VM execution panicked: {}", e),
            line: 0,
            col: 0,
        })?
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_executor_config_defaults() {
        let config = AsyncExecutorConfig::default();
        assert!(config.max_concurrent > 0);
    }

    #[test]
    fn test_sendable_value_int_roundtrip() {
        let nv = NValue::int(42);
        let sv = SendableValue::from_nvalue(&nv);
        let nv2 = sv.to_nvalue();
        assert_eq!(nv, nv2);
    }

    #[test]
    fn test_sendable_value_string_roundtrip() {
        let nv = NValue::string(RcStr::from("hello"));
        let sv = SendableValue::from_nvalue(&nv);
        let nv2 = sv.to_nvalue();
        assert_eq!(nv.as_str(), nv2.as_str());
    }

    #[test]
    fn test_sendable_handler_function() {
        let handler = SendableHandler::Function(42);
        let nv = handler.to_nvalue();
        assert!(nv.is_function());
        assert_eq!(nv.as_function(), 42);
    }

    #[tokio::test]
    async fn test_executor_semaphore_limits_concurrency() {
        let config = AsyncExecutorConfig { max_concurrent: 2 };
        let executor = AsyncVmExecutor::new(vec![], config);

        // Semaphore should have 2 permits
        assert_eq!(executor.semaphore.available_permits(), 2);
    }
}
