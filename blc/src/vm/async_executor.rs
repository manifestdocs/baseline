//! Async VM Executor - Bridge between async HTTP server and synchronous VM
//!
//! This module provides a way to execute VM handlers from async contexts
//! by using a thread pool with tokio::task::spawn_blocking.
//!
//! Optimization: uses thread-local VMs to avoid per-request allocation,
//! and builds NValue/Response on the worker thread to skip SendableValue round-trips.

use std::cell::RefCell;
use std::sync::Arc;

use bytes::Bytes;
use http_body_util::Full;
use hyper::Response;
use tokio::sync::Semaphore;

use crate::vm::chunk::{Chunk, CompileError};
use crate::vm::nvalue::NValue;
use crate::vm::sendable::{SendableHandler, SendableValue};
use crate::vm::vm::Vm;

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
// Thread-local VM pool
// ---------------------------------------------------------------------------

thread_local! {
    static WORKER_VM: RefCell<Vm> = RefCell::new(Vm::new());
}

// ---------------------------------------------------------------------------
// Async Executor
// ---------------------------------------------------------------------------

/// Async executor that runs VM handlers in a controlled thread pool.
///
/// Uses tokio's spawn_blocking with thread-local VMs and a semaphore
/// to limit concurrent executions.
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

    /// Execute a VM handler directly: builds NValue on worker thread,
    /// returns hyper Response. Eliminates SendableValue round-trip and
    /// reuses thread-local VM.
    ///
    /// Uses `block_in_place` to run the VM inline on the Tokio worker thread,
    /// avoiding the cross-thread scheduling overhead of `spawn_blocking`.
    pub async fn execute_handler_direct(
        &self,
        handler: SendableHandler,
        request: crate::vm::hyper_server::AsyncRequest,
    ) -> Result<Response<Full<Bytes>>, CompileError> {
        let _permit = self.semaphore.acquire().await.unwrap();
        let chunks = &self.chunks;

        tokio::task::block_in_place(|| {
            WORKER_VM.with(|cell| {
                let mut vm = cell.borrow_mut();

                // Build NValue directly on this thread (skips SendableValue)
                let request_nv = request.to_nvalue();
                let handler_nv = handler.to_nvalue();

                match vm.call_nvalue(&handler_nv, &[request_nv], chunks, 0, 0) {
                    Ok(result) => {
                        Ok(crate::vm::hyper_server::build_hyper_response_from_nvalue(&result))
                    }
                    Err(e) => {
                        vm.reset();
                        Err(e)
                    }
                }
            })
        })
    }

    /// Execute a VM handler asynchronously (legacy path via SendableValue).
    pub async fn execute_handler(
        &self,
        handler: SendableHandler,
        request: SendableValue,
    ) -> Result<SendableValue, CompileError> {
        let _permit = self.semaphore.acquire().await.unwrap();
        let chunks = Arc::clone(&self.chunks);

        tokio::task::spawn_blocking(move || {
            WORKER_VM.with(|cell| {
                let mut vm = cell.borrow_mut();
                let handler_nv = handler.to_nvalue();
                let request_nv = request.to_nvalue();

                match vm.call_nvalue(&handler_nv, &[request_nv], &chunks, 0, 0) {
                    Ok(result) => Ok(SendableValue::from_nvalue(&result)),
                    Err(e) => {
                        vm.reset();
                        Err(e)
                    }
                }
            })
        })
        .await
        .map_err(|e| CompileError {
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
    ) -> Result<SendableValue, CompileError> {
        let _permit = self.semaphore.acquire().await.unwrap();
        let chunks = Arc::clone(&self.chunks);

        tokio::task::spawn_blocking(move || {
            WORKER_VM.with(|cell| {
                let mut vm = cell.borrow_mut();
                let handler_nv = handler.to_nvalue();
                let request_nv = request.to_nvalue();
                let mw_nvalues: Vec<NValue> =
                    middleware.iter().map(|h| h.to_nvalue()).collect();

                match vm.apply_mw_chain(&mw_nvalues, &handler_nv, &request_nv, &chunks, 0, 0) {
                    Ok(result) => Ok(SendableValue::from_nvalue(&result)),
                    Err(e) => {
                        vm.reset();
                        Err(e)
                    }
                }
            })
        })
        .await
        .map_err(|e| CompileError {
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
    use crate::vm::value::RcStr;

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
