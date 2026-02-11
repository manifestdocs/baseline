//! Async VM Executor - Bridge between async HTTP server and synchronous VM
//!
//! This module provides a way to execute VM handlers from async contexts
//! by using a thread pool with tokio::task::spawn_blocking.
//!
//! Since NValue uses Rc (not Send), we convert to SendableValue for cross-thread transfer.

use std::sync::Arc;

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
    ) -> Result<SendableValue, CompileError> {
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
            let mut vm = Vm::new();
            let handler_nv = handler.to_nvalue();
            let request_nv = request.to_nvalue();
            let mw_nvalues: Vec<NValue> = middleware.iter().map(|h| h.to_nvalue()).collect();

            let result = vm.apply_mw_chain(&mw_nvalues, &handler_nv, &request_nv, &chunks, 0, 0)?;
            Ok(SendableValue::from_nvalue(&result))
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
