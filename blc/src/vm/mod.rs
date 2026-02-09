#[cfg(feature = "async-server")]
pub mod async_executor;
pub mod chunk;
pub mod codegen;
pub mod compiler;
#[cfg(feature = "async-server")]
pub mod hyper_server;
#[cfg(all(test, feature = "async-server"))]
mod hyper_server_tests;
pub mod ir;
#[cfg(feature = "jit")]
pub mod jit;
pub mod lower;
pub mod module_compiler;
pub mod natives;
pub mod nvalue;
pub mod optimize_ir;
pub mod test_runner;
pub mod value;
#[allow(clippy::module_inception)]
pub mod vm;
