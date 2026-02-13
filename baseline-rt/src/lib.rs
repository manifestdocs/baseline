// JIT/AOT helper functions receive raw pointers from generated native code.
// These are FFI entry points, not meant to be called from safe Rust.
#![allow(clippy::not_unsafe_ptr_arg_deref)]

pub mod helpers;
pub mod natives;
pub mod nvalue;
pub mod value;
