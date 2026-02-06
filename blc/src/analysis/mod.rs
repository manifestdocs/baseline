//! Semantic analysis passes for Baseline source files.
//!
//! This module contains various analysis passes that run after parsing,
//! including effect checking, refinement checking, and other verifications.

pub mod effects;
pub mod refinements;
pub mod types;

pub use effects::check_effects;
pub use refinements::check_refinements;
pub use types::check_types;
pub use types::check_types_with_loader;
