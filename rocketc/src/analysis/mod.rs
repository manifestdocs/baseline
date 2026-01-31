//! Semantic analysis passes for Rocket source files.
//!
//! This module contains various analysis passes that run after parsing,
//! including effect checking, refinement checking, and other verifications.

pub mod effects;
pub mod refinements;

pub use effects::check_effects;
pub use refinements::check_refinements;
