//! Semantic analysis passes for Baseline source files.
//!
//! This module contains various analysis passes that run after parsing,
//! including effect checking, refinement checking, and other verifications.

pub mod effects;
pub mod infer;
pub mod refinements;
pub mod smt;
pub mod types;

pub use effects::check_effects;
pub use refinements::check_refinements;
pub use refinements::collect_refined_types;
pub use refinements::{Constraint, StringConstraint};
pub use smt::check_specs;
pub use types::check_closure_cycles;
pub use types::check_types;
pub use types::check_types_with_loader;
pub use types::check_types_with_map;
pub use types::check_types_with_loader_and_map;
pub use types::check_types_with_map_and_defs;
pub use types::check_types_with_loader_map_and_defs;
pub use types::{DictMap, Type, TypeMap};
