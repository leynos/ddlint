//! Cohesive scanners for top-level statement spans.
//!
//! Each submodule focuses on a specific statement category, keeping scanning
//! logic small and composable. The parent `span_scanner` module orchestrates
//! these specialised scanners to collect spans before full parsing.

pub(super) mod functions;
pub(super) mod imports;
pub(super) mod indexes;
pub(super) mod relations;
pub(super) mod rules;
pub(super) mod transformers;
pub(super) mod typedefs;
pub(super) mod utils;

pub(super) use functions::collect_function_spans;
pub(super) use imports::collect_import_spans;
pub(super) use indexes::collect_index_spans;
pub(super) use relations::collect_relation_spans;
pub(super) use rules::collect_rule_spans;
pub(super) use transformers::collect_transformer_spans;
pub(super) use typedefs::collect_typedef_spans;

pub(super) mod apply;
#[cfg(test)]
mod tests;
pub(super) use apply::collect_apply_spans;
