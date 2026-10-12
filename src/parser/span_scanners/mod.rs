//! Cohesive scanners for top-level statement spans.
//!
//! Each submodule focuses on a specific statement category, keeping scanning
//! logic small and composable. The parent `span_scanner` module orchestrates
//! these specialised scanners to collect spans before full parsing.

pub(super) mod functions;
pub(super) mod imports;
pub(super) mod indexes;
mod indexes_support;
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
pub(super) mod attributes;
#[cfg(test)]
mod tests;
pub(super) use apply::collect_apply_spans;
pub(super) use attributes::collect_attribute_spans;

/// Relation body delimiters retained for suffix eligibility checks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BodyForm {
    /// Parenthesized record columns.
    Record,
    /// Bracketed element type.
    Bracket,
}

/// Body information used only to validate and recover a relation suffix.
/// For example, an input record permits a primary key, while a bracket body does not.
#[derive(Clone, Copy)]
struct RelationSuffixContext {
    /// Explicit preamble role determining primary-key eligibility.
    role: Option<relations::RelationRole>,
    /// Parsed body form determining whether primary keys are permitted.
    body: BodyForm,
    /// Body end retained when an invalid suffix must be skipped during recovery.
    body_end: usize,
}
