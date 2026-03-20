//! Semantic rule representation for desugared constructs.
//!
//! `SemanticRule` captures rule-like output produced by parse-time lowering
//! steps. It is intentionally separate from CST-backed `Rule` nodes so the
//! parser can keep source-preserving syntax trees while still exposing derived
//! semantics such as top-level `for` desugaring.

use crate::Span;

use super::{Expr, Pattern};

/// Origin of a semantic rule emitted by the parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticRuleOrigin {
    /// Rule produced by lowering a top-level `for` statement.
    TopLevelFor,
}

/// Parameters for constructing a desugared semantic rule.
#[derive(Debug, Clone, PartialEq)]
pub struct SemanticRuleSpec {
    pub origin: SemanticRuleOrigin,
    pub source_span: Span,
    pub patterns: Vec<Pattern>,
    pub head: Expr,
    pub body: Vec<Expr>,
}

/// Desugared semantic rule.
#[derive(Debug, Clone, PartialEq)]
pub struct SemanticRule {
    origin: SemanticRuleOrigin,
    source_span: Span,
    patterns: Vec<Pattern>,
    head: Expr,
    body: Vec<Expr>,
}

impl SemanticRule {
    /// Construct a semantic rule.
    #[must_use]
    pub fn new(spec: SemanticRuleSpec) -> Self {
        Self {
            origin: spec.origin,
            source_span: spec.source_span,
            patterns: spec.patterns,
            head: spec.head,
            body: spec.body,
        }
    }

    /// Origin category for this rule.
    #[must_use]
    pub fn origin(&self) -> SemanticRuleOrigin {
        self.origin
    }

    /// Source span that produced this semantic rule.
    #[must_use]
    pub fn source_span(&self) -> Span {
        self.source_span.clone()
    }

    /// Lowered `for`-loop patterns that bind names for this rule.
    #[must_use]
    pub fn patterns(&self) -> &[Pattern] {
        &self.patterns
    }

    /// Rule head expression.
    #[must_use]
    pub fn head(&self) -> &Expr {
        &self.head
    }

    /// Rule body expressions in evaluation order.
    #[must_use]
    pub fn body(&self) -> &[Expr] {
        &self.body
    }
}
