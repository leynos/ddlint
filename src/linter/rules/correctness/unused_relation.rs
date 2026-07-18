//! `unused-relation` warns about declared relations with no resolved read-like uses.
//!
//! A relation counts as read when it appears in a rule body, `for` iterable, or
//! `for` guard position and that use resolves to the declaration. Rule-head
//! writes and unresolved relation uses do not count as reads, so head-only
//! relations and relations referenced only in broken rules still trigger
//! warnings.
//!
//! This rule uses `SemanticModel::has_resolved_relation_read()` to check
//! whether a relation has at least one resolved read-like use.

use crate::linter::{LintDiagnostic, Rule};
use crate::parser::ast::rule::text_range_to_span;
use crate::{SyntaxKind, declare_lint};

declare_lint! {
    /// Detects relations declared but with no resolved read-like uses.
    ///
    /// A relation is considered read when it appears in a rule body, `for`
    /// iterable, or `for` guard and that use resolves to the declaration.
    /// Rule-head writes and unresolved uses do not count.
    pub UnusedRelationRule {
        name: "unused-relation",
        group: "correctness",
        level: warn,
        target_kinds: [SyntaxKind::N_RELATION_DECL],
        fn check_node(&self, node, ctx, diagnostics) {
            let declaration_range = node.text_range();
            let declaration_span = text_range_to_span(declaration_range);
            let Some(symbol_id) = ctx
                .semantic_model()
                .relation_symbol_at_span(&declaration_span)
            else {
                return;
            };
            let Some(symbol) = ctx.semantic_model().symbol(symbol_id) else {
                return;
            };

            if ctx.semantic_model().has_resolved_relation_read(symbol_id) {
                return;
            }

            diagnostics.push(LintDiagnostic::new(
                self.name(),
                format!("relation `{}` is declared but never read from", symbol.name()),
                declaration_range,
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    //! Tests for the unused relation rule.
    use crate::linter::testing::run_rule;
    use rstest::rstest;

    #[rstest]
    fn warns_for_declared_relation_with_no_reads() {
        let diagnostics = run_rule(
            super::UnusedRelationRule,
            concat!(
                "input relation Source(x: u32)\n",
                "relation Sink(x: u32)\n",
                "Sink(x) :- Source(x).\n",
            ),
        );

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics
                .first()
                .map(crate::linter::LintDiagnostic::rule_name),
            Some("unused-relation")
        );
        assert_eq!(
            diagnostics
                .first()
                .map(crate::linter::LintDiagnostic::message),
            Some("relation `Sink` is declared but never read from")
        );
    }

    #[rstest]
    fn does_not_warn_for_relation_with_resolved_read() {
        let diagnostics = run_rule(
            super::UnusedRelationRule,
            concat!(
                "input relation Source(x: u32)\n",
                "relation Used(x: u32)\n",
                "relation Sink(x: u32)\n",
                "Used(x) :- Source(x).\n",
                "Sink(x) :- Used(x).\n",
            ),
        );

        assert!(
            diagnostics.iter().all(|diagnostic| diagnostic.message()
                != "relation `Used` is declared but never read from"),
            "Used should not be reported once it is read from a rule body",
        );
    }

    #[rstest]
    fn ignores_unresolved_relation_uses_when_checking_reads() {
        let diagnostics = run_rule(
            super::UnusedRelationRule,
            concat!(
                "relation Declared(x: u32)\n",
                "relation Sink(x: u32)\n",
                "Sink(x) :- Missing(x).\n",
            ),
        );

        let messages: Vec<_> = diagnostics
            .iter()
            .map(crate::linter::LintDiagnostic::message)
            .collect();
        assert!(messages.contains(&"relation `Declared` is declared but never read from"));
    }
}
