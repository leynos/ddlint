//! `unused-variable` warns about rule-local bindings with no resolved uses.
//!
//! The rule operates over semantic `RuleBinding` symbols rather than raw CST
//! name matching so shadowing, unresolved names, and wildcard ignores follow
//! the semantic model's existing contracts. Diagnostics currently use the
//! binding symbol's precise identifier span when semantic collection captured
//! one, falling back to the existing coarse provenance otherwise.

use crate::linter::{LintDiagnostic, Rule};
use crate::{SyntaxKind, declare_lint};

declare_lint! {
    /// Detects rule-local bindings that are defined but never used.
    ///
    /// This includes bindings introduced by rule heads, assignment patterns,
    /// and `for`-loop patterns. The wildcard name `_` is an explicit ignore
    /// and is not recorded as a warning-eligible binding.
    pub UnusedVariableRule {
        name: "unused-variable",
        group: "correctness",
        level: warn,
        target_kinds: [SyntaxKind::N_DATALOG_PROGRAM],
        fn check_node(&self, node, ctx, diagnostics) {
            if node != ctx.cst_root() {
                return;
            }

            for (symbol_id, symbol) in ctx.semantic_model().rule_binding_symbols() {
                if ctx.semantic_model().has_resolved_variable_use(symbol_id) {
                    continue;
                }

                let span = symbol.name_span().unwrap_or_else(|| symbol.span());
                let Some(range) = crate::linter::span_utils::span_to_text_range(span) else {
                    continue;
                };

                diagnostics.push(LintDiagnostic::new(
                    self.name(),
                    format!(
                        "variable `{}` is defined but never used in this rule",
                        symbol.name()
                    ),
                    range,
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    //! Tests for the unused variable rule.
    use crate::linter::testing::run_rule;
    use rstest::rstest;

    #[rstest]
    fn warns_for_unused_head_binding() {
        let diagnostics = run_rule(super::UnusedVariableRule, "Output(head_x) :- Source(_).");

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics
                .first()
                .map(crate::linter::LintDiagnostic::rule_name),
            Some("unused-variable")
        );
        assert_eq!(
            diagnostics
                .first()
                .map(crate::linter::LintDiagnostic::message),
            Some("variable `head_x` is defined but never used in this rule")
        );
        assert_eq!(
            diagnostics.first().map(crate::linter::LintDiagnostic::span),
            Some(rowan::TextRange::new(7.into(), 13.into()))
        );
    }

    #[rstest]
    fn does_not_warn_for_binding_with_resolved_use() {
        let diagnostics = run_rule(
            super::UnusedVariableRule,
            "Output(head_x) :- Source(head_x), var assigned_x = Seed(head_x), Use(assigned_x).",
        );

        assert!(
            diagnostics.is_empty(),
            "resolved variable uses should suppress the warning",
        );
    }

    #[rstest]
    fn ignores_wildcards_and_unresolved_names() {
        let diagnostics = run_rule(
            super::UnusedVariableRule,
            "Output(head_x) :- Missing(other_y).",
        );

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics
                .first()
                .map(crate::linter::LintDiagnostic::message),
            Some("variable `head_x` is defined but never used in this rule")
        );
    }
}
