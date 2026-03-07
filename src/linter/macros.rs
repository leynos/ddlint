//! Declarative macro support for defining CST lint rules.

/// Declare a zero-state lint rule with metadata and optional CST handlers.
#[macro_export]
macro_rules! declare_lint {
    (
        $(#[doc = $doc:literal])*
        $vis:vis $rule_name:ident {
            name: $name:literal,
            group: $group:literal,
            level: $level:ident,
            target_kinds: [$($kind:path),* $(,)?],
            fn check_node(&$node_self:ident, $node:ident, $node_ctx:ident, $node_diagnostics:ident) $node_body:block,
            fn check_token(&$token_self:ident, $token:ident, $token_ctx:ident, $token_diagnostics:ident) $token_body:block
            $(,)?
        }
    ) => {
        $crate::declare_lint!(@base
            $(#[doc = $doc])*
            $vis $rule_name,
            $name,
            $group,
            $level,
            [$($kind),*],
            {
                fn check_node(
                    &$node_self,
                    $node: &rowan::SyntaxNode<$crate::DdlogLanguage>,
                    $node_ctx: &$crate::linter::RuleCtx,
                    $node_diagnostics: &mut Vec<$crate::linter::LintDiagnostic>,
                ) $node_body

                fn check_token(
                    &$token_self,
                    $token: &rowan::SyntaxToken<$crate::DdlogLanguage>,
                    $token_ctx: &$crate::linter::RuleCtx,
                    $token_diagnostics: &mut Vec<$crate::linter::LintDiagnostic>,
                ) $token_body
            }
        );
    };
    (
        $(#[doc = $doc:literal])*
        $vis:vis $rule_name:ident {
            name: $name:literal,
            group: $group:literal,
            level: $level:ident,
            target_kinds: [$($kind:path),* $(,)?],
            fn check_node(&$node_self:ident, $node:ident, $node_ctx:ident, $node_diagnostics:ident) $node_body:block
            $(,)?
        }
    ) => {
        $crate::declare_lint!(@base
            $(#[doc = $doc])*
            $vis $rule_name,
            $name,
            $group,
            $level,
            [$($kind),*],
            {
                fn check_node(
                    &$node_self,
                    $node: &rowan::SyntaxNode<$crate::DdlogLanguage>,
                    $node_ctx: &$crate::linter::RuleCtx,
                    $node_diagnostics: &mut Vec<$crate::linter::LintDiagnostic>,
                ) $node_body
            }
        );
    };
    (
        $(#[doc = $doc:literal])*
        $vis:vis $rule_name:ident {
            name: $name:literal,
            group: $group:literal,
            level: $level:ident,
            target_kinds: [$($kind:path),* $(,)?],
            fn check_token(&$token_self:ident, $token:ident, $token_ctx:ident, $token_diagnostics:ident) $token_body:block
            $(,)?
        }
    ) => {
        $crate::declare_lint!(@base
            $(#[doc = $doc])*
            $vis $rule_name,
            $name,
            $group,
            $level,
            [$($kind),*],
            {
                fn check_token(
                    &$token_self,
                    $token: &rowan::SyntaxToken<$crate::DdlogLanguage>,
                    $token_ctx: &$crate::linter::RuleCtx,
                    $token_diagnostics: &mut Vec<$crate::linter::LintDiagnostic>,
                ) $token_body
            }
        );
    };
    (
        $(#[doc = $doc:literal])*
        $vis:vis $rule_name:ident {
            name: $name:literal,
            group: $group:literal,
            level: $level:ident,
            target_kinds: [$($kind:path),* $(,)?]
            $(,)?
        }
    ) => {
        $crate::declare_lint!(@base
            $(#[doc = $doc])*
            $vis $rule_name,
            $name,
            $group,
            $level,
            [$($kind),*],
            {}
        );
    };
    (@base
        $(#[doc = $doc:literal])*
        $vis:vis $rule_name:ident,
        $name:literal,
        $group:literal,
        $level:ident,
        [$($kind:path),*],
        {$($body:item)*}
    ) => {
        $(#[doc = $doc])*
        #[derive(Debug, Clone, Copy, Default)]
        $vis struct $rule_name;

        impl $crate::linter::Rule for $rule_name {
            fn name(&self) -> &'static str {
                $name
            }

            fn group(&self) -> &'static str {
                $group
            }

            fn docs(&self) -> &'static str {
                $crate::declare_lint!(@docs $($doc),*)
            }

            fn default_level(&self) -> $crate::linter::RuleLevel {
                $crate::declare_lint!(@level $level)
            }
        }

        impl $crate::linter::CstRule for $rule_name {
            fn target_kinds(&self) -> &'static [$crate::SyntaxKind] {
                &[$($kind),*]
            }

            $($body)*
        }
    };
    (@docs) => {
        ""
    };
    (@docs $($doc:literal),+ $(,)?) => {
        concat!($($doc, "\n"),+)
    };
    (@level allow) => {
        $crate::linter::RuleLevel::Allow
    };
    (@level hint) => {
        $crate::linter::RuleLevel::Hint
    };
    (@level warn) => {
        $crate::linter::RuleLevel::Warn
    };
    (@level error) => {
        $crate::linter::RuleLevel::Error
    };
}

#[cfg(test)]
mod tests {
    use rowan::NodeOrToken;

    use crate::linter::{CstRule, LintDiagnostic, Rule, RuleConfig, RuleCtx, RuleLevel};
    use crate::{SyntaxKind, parse};

    crate::declare_lint! {
        /// ## What it does
        /// Flags relation declarations.
        ///
        /// ## Example
        /// `input relation R(x: u32);`
        pub DocumentedRule {
            name: "documented-rule",
            group: "correctness",
            level: error,
            target_kinds: [SyntaxKind::N_RELATION_DECL],
            fn check_node(&self, node, _ctx, diagnostics) {
                diagnostics.push(LintDiagnostic::new(
                    self.name(),
                    "node hit",
                    node.text_range(),
                ));
            }
        }
    }

    crate::declare_lint! {
        /// Flags identifier tokens.
        pub TokenOnlyRule {
            name: "token-only-rule",
            group: "style",
            level: hint,
            target_kinds: [SyntaxKind::T_IDENT],
            fn check_token(&self, token, _ctx, diagnostics) {
                diagnostics.push(LintDiagnostic::new(
                    self.name(),
                    "token hit",
                    token.text_range(),
                ));
            }
        }
    }

    crate::declare_lint! {
        /// Matches but intentionally does nothing.
        pub NoOpRule {
            name: "no-op-rule",
            group: "test",
            level: allow,
            target_kinds: [SyntaxKind::N_RELATION_DECL, SyntaxKind::T_IDENT]
        }
    }

    fn run_rule_over_cst(source: &str, rule: &dyn CstRule) -> Vec<LintDiagnostic> {
        let parsed = parse(source);
        assert!(parsed.errors().is_empty());
        let ctx = RuleCtx::from_parsed(source, &parsed, RuleConfig::new());
        let mut diagnostics = Vec::new();

        for element in parsed.root().syntax().descendants_with_tokens() {
            if rule.target_kinds().contains(&element.kind()) {
                match &element {
                    NodeOrToken::Node(node) => {
                        rule.check_node(node, &ctx, &mut diagnostics);
                    }
                    NodeOrToken::Token(token) => {
                        rule.check_token(token, &ctx, &mut diagnostics);
                    }
                }
            }
        }

        diagnostics
    }

    #[test]
    fn macro_generated_metadata_is_available_through_trait_object() {
        let rule: &dyn Rule = &DocumentedRule;
        assert_eq!(rule.name(), "documented-rule");
        assert_eq!(rule.group(), "correctness");
        assert_eq!(rule.default_level(), RuleLevel::Error);
        assert!(rule.docs().contains("## What it does"));
        assert!(rule.docs().contains("Flags relation declarations."));
        assert!(rule.docs().contains("## Example"));
        assert!(rule.docs().contains("`input relation R(x: u32);`"));
    }

    #[test]
    fn macro_generated_target_kinds_are_exposed() {
        let rule: &dyn CstRule = &DocumentedRule;
        assert_eq!(rule.target_kinds(), &[SyntaxKind::N_RELATION_DECL]);
    }

    #[test]
    fn macro_generated_handlers_emit_diagnostics() {
        let source = "input relation R(x: u32);";
        let node_hits = run_rule_over_cst(source, &DocumentedRule);
        let token_hits = run_rule_over_cst(source, &TokenOnlyRule);

        assert_eq!(node_hits.len(), 1);
        assert_eq!(
            node_hits.first().map(LintDiagnostic::rule_name),
            Some("documented-rule")
        );
        assert!(!token_hits.is_empty());
        assert!(
            token_hits
                .iter()
                .all(|d| d.rule_name() == "token-only-rule")
        );
    }

    #[test]
    fn omitted_handlers_remain_no_op() {
        let diagnostics = run_rule_over_cst("input relation R(x: u32);", &NoOpRule);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn level_keywords_map_to_rule_levels() {
        let token_rule: &dyn Rule = &TokenOnlyRule;
        let no_op_rule: &dyn Rule = &NoOpRule;

        assert_eq!(token_rule.default_level(), RuleLevel::Hint);
        assert_eq!(no_op_rule.default_level(), RuleLevel::Allow);
    }
}
