//! Internal semantic-model builder state and high-level collection passes.

use std::collections::{HashMap, HashSet};

use rowan::SyntaxNode;

use crate::Span;
use crate::SyntaxKind;
use crate::parser::ast;
use crate::parser::ast::SemanticRule;
use crate::parser::ast::rule::text_range_to_span;
use crate::sema::model::{
    DeclarationKind, Resolution, Scope, ScopeId, ScopeKind, ScopeOrigin, SemanticModel, Symbol,
    SymbolId, SymbolOrigin, UseKind,
};

use super::resolve::{collect_head_binding_names, collect_pattern_binding_names};

pub(crate) struct SymbolSpec {
    pub(crate) name: String,
    pub(crate) kind: DeclarationKind,
    pub(crate) origin: SymbolOrigin,
    pub(crate) scope: ScopeId,
    pub(crate) span: Span,
    pub(crate) name_span: Option<Span>,
    pub(crate) source_order: usize,
    pub(crate) visible_from_rule_order: usize,
}

pub(crate) struct ScopeSpec {
    pub(crate) kind: ScopeKind,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) origin: ScopeOrigin,
    pub(crate) span: Span,
}

pub(crate) struct SemanticModelBuilder {
    pub(crate) scopes: Vec<Scope>,
    pub(crate) symbols: Vec<Symbol>,
    pub(crate) symbols_by_scope_and_name: HashMap<ScopeId, HashMap<String, Vec<usize>>>,
    pub(crate) uses: Vec<crate::sema::UseSite>,
    pub(crate) program_scope: ScopeId,
}

impl SemanticModelBuilder {
    pub(crate) fn new(root: &SyntaxNode<crate::DdlogLanguage>) -> Self {
        let program_scope = ScopeId(0);
        Self {
            scopes: vec![Scope {
                kind: ScopeKind::Program,
                parent: None,
                origin: ScopeOrigin::Program,
                span: text_range_to_span(root.text_range()),
            }],
            symbols: Vec::new(),
            symbols_by_scope_and_name: HashMap::new(),
            uses: Vec::new(),
            program_scope,
        }
    }

    fn is_relation_read_use(use_site: &crate::sema::UseSite) -> bool {
        use_site.kind() == UseKind::Relation && use_site.origin().is_relation_read()
    }

    fn is_resolved_variable_use(use_site: &crate::sema::UseSite) -> bool {
        use_site.kind() == UseKind::Variable
            && matches!(use_site.resolution(), Resolution::Resolved(_))
    }

    fn resolved_symbol_id(use_site: &crate::sema::UseSite) -> Option<SymbolId> {
        match use_site.resolution() {
            Resolution::Resolved(symbol_id) => Some(symbol_id),
            Resolution::Unresolved | Resolution::Ignored => None,
        }
    }

    fn resolved_variable_symbol_id(use_site: &crate::sema::UseSite) -> SymbolId {
        debug_assert!(
            Self::is_resolved_variable_use(use_site),
            "resolved_variable_symbol_id requires a resolved variable use"
        );

        let Resolution::Resolved(symbol_id) = use_site.resolution() else {
            unreachable!("resolved variable uses must carry resolved symbol ids");
        };
        symbol_id
    }

    pub(crate) fn finish(self) -> SemanticModel {
        // Precompute span-to-relation-symbol index
        let span_to_relation_symbol: HashMap<Span, SymbolId> = self
            .symbols
            .iter()
            .enumerate()
            .filter(|(_, symbol)| symbol.kind() == DeclarationKind::Relation)
            .map(|(index, symbol)| (symbol.span().clone(), SymbolId(index)))
            .collect();

        // Precompute symbols-with-reads set
        let symbols_with_reads: HashSet<SymbolId> = self
            .uses
            .iter()
            .filter(|u| Self::is_relation_read_use(u))
            .filter_map(Self::resolved_symbol_id)
            .collect();

        let symbols_with_variable_uses: HashSet<SymbolId> = self
            .uses
            .iter()
            .filter(|u| Self::is_resolved_variable_use(u))
            .map(Self::resolved_variable_symbol_id)
            .collect();

        SemanticModel {
            program_scope: self.program_scope,
            scopes: self.scopes,
            symbols: self.symbols,
            uses: self.uses,
            span_to_relation_symbol,
            symbols_with_reads,
            symbols_with_variable_uses,
        }
    }

    pub(crate) fn collect_program_declarations(&mut self, root: &ast::Root) {
        let mut order = 0usize;
        for child in root.syntax().children() {
            if let Some(spec) = self.program_declaration_spec(&child, order) {
                self.declare_symbol(spec);
                order += 1;
            }
        }
    }

    pub(crate) fn collect_ast_rules(&mut self, root: &ast::Root) {
        for (rule_index, rule) in root.rules().into_iter().enumerate() {
            let rule_span = text_range_to_span(rule.syntax.text_range());
            let rule_scope = self.new_scope(ScopeSpec {
                kind: ScopeKind::Rule,
                parent: Some(self.program_scope),
                origin: ScopeOrigin::AstRule { index: rule_index },
                span: rule_span.clone(),
            });

            self.collect_rule_heads(
                super::traverse::RuleHeadContext {
                    scope: rule_scope,
                    span: &rule_span,
                    origin: SymbolOrigin::RuleHead,
                },
                &rule,
            );

            if let Ok(terms) = rule.body_terms() {
                let body_nodes = rule.body_expression_nodes();
                debug_assert_eq!(
                    body_nodes.len(),
                    terms.len(),
                    "rule body node/term count mismatch should stay visible during semantic collection"
                );

                for (literal_index, term) in terms.into_iter().enumerate() {
                    let maybe_node = body_nodes.get(literal_index);
                    let span =
                        maybe_node.map_or_else(|| rule_span.clone(), ast::RuleBodyExpression::span);
                    self.collect_rule_term(
                        super::variables::VariableUseContext::new(
                            rule_scope,
                            literal_index,
                            &span,
                            literal_index,
                        ),
                        maybe_node.map(ast::AstNode::syntax),
                        &term,
                    );
                }
            }
        }
    }

    pub(crate) fn collect_semantic_rules(&mut self, semantic_rules: &[SemanticRule]) {
        for (rule_index, rule) in semantic_rules.iter().enumerate() {
            let rule_span = rule.source_span();
            let rule_scope = self.new_scope(ScopeSpec {
                kind: ScopeKind::Rule,
                parent: Some(self.program_scope),
                origin: ScopeOrigin::SemanticRule {
                    index: rule_index,
                    origin: rule.origin(),
                },
                span: rule_span.clone(),
            });

            self.collect_head_expr(
                rule.head(),
                super::traverse::RuleHeadContext {
                    scope: rule_scope,
                    span: &rule_span,
                    origin: SymbolOrigin::SemanticRuleHead,
                },
            );
            for binding_name in collect_head_binding_names(rule.head()) {
                self.declare_symbol(SymbolSpec {
                    name: binding_name,
                    kind: DeclarationKind::RuleBinding,
                    origin: SymbolOrigin::SemanticRuleHead,
                    scope: rule_scope,
                    span: rule_span.clone(),
                    name_span: None,
                    source_order: self.symbols.len(),
                    visible_from_rule_order: 0,
                });
            }

            for pattern in rule.patterns() {
                // Parse-time semantic rules do not currently preserve CST token
                // handles, so precise identifier spans are unavailable here.
                for binding_name in collect_pattern_binding_names(pattern) {
                    self.declare_symbol(SymbolSpec {
                        name: binding_name,
                        kind: DeclarationKind::RuleBinding,
                        origin: SymbolOrigin::ForPattern,
                        scope: rule_scope,
                        span: rule_span.clone(),
                        name_span: None,
                        source_order: self.symbols.len(),
                        visible_from_rule_order: 0,
                    });
                }
            }

            for (literal_index, expr) in rule.body().iter().enumerate() {
                self.collect_expression_term(
                    expr,
                    super::variables::VariableUseContext::new(
                        rule_scope,
                        literal_index,
                        &rule_span,
                        literal_index,
                    ),
                );
            }
        }
    }

    pub(crate) fn new_scope(&mut self, spec: ScopeSpec) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len());
        self.scopes.push(Scope {
            kind: spec.kind,
            parent: spec.parent,
            origin: spec.origin,
            span: spec.span,
        });
        scope_id
    }

    fn program_declaration_spec(
        &self,
        child: &SyntaxNode<crate::DdlogLanguage>,
        source_order: usize,
    ) -> Option<SymbolSpec> {
        let (name, name_span, kind, origin) = match child.kind() {
            SyntaxKind::N_RELATION_DECL => {
                let relation = ast::Relation {
                    syntax: child.clone(),
                };
                (
                    relation.name(),
                    relation.name_span(),
                    DeclarationKind::Relation,
                    SymbolOrigin::RelationDeclaration,
                )
            }
            SyntaxKind::N_FUNCTION => {
                let function = ast::Function {
                    syntax: child.clone(),
                };
                (
                    function.name(),
                    function.name_span(),
                    DeclarationKind::Function,
                    SymbolOrigin::FunctionDeclaration,
                )
            }
            SyntaxKind::N_TYPE_DEF => {
                let type_def = ast::TypeDef {
                    syntax: child.clone(),
                };
                (
                    type_def.name(),
                    type_def.name_span(),
                    DeclarationKind::Type,
                    SymbolOrigin::TypeDeclaration,
                )
            }
            _ => return None,
        };

        name.map(|name| SymbolSpec {
            name,
            kind,
            origin,
            scope: self.program_scope,
            span: text_range_to_span(child.text_range()),
            name_span,
            source_order,
            visible_from_rule_order: 0,
        })
    }

    pub(crate) fn declare_symbol(&mut self, spec: SymbolSpec) -> SymbolId {
        let symbol_id = SymbolId(self.symbols.len());
        self.symbols_by_scope_and_name
            .entry(spec.scope)
            .or_default()
            .entry(spec.name.clone())
            .or_default()
            .push(symbol_id.0);
        self.symbols.push(Symbol {
            name: spec.name,
            kind: spec.kind,
            origin: spec.origin,
            scope: spec.scope,
            span: spec.span,
            name_span: spec.name_span,
            source_order: spec.source_order,
            visible_from_rule_order: spec.visible_from_rule_order,
        });
        symbol_id
    }
}
