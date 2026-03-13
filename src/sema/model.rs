//! Owned semantic model types for symbol tables and scope resolution.
//!
//! The semantic model stores only owned data and opaque identifiers so it can
//! be shared safely across linter worker threads.

use crate::Span;
use crate::parser::ast::SemanticRuleOrigin;

/// Opaque identifier for one scope in a semantic model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(pub(crate) usize);

/// Opaque identifier for one declared symbol in a semantic model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(pub(crate) usize);

/// Scope category recorded by semantic analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Root scope containing top-level declarations.
    Program,
    /// One syntactic rule or one desugared semantic rule.
    Rule,
    /// Nested scope created for a `for`-loop body.
    ForLoop,
}

/// Provenance for one recorded scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeOrigin {
    /// The root program scope.
    Program,
    /// Scope for an AST-backed rule.
    AstRule { index: usize },
    /// Scope for a parse-time semantic rule.
    SemanticRule {
        index: usize,
        origin: SemanticRuleOrigin,
    },
    /// Scope for a nested `for`-loop body.
    ForLoop { literal_index: usize },
}

/// Declaration category recorded in the symbol table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationKind {
    /// Top-level relation declaration.
    Relation,
    /// Top-level function declaration.
    Function,
    /// Top-level type declaration.
    Type,
    /// Rule-local binding introduced by a head, assignment, or `for` pattern.
    RuleBinding,
}

/// Provenance for one symbol record.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolOrigin {
    /// Symbol comes from a top-level relation declaration.
    RelationDeclaration,
    /// Symbol comes from a top-level function declaration.
    FunctionDeclaration,
    /// Symbol comes from a top-level type declaration.
    TypeDeclaration,
    /// Symbol comes from a variable in a rule head.
    RuleHead,
    /// Symbol comes from a variable in a desugared semantic rule head.
    SemanticRuleHead,
    /// Symbol comes from an assignment pattern.
    AssignmentPattern,
    /// Symbol comes from a `for`-loop pattern.
    ForPattern,
}

/// Name-use category recorded by semantic analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UseKind {
    /// Use of a relation-like atom in a rule position.
    Relation,
    /// Use of a rule-local variable.
    Variable,
}

/// Final name-resolution result for one use site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    /// The use resolved to a recorded symbol.
    Resolved(SymbolId),
    /// The use did not resolve to any visible symbol.
    Unresolved,
    /// The use is intentionally ignored, for example `_`.
    Ignored,
}

/// One lexical scope in the semantic model.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub(crate) kind: ScopeKind,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) origin: ScopeOrigin,
    pub(crate) span: Span,
}

impl Scope {
    /// Category of this scope.
    #[must_use]
    pub fn kind(&self) -> ScopeKind {
        self.kind
    }

    /// Parent scope, if any.
    #[must_use]
    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    /// Provenance for this scope.
    #[must_use]
    pub fn origin(&self) -> ScopeOrigin {
        self.origin
    }

    /// Source span covering the scope owner.
    #[must_use]
    pub fn span(&self) -> &Span {
        &self.span
    }
}

/// One declaration or binding recorded in the symbol table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub(crate) name: String,
    pub(crate) kind: DeclarationKind,
    pub(crate) origin: SymbolOrigin,
    pub(crate) scope: ScopeId,
    pub(crate) span: Span,
    pub(crate) source_order: usize,
    pub(crate) visible_from_rule_order: usize,
}

impl Symbol {
    /// Symbol name as written in the source.
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Declaration category.
    #[must_use]
    pub fn kind(&self) -> DeclarationKind {
        self.kind
    }

    /// Provenance for this symbol.
    #[must_use]
    pub fn origin(&self) -> SymbolOrigin {
        self.origin
    }

    /// Scope that owns this symbol.
    #[must_use]
    pub fn scope(&self) -> ScopeId {
        self.scope
    }

    /// Source span that introduced this symbol.
    #[must_use]
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Stable source-order index within the owning scope.
    #[must_use]
    pub fn source_order(&self) -> usize {
        self.source_order
    }

    /// First top-level rule-body position where this symbol becomes visible.
    #[must_use]
    pub fn visible_from_rule_order(&self) -> usize {
        self.visible_from_rule_order
    }
}

/// One recorded name use together with its resolution outcome.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseSite {
    pub(crate) name: String,
    pub(crate) kind: UseKind,
    pub(crate) scope: ScopeId,
    pub(crate) span: Span,
    pub(crate) source_order: usize,
    pub(crate) resolution: Resolution,
}

impl UseSite {
    /// Name text used at this site.
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Use category.
    #[must_use]
    pub fn kind(&self) -> UseKind {
        self.kind
    }

    /// Scope in which the use occurred.
    #[must_use]
    pub fn scope(&self) -> ScopeId {
        self.scope
    }

    /// Source span covering this use site.
    #[must_use]
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Stable source-order index for this use site.
    #[must_use]
    pub fn source_order(&self) -> usize {
        self.source_order
    }

    /// Resolution outcome for this use site.
    #[must_use]
    pub fn resolution(&self) -> Resolution {
        self.resolution
    }
}

/// Immutable semantic-analysis result for one parsed `DDlog` program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticModel {
    pub(crate) program_scope: ScopeId,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) symbols: Vec<Symbol>,
    pub(crate) uses: Vec<UseSite>,
}

impl SemanticModel {
    /// Return the program scope identifier.
    #[must_use]
    pub fn program_scope(&self) -> ScopeId {
        self.program_scope
    }

    /// Return every recorded scope in stable build order.
    #[must_use]
    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    /// Return every recorded symbol in stable build order.
    #[must_use]
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    /// Return every recorded use site in stable build order.
    #[must_use]
    pub fn uses(&self) -> &[UseSite] {
        &self.uses
    }

    /// Return one scope by opaque identifier.
    ///
    /// # Panics
    ///
    /// Panics if `id` does not refer to a recorded semantic scope.
    #[must_use]
    pub fn scope(&self, id: ScopeId) -> &Scope {
        self.scopes.get(id.0).map_or_else(
            || panic!("invalid semantic scope id: {}", id.0),
            |scope| scope,
        )
    }

    /// Return one symbol by opaque identifier.
    ///
    /// # Panics
    ///
    /// Panics if `id` does not refer to a recorded semantic symbol.
    #[must_use]
    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        self.symbols.get(id.0).map_or_else(
            || panic!("invalid semantic symbol id: {}", id.0),
            |symbol| symbol,
        )
    }

    /// Return the resolved symbol for a use site when resolution succeeded.
    #[must_use]
    pub fn resolved_symbol(&self, use_site: &UseSite) -> Option<&Symbol> {
        match use_site.resolution() {
            Resolution::Resolved(symbol_id) => Some(self.symbol(symbol_id)),
            Resolution::Unresolved | Resolution::Ignored => None,
        }
    }
}
