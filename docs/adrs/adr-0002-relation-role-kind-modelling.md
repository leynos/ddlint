# Architectural decision record (ADR) 002: relation role and kind modelling

## Status

Proposed.

## Date

2026-11-09

## Context and problem statement

Relation declarations are the first parser surface that combines several
independent declaration axes:

- role: explicit `input`, explicit `output`, or implicit internal;
- kind: explicit or implicit `relation`, `stream`, or `multiset`;
- reference marker: optional declaration-level `&`;
- body form: record fields or bracketed element type; and
- primary-key suffix: accepted only for input record relations.

Before roadmap item `2.6.6`, the scanner accepted only a narrow subset of this
surface and the typed `Relation` wrapper exposed only `is_input()` and
`is_output()`. That forced callers to infer kind, defaulting, ref status, and
body form from raw concrete syntax tree (CST) tokens.

ADR-001 defines a future split between the lossless syntax layer and the owned
semantic layer. Relation modelling therefore needs a syntax-layer API that is
explicit enough for lint and migration work without pretending to be the final
compiler semantic model.

## Decision drivers

- Preserve lossless CST access for source fidelity and diagnostics.
- Give callers typed access to relation role, kind, ref status, and body form.
- Keep implicit defaults distinguishable from source-present keywords.
- Avoid reserving an `internal` keyword that upstream DDlog does not reserve.
- Keep existing `is_input()` and `is_output()` users working through phase 2.
- Defer typed primary-key expression access until the semantic boundary is
  ready to own expression-level modelling (superseded by the amendment below,
  roadmap item `2.6.6.1`).

## Options considered

<!-- markdownlint-disable MD013 --><!-- ADR option rows stay intact for
side-by-side reviewability. -->

| Option                               | Summary                                                                                                                                                | Pros                                                                                                                                               | Cons                                                                                                                                                                |
| ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A: predicate helpers only            | Keep `is_input()` and `is_output()`, then add predicates such as `is_stream()` and `has_bracket_body()`.                                               | Minimal call-site churn; simple for narrow lint predicates.                                                                                        | Hides the full declaration shape; makes implicit defaults hard to distinguish; scales poorly as declaration axes grow.                                              |
| B: enums plus keyword-presence flags | Expose `RelationRole`, `RelationKind`, `RelationBody`, `is_ref()`, and parallel keyword-presence predicates while retaining derived legacy predicates. | Gives one coherent access pattern; models implicit defaults explicitly; supports exhaustive body matching; keeps existing predicate users working. | Callers that only need a boolean must learn the richer API; multi-token element type text is owned.                                                                 |
| C: defer modelling to semantic layer | Keep syntax wrappers minimal until the ADR-001 semantic crate split.                                                                                   | Avoids stabilizing syntax-layer helper names early; keeps compiler-facing modelling decisions out of the current crate.                            | Leaves lint rules and docs dependent on raw CST reconstruction; prevents conformance item `13` from closing; weakens parser contract tests before crate extraction. |

<!-- markdownlint-enable MD013 -->

## Decision outcome / proposed direction

Adopt **Option B**.

The syntax-layer `Relation` wrapper exposes:

- `RelationRole::{Input, Output, Internal}`;
- `RelationKind::{Relation, Stream, Multiset}`;
- `RelationBody::{Fields, ElementType}`;
- `role()` and `kind()` as canonical typed accessors;
- `role_keyword_present()` and `kind_keyword_present()` for lossless keyword-
  presence fidelity;
- `is_ref()` for declaration-level reference relations;
- `body()`, `element_type()`, and `columns()` for body-form inspection,
  returning `Result<_, RelationParseErrors>` so malformed or missing bodies are
  explicit errors rather than an empty field list; and
- derived `is_input()` and `is_output()` helpers for compatibility.

`Internal` means no role keyword was present. The parser does not reserve an
`internal` keyword. `RelationKind::Relation` is the default when no kind
keyword was present. `kind_keyword_present()` means the source explicitly wrote
one of `relation`, `stream`, or `multiset`.

`Relation::primary_key()` returns
`Result<Option<PrimaryKey>, RelationParseErrors>`: `Ok(None)` when no clause is
present, `Ok(Some(..))` when a well-formed clause is present, and `Err(..)`
when a clause is present but malformed. `PrimaryKey` exposes `binders()`,
`binder_names()`, `span()`, and `expression()`; each `PrimaryKeyBinder` carries
`name()` and a source-absolute `span()`; `PrimaryKeyExpression` carries
`text()`, `span()`, and `parse()`. See the amendment below.

## Goals and non-goals

Goals:

- make relation declaration axes explicit to syntax-layer callers,
- preserve CST fidelity and keyword-presence information,
- keep existing predicate helpers stable through the current parser phase, and
- align relation modelling with the conformance register and syntax spec.

Non-goals:

- define the final compiler semantic relation model,
- type-check relation semantics beyond parser-level diagnostics, or
- interpret primary-key binders or expressions; binder scope and
  row-versus-column interpretation stay in the semantic layer.

## Known risks and limitations

- `Relation::element_type()` allocates because bracket element types may span
  several CST tokens.
- The syntax-layer API can still evolve before the ADR-001 crate split freezes
  public parser contracts.
- `Relation::primary_key()` parses the clause on demand; callers that only need
  binder text should use `binder_names()` rather than `expression()`.

## Architectural rationale

The enum-plus-predicate shape keeps the syntax layer precise without confusing
implicit defaults with explicit source keywords. It also gives the future
semantic layer a clear extraction source: semantic modelling can consume
`role()`, `kind()`, `is_ref()`, and `body()` directly while still using CST
spans for diagnostics and provenance.

## Outstanding decisions

- Phase `2.8` must decide whether `is_input()` and `is_output()` remain public
  stable helpers or become compatibility shims.

## Amendment, 2026-11-09: primary-key expression access (2.6.6.1)

Roadmap item `2.6.6.1` amends this ADR. It adds typed syntax-layer access to
the primary-key clause and its optional spec-form expression without adding a
CST node.

- `Relation::primary_key()` returns
  `Result<Option<PrimaryKey>, RelationParseErrors>`; the `Vec<String>`
  binder/list form is retired.
- `PrimaryKey` exposes `binders()`, `binder_names()`, `span()`, and
  `expression()`. `PrimaryKeyBinder` carries `name()` and a source-absolute
  `span()`. `PrimaryKeyExpression` carries `text()`, `span()`, and `parse()`.
- `PrimaryKeyExpression::parse()` returns
  `Result<Expr, Vec<Simple<SyntaxKind>>>` with source-absolute error spans. It
  parses the preserved text lazily; the syntax layer owns no expression scope.
- The expression is the non-trivia hull of the existing `N_RELATION_DECL`
  tokens after the binder list. No `N_EXPR_NODE` nests inside the relation
  node. Nesting one is the migration path recorded for the ADR-001 crate split.
- The syntax layer does not decide whether a binder names a row or a column.
  That remains semantic-layer work in `ddlog-sema`.

The API is shaped by three invariants:

- `INV-INDEP`: for every clause the scanner accepts, a malformed or unparsable
  expression never hides the binders. `expression()` and `parse()` are separate
  observations. The guarantee is scoped to accepted clauses because a clause
  the scanner rejects is not attached to a relation at all: an unbalanced
  delimiter, as in `primary key (x) (`, reports `D-REL-007`, recovery skips the
  line, and no relation span is recorded, so `primary_key()` has nothing to
  read. Extending the guarantee to rejected clauses would change scanner
  recovery, which is outside this amendment.
- `INV-SHIFT`: errors returned by `parse()` are shifted to source-absolute
  spans, so a diagnostic can point into the original file.
- `INV-AGREE`: the scanner and the accessor agree on clause well-formedness. A
  depth-zero `;` ends the clause, and a binder list that is not a
  comma-separated identifier list is rejected with `D-REL-007`.
- `INV-TERM`: a `;` after a key changes nothing that follows. A keyed relation
  node owns the same trailing trivia as its keyless twin, so their spans differ
  only by the length of the removed clause.

Decisions `D1` to `D12` in the ExecPlan record the alternatives considered. In
particular, `D3` notes that the ADR-001 crate split should replace the exposed
`chumsky::error::Simple` type with a domain error type, and `D12` records why a
keyed relation node owns the trailing trivia that its keyless twin absorbs.

## Consequences

- Lint rules should prefer `role()` and `kind()` for new code.
- Existing `is_input()` and `is_output()` call sites remain valid and are backed
  by `role()`.
- Future semantic-layer work can map this CST-backed relation view into an
  owned model without reparsing source text.
- Any future relation body variant must update the `RelationBody` enum, forcing
  exhaustiveness at match sites.
- The ADR-001 split should treat these helpers as syntax-layer convenience APIs,
  not as the final compiler semantic relation model.

## Related documents

- [ADR-001: parser crate split for multi-consumer use](./adr-0001-parser-crate-split.md)
- [Differential Datalog parser syntax specification updated](../differential-datalog-parser-syntax-spec-updated.md)
- [Parser conformance register](../parser-conformance-register.md)
- [Parser implementation notes](../parser-implementation-notes.md)
