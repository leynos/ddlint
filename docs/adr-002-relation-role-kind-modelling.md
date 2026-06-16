# Architectural decision record (ADR) 002: relation role and kind modelling

## Status

Proposed.

## Date

2026-06-16

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
body form from raw CST tokens.

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
  ready to own expression-level modelling.

## Options considered

<!-- markdownlint-disable MD013 --><!-- ADR option rows stay intact for
side-by-side reviewability. -->

| Option                               | Summary                                                                                                                                                | Pros                                                                                                                                               | Cons                                                                                                                                                                |
| ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A: predicate helpers only            | Keep `is_input()` and `is_output()`, then add predicates such as `is_stream()` and `has_bracket_body()`.                                               | Minimal call-site churn; simple for narrow lint predicates.                                                                                        | Hides the full declaration shape; makes implicit defaults hard to distinguish; scales poorly as declaration axes grow.                                              |
| B: enums plus keyword-presence flags | Expose `RelationRole`, `RelationKind`, `RelationBody`, `is_ref()`, and parallel keyword-presence predicates while retaining derived legacy predicates. | Gives one coherent access pattern; models implicit defaults explicitly; supports exhaustive body matching; keeps existing predicate users working. | Callers that only need a boolean must learn the richer API; multi-token element type text is owned.                                                                 |
| C: defer modelling to semantic layer | Keep syntax wrappers minimal until the ADR-001 semantic crate split.                                                                                   | Avoids stabilizing syntax-layer helper names early; keeps compiler-facing modelling decisions out of the current crate.                            | Leaves lint rules and docs dependent on raw CST reconstruction; prevents conformance item `13` from closing; weakens parser contract tests before crate extraction. |

<!-- markdownlint-enable MD013 -->

## Decision Outcome / Proposed Direction

Adopt **Option B**.

The syntax-layer `Relation` wrapper exposes:

- `RelationRole::{Input, Output, Internal}`;
- `RelationKind::{Relation, Stream, Multiset}`;
- `RelationBody::{Fields, ElementType}`;
- `role()` and `kind()` as canonical typed accessors;
- `role_keyword_present()` and `kind_keyword_present()` for lossless default
  fidelity;
- `is_ref()` for declaration-level reference relations;
- `body()` and `element_type()` for body-form inspection; and
- derived `is_input()` and `is_output()` helpers for compatibility.

`Internal` means no role keyword was present. The parser does not reserve an
`internal` keyword. `Relation` means no kind keyword was present unless
`kind_keyword_present()` returns true.

`Relation::primary_key()` keeps returning the names from the parenthesized
binder/list portion of the suffix. Spec-form trailing primary-key expressions
are preserved in CST text, but typed expression access is deferred to roadmap
follow-up `2.6.6.1`.

## Goals and Non-Goals

Goals:

- make relation declaration axes explicit to syntax-layer callers,
- preserve CST fidelity and keyword-presence information,
- keep existing predicate helpers stable through the current parser phase, and
- align relation modelling with the conformance register and syntax spec.

Non-goals:

- define the final compiler semantic relation model,
- type-check relation semantics beyond parser-level diagnostics, or
- expose typed access to primary-key expressions in this ADR.

## Known Risks and Limitations

- `Relation::element_type()` allocates because bracket element types may span
  several CST tokens.
- The syntax-layer API can still evolve before the ADR-001 crate split freezes
  public parser contracts.
- `Relation::primary_key()` exposes only binder/list names; callers that need
  the trailing expression must wait for `2.6.6.1`.

## Architectural Rationale

The enum-plus-predicate shape keeps the syntax layer precise without confusing
implicit defaults with explicit source keywords. It also gives the future
semantic layer a clear extraction source: semantic modelling can consume
`role()`, `kind()`, `is_ref()`, and `body()` directly while still using CST
spans for diagnostics and provenance.

## Outstanding Decisions

- Roadmap item `2.6.6.1` must decide the typed shape for primary-key expression
  access.
- Phase `2.8` must decide whether `is_input()` and `is_output()` remain public
  stable helpers or become compatibility shims.

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

- [ADR-001: parser crate split for multi-consumer use](./adr-001-parser-crate-split.md)
- [Differential Datalog parser syntax specification updated](./differential-datalog-parser-syntax-spec-updated.md)
- [Parser conformance register](./parser-conformance-register.md)
- [Parser implementation notes](./parser-implementation-notes.md)
