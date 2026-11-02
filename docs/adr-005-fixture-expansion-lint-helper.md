# Architectural decision record (ADR) 005: fixture expansion lint helper

## Status

Accepted on 5 November 2026. The repository uses a dedicated `fixture-lints`
proc-macro crate to scope `unused_braces` suppression to qualifying `rstest`
fixtures.

## Date

2026-11-05.

## Context and problem statement

The repository formats Rust with `fn_single_line`, which collapses a
single-expression function onto one line. `rstest` re-emits an annotated
fixture's body inside its generated code as `{ { body } }`. Because the body
already carries the fixture's braces, rustc's `unused_braces` lint reports the
outer pair as redundant. The finding is an artefact of formatting plus macro
expansion, not a defect in the fixture, and it would fail builds that treat
warnings as errors.

A source-level `#[allow(unused_braces)]` on the fixture would suppress the lint
for the whole expanded item, including any genuine finding in the same fixture.
A source-level `#[expect(unused_braces)]` is not a workable alternative either,
because `rstest` drops user-written expectations. The suppression therefore
needs to be emitted conditionally, by a macro that runs before `rstest` expands
the item, and only for the fixture shape that produces the artefact.

## Decision drivers

- Keep the suppression narrow, explicit, and auditable.
- Preserve genuine `unused_braces` findings in fixtures.
- Satisfy the repository's lint policy: attributes carry a reason, and `expect`
  must not be used silently.
- Avoid a runtime dependency and keep the publishable root crate packageable.
- Bind the boundary with automated compile tests rather than prose alone.

## Decision

A separate, unpublished `fixture-lints` proc-macro crate owns
`#[allow_fixture_expansion_lints]`. The attribute is applied immediately above
`#[fixture]`, so the macro sees the original item before `rstest` expands it.

The macro parses the item as a function and emits
`#[allow(unused_braces, reason = "rstest fixture expansion")]` only when the
body is exactly one expression statement without a trailing semicolon, that is
`[syn::Stmt::Expr(_, None)]`. Every other item passes through unchanged.

A proc-macro crate is required rather than a source-level attribute for three
reasons. First, the decision to suppress depends on the parsed shape of the
body, which cannot be expressed by a fixed source attribute. Second, an
attribute macro runs before `rstest`, so the emitted allowance is carried into
the generated code that triggers the diagnostic. Third, centralizing the rule
in one crate prevents the suppression from being copied and widened at
individual call-sites.

The helper narrows to a single expression statement without a trailing
semicolon because only that shape is collapsed by `fn_single_line` into the
double-braced expansion. A single statement ending in `;`, such as
`return { value };`, is `syn::Stmt::Expr(_, Some(_))`; it is not the formatting
artefact and must retain its genuine `unused_braces` diagnostic.
Multi-statement bodies are likewise untouched.

The helper emits `allow` with a reason rather than `expect` because `rstest`
drops user-written expectations, and a macro-emitted expectation is silently
unchecked when `report_in_external_macro` is false. An unchecked expectation
would defeat its purpose. The reasoned `allow` also satisfies the repository's
`allow_attributes` and `allow_attributes_without_reason` policy.

Genuine findings remain visible. The boundary is bound by three compile-fail
cases in `tests/fixture_expansion_compile.rs`: the diagnostic appears without
the attribute, it is not masked in a multi-statement fixture, and a single
statement with a trailing semicolon does not qualify.

## Consequences

- Every qualifying `rstest` fixture must place the attribute above `#[fixture]`.
- Fixtures with a trailing semicolon, with multiple statements, or that do not
  parse as functions are returned unchanged.
- The helper is a path-only development dependency, so
  `cargo package -p ddlint` does not try to resolve it from a registry.
- The compile-test contract must be updated whenever the qualification rule
  changes; the three compile-fail cases pin the current boundary.
- A formatting change that stops collapsing a fixture could remove the
  artefact, and one that starts collapsing a different shape could introduce
  it. The compile cases make either change visible.

## Alternatives considered

- **Source-level `#[allow(unused_braces)]` on each fixture.** Unconditional for
  the whole expanded item, so it masks genuine findings in the same fixture.
- **Source-level `#[expect(unused_braces)]`.** `rstest` drops user-written
  expectations, so the attribute neither checks nor suppresses the diagnostic.
- **Crate-level `unused_braces` allowance.** Masks every instance in the crate
  rather than the macro-expansion artefact.
- **Disabling `fn_single_line`.** Changes the repository formatting standard to
  accommodate a macro's expansion, with a wide blast radius.
- **Emitting the allowance for every fixture.** Simpler, but it would hide a
  genuine finding in any multi-statement or trailing-semicolon fixture.
