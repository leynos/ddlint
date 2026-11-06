# Architectural decision record (ADR) 005: fixture expansion lint helper

## Status

Accepted on 5 November 2026. The repository uses a dedicated `fixture-lints`
proc-macro crate to scope `unused_braces` suppression to qualifying `rstest`
fixtures.

## Date

2026-11-05.

## Context and problem statement

A fixture written as a single expression on one line — whether by hand or by a
formatter configured with `fn_single_line` — is re-emitted by `rstest` inside
its generated code as `{ { body } }`. Because the body already carries the
fixture's braces, rustc's `unused_braces` lint reports the outer pair as
redundant. The finding is an artefact of source shape plus macro expansion, not
a defect in the fixture, and it would fail builds that treat warnings as errors.

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
semicolon because only that shape is collapsed onto one line and then
re-emitted as the double-braced expansion. A single statement ending in `;`,
such as `return { value };`, is `syn::Stmt::Expr(_, Some(_))`; it is not the
formatting artefact and must retain its genuine `unused_braces` diagnostic.
Multi-statement bodies are likewise untouched.

The helper emits `allow` with a reason rather than `expect` because `rstest`
drops user-written expectations, and a macro-emitted expectation is silently
unchecked when `report_in_external_macro` is false. An unchecked expectation
would defeat its purpose. The reasoned `allow` also satisfies the repository's
`allow_attributes` and `allow_attributes_without_reason` policy.

Genuine findings remain visible. The boundary is bound by four compile-fail
cases in `tests/fixture_expansion_compile.rs`: the diagnostic appears without
the attribute, it is not masked in a multi-statement fixture, a single
statement with a trailing semicolon does not qualify, and an unparsable
non-function item keeps its own diagnostic.

## Consequences

- Every qualifying `rstest` fixture must place the attribute above `#[fixture]`.
- Fixtures with a trailing semicolon, with multiple statements, or that do not
  parse as functions are returned unchanged.
- The helper is a path-only development dependency, so
  `cargo package -p ddlint` does not try to resolve it from a registry.
- The compile-test contract must be updated whenever the qualification rule
  changes; the four compile-fail cases pin the current boundary.
- A formatting change that stops collapsing a fixture could remove the
  artefact, and one that starts collapsing a different shape could introduce
  it. The compile cases make either change visible.

## Addendum 2026-11-06: scope and limitations

- The helper is independent of formatter configuration: it triggers on the
  item's syntactic shape alone, matching a function whose body is a single
  expression statement without a trailing semicolon. It is therefore correct
  whether or not `fn_single_line` is enabled.
- It is a no-op for multi-statement bodies and for items that are not
  functions. Those items pass through unchanged.
- It cannot use `#[expect]`; the [decision](#decision) above explains why
  `rstest` drops user-written expectations and a macro-emitted expectation is
  unchecked.
- It does not detect or enforce the repository's formatter configuration.
  Keeping a qualifying fixture on one line remains a source-authoring and
  formatting concern.

## Addendum 2026-11-06: item-wide allowance qualification

This addendum supersedes the broader claim, retained above as history, that
"Genuine findings remain visible" and that the helper "leaves every other
`unused_braces` finding visible".

- The emitted `allow` is item-wide. It applies to the whole qualified function,
  not only to the braces `rstest` introduces.
- The qualification rule is syntactic: a function whose body is exactly one
  expression statement without a trailing semicolon, that is
  `[syn::Stmt::Expr(_, None)]`. That shape includes a semicolon-free
  `return { value }`, which is a genuine `unused_braces` finding rather than a
  macro-expansion artefact. The item-wide `allow` therefore also suppresses
  that genuine finding in such a body.
- The pass-through boundary remains accurate: multi-statement bodies, a single
  expression statement ending in a trailing semicolon, and items that do not
  parse as functions pass through unchanged, so genuine findings in those
  shapes remain visible. The four compile-fail cases pin that boundary.
- `fn_single_line` remains a separate, planned formatter configuration. It is
  not enabled by this helper, and the helper neither inspects nor enforces it.

## Alternatives considered

- **Source-level `#[allow(unused_braces)]` on each fixture.** Unconditional for
  the whole expanded item, so it masks genuine findings in the same fixture.
- **Source-level `#[expect(unused_braces)]`.** `rstest` drops user-written
  expectations, so the attribute neither checks nor suppresses the diagnostic.
- **Crate-level `unused_braces` allowance.** Masks every instance in the crate
  rather than the macro-expansion artefact.
- **Not collapsing single-expression functions onto one line.** Removes a
  formatting option that produces the artefact, but changes the repository
  formatting standard to accommodate a macro's expansion, with a wide blast
  radius.
- **Emitting the allowance for every fixture.** Simpler, but it would hide a
  genuine finding in any multi-statement or trailing-semicolon fixture.
