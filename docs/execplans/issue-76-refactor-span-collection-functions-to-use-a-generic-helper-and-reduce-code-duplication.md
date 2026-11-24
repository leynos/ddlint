# Issue 76: share single-keyword span collection without merging scanner policy

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: IMPLEMENTED; local validation passed; delivery tracked in
[PR #481](https://github.com/leynos/ddlint/pull/481)

Branch:
`issue-76-refactor-span-collection-functions-to-use-a-generic-helper-and-reduce-code-duplication`

Roadmap item: none. This is a parser maintainability change tracked by
[issue 76](https://github.com/leynos/ddlint/issues/76).

## Purpose / big picture

Several functions in `src/parser/span_scanners/` walk the token stream and
record the span of one top-level declaration kind. Fourteen of them repeat the
same four steps: create a `SpanCollector`, match one or more keyword tokens,
run a Chumsky parser over the remaining tokens, and either record the resulting
span or recover at the next line.

Issue 76 asks for a generic helper so each scanner only supplies its keyword
and grammar. After the change, a maintainer adding another single-keyword
scanner writes one small grammar function and one call to
`collect_with_parser`, rather than copying the loop, the collector setup, the
success path, and the recovery path. Existing users observe no change: the same
declarations produce the same spans and the same diagnostics, in the same order.

The honest scope is narrower than the issue's estimate. Only `imports.rs` and
`apply.rs` match one keyword, use no guard, accept the whole parsed span, and
recover exactly as `parse_and_record` does. The other scanners differ in
grammar shape, candidate discovery, span acceptance, or diagnostic policy, so
they keep their own orchestration. The roughly 120-line reduction claimed by
the issue is therefore not reachable; this plan records that in the pull
request instead of forcing unrelated scanners through one shape.

## Constraints

- `src/parser/span_scanners/utils.rs` must stay at or below 400 lines, and every
  module (including test modules) must begin with a `//!` comment. The 400-line
  ceiling comes from repository convention in `AGENTS.md` and the Whitaker gate.
- No `#[allow]` attributes and no `#[expect]` additions for this work. Unused
  imports left by the refactor must be deleted.
- Production code must not use `.unwrap()` or `.expect()`.
- `collect_with_parser` must compose the existing `parse_and_record` policy:
  record on success, advance to the span end, append diagnostics, and skip the
  line on failure. The issue's example handler silently drops diagnostics on
  success; that behaviour must not be adopted.
- `collect_with_parser` is restricted to single-keyword scanners whose policy
  equals `parse_and_record`. It must not absorb grammar, span filtering,
  diagnostic replacement, or custom recovery.
- The grammars in `imports.rs` and `apply.rs` must not change. Accepted syntax
  and produced diagnostics stay identical.
- `lexer_helpers.rs`, `span_collector.rs`, the specialized scanners, and
  completed ExecPlans must not be modified.
- Public API: none changes. The helper is `pub(super)`.
- Dependencies: none added. Chumsky 0.9 is already a dependency.
- Implementation proceeds under the user's standing instruction to complete the
  work and open a draft pull request; no separate approval round applies.

## Tolerances (exception triggers)

- Files: more than 11 non-ExecPlan files changed, excluding this ExecPlan.
- Code: more than 200 net lines changed (additions minus deletions) across
  `src/` and `tests/`, excluding this ExecPlan file.
- Documentation: more than 150 net lines changed outside `docs/execplans/`.
- Interface: any change to `pub` items or to the parser's observable
  diagnostics beyond the pinned baseline.
- Dependencies: any new or upgraded dependency.
- Architecture: any change to `SpanCollector`, `TokenStream`, or the
  `parse_and_record` contract.
- Iterations: more than 3 failed attempts at the same test or lint finding.
- Ambiguity: a scanner cannot be classified as policy-equal to
  `parse_and_record` without a judgement call that changes output.

Baseline survey: `utils.rs` is 56 lines, `imports.rs` 55, `apply.rs` 54, and
`tests/mod.rs` 353. The test module is already near the 400-line ceiling, so
new tests go into sibling modules. No comparable past ExecPlan added a
shared-helper abstraction to the span scanners; the nearest precedent
(`2-6-7-finalize-legacy-token-compatibility-policy`) split an oversized test
module instead of growing it.

## Risks

- Risk: the helper changes span boundaries or diagnostic order for imports or
  apply. Severity: high Likelihood: medium Mitigation: pin both scanners and
  `parse_and_record` with regression tests before the refactor, then rerun them
  unchanged after it.
- Risk: a `FnMut() -> P` factory bound conflicts with opaque return types or
  borrows. Severity: medium Likelihood: low Mitigation: the helper only calls
  `make_parser()` per match and passes the value straight to
  `parse_and_record`; no `Clone`, `'static`, or boxing bounds are added.
- Risk: documentation drifts from behaviour. Severity: medium Likelihood:
  medium Mitigation: the transformer recovery prose is corrected only after
  re-reading `transformers.rs` and the recovery-path tests.
- Risk: a scanner is wrongly classified as shareable. Severity: medium
  Likelihood: low Mitigation: each excluded scanner is named with its reason in
  `docs/developers-guide.md`.

## Progress

- [x] (2026-11-23) Reconnaissance: read every span scanner, `SpanCollector`,
  `TokenStream`, and the two affected documentation sections.
- [x] (2026-11-23) Branch renamed and Lody session titled.
- [x] (2026-11-24) `EP-M1` Regression tests pin current import, apply, and
  `parse_and_record` behaviour. Focused run: 93 passed, 0 failed. Full gates
  passed: `make check-fmt`, `make typecheck`, `make lint`, `make test` (1600
  passed across 53 result lines), `make markdownlint`, `make nixie`.
- [x] (2026-11-24) `EP-M2` `collect_with_parser` added; imports and apply
  refactored onto it; helper tests added. Red: the helper tests failed to
  compile (`cannot find function collect_with_parser`). Green: 100 focused
  tests passed, then 854 parser library tests passed with the `EP-M1` tables
  unchanged. `utils.rs` is 124 lines, `imports.rs` 50, and `apply.rs` 51.
- [x] (2026-11-24) `EP-M3` Documentation corrected and full gates passed:
  `make check-fmt`, `make typecheck`, `make lint` (Clippy, Rustdoc, Clippy
  policy, Whitaker, spelling), `make test` (1607 passed, 0 failed),
  `make markdownlint`, `make nixie`. The spelling gate rejected `-ise` forms in
  two documents; those now use `-ize`.
- [x] (2026-11-24) CodeRabbit review cleared. Two findings were fixed:
  regression cases now pin each diagnostic's ordered span instead of only its
  presence, and the `collect_with_parser` doc example pads trivia so it would
  actually collect both declarations.
- [x] (2026-11-24) Draft pull request opened:
  [PR #481](https://github.com/leynos/ddlint/pull/481). Final CodeRabbit review
  reported zero findings.
- [x] (2026-11-24) Hosted CodeScene failed the advisory code-health gate on one
  duplication finding in `declaration_scanner_tests.rs`. The four test
  functions were collapsed into two `rstest` tables driven by a `SpanScanner`
  function pointer plus one `assert_scanned` helper. Local `cs check` reports
  10.00, and the hosted check now passes.

## Surprises & discoveries

- Observation: `inline_ws().repeated()` accepts whitespace tokens containing
  newlines, so a successful import or apply span already swallows the trailing
  newline. Evidence: the existing case `"import foo\n" -> 0..11` in
  `src/parser/span_scanners/tests/mod.rs`. Impact: new tests assert
  whole-source spans for single declarations rather than line-bounded spans.
- Observation: `parse_recovery` returns `Some(output)` while still reporting
  non-terminal `validate` errors. Evidence: Chumsky 0.9.3
  `parse_recovery_inner` keeps `Ok((out, _))` and appends emitted errors.
  Impact: the success-with-diagnostics test can assert both a recorded span and
  an appended diagnostic.
- Observation: the minimal test grammar's trailing trivia padding absorbs a
  newline, so a "recovered then collected the next line" fixture records a span
  ending past the newline. Evidence: an initial expectation of `"type alpha"`
  failed against the actual `"type alpha\n"`. Impact: the fixture expectation
  documents the trailing-trivia contract instead of trimming it away.

## Decision log

- D1. Decision: implement `collect_with_parser` on top of `parse_and_record`.
  Rationale: the existing helper already owns recording, advancement, and
  recovery, and every current call site uses it. Reimplementing the policy in
  the new helper (as the issue example does) would silently drop diagnostics on
  success and duplicate the contract. Date/Author: 2026-11-23, implementing
  agent.
- D2. Decision: refactor only `imports.rs` and `apply.rs`. Rationale:
  `functions` matches two keywords with an extern guard and a start-dependent
  span map; `transformers` validates names, omits spans, and replaces
  diagnostics; `indexes`, `relations`, `rules`, `attributes`, and `typedefs`
  use cursor-based or line-based scanners that are not Chumsky parsers. Forcing
  them through one helper would change behaviour. Date/Author: 2026-11-23.
- D3. Decision: use a runtime keyword comparison loop, not `token_dispatch!`.
  Rationale: `token_dispatch!` expands a `match` over literal patterns, so a
  runtime `keyword` parameter cannot be used with it. Date/Author: 2026-11-23.
- D4. Decision: put new tests in sibling modules under
  `src/parser/span_scanners/tests/` rather than growing `tests/mod.rs` past 400
  lines. Rationale: repository module-size ceiling. Date/Author: 2026-11-23.
- D5. Decision: pin each malformed declaration's ordered diagnostic span, not
  just its presence. Rationale: a CodeRabbit finding showed presence alone
  would accept a diagnostic on the wrong token or a duplicated diagnostic.
  Chumsky's final failure error names the offending token, so the pinned spans
  are stable across the refactor while still failing on a wrong recovery path.
  Date/Author: 2026-11-24.

- Observation: a table of `#[case]` rows with one wrapper function per scanner
  scores well below the duplication threshold, but four separate test functions
  with the same three-line body trip CodeScene's advisory rule. Evidence: the
  first hosted CodeScene run failed on `declaration_scanner_tests.rs` with "1
  file with Code Duplication"; after collapsing the bodies, `cs check` reports
  10.00 and the hosted check passes. Impact: prefer a scanner function pointer
  plus one shared assertion helper over per-scanner copies when several
  scanners share a test shape.

## Outcomes & retrospective

The change met its purpose: `collect_import_spans` and `collect_apply_spans`
now share one collector implementation, and every scanner keeps its own grammar
and policy. The issue's roughly 120-line estimate was not reachable because
only those two scanners are policy-equal to `parse_and_record`; the real net
change is 68 added helper lines and two scanner bodies replaced by one call
each, offset by the regression tests that make the refactor safe.

What went well: pinning behaviour before refactoring turned a
behaviour-preserving change into an observable one, and the pinned tables
passed unchanged across the refactor. What would be done differently: write the
regression tests as one shared-body table from the start. Four separate test
functions with the same body passed every local gate and still failed the
hosted CodeScene duplication rule, costing a review cycle and a rebase-free
rewrite of the test module.

## Context and orientation

`parse` builds a `Parsed` by scanning top-level statement spans before building
the concrete syntax tree. `src/parser/span_scanner.rs` orchestrates the
specialized collectors, merges their spans, and hands the result to rule
scanning and CST construction.

Every collector receives `tokens: &[(SyntaxKind, Span)]` (the tokenized source,
including trivia) and `src: &str`, and returns
`(Vec<Span>, Vec<Simple<SyntaxKind>>)`.

`src/parser/span_collector.rs` owns `SpanCollector<'a, Extra>` with three
public fields: `stream` (`TokenStream`), `spans`, and `extra`. Its `parse_span`
method builds a Chumsky stream from the current cursor using the statement
start as the stream base, and returns
`(Option<Span>, Vec<Simple<SyntaxKind>>)`. Its `skip_line` advances to the end
of the current line.

`src/parser/span_scanners/utils.rs` defines `State<'a>` (a
`SpanCollector<'a, Vec<Simple<SyntaxKind>>>`) and `parse_and_record`, which runs
`parse_span`, records and advances on success, skips the line on failure, and
appends every diagnostic to `st.extra`.

The two policy-equal collectors are:

- `src/parser/span_scanners/imports.rs`: `K_IMPORT`, module path with `::`
  segments, optional `as` alias, padding, `map_with_span`.
- `src/parser/span_scanners/apply.rs`: `K_APPLY`, identifier, parenthesized
  identifier lists around `->`, padding, `map_with_span`.

The non-shareable collectors are `functions.rs`, `transformers.rs`,
`indexes.rs`, `relations/`, `rules.rs`, `attributes.rs`, and `typedefs.rs`.

Tests live under `src/parser/span_scanners/tests/`. `tests/mod.rs` declares
sibling modules and holds shared collector cases. `crate::test_util::tokenize`
produces `(SyntaxKind, Span)` pairs.

Documentation: `docs/developers-guide.md` has `Parser module ownership` and
`Boundary rules` sections. `docs/parser-implementation-notes.md` has
`Shared declaration parsing utilities`. `docs/repository-layout.md` owns
file-by-file parser module entries.

Gates: `make fmt`, `make check-fmt`, `make lint` (interrogate, Clippy, Rustdoc,
Whitaker, spelling), `make typecheck`, `make test`, `make markdownlint`, and
`make nixie`. Compiler admission is shared on this host; queued builds are
normal and must not be retried or interrupted.

## Conformance basis

- Issue 76 (GitHub) asks for a generic span-collection helper and reduced
  duplication. No Terms of Reference or technical design document exists for
  this internal refactor.
- `AGENTS.md` supplies the file-size, documentation, test, and gate rules.
- `docs/developers-guide.md` supplies the parser boundary conventions.
- Requirement `REQ-shared-mechanics`: single-keyword, policy-equal collectors
  share one implementation of the collector loop.
- Requirement `REQ-policy-unchanged`: spans and diagnostics for imports,
  apply, and `parse_and_record` are unchanged.
- Requirement `REQ-scope-honest`: scanners with different policy keep their own
  orchestration, and the documentation says why.
- Gap `GAP-duplication`: `imports.rs` and `apply.rs` duplicate the collector
  loop, handler closure, and keyword dispatch.
- Gap `GAP-stale-recovery-prose`: `docs/parser-implementation-notes.md` claims
  transformer recovery has no token-walking heuristic and that all declaration
  recovery is newline-bounded; both claims are false.

Trace:

```plaintext
REQ-shared-mechanics -> EP-M2 -> utils_tests::collect_with_parser_*
REQ-policy-unchanged -> EP-M1 -> declaration_scanner_tests::*
REQ-policy-unchanged -> EP-M2 -> utils_tests::parse_and_record_*
REQ-scope-honest     -> EP-M3 -> developers-guide boundary rules
```

## Verification plan

### Abstract model

A collector is a state machine over the token cursor. At each step it inspects
the current token: a matching keyword invokes a fresh parser at that token's
start; any other token advances the cursor by one. A parser either yields a
span (record it, advance to the span end) or yields nothing (advance to the end
of the current line). Diagnostics are always appended to `extra`.

### Invariants and lemmas

- `INV-progress`: every iteration advances the cursor. Established by
  `parse_and_record` advancing on both branches and by the non-match branch
  advancing one token. A parser that returns an already-consumed span would
  violate this; the documentation states the caller's obligation.
- `INV-order`: recorded spans appear in source order. Follows from the cursor
  only moving forward.
- `INV-policy`: for imports and apply, `collect_with_parser` produces exactly
  the spans and diagnostics the pre-refactor collectors produce.
- `L-equivalence`: `collect_with_parser(k, p)` is observationally equal to
  `token_dispatch!` with a handler calling `parse_and_record(p())` on `k` and
  advancing otherwise.

### Axioms (trusted, not verified here)

- `AXIOM-chumsky-recovery`: Chumsky 0.9.3 `parse_recovery` returns
  `(Some(output), errors)` when a `validate` parser emits non-terminal errors,
  and `(None, errors)` when the parser fails. Verified by reading the vendored
  source; not re-proved.
- `AXIOM-tokenize`: `crate::test_util::tokenize` returns trivia-preserving
  tokens whose spans are source byte ranges.

### Methods per obligation

- Obligation `INV-policy` for imports and apply.
  Method: parameterized `rstest` tables over finite source fixtures. Rationale:
  the input space is a small, normative set of declaration shapes. Domain:
  valid, multiple, empty, malformed-then-valid, EOF without newline, trailing
  whitespace, adjacent declarations, missing arrow, missing parenthesis.
  Artefact: `src/parser/span_scanners/tests/declaration_scanner_tests.rs`.
  Evidence: the cases pass before and after the refactor; exact span ranges and
  error presence are asserted. Non-vacuity: at least one malformed case must
  produce a diagnostic, and at least one success case must produce a non-empty
  span; both are asserted.
- Obligation `INV-policy` for `parse_and_record`.
  Method: named unit tests with a `validate` parser. Rationale: the contract
  has three branches (success with diagnostics, success without, failure) that
  each need an explicit witness. Domain: a single-token statement inside a
  longer line, so line-skipping and span-end advancement differ observably.
  Artefact: `src/parser/span_scanners/tests/utils_tests.rs`. Evidence: success
  records one span and leaves the cursor mid-line; failure records no span and
  advances past the newline; diagnostics append after pre-existing entries.
  Non-vacuity: the cursor assertions distinguish `skip_until(span.end)` from
  `skip_line()`, so a wrong branch fails the test.
- Obligation `L-equivalence`/`INV-progress`.
  Method: parameterized helper tests with a minimal grammar. Rationale: the
  shared loop is small but its match/non-match/termination behaviour is not
  covered by the production scanners alone. Domain: two declarations, a leading
  non-keyword token, a failed parse followed by a valid declaration, and
  success-with-diagnostics. Artefact:
  `src/parser/span_scanners/tests/utils_tests.rs`. Evidence: exact span list,
  error presence, and factory call count. Non-vacuity: the failed-parse case
  must both record the later span and report the earlier diagnostic; the
  factory-count case would fail if the helper reused a parser instance.

### Formal verification decision

No formal method is warranted. The change is a mechanical extraction over an
existing cursor loop with no arithmetic, unsafe code, or concurrency. Finite
parameterized tests over the normative declaration shapes and the three
`parse_and_record` branches give proportionate evidence. Kani or Verus would
add cost without reaching Chumsky's opaque parser internals.

### Suggested BDD features

No BDD runner covers this private helper. Existing unit tests exercise the
scenarios directly; no feature file is proposed.

## Plan of work

### `EP-M1` pin the current behaviour

Add `src/parser/span_scanners/tests/declaration_scanner_tests.rs` with `rstest`
tables for `collect_import_spans` and `collect_apply_spans`, plus
`src/parser/span_scanners/tests/utils_tests.rs` covering `parse_and_record`.
Declare both modules in `tests/mod.rs`. Run the focused tests; every case must
pass against unmodified production code. Commit.

### `EP-M2` add the shared helper and refactor two scanners

Add `collect_with_parser` to `src/parser/span_scanners/utils.rs` with Rustdoc
covering fresh parser construction, whole-statement spans, and the
`parse_and_record` policy. Extract `import_parser()` and `apply_parser()` as
private grammar functions and replace both collector bodies with helper calls.
Extend `utils_tests.rs` with the helper tests, written first against the
missing helper (compile failure is the red state). Run focused tests, then the
full gates. Commit.

### `EP-M3` documentation and delivery

Update `docs/developers-guide.md` (ownership and boundary rules),
`docs/parser-implementation-notes.md` (helper composition and corrected
transformer recovery), and `docs/repository-layout.md` (file-by-file entry).
Index this ExecPlan in `docs/contents.md`. Run every gate, request a CodeRabbit
review, push, and open a draft pull request whose description records the
unreachable 120-line estimate.

## Concrete steps

Working directory: repository root.

```plaintext
Red (EP-M2 helper tests): cargo test -p ddlint --lib span_scanners -> compile
  error: cannot find function `collect_with_parser`
Green: same command -> all span-scanner cases pass
Gates: make fmt && make check-fmt && make typecheck && make lint && make test &&
  make markdownlint && make nixie
```

Focused log paths follow `/tmp/<action>-ddlint-<branch>.out`.

## Validation and acceptance

- `cargo test -p ddlint --lib parser::span_scanners` passes.
- `make test` passes with the pre-existing case count increased only by the new
  regression and helper cases.
- Import and apply spans and diagnostics are identical before and after
  `EP-M2`; the `EP-M1` tests prove it by passing unchanged.
- `make lint` passes without new suppressions.

## Idempotence and recovery

All steps are additive and re-runnable. Test modules can be deleted to roll back
`EP-M2`; reverting the two scanner bodies restores the pre-refactor behaviour.
Gates may be rerun after fixing a finding.

## Artefacts and notes

Reconnaissance findings are captured under `Surprises & discoveries`.

`EP-M1` baseline (unmodified production code, 2026-11-24):

```plaintext
cargo test -p ddlint --lib span_scanners
test result: ok. 93 passed; 0 failed; 0 ignored; 0 measured; 849 filtered out
```

The pinned ranges agreed with the pre-refactor collectors on the first run,
including whole-source spans that absorb the trailing newline.

`EP-M2` red evidence:

```plaintext
cargo test -p ddlint --lib span_scanners::tests::utils_tests --no-run
error[E0432]: unresolved import `super::super::utils::collect_with_parser`
```

`EP-M2` green evidence after the refactor:

```plaintext
cargo test -p ddlint --lib span_scanners
test result: ok. 100 passed; 0 failed

cargo test -p ddlint --lib parser::
test result: ok. 854 passed; 0 failed; 95 filtered out
```

The `EP-M1` tables ran unchanged, so the refactor preserved import and apply
spans, diagnostics, and recovery. Net change: 259 insertions and 74 deletions
across four files, of which the helper itself is 68 added lines and the two
scanner bodies shrink to one call each. Only imports and apply were converted,
so the issue's roughly 120-line estimate was not reachable.

## Interfaces and dependencies

`src/parser/span_scanners/utils.rs`:

```rust
pub(super) fn collect_with_parser<P, F>(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    keyword: SyntaxKind,
    make_parser: F,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>)
where
    F: FnMut() -> P,
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>;
```

Ownership: `utils.rs` owns `State`, `parse_and_record`, and
`collect_with_parser`. Callers are limited to single-keyword scanners with no
guards, span filtering, or diagnostic replacement. No dependency changes.

## Revision note

- 2026-11-23: Initial plan derived from the issue's coding plan and repository
  reconnaissance.
- 2026-11-24: Recorded the CodeRabbit remediation (ordered diagnostic spans and
  a corrected doc example) and the CodeScene duplication remediation (one
  shared test body driven by a scanner function pointer). The API, obligations,
  and remaining scope are unchanged; the plan is implemented and tracked in
  [PR #481](https://github.com/leynos/ddlint/pull/481).
