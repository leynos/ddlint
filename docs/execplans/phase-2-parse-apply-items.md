# Align parser with updated DDlog syntax: apply items and extern transformers

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & discoveries`, `Decision log`, and
`Outcomes & retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

PLANS.md is not present in this repository.

## Purpose / big picture

Implement parsing of top-level `apply` items and ensure non-extern
`transformer` declarations emit diagnostics. Success means a user can parse a
Differential Datalog (DDlog) file containing valid `apply` items and see
structured abstract syntax tree (AST) nodes for those items, while invalid
`transformer` declarations without `extern` produce clear, spanned errors. Unit
tests and behavioural tests must prove both the new parsing and the
diagnostics. The roadmap entry for this task is marked "done" only after all
validation steps pass.

## Constraints

- Follow the updated DDlog syntax spec and related parser docs in `docs/`.
- Use the existing parser pipeline (tokenizer -> span scanners -> concrete
  syntax tree (CST) -> abstract syntax tree (AST)).
- Do not add new dependencies without escalation.
- Keep module files under 400 lines; split modules if needed.
- Every new Rust module starts with a `//!` module comment.
- Use en-GB spelling in comments and documentation.
- Prefer Makefile targets and capture output with `tee` and `set -o pipefail`.
- Update relevant design docs for any grammar or behaviour decisions.

## Tolerances (exception triggers)

- Scope: if implementation requires changes to more than 12 files or more than
  400 net new lines, stop and escalate.
- Grammar ambiguity: if `apply` syntax is not specified in `docs/` and cannot
  be inferred without assumptions, stop and ask for guidance.
- Public API: if changes go beyond adding `apply` accessors in AST/root,
  escalate before proceeding.
- Dependencies: if any new crate is needed, stop and escalate.
- Test churn: if `make test` fails after two fix attempts, stop and escalate.

## Risks

- Risk: `apply` item grammar is not fully documented in current specs.
  Severity: high Likelihood: medium Mitigation: confirm grammar from docs or
  Haskell analysis; document decisions in
  `docs/differential-datalog-parser-syntax-spec-updated.md` and pause if
  ambiguity remains.
- Risk: span scanner changes can break CST span ordering if spans overlap.
  Severity: medium Likelihood: low Mitigation: update span validation tests and
  keep spans sorted.
- Risk: new diagnostics for non-extern transformers could conflict with error
  recovery paths, hiding the error. Severity: low Likelihood: low Mitigation:
  add explicit tests for the diagnostic message and span.

## Progress

- [x] (2026-02-02 00:00Z) Drafted ExecPlan and surveyed current docs and code.
- [x] (2026-02-02 01:10Z) Confirmed `apply` grammar and updated the syntax
  spec.
- [x] (2026-02-02 01:25Z) Added unit and behavioural tests for `apply` items
  and transformer diagnostics.
- [x] (2026-02-02 01:45Z) Implemented span scanning, CST nodes, AST wrappers,
  and diagnostics for `apply` and non-extern transformers.
- [x] (2026-02-02 02:35Z) Ran validation gates and confirmed the roadmap entry
  remains marked as done.

## Surprises & discoveries

- Observation: `make fmt` requires `fd` to be available on `PATH`.
  Evidence: `mdformat-all` invokes `fd` and fails without it. Impact: keep `fd`
  installed or add a wrapper when running formatters.

## Decision log

- Decision: `apply` uses `apply Transformer(Args...) -> (Outputs...)` with
  inputs accepting relation or function identifiers and allowing trailing
  commas, mirroring the legacy DDlog parser. Rationale: matches upstream parse
  behaviour and keeps the grammar compact. Date/Author: 2026-02-02 (assistant)
- Decision: diagnostic text for non-extern transformers is
  "transformer declarations must be extern". Rationale: aligns with the updated
  syntax spec's error example. Date/Author: 2026-02-02 (assistant)

## Outcomes & retrospective

Implemented `apply` parsing with CST/AST coverage, added diagnostics for
non-extern transformers, updated the syntax spec and roadmap entry, and added
unit and behavioural tests. Validation gates (`make check-fmt`, `make lint`,
`make test`, plus Markdown tooling) all passed. Future work can build on the
new `Apply` AST wrapper and root accessor without additional parsing changes.

## Context and orientation

The parser pipeline works by scanning token spans per top-level statement,
building a CST (via `rowan`), and exposing AST wrappers. `SyntaxKind` already
includes `K_APPLY` and `N_APPLY`, but there is no span scanner or AST wrapper
for `apply` items, and `Root` cannot collect them. Transformer spans are
collected only for `extern transformer` declarations, so a non-extern
`transformer` currently produces no diagnostic.

Key files to touch:

- `src/parser/span_scanners/` for new `apply` scanning and transformer
  diagnostics.
- `src/parser/span_scanner.rs` and `src/parser/cst_builder/` for span
  integration into the CST.
- `src/parser/ast/` and `src/parser/ast/root.rs` for new AST wrappers and
  accessors.
- `src/parser/tests/` for unit-level parser tests.
- `tests/` for behavioural tests covering end-to-end parsing.
- `docs/differential-datalog-parser-syntax-spec-updated.md` (or a more
  appropriate design doc) for recording grammar decisions.
- `docs/roadmap.md` to mark the Phase 2 entry as done after validation.

## Plan of work

Stage A: confirm grammar and requirements (no code changes). Read the
referenced docs, especially
`docs/differential-datalog-parser-syntax-spec-updated.md` and
`docs/parser-gap-analysis.md`, and determine the exact `apply` item grammar and
expected diagnostics. If the grammar is not defined, pause and agree on a
concrete syntax, then record it in the syntax spec and decision log before
proceeding.

Stage B: add tests first. Create unit tests in `src/parser/tests/` for parsing
`apply` items, including at least one valid case and one invalid case. Add a
unit test that asserts a non-extern `transformer` produces a diagnostic with a
precise span. Add a behavioural test in `tests/` that parses a short program
containing a valid `apply` item, asserting `parse()` returns no errors and the
AST exposes the item. Update CST integration tests to count `N_APPLY` nodes.

Stage C: implement parsing and diagnostics. Add a span scanner for `apply`
items and wire it into `ParsedSpans`, the CST builder, and the root AST.
Introduce a new AST wrapper (for example, `Apply`) with accessors that reflect
whatever grammar was confirmed in Stage A. Add diagnostic emission for
non-extern `transformer` declarations, ensuring that the invalid declaration
still occupies a span in the CST for round-tripping but does not register as a
valid transformer.

Stage D: documentation, validation, and cleanup. Update the syntax spec (and
any other relevant design doc) to capture the final `apply` grammar and
transformer diagnostic wording. Update `docs/roadmap.md` to mark the Phase 2
entry as "done" only after tests and linting succeed. Run all required
formatting, linting, and test commands with logs captured via `tee`.

## Concrete steps

1) Confirm `apply` grammar

   - Read `docs/differential-datalog-parser-syntax-spec-updated.md` and
     `docs/parser-gap-analysis.md`.
   - If grammar is missing, pause and ask for the exact syntax. Record the
     decision in the syntax spec and `Decision log` before coding.

2) Add unit tests (examples to adapt once grammar is confirmed)

   - Create `src/parser/tests/apply.rs` with fixtures and `#[rstest]` cases.
   - Add helper `parse_apply` in `src/parser/tests/helpers.rs`.
   - Update `src/parser/tests/mod.rs` to include the new module.
   - Add a transformer error test in `src/parser/tests/transformers.rs` using
     `assert_parse_error` with the chosen diagnostic message and span.

3) Add behavioural tests

   - Add a new `tests/apply_items.rs` (or extend an existing behavioural test)
     that parses a short DDlog programme containing at least one `apply` item
     and asserts `parse()` reports no errors and `root.applys()` returns the
     expected count.

4) Implement span scanning and CST nodes

   - Add `src/parser/span_scanners/apply.rs` with a parser for the `apply`
     statement and span recording, following the pattern in
     `src/parser/span_scanners/imports.rs`.
   - Update `src/parser/span_scanners/mod.rs` to export
     `collect_apply_spans`.
   - Update `src/parser/span_scanner.rs` to call the new collector, merge spans
     into `ParsedSpans`, and include any errors.
   - Extend `src/parser/cst_builder/spans.rs` to store `apply` spans and ensure
     span validation includes the new list.
   - Update `src/parser/cst_builder/tree.rs` to include `N_APPLY` in
     `SPAN_CURSOR_KINDS`.

5) Implement AST wrapper and root accessors

   - Add `src/parser/ast/apply.rs` with a `//!` comment and typed accessors
     that reflect the chosen grammar (e.g., `name()`, `arguments()`).
   - Update `src/parser/ast/mod.rs` to include the new module and re-export the
     type.
   - Update `src/parser/ast/root.rs` to collect `N_APPLY` nodes in a new
     `applys()` accessor (or another agreed name, but keep it consistent in
     tests and docs).

6) Add diagnostics for non-extern transformers

   - Extend `src/parser/span_scanners/transformers.rs` (or add a small helper
     in `utils.rs`) to detect `transformer` without a leading `extern` and push
     a `Simple::custom` error with the agreed message and span.
   - Ensure the error is appended to the aggregated parse errors in
     `parse_tokens`.

7) Update documentation and roadmap

   - Record the final `apply` grammar and diagnostic wording in
     `docs/differential-datalog-parser-syntax-spec-updated.md` (or another
     relevant design doc identified in Stage A).
   - Mark the Phase 2 roadmap entry as "done" in `docs/roadmap.md` after
     validation succeeds.

8) Validation commands (run from repo root)

   - Markdown validation (only if docs changed):

         set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
         set -o pipefail && make fmt 2>&1 | tee /tmp/ddlint-fmt.log
         set -o pipefail && make nixie 2>&1 | tee /tmp/ddlint-nixie.log

   - Rust formatting, linting, and tests:

         set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
         set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
         set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log

   - If any command fails, fix the issue and re-run the failed command before
     proceeding.

## Validation and acceptance

Acceptance is met when:

- Parsing a valid DDlog programme containing `apply` items yields no parse
  errors and `root.applys()` returns the correct number of items.
- A non-extern `transformer` declaration produces a diagnostic that includes
  the agreed wording and the span covers the declaration keyword or entire
  statement, as defined in tests.
- `make check-fmt`, `make lint`, and `make test` succeed.
- If documentation changed, `make markdownlint`, `make fmt`, and `make nixie`
  succeed.
- The Phase 2 roadmap entry is marked "done" after all checks pass.

## Idempotence and recovery

All steps are safe to re-run. If a span scanner change corrupts CST output,
revert to the last passing tests and re-apply modifications incrementally. When
validation fails, fix the specific failure and re-run the failed command only,
then re-run the full validation set before marking the work complete.

## Artifacts and notes

Keep test fixtures small and place any long sample programmes under `tests/`
for readability. Ensure any new diagnostic text is stable and asserted in unit
tests.

## Interfaces and dependencies

No new dependencies are required. The new public surface is limited to an AST
wrapper and root accessor for `apply` items. After Stage A confirms grammar,
define the accessors clearly, for example:

    // src/parser/ast/apply.rs
    pub struct Apply { /* wraps a SyntaxNode */ }
    impl Apply {
        pub fn name(&self) -> Option<String> { /* … */ }
        pub fn arguments(&self) -> Vec<String> { /* … */ }
    }

Update `src/parser/ast/root.rs` to expose `applys()` (or an agreed name) that
returns `Vec<Apply>`.
