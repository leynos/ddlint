# Align transformer declaration grammar and output signatures

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: IMPLEMENTED

## Purpose / big picture

Roadmap item `2.6.5` is open because the parser, tests, and syntax
specification describe different transformer declarations. The current parser
accepts only `extern transformer name(inputs...): outputs...`, exposes those
outputs through `Transformer::outputs()`, and rejects declarations without an
output list. The normative syntax spec in
`docs/differential-datalog-parser-syntax-spec-updated.md` still documents the
older semicolon-only shape with no output signature, so a novice cannot tell
which contract is real.

After this change, a novice should be able to read one transformer grammar,
parse a declaration such as
`extern transformer normalise(input: User): Normalised`, inspect both inputs
and outputs through the Abstract Syntax Tree (AST), and get deterministic
diagnostics when the required output signature is missing or empty. Success is
observable when:

- the parser, unit tests, behavioural tests, and active docs all describe the
  same transformer grammar;
- missing-output and non-`extern` declarations produce stable diagnostics;
- `make check-fmt`, `make lint`, and `make test` pass after the implementation
  change; and
- `docs/roadmap.md` item `2.6.5` is marked done only after those checks
  succeed.

## Recommended decision

Close item `2.6.5` by aligning the syntax spec and parser contract to the
existing parser-side declaration shape rather than weakening the parser back to
the older semicolon-only form.

The canonical parser contract for this milestone should become:

- `extern transformer <identifier>(<name: type pairs>?): <output-identifiers>`
- the output signature after `:` is mandatory and must contain at least one
  identifier;
- zero inputs remain valid, but zero outputs do not;
- multiple outputs remain comma-separated and ordered;
- non-`extern` transformer declarations keep the existing targeted diagnostic;
- semicolon policy remains unchanged and out of scope for this item; and
- parser-stage case-class validation for transformer names and outputs remains
  out of scope unless the existing code cannot be documented coherently without
  it.

This is the smallest decision that matches the implemented AST surface,
existing parser tests, and the already-landed `apply` item behaviour. The
repository already models transformer outputs in `Transformer::outputs()` and
uses them in `apply`-related tests. Removing outputs from the declaration
grammar would therefore be a broader parser and API redesign, not a conformance
alignment task.

The implementation should also tighten diagnostics around the output signature.
Today, declarations such as `extern transformer t(x: A):` fail with generic
unexpected-token errors. This roadmap item should replace those generic
failures with one deterministic transformer-specific message for missing or
empty output signatures, for example
`transformer declarations require ':' followed by at least one output identifier`.

If implementation reveals a hidden consumer that depends on the old
semicolon-only grammar, stop and escalate. That would be a wider compatibility
policy decision, not an isolated grammar-alignment fix.

## Constraints

- Align the active written contract in
  `docs/differential-datalog-parser-syntax-spec-updated.md` section `5.4` to
  the final parser behaviour.
- Keep the scope centred on transformer declarations. Do not fold relation
  form, apply grammar, legacy-token policy, or global statement terminator work
  from roadmap items `2.6.6` through `2.6.8` into this change.
- Preserve the existing AST surface unless implementation proves a small,
  clearly justified refinement is required. In particular,
  `Transformer::inputs()` and `Transformer::outputs()` should remain available.
- Keep non-`extern` transformer diagnostics working with the existing message
  unless a tightly scoped wording change is made everywhere in one pass.
- Add both unit tests and behavioural tests that prove the accepted grammar and
  the new deterministic output-signature failures.
- Update the relevant design documents, not only parser tests and the
  conformance register.
- Mark `docs/roadmap.md` item `2.6.5` done only after all documentation,
  implementation, and validation steps succeed.
- Run repository quality gates through Make targets using `set -o pipefail` and
  `tee`.
- Keep comments and documentation in en-GB-oxendict spelling.

## Tolerances (exception triggers)

- Scope: if the implementation requires edits to more than 10 files or more
  than 260 net new code lines, stop and escalate. That likely means the work is
  drifting into a broader grammar or API redesign.
- Interface: if the only coherent fix requires removing `Transformer::outputs()`
  or reshaping `Apply` consumers, stop and escalate.
- Compatibility: if non-test workspace code depends on parsing transformer
  declarations without output signatures, stop and document those callers
  before changing behaviour.
- Grammar: if aligning the spec to the implemented output signature still
  leaves an unavoidable contradiction around identifier case classes or
  terminators, stop and escalate with concrete file references instead of
  silently widening the task.
- Validation: if `make check-fmt`, `make lint`, or `make test` still fails
  after three focused fix rounds, stop and escalate with the specific failing
  targets.
- Runner stability: if `make test` hits the known repository-wide nextest stall
  rather than a deterministic regression from this task, capture the evidence
  and escalate instead of silently substituting a different profile.

## Risks

- Risk: the repository already has two overlapping transformer contracts: the
  syntax spec's semicolon-only form and the parser/tests' required output list.
  Severity: high. Likelihood: high. Mitigation: search for transformer grammar
  examples across `docs/`, `src/parser/tests/`, and `tests/`, then update all
  contract-bearing examples in one atomic change.

- Risk: output-signature parsing currently fails through generic parser errors,
  so merely updating the spec could leave usability gaps unaddressed. Severity:
  high. Likelihood: high. Mitigation: add red tests for missing colon and
  empty-output cases first, then implement transformer-specific diagnostics in
  `src/parser/span_scanners/transformers.rs`.

- Risk: transformer declarations and `apply` items are conceptually coupled,
  so a grammar change could accidentally invalidate existing `apply` examples
  or behavioural assumptions. Severity: medium. Likelihood: medium. Mitigation:
  add at least one behavioural test containing both a transformer declaration
  and an `apply` item in the same programme.

- Risk: identifier case restrictions are inconsistent between the spec
  (`UcName`) and the parser/tests (generic identifiers such as `normalise` and
  `out`). Severity: medium. Likelihood: high. Mitigation: keep this milestone
  focused on documenting current parser behaviour unless stronger case
  validation can be added without touching `apply` parsing or broader naming
  policy.

- Risk: semicolon examples in the spec can distract the implementation into a
  top-level terminator policy change. Severity: medium. Likelihood: medium.
  Mitigation: treat terminator policy as explicitly out of scope, as in the
  completed index-grammar conformance work.

## Progress

- [x] (2026-03-27) Reviewed roadmap item `2.6.5`, conformance register item
  `12`, the syntax spec transformer grammar, parser implementation notes,
  design doc guidance, current transformer scanner and AST wrappers, and
  existing parser and behavioural tests.
- [x] (2026-03-27) Confirmed the live parser contract already requires a colon
  plus a non-empty output list and exposes that list through
  `Transformer::outputs()`.
- [x] (2026-03-27) Drafted this ExecPlan in
  `docs/execplans/2-6-5-align-transformer-declaration-grammar.md`.
- [x] (2026-03-30) Updated parser unit and behavioural tests to require the
  transformer-specific missing-output diagnostic for both missing-colon and
  empty-output forms.
- [x] (2026-03-30) Implemented transformer-local extern scan recovery so
  missing or empty output signatures emit one deterministic custom diagnostic
  instead of a generic `Unexpected` parser failure.
- [x] (2026-03-30) Aligned the syntax spec, parser implementation notes,
  conformance register, and roadmap with the mandatory output-signature grammar.
- [x] (2026-03-30) Ran `make fmt`, `make markdownlint`, `make nixie`,
  `make check-fmt`, `make lint`, and `Continuous Integration (CI)=1 make test`.
- [x] (2026-03-30) Confirmed the gate results and kept roadmap item `2.6.5`
  marked done.

## Surprises & Discoveries

- The parser side is already much more opinionated than the syntax spec.
  `src/parser/span_scanners/transformers.rs` requires a top-level `:` followed
  by at least one output identifier, and malformed declarations yield no
  transformer node.

- The AST and tests are already built around output signatures.
  `src/parser/ast/transformer.rs` exposes `outputs()`, and the existing unit
  and parser-integration tests assert concrete output lists for all successful
  transformer fixtures.

- `apply` behavioural coverage already assumes the richer transformer contract.
  `tests/apply_items.rs` pairs a transformer declaration with an `apply`
  statement, so reverting to a no-output transformer grammar would create
  additional downstream ambiguity.

- The syntax spec mismatch is not limited to the colon. Section `5.4` also uses
  `UcName`, while current parser fixtures and behavioural tests use lower-case
  transformer names such as `normalise`, `correlate`, and `reserved`. This plan
  keeps that case-policy question tightly bounded so the output signature
  decision can land first.

- Existing failure coverage for missing outputs is weak. The tests assert only
  a generic `Unexpected` pattern today, which is too vague for a conformance
  item whose whole point is to freeze the grammar contract.

## Decision Log

- Decision: recommend aligning the spec and docs to the implemented
  colon-plus-output-list grammar rather than rewriting the parser to match the
  older semicolon-only spec. Rationale: this is the smaller change, it matches
  the existing AST surface and tests, and it avoids breaking the
  already-implemented transformer/apply story.

- Decision: require a non-empty output signature and add a deterministic
  transformer-specific diagnostic for missing or empty outputs. Rationale: the
  current parser already rejects zero-output declarations, but the failure mode
  is too generic to serve as a stable documented contract.

- Decision: keep semicolon policy out of scope for this milestone.
  Rationale: the open conformance item is about declaration shape and output
  signatures, not about harmonizing top-level terminators across the whole
  grammar.

- Decision: do not widen this milestone into a general identifier case-policy
  change unless implementation proves the docs cannot be aligned otherwise.
  Rationale: lower-case transformer names already appear in tests and related
  `apply` examples, so tightening case classes would have a larger blast radius
  than the roadmap item requires.

## Outcomes & Retrospective

Implementation landed with full validation. The parser, unit tests, behavioural
tests, syntax spec, conformance register, parser implementation notes, roadmap,
and this ExecPlan now agree on the transformer contract:
`extern transformer name(params...): output(, output)*`, with a deterministic
diagnostic when the output signature is missing or empty. Validation passed via
`make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`, `make lint`,
and `CI=1 make test`.

## Context and orientation

The relevant code and documents are concentrated in a small set of files:

- `src/parser/span_scanners/transformers.rs` recognises top-level transformer
  declarations and already enforces the `extern` requirement plus the current
  colon-output grammar.
- `src/parser/ast/transformer.rs` exposes `Transformer::name()`,
  `Transformer::inputs()`, and `Transformer::outputs()`.
- `src/parser/ast/parse_utils/outputs.rs` contains the output-list extraction
  helper and its local tests.
- `src/parser/tests/transformers.rs` contains the feature-focused unit tests
  for accepted and rejected transformer declarations.
- `src/parser/tests/programs.rs`, `src/parser/tests/specs.rs`, and
  `src/parser/tests/parser.rs` define parser-level integration fixtures and
  assertions for transformer declarations.
- `tests/apply_items.rs`, `tests/attribute_placement.rs`, and
  `tests/name_uniqueness.rs` provide behavioural coverage that already touches
  transformers indirectly.
- `docs/differential-datalog-parser-syntax-spec-updated.md`,
  `docs/parser-conformance-register.md`, `docs/parser-implementation-notes.md`,
  and `docs/ddlint-design.md` carry the active written contract.

The concrete mismatch today is:

- Spec: `extern transformer UcName(params?);`
- Code/tests: `extern transformer ident(params?): output(, output)*`

The implementation should resolve that mismatch in code, tests, and docs in one
atomic change.

## Plan of work

### Stage A: establish red tests for the intended contract

Start by changing tests so they describe the chosen grammar before touching the
scanner logic.

- Update `src/parser/tests/transformers.rs` so missing outputs and missing
  output signatures assert one deterministic transformer-specific message
  rather than a generic `Unexpected` pattern.
- Add an explicit rejection case for the legacy semicolon-only shape, for
  example `extern transformer legacy(x: A)` or
  `extern transformer legacy(x: A);`, depending on the repository's current
  top-level terminator policy.
- Keep success coverage for zero-input, single-output, and multi-output
  declarations.
- Add a parser-level integration assertion in `src/parser/tests/parser.rs` and
  related fixtures so the shared parser suite encodes the final grammar too.

At the end of Stage A, the targeted transformer tests should fail for the right
reason: the grammar contract is now stricter and more specific than the current
diagnostics.

### Stage B: implement deterministic grammar handling

Adjust the transformer scanner so the chosen contract is enforced explicitly
and recoverably.

- Refactor `src/parser/span_scanners/transformers.rs` so it can distinguish:
  `non-extern transformer`, `missing ':'`, and
  `':' present but no output identifiers`.
- Keep the span recovery behaviour that skips malformed declarations cleanly so
  downstream Concrete Syntax Tree (CST) construction remains stable.
- Preserve successful parsing of existing valid fixtures, including multiple
  outputs and zero inputs.
- If needed, add a small helper near the scanner or in
  `src/parser/ast/parse_utils/outputs.rs` to keep output-signature parsing and
  output-signature diagnostics consistent.

### Stage C: align documentation and public contract text

Once the parser and tests agree, update every active document that carries the
grammar contract.

- In `docs/parser-conformance-register.md`, rewrite item `12` so the
  spec/target behaviour matches the chosen grammar and mark the entry
  `implemented` once the full change is complete.
- In `docs/differential-datalog-parser-syntax-spec-updated.md` section `5.4`,
  replace the semicolon-only grammar with the colon-plus-output-list grammar
  and add one short note stating that empty output signatures are rejected.
- In `docs/parser-implementation-notes.md`, add a brief parser-boundary note
  describing the transformer scanner's explicit output-signature requirement
  and deterministic diagnostics.
- In `docs/ddlint-design.md`, add one sentence in the parser-contract section
  clarifying that transformer declarations expose output signatures directly
  from the CST-backed AST surface because downstream analysis and `apply`
  handling already depend on them.
- In code comments, update `src/parser/ast/transformer.rs` and any adjacent
  parse-helper docs so they describe the frozen contract accurately.

### Stage D: add behavioural coverage for end-to-end parser behaviour

Add or update behavioural tests in `tests/` so the public `parse()` entrypoint
proves the decision, not just the internal unit suites.

- Add the behavioural coverage to `tests/apply_items.rs`, which is already the
  canonical parser-boundary home for transformer-plus-`apply` behaviour.
- Include at least one success case proving that a programme containing a valid
  transformer declaration yields one transformer with the expected outputs.
- Include at least one failure case proving that a declaration with no output
  signature yields the deterministic diagnostic and no transformer node.
- Include one combined case with a transformer declaration plus an `apply`
  statement to show the broader parser surface still behaves coherently.

### Stage E: validation and close-out

After implementation and documentation updates:

1. Run `make fmt`, `make markdownlint`, and `make nixie` because this change
   updates Markdown.
2. Run `make check-fmt`, `make lint`, and `make test`.
3. Only after those gates pass, update `docs/roadmap.md` to mark item `2.6.5`
   done.
4. Record any final wording or scope decisions back into this ExecPlan's
   `Decision Log`, `Progress`, and `Outcomes & Retrospective` sections.

## Concrete steps

1. Update unit tests in `src/parser/tests/transformers.rs`.

   - Replace generic output-failure assertions with a transformer-specific
     diagnostic expectation.
   - Add one explicit legacy-form rejection case.
   - Keep success coverage for one output, many outputs, and zero inputs.

2. Update parser-level shared fixtures and assertions.

   - Adjust `src/parser/tests/programs.rs` if new failure fixtures are needed.
   - Adjust `src/parser/tests/specs.rs` and `src/parser/tests/parser.rs` so the
     shared parser suite encodes the same contract as the feature-specific
     transformer tests.

3. Implement scanner diagnostics in `src/parser/span_scanners/transformers.rs`.

   - Preserve existing success cases.
   - Emit one deterministic message for missing or empty output signatures.
   - Keep the existing `transformer declarations must be extern` message for
     non-`extern` declarations.

4. Update AST and helper docs only if required.

   - Keep `Transformer::outputs()` intact.
   - Update documentation in `src/parser/ast/transformer.rs` and
     `src/parser/ast/parse_utils/outputs.rs` if it no longer matches the frozen
     contract.

5. Add behavioural tests in `tests/apply_items.rs`.

   - One success case.
   - One output-signature failure case.
   - One success case combined with `apply`.

6. Update active documentation.

   - `docs/parser-conformance-register.md`
   - `docs/differential-datalog-parser-syntax-spec-updated.md`
   - `docs/parser-implementation-notes.md`
   - `docs/ddlint-design.md`
   - `docs/roadmap.md` only after all gates pass

## Validation commands

Run the following from the repository root, capturing output with `tee`:

```shell
set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-5-make-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-5-make-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-5-make-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-5-make-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/2-6-5-make-lint.log
set -o pipefail; make test 2>&1 | tee /tmp/2-6-5-make-test.log
```

Successful completion means all six commands exit with status `0`, the new
transformer unit and behavioural tests pass, and the roadmap entry can be
closed without leaving the conformance register in `scheduled` state.
