# Close top-level `for` parser conformance and documentation drift

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`,
`Decision log`, and `Outcomes & retrospective` must be kept up to date as work
proceeds.

Status: DRAFT

## Purpose / big picture

Close roadmap item 2.6.1 by making the parser, tests, and specification agree
on one top-level `for` contract. This plan adopts the lowest-risk contract:
top-level `for` is not supported in this parser generation, and parsing must
emit a deterministic, span-accurate diagnostic while preserving recovery.
Success is observable when a top-level `for` produces the expected diagnostic,
rule-body `for` behaviour remains unchanged, docs are aligned, and quality
gates pass.

## Constraints

- Keep parser architecture intact: tokenizer -> span scanners -> CST -> AST
  wrappers.
- Do not add dependencies.
- Do not change semantics of rule-body `for` loops.
- Keep diagnostics deterministic and tied to source spans.
- Keep edits focused to parser scanning/tests and documentation for this
  conformance item.
- Update design documentation in `docs/` in the same change as code updates.
- Use Makefile targets for validation and capture command output with
  `set -o pipefail` and `tee`.

## Tolerances (exception triggers)

- Scope: if the implementation exceeds 10 files changed or 350 net lines,
  stop and escalate.
- Contract: if closing 2.6.1 requires implementing full top-level desugaring
  machinery (statement-to-rule lowering), stop and escalate.
- API surface: if a public AST type/signature change is required, stop and
  escalate.
- Test loop: if the same failing tests persist after two fix iterations, stop
  and escalate with failing test names.
- Ambiguity: if roadmap item 2.5.4 ownership conflicts with 2.6.1 closure,
  stop and document options in `Decision log`.

## Risks

- Risk: scanners may misclassify `for` inside rule bodies as top-level.
  Severity: high Likelihood: medium Mitigation: gate diagnostics on top-level
  context and existing exclusion spans; add regression tests for
  nested/rule-body loops.

- Risk: introducing a new diagnostic path may reduce error recovery quality.
  Severity: medium Likelihood: medium Mitigation: add behavioural recovery
  tests proving later statements/rules are still parsed.

- Risk: documentation drift remains if only one document is updated.
  Severity: medium Likelihood: medium Mitigation: update spec, conformance
  register, implementation notes, and roadmap atomically.

## Progress

- [x] (2026-02-26 00:00Z) Reviewed roadmap item 2.6.1 and conformance register
  item 8, plus parser scanner implementation and current tests.
- [x] (2026-02-26 00:10Z) Drafted this ExecPlan with a single recommended
  contract (top-level `for` unsupported with deterministic diagnostics).
- [ ] Add failing unit and behavioural tests for unsupported top-level `for`.
- [ ] Implement scanner diagnostic behaviour for top-level `for`.
- [ ] Align spec/register/implementation notes and mark roadmap items done.
- [ ] Run formatting, lint, and test gates and record evidence.

## Surprises & discoveries

- Observation: current rule scanning only starts from `T_IDENT`, `T_IMPLIES`,
  and `T_AMP`, so top-level `K_FOR` is skipped without explicit diagnostics.
  Evidence: `src/parser/span_scanners/rules.rs` `collect_rule_spans` match arm.
  Impact: explicit top-level `for` detection/diagnostics are required to close
  conformance cleanly.

- Observation: roadmap currently has two related open items (`2.5.4` and
  `2.6.1`) on the same contract. Evidence: `docs/roadmap.md` entries 2.5.4 and
  2.6.1 are unchecked. Impact: completion updates must explicitly handle both
  items or explain why one remains open.

## Decision log

- Decision: plan to close the contract by making top-level `for` unsupported,
  with parser diagnostics, rather than implementing parser-stage desugaring to
  rules. Rationale: this matches current architecture and behaviour, avoids
  unbounded lowering scope before ADR-001 planning, and still delivers
  deterministic parser semantics and test coverage. Date/Author: 2026-02-26 /
  assistant

- Decision: keep rule-body `for` support unchanged and treat top-level `for`
  handling strictly as scanner-level conformance. Rationale: item 2.6.1 is
  scoped to conformance closure, scanner behaviour, tests, and spec language.
  Date/Author: 2026-02-26 / assistant

## Outcomes & retrospective

Pending implementation. Completion requires a single documented contract,
passing tests that prove it, and roadmap/docs alignment.

## Context and orientation

The parser builds top-level structure by scanning spans before CST creation.
Top-level item identification happens in `src/parser/span_scanner.rs` and
scanner modules under `src/parser/span_scanners/`. Rule span detection lives in
`src/parser/span_scanners/rules.rs`; this is where top-level entry criteria are
currently enforced.

Files expected to change:

- `src/parser/span_scanners/rules.rs`
- `src/parser/span_scanners/tests/mod.rs`
- `src/parser/tests/parser.rs` (or another parser-level unit test module)
- `tests/` behavioural tests (new or existing parser behaviour test file)
- `docs/differential-datalog-parser-syntax-spec-updated.md`
- `docs/parser-conformance-register.md`
- `docs/parser-implementation-notes.md`
- `docs/roadmap.md`

## Plan of work

Stage A: lock contract with failing tests first.

Add unit and behavioural tests that define the intended contract before scanner
changes. Include at least one parser-level test asserting a top-level `for`
produces one deterministic diagnostic and no rule span, plus one recovery test
showing a subsequent valid rule still parses. Confirm these tests fail on the
current codebase.

Go/no-go: proceed only after red tests fail for the expected reason (missing or
wrong diagnostic for top-level `for`).

Stage B: implement scanner behaviour.

In `src/parser/span_scanners/rules.rs`, add explicit top-level `K_FOR`
detection in `collect_rule_spans`. Reuse line-start and exclusion logic so only
true top-level contexts trigger this path. Emit a stable `Simple::custom` error
with a specific message (for example: "top-level `for` is unsupported; rewrite
as explicit rule(s)") and consume the statement safely enough to preserve
parsing of following statements.

Go/no-go: targeted scanner and parser tests pass, including recovery cases.

Stage C: align documentation and conformance records.

Update the normative spec text in
`docs/differential-datalog-parser-syntax-spec-updated.md` to mark top-level
`for` unsupported in this parser generation, replacing desugaring language in
sections 5.10, 6.5, and the AST crosswalk wording where needed. Update
`docs/parser-conformance-register.md` item 8 from `scheduled` to `implemented`,
describing scanner diagnostics as the contract. Update
`docs/parser-implementation-notes.md` to document the scanner invariant and
error policy for top-level `for`.

Go/no-go: all docs describe exactly one contract; no contradictory wording
remains.

Stage D: close roadmap entries and run gates.

Mark roadmap item `2.6.1` done in `docs/roadmap.md`. Because `2.5.4` is the
same contract decision, mark it done too unless maintainers explicitly prefer
keeping historical split tracking. Then run documentation validators and the
required Rust gates.

Go/no-go: all commands succeed and logs show green results.

## Concrete steps

1. Add red tests for top-level `for` contract.

   - Unit/scanner tests in `src/parser/span_scanners/tests/mod.rs`:
     - top-level `for` yields exactly one error with expected message.
     - top-level `for` does not create rule spans.
     - rule-body `for` still parses without top-level diagnostic.
   - Parser-level unit test in `src/parser/tests/`:
     - parse source beginning with top-level `for` and assert one diagnostic.
   - Behavioural test in `tests/`:
     - source with top-level `for` followed by valid rule still yields parsed
       rule plus one top-level `for` diagnostic.

2. Run targeted tests and capture red state.

   ```shell
   set -o pipefail && cargo test top_level_for 2>&1 | tee /tmp/2-6-1-red.log
   ```

   Expected pre-implementation outcome: at least one new test fails because the
   diagnostic path is not implemented.

3. Implement scanner diagnostic logic in `src/parser/span_scanners/rules.rs`.

4. Re-run targeted tests and confirm green state.

   ```shell
   set -o pipefail && cargo test top_level_for 2>&1 | tee /tmp/2-6-1-targeted-green.log
   ```

5. Update docs and roadmap entries.

6. Run documentation validation gates (docs changed).

   ```shell
   set -o pipefail && make markdownlint 2>&1 | tee /tmp/2-6-1-markdownlint.log
   set -o pipefail && make fmt 2>&1 | tee /tmp/2-6-1-fmt.log
   set -o pipefail && make nixie 2>&1 | tee /tmp/2-6-1-nixie.log
   ```

7. Run required Rust quality gates.

   ```shell
   set -o pipefail && make check-fmt 2>&1 | tee /tmp/2-6-1-check-fmt.log
   set -o pipefail && make lint 2>&1 | tee /tmp/2-6-1-lint.log
   set -o pipefail && make test 2>&1 | tee /tmp/2-6-1-test.log
   ```

## Validation and acceptance

Acceptance criteria:

- Parsing a top-level `for (...) ...` produces a deterministic,
  span-accurate parser diagnostic with stable wording.
- Top-level `for` does not create rule spans or fake AST rule nodes.
- Recovery works: valid statements/rules after an invalid top-level `for` are
  still scanned and parsed.
- Rule-body `for` parsing and existing `RuleBodyTerm::ForLoop` behaviour remain
  unchanged.
- Docs are aligned: spec, conformance register, implementation notes, and
  roadmap contain no contradictory top-level `for` claims.
- `make check-fmt`, `make lint`, and `make test` succeed.
- Because docs are changed, `make markdownlint`, `make fmt`, and `make nixie`
  also succeed.

## Idempotence and recovery

All steps are safe to re-run. If scanner recovery regresses, revert only the
latest scanner edits, keep tests, and re-implement with a narrower token
consumption strategy. If any gate fails, fix the failure and re-run the failed
command, then re-run the full required gate set before closing the task.

## Artifacts and notes

Keep command logs under `/tmp/2-6-1-*.log` and reference them in the
implementation summary. Prefer small, explicit test fixtures to avoid hiding
conformance intent.

## Interfaces and dependencies

No external dependencies are required.

Expected interface-level effects:

- Parser diagnostics surface one new stable top-level `for` error path.
- No new public AST types.
- Existing parser entrypoint remains `ddlint::parse`.

## Revision note

Initial draft created on 2026-02-26 to guide implementation of roadmap item
2.6.1 (and the overlapping decision item 2.5.4).
