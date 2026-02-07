# Align parser with updated DDlog syntax: qualified-call parsing rule

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: DRAFT

`PLANS.md` is not present in this repository.

## Purpose / big picture

Implement the syntax-spec rule that only fully scoped identifiers
(`module::path::name`) are parsed as function calls. Bare `name(...)` must be
parsed as variable application and left unresolved for later name resolution.

Success is observable in both parser layers:

- Unit-level expression parsing distinguishes bare application from qualified
  function call.
- Behavioural rule parsing still extracts `group_by` and legacy `Aggregate`
  correctly from unresolved bare applications.
- The roadmap entry
  `Align Parser with Updated DDlog Syntax -> Enforce the qualified-call rule`
  is marked done after all gates pass.

## Constraints

- Follow the normative rule in
  `docs/differential-datalog-parser-syntax-spec-updated.md` section 2.2.
- Preserve existing Pratt precedence and postfix behaviour for method calls,
  tuple indices, bit slices, diff markers, and delay markers.
- Keep parser error recovery behaviour intact (no panic-first shortcuts).
- Do not add new dependencies.
- Keep module files under 400 lines by extracting helpers when needed.
- Use Makefile targets for final quality gates, with `set -o pipefail` and
  `tee` logs.
- Record implementation decisions in relevant design docs under `docs/`.

## Tolerances (exception triggers)

- Scope: if implementation requires touching more than 14 files or more than
  650 net changed lines, stop and escalate.
- Public API: if this requires breaking non-parser public APIs beyond the
  expression AST surface, stop and escalate.
- Representation ambiguity: if there is no low-risk way to encode
  "function call" vs "variable application" in the current AST, stop and
  present options with trade-offs.
- Test churn: if `make test` still fails after 3 full fix attempts, stop and
  escalate with failing test clusters.
- Time: if one milestone exceeds 2 hours without green targeted tests, stop
  and report blocker evidence.

## Risks

- Risk: expression AST representation change causes broad fixture/test churn.
  Severity: medium Likelihood: high Mitigation: choose one explicit
  representation and update test helpers first so compile errors converge
  quickly.

- Risk: rule-body aggregation extraction regresses because it currently keys on
  `Expr::Call` with `Expr::Variable` callee names. Severity: high Likelihood:
  medium Mitigation: update aggregation classification alongside AST changes
  and add focused regression tests for `group_by` and `Aggregate`.

- Risk: introducing scoped identifier parsing changes postfix parsing around
  `.` and `[` chains. Severity: medium Likelihood: medium Mitigation: add
  targeted postfix regression cases before refactor.

- Risk: roadmap updated prematurely before all gates pass.
  Severity: low Likelihood: low Mitigation: keep roadmap checkbox update in the
  final milestone only.

## Progress

- [x] (2026-02-07 02:24Z) Reviewed roadmap, spec, and parser code paths for
  call parsing and rule-term classification.
- [x] (2026-02-07 02:24Z) Drafted this ExecPlan in
  `docs/execplans/phase-2-parse-apply-items.md`.
- [ ] Finalize AST representation for qualified call vs bare application.
- [ ] Add/adjust unit tests to encode the new rule before parser edits.
- [ ] Implement scoped-identifier parsing and postfix call disambiguation.
- [ ] Update rule-body aggregation classification for the new representation.
- [ ] Add behavioural regression tests.
- [ ] Update design docs and roadmap checkbox.
- [ ] Run all quality gates and capture logs.

## Surprises & Discoveries

- Observation: project-memory CLI tools (`qdrant-find`) are unavailable in this
  environment (`command not found`). Evidence: shell execution failed with exit
  code 127. Impact: proceeded using repository docs and source as primary
  context.

- Observation: this ExecPlan file previously documented a completed earlier
  subtask (apply items + extern transformer diagnostics). Evidence: existing
  file content had Status `COMPLETE` for that subtask. Impact: file is now
  repurposed for the next unchecked roadmap item.

- Observation: expression prefix parsing currently consumes only one
  `T_IDENT`; scoped tokens (`T_SCOPE`) are not assembled into a path in the
  Pratt parser. Evidence: `src/parser/expression/prefix.rs` and
  `src/parser/expression/data_structures.rs`. Impact: scoped-call support
  requires identifier-path parsing changes.

## Decision Log

- Decision: represent the syntax distinction explicitly in the expression AST
  so parser output can encode "qualified function call" vs "unresolved variable
  application" without needing name resolution. Rationale: this is the only
  reliable way to enforce and test the roadmap requirement at parse time.
  Date/Author: 2026-02-07 / assistant

- Decision: keep aggregation extraction (`group_by`, `Aggregate`) as a parse
  responsibility in rule-body classification, even though bare calls become
  unresolved applications. Rationale: the spec requires parser-time extraction
  and normalization. Date/Author: 2026-02-07 / assistant

- Decision: update roadmap checkbox only after formatting, lint, and full test
  suite are green. Rationale: avoids reporting completion before objective
  validation. Date/Author: 2026-02-07 / assistant

## Outcomes & Retrospective

Not complete yet. This section will be updated after implementation with: what
shipped, what changed from this draft, which risks materialized, and which
follow-up tasks remain.

## Context and orientation

Current call parsing flow:

- `src/parser/expression/prefix.rs` dispatches `T_IDENT` to
  `parse_identifier_or_struct`.
- `src/parser/expression/data_structures.rs` currently turns identifiers into
  `Expr::Variable` (or struct literal), not scoped identifier paths.
- `src/parser/expression/pratt.rs` treats any `(...)` postfix as `Expr::Call`.

Current downstream coupling:

- `src/parser/ast/rule.rs` classifies `group_by`/`Aggregate` by matching
  `Expr::Call { callee: Expr::Variable(name), ... }`.
- Test helpers in `src/test_util/expressions.rs` assume bare `name(...)`
  always maps to `Expr::Call`.
- Behavioural tests in `tests/expression_var_and_call.rs` and rule tests in
  `tests/rule_behaviour.rs` assert this old shape.

Key files expected to change:

- `src/parser/ast/expr.rs`
- `src/parser/expression/data_structures.rs`
- `src/parser/expression/pratt.rs`
- `src/parser/ast/rule.rs`
- `src/test_util/expressions.rs`
- `src/parser/tests/expression.rs`
- `src/parser/tests/rules/aggregations.rs`
- `tests/expression_var_and_call.rs`
- `tests/rule_behaviour.rs`
- `docs/pratt-parser-for-ddlog-expressions.md`
- `docs/roadmap.md`

## Plan of work

Stage A: lock behaviour with tests first (no parser edits yet).

Add failing unit and behavioural cases that capture the target rule:

- Bare `foo(x)` is parsed as unresolved application.
- Qualified `pkg::foo(x)` is parsed as function call.
- Existing postfix chains and diagnostics continue to behave.

Go/no-go for Stage A: new tests fail only for the expected disambiguation gap,
not for unrelated parser regressions.

Stage B: add AST representation for call disambiguation.

Update `src/parser/ast/expr.rs` (and test helpers) to encode both forms:
qualified function call and unresolved variable application. Keep the shape
simple and explicit so rule-body classification can pattern-match safely.

Go/no-go for Stage B: crate compiles and expression unit tests can assert both
forms without ambiguity.

Stage C: implement scoped-identifier parsing and postfix call routing.

In `src/parser/expression/data_structures.rs`, parse identifier paths that
include `::` into a dedicated expression form. In
`src/parser/expression/pratt.rs`, route `(...)` postfix to:

- function-call node only when callee is fully scoped identifier,
- unresolved application node otherwise.

Go/no-go for Stage C: targeted expression tests for bare vs qualified calls
pass, and no new postfix regressions appear.

Stage D: adapt rule-term classification and docs.

Update `src/parser/ast/rule.rs` aggregation classification to recognise
`group_by`/`Aggregate` from unresolved application form. Record the design in
`docs/pratt-parser-for-ddlog-expressions.md` (and in the syntax spec only if
normative wording needs clarification).

Go/no-go for Stage D: rule-term unit/behaviour tests pass for both aggregation
forms.

Stage E: full validation and roadmap update.

Run all required gates. Only after green results, mark the relevant roadmap
entry as done in `docs/roadmap.md`.

Go/no-go for Stage E: all commands succeed and logs confirm zero failures.

## Concrete steps

1. Add/adjust unit tests in parser internals:

   - `src/parser/tests/expression.rs`: add explicit cases for bare and
     qualified calls.
   - `src/parser/tests/rules/aggregations.rs`: ensure `group_by` and
     `Aggregate` extraction still works under the new call representation.

2. Add/adjust behavioural tests:

   - `tests/expression_var_and_call.rs`: assert disambiguation at public API
     level.
   - `tests/rule_behaviour.rs`: assert aggregation extraction remains correct.

3. Implement expression AST and parser changes:

   - Update `src/parser/ast/expr.rs` with explicit representation and
     `to_sexpr()` output.
   - Update `src/test_util/expressions.rs` helpers so tests can construct the
     two call forms cleanly.
   - Update `src/parser/expression/data_structures.rs` to parse scoped
     identifiers.
   - Update `src/parser/expression/pratt.rs` call-postfix handling.

4. Update downstream rule classification:

   - Modify `src/parser/ast/rule.rs` aggregation detection to match the new
     unresolved application representation.

5. Update design documentation:

   - Add a section in `docs/pratt-parser-for-ddlog-expressions.md` describing
     the chosen AST representation and why disambiguation is deferred.

6. Run focused checks during implementation:

   - `set -o pipefail && cargo test --workspace expression_var_and_call 2>&1 \`
     `| tee /tmp/ddlint-qualified-call-focused-1.log`
   - `set -o pipefail && cargo test --workspace rule_behaviour 2>&1 \`
     `| tee /tmp/ddlint-qualified-call-focused-2.log`

7. Run final quality gates from repository root:

   - `set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log`
   - `set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log`
   - `set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log`

8. Mark roadmap item done only after Step 7 is fully green:

   - Update `docs/roadmap.md` checkbox under
     `Align Parser with Updated DDlog Syntax` for qualified-call rule.

Expected success transcript highlights:

- `make check-fmt`: exits 0 with no formatting diffs.
- `make lint`: exits 0 with zero Clippy warnings.
- `make test`: exits 0 and includes all parser/unit/behaviour tests passing.

## Validation and acceptance

Feature acceptance is complete when all of the following are true:

- `parse_expression("foo(x)")` yields unresolved variable application (not a
  qualified function-call form).
- `parse_expression("pkg::foo(x)")` yields a qualified function-call form.
- Rule-body aggregation extraction still recognises and normalizes
  `group_by(...)` and `Aggregate(...)`.
- Existing postfix behaviour (method call, tuple index, bit-slice, diff,
  delay) remains green.
- `make check-fmt`, `make lint`, and `make test` succeed.
- Roadmap entry is marked done only after the above pass.

## Idempotence and recovery

All plan steps are safe to rerun.

If parser edits introduce broad compile churn:

- Re-run focused tests first to isolate call-disambiguation failures.
- Keep AST representation changes and parser logic changes in separate commits
  during implementation for easier rollback.
- If regression scope exceeds tolerance, stop and escalate with failing file
  list and failing test groups.

## Artifacts and notes

During implementation, store command logs at:

- `/tmp/ddlint-qualified-call-focused-1.log`
- `/tmp/ddlint-qualified-call-focused-2.log`
- `/tmp/ddlint-check-fmt.log`
- `/tmp/ddlint-lint.log`
- `/tmp/ddlint-test.log`

Include concise excerpts from these logs in the final implementation report to
prove behavioural and quality-gate success.

## Interfaces and dependencies

No new dependencies are expected.

The implementation must leave parser-facing interfaces explicit enough for
name-resolution follow-up work. At completion, expression AST and parser code
must support two parse-time call forms:

- Qualified function call (callee is fully scoped identifier).
- Unresolved variable application (callee is not fully scoped).

Rule-body aggregation code must consume the unresolved application form for
`group_by` and `Aggregate` detection without requiring name resolution.

## Revision note

Updated this file from the previous completed apply-items ExecPlan to a new
DRAFT plan for the next unchecked roadmap item: qualified-call parsing
alignment. This changes remaining work from closed historical notes to an
executable implementation plan with current milestones, tolerances, and
validation gates.
