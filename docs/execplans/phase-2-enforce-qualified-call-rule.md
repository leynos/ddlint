# Align parser with updated DDlog syntax: enforce qualified-call rule

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & discoveries`, `Decision log`, and
`Outcomes & retrospective` must be kept up to date as work proceeds.

Status: DRAFT

PLANS.md is not present in this repository.

## Purpose / big picture

Enforce the updated DDlog call parsing rule so only fully scoped identifiers
(for example `pkg::math::sum`) parse as function calls at parse time. Bare
`name(...)` forms must remain syntactically valid but be represented as
unresolved application for later name resolution. Success means users can parse
all existing DDlog expression forms, the parser now distinguishes qualified
function calls from unresolved applies, unit and behavioural tests cover this
behaviour, and Phase 2 in `docs/roadmap.md` can be marked done for this item
only after all quality gates pass.

## Constraints

- Follow `docs/differential-datalog-parser-syntax-spec-updated.md` as the
  normative grammar and portability guidance.
- Preserve existing parser architecture: tokenizer -> Pratt expression parser ->
  rule/body classification -> CST/AST wrappers.
- Do not add new dependencies.
- Keep Rust modules below 400 lines; split files if required.
- Every new Rust module must start with a `//!` module comment.
- Keep diagnostics span-accurate and deterministic.
- Keep existing behaviour for method calls, field access, aggregation
  extraction, and rule-head lowering unless explicitly changed in this plan.
- Use Make targets and run required gates with `set -o pipefail` and `tee`.
- Record design decisions in relevant docs under `docs/`.

## Tolerances (exception triggers)

- Scope: if this work exceeds 14 files changed or 450 net new lines, stop and
  escalate.
- AST blast radius: if enforcing the rule requires changing public AST accessors
  outside expression/rule modules, stop and escalate.
- Grammar ambiguity: if docs conflict on whether unresolved applications need a
  distinct AST variant, pause and resolve in `Decision log` before coding.
- Regression loop: if `make test` still fails after two focused fix iterations,
  stop and escalate with failing test names.
- Hidden coupling: if aggregation extraction or rule flattening semantics must
  change beyond matching the new call distinction, stop and escalate.

## Risks

- Risk: introducing a distinct unresolved-application representation can break
  tests and consumers that currently assume `Expr::Call` for `name(...)`.
  Severity: high Likelihood: high Mitigation: stage changes behind tests first,
  and update helper constructors plus rule classifiers in one atomic pass.
- Risk: parsing scoped identifiers (`::`) in Pratt prefix may regress existing
  postfix parsing and delimiter diagnostics. Severity: medium Likelihood:
  medium Mitigation: add dedicated unit tests for scoped identifier lexeme
  assembly and malformed scoped paths.
- Risk: aggregation extraction currently keys off `Expr::Call` with variable
  callee names (`group_by`, `Aggregate`); call-kind changes can silently
  disable extraction. Severity: high Likelihood: medium Mitigation: add
  explicit regression tests in rule-body classification for both forms before
  implementation.
- Risk: documentation drift between roadmap/spec and parser behaviour.
  Severity: medium Likelihood: medium Mitigation: update relevant docs in the
  same change and only mark roadmap item done after gates pass.

## Progress

- [x] (2026-02-09 00:00Z) Drafted ExecPlan after reviewing roadmap, parser
  spec, parser design docs, and current parser/tests.
- [ ] Finalize AST representation choice for unresolved application and record
  it in `Decision log`.
- [ ] Add failing unit and behavioural tests for qualified vs bare call forms.
- [ ] Implement parser and AST changes, then adapt rule classification.
- [ ] Update docs/design notes and mark roadmap entry done.
- [ ] Run full quality gates and verify green logs.

## Surprises & discoveries

- Observation: current Pratt parsing does not consume `T_COLON_COLON` as part
  of expression identifiers, so `pkg::f()` cannot be represented correctly yet.
  Evidence: `parse_identifier_or_struct` consumes only one `T_IDENT`, and
  remaining tokens become unexpected trailing tokens. Impact: scoped-identifier
  assembly must be added before qualified-call enforcement is possible.
- Observation: rule aggregation extraction currently pattern matches only
  `Expr::Call` with `Expr::Variable` callee. Evidence: `classify_expression` in
  `src/parser/ast/rule.rs` checks `Expr::Call` and name string directly.
  Impact: aggregation classification must be updated in the same milestone as
  call disambiguation to avoid regressions.

## Decision log

- Decision: represent unresolved `name(...)` application explicitly in the AST
  (for example `Expr::Apply`) and reserve `Expr::Call` for qualified function
  calls. Rationale: this makes parse-time intent explicit and satisfies the
  spec rule without depending on unavailable name-resolution infrastructure.
  Date/Author: 2026-02-09 (assistant)
- Decision: treat all postfix invocation forms whose callee is not a fully
  scoped lower-case identifier as unresolved application, including `(f)(x)`.
  Rationale: keeps parser deterministic and defers semantic disambiguation to
  later resolution passes. Date/Author: 2026-02-09 (assistant)

## Outcomes & retrospective

Planned outcome: parse-time call classification matches the updated DDlog spec,
regression risk is bounded by tests, and docs/roadmap stay aligned. This
section should be updated during implementation with actual outcomes, gaps, and
lessons.

## Context and orientation

Relevant implementation areas:

- `src/parser/expression/prefix.rs` and
  `src/parser/expression/data_structures.rs`: identifier parsing currently
  handles only single-token identifiers.
- `src/parser/expression/pratt.rs`: postfix `(...)` currently always produces
  `Expr::Call` regardless of callee shape.
- `src/parser/ast/expr.rs`: AST definition and S-expression rendering for call
  forms.
- `src/parser/ast/rule.rs`: body-term classification and aggregation extraction
  currently keyed to `Expr::Call`.
- `src/test_util/expressions.rs`: helper constructors used by many tests.
- Unit parser tests: `src/parser/tests/expression.rs` and
  `src/parser/tests/rules/body_terms.rs`.
- Behavioural tests: `tests/expression_var_and_call.rs`,
  `tests/expression_postfix.rs`, and `tests/rule_behaviour.rs`.
- Design/roadmap docs:
  `docs/differential-datalog-parser-syntax-spec-updated.md`,
  `docs/pratt-parser-for-ddlog-expressions.md`, `docs/parser-plan.md`, and
  `docs/roadmap.md`.

## Plan of work

Stage A: lock expected behaviour and test surface (no production code edits).
Add or update tests so they fail against current behaviour: bare `foo()` should
not be treated as a qualified function call, while `pkg::foo()` should. Add
rule-body classification tests to ensure aggregation extraction still works
once calls are split by kind. Do not proceed until failing tests capture each
new acceptance criterion.

Stage B: implement scoped identifier parsing and call/apply split in Pratt.
Extend identifier parsing to consume `T_IDENT (T_COLON_COLON T_IDENT)+` and
create a scoped identifier expression value. Update postfix invocation parsing
so only fully scoped lower-case identifiers emit `Expr::Call`; everything else
emits unresolved application (`Expr::Apply` or equivalent). Preserve existing
method-call and field-access semantics.

Stage C: integrate downstream consumers and helper APIs. Update `Expr`
rendering, test helpers, and rule-body classification so aggregation detection,
loop flattening, and existing diagnostics continue to work. Ensure
backward-compatible behaviour for constructs unrelated to this feature
(operators, literals, control flow).

Stage D: documentation, validation, and roadmap update. Record final parsing
decision and AST representation in the relevant design doc
(`docs/pratt-parser-for-ddlog-expressions.md` and/or syntax spec portability
notes). Mark the specific Phase 2 roadmap checklist item as done in
`docs/roadmap.md`. Run all required checks and keep logs.

## Concrete steps

1. Add/adjust failing tests for call qualification

   - Update `src/parser/tests/expression.rs` with cases covering:
     - `pkg::foo()` -> qualified function call.
     - `foo()` -> unresolved application.
     - `(foo)(x)` -> unresolved application.
     - malformed `pkg::` path -> span-aware parse error.
   - Update `tests/expression_var_and_call.rs` behavioural assertions to reflect
     new distinction.
   - Add regression cases in `src/parser/tests/rules/body_terms.rs` showing
     `group_by(...)` and `Aggregate(...)` still classify as aggregation terms.

2. Implement scoped identifier parsing in Pratt prefix path

   - Extend identifier parsing logic in
     `src/parser/expression/data_structures.rs` (or a new helper module) to
     consume `::` segments and return canonical text.
   - Preserve existing struct-literal disambiguation rules.

3. Implement qualified-call enforcement in postfix parsing

   - Update `parse_function_call_postfix` in `src/parser/expression/pratt.rs`
     to choose `Expr::Call` only for fully scoped, lower-case final-segment
     identifiers.
   - Emit unresolved application AST node for all other callee shapes.

4. Update AST and helper layers

   - Update `src/parser/ast/expr.rs` to add/represent unresolved application
     and keep `to_sexpr()` deterministic.
   - Update `src/test_util/expressions.rs` with constructors for both qualified
     call and unresolved apply forms.
   - Update affected tests in `tests/expr_to_sexpr.rs` and
     `tests/expression_to_sexpr.rs` if rendering changes.

5. Update rule-body classification and any call consumers

   - Update `src/parser/ast/rule.rs` classification so aggregation detection
     accepts the new call representation where appropriate.
   - Ensure no regressions in for-loop flattening and assignment handling.

6. Record design decisions in docs

   - Update `docs/pratt-parser-for-ddlog-expressions.md` with the final AST
     call/apply distinction and rationale.
   - If wording needs tightening, update portability notes in
     `docs/differential-datalog-parser-syntax-spec-updated.md`.

7. Mark roadmap entry done

   - In `docs/roadmap.md`, mark the checklist item
     "Enforce the qualified-call rule …" as done only after all gates pass.

8. Run validation commands from repository root

   - Documentation checks (required because docs change):

     ```shell
     set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
     set -o pipefail && make fmt 2>&1 | tee /tmp/ddlint-fmt.log
     set -o pipefail && make nixie 2>&1 | tee /tmp/ddlint-nixie.log
     ```

   - Required Rust gates:

     ```shell
     set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
     set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
     set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log
     ```

## Validation and acceptance

Acceptance criteria:

- Unit tests show only fully scoped identifiers parse as qualified function
  calls.
- Behavioural tests confirm bare `name(...)` parses as unresolved application
  and no longer as qualified function call.
- Rule aggregation behaviour remains correct for `group_by` and `Aggregate`.
- No regression in method calls, field access, and delimiter diagnostics.
- Design docs reflect the final decision and rationale.
- `docs/roadmap.md` marks this Phase 2 item as done.
- `make check-fmt`, `make lint`, and `make test` all succeed.
- `make markdownlint`, `make fmt`, and `make nixie` succeed for doc updates.

## Idempotence and recovery

All steps are re-runnable. If AST-shape changes cause broad breakage, first
restore passing state by keeping old call behaviour behind a temporary helper,
land scoped-identifier parsing independently, then re-apply call/apply split
with tests in smaller commits. If a gate fails, fix the specific failure,
re-run the failed target, then re-run full gates.

## Artifacts and notes

Keep command logs under `/tmp/ddlint-*.log` and summarise relevant pass/fail
lines in the implementation handoff. Prefer focused fixture additions over
large golden files.

## Interfaces and dependencies

No new dependencies are expected. Planned interface changes are internal to the
parser AST and test helpers. If `Expr` gains an unresolved-application variant,
all pattern matches over `Expr` in parser and tests must be exhaustively
updated.
