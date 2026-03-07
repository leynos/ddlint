# Decide the aggregation extraction boundary

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: DRAFT

## Purpose / big picture

Roadmap item `2.6.2` is open because the normative syntax spec still describes
`group_by` and legacy `Aggregate` as parse-stage rewrites performed before the
Abstract Syntax Tree (AST), while the implementation performs aggregation
classification only when `Rule::body_terms()` or `Rule::flattened_body_terms()`
is called. `parse()` currently guarantees a lossless Concrete Syntax Tree
(CST), top-level `for` semantic rules, and name-uniqueness validation, but it
does not emit aggregation diagnostics or a parser-wide lowered aggregation
representation.

After this change, the boundary will be explicit and testable. A novice should
be able to read the docs, call `parse()`, inspect `Parsed::errors()`, and know
exactly whether aggregation misuse is detected there or only when rule-body
semantic helpers are invoked. Success is observable through aligned docs, new
unit and behavioural tests, green quality gates, and `docs/roadmap.md` item
`2.6.2` marked done only after implementation completes.

## Recommended decision

Close item `2.6.2` by standardizing aggregation extraction as a rule-analysis
stage, not as a base `parse()` pipeline guarantee.

Concretely:

- `parse()` continues to build the CST, collect top-level `for` semantic rules,
  and run existing parser validators.
- `Rule::body_terms()` remains the contract that classifies `group_by` and
  `Aggregate`, normalizes legacy argument order, and enforces the at-most-one
  rule.
- The syntax spec and design docs are narrowed to match this implemented
  boundary instead of promising a non-existent `RHSGroupBy`/`__group`
  parse-stage rewrite.

If repository evidence discovered during implementation proves that downstream
code already depends on parse-time aggregation nodes or parser-level
aggregation diagnostics, stop and escalate. That would be a materially larger
feature than the conformance-alignment task captured here.

## Constraints

- Preserve lossless CST behaviour. `Root::text()` and existing CST-backed AST
  wrappers must continue to reflect the original source text exactly.
- Do not inject synthetic aggregation nodes into the CST.
- Treat the current public surface as the baseline:
  `parse()`, `Parsed::errors()`, `Parsed::semantic_rules()`,
  `Rule::body_terms()`, `Rule::flattened_body_terms()`,
  `RuleBodyTerm::Aggregation`, and `RuleAggregation`.
- Prefer clarifying and codifying the current helper-stage contract over
  re-architecting the parse pipeline.
- If code changes are needed, keep them additive and narrowly scoped to
  documentation, doc comments, diagnostics wording, or tests.
- Add both unit tests and behavioural tests that prove the chosen boundary.
- Record the finalized boundary in the relevant design documents, not only in
  the conformance register.
- Mark `docs/roadmap.md` item `2.6.2` done only after docs, tests, and all
  required gates pass.
- Run repository quality gates through Make targets using `set -o pipefail` and
  `tee`.

## Tolerances (exception triggers)

- Scope: if implementing this decision requires more than 8 files of code
  changes or more than 250 net new code lines, stop and escalate. That is a
  sign the work is drifting into a parser-architecture change rather than a
  conformance decision.
- Interface: if the change requires a new public `Parsed` aggregation API,
  stop and escalate.
- Semantics: if evidence shows that the spec's parse-stage `RHSGroupBy`
  contract is intentionally required for the current parser generation, stop
  and escalate with concrete file references.
- Compatibility: if existing tests or downstream callers rely on aggregation
  diagnostics appearing in `Parsed::errors()`, stop and escalate.
- Validation: if `make test` or `make lint` still fails after three focused fix
  attempts, stop and escalate with the failing test names or lint messages.

## Risks

- Risk: narrowing the spec to the current helper-stage behaviour could hide a
  still-desired long-term parser rewrite. Severity: medium. Likelihood: medium.
  Mitigation: document the chosen boundary as a current-generation parser
  contract and leave future parser-wide lowering to ADR follow-up work rather
  than implying it already exists.

- Risk: docs can drift again because aggregation behaviour is described in
  multiple places: the syntax spec, implementation notes, conformance register,
  roadmap, and design doc. Severity: high. Likelihood: high. Mitigation: update
  all affected documents in the same change and add tests that assert
  observable pipeline behaviour.

- Risk: wording such as "parse stage", "parser", and "semantic stage" can be
  interpreted loosely. Severity: medium. Likelihood: high. Mitigation: define
  each stage concretely in the docs using repository types and functions
  (`parse()`, `Parsed`, `Rule::body_terms()`).

- Risk: behavioural tests may currently over-focus on `Rule::body_terms()`
  success and never assert the absence of parser-level aggregation errors.
  Severity: medium. Likelihood: high. Mitigation: add dedicated contract tests
  for `Parsed::errors()` and `Parsed::semantic_rules()`.

## Progress

- [x] (2026-03-07 00:00Z) Reviewed roadmap item `2.6.2`, conformance register
  item 9, the syntax spec, implementation notes, archived parser plan, and the
  current aggregation tests.
- [x] (2026-03-07 00:00Z) Drafted this ExecPlan in
  `docs/execplans/2-6-2-decide-the-aggregation-extraction-boundary.md`.
- [ ] Freeze the aggregation boundary decision in the docs and doc comments.
- [ ] Add red unit tests proving that aggregation classification belongs to
  `Rule::body_terms()` rather than `parse()`.
- [ ] Add red behavioural tests proving the same contract through the public
  parser API.
- [ ] Implement the minimal code and documentation updates needed to make the
  contract explicit and non-contradictory.
- [ ] Mark conformance register item 9 as `implemented`.
- [ ] Mark roadmap item `2.6.2` done.
- [ ] Run `make fmt`, `make markdownlint`, `make nixie`,
  `make check-fmt`, `make lint`, and `make test`.

## Surprises & Discoveries

- The normative spec currently overstates implemented behaviour. Section 1 says
  the parser performs `group_by` extraction and legacy `Aggregate` lowering,
  section 6.1 says the parser extracts `group_by` into an explicit `RHSGroupBy`
  node bound to `__group`, and section 13 says this lowering should stay in the
  parser or immediately after. No corresponding `RHSGroupBy` node or
  parse-stage `__group` rewrite exists in the code.

- The current implementation already has a stable semantic helper boundary.
  `src/parser/ast/rule.rs` classifies aggregations inside `Rule::body_terms()`,
  normalizes `Aggregate((key), project)` into `(project, key)`, and emits
  duplicate or wrong-arity diagnostics there.

- `parse()` does not surface aggregation diagnostics today. In
  `src/parser/mod.rs`, the parse pipeline only collects scanner/CST errors,
  top-level `for` semantic rules, and name-uniqueness validation.

- Existing tests already imply the helper-stage contract. Unit tests in
  `src/parser/tests/rules/aggregations.rs` and behavioural tests in
  `tests/rule_behaviour.rs` call `rule.body_terms()` directly to observe
  aggregation classification and errors.

- The archived parser plan in `docs/archive/parser-plan.md` already describes
  aggregation extraction during `Rule::body_terms()` rule-body analysis. The
  live syntax spec drifted away from that implementation-level boundary.

## Decision Log

- Decision: this plan recommends standardizing aggregation extraction as a
  rule-analysis helper boundary rather than moving it into the base parse
  pipeline. Rationale: this matches the implemented architecture, preserves CST
  losslessness, avoids inventing a new `Parsed` contract, and is proportionate
  to a pre-ADR conformance decision. Date/Author: 2026-03-07 / assistant

- Decision: the design-doc update should include `docs/ddlint-design.md`, not
  just parser-specific docs. Rationale: the boundary question is architectural,
  because it defines what the parser pipeline promises to downstream rule
  engines and future crate-split work. Date/Author: 2026-03-07 / assistant

- Decision: if implementation uncovers a real consumer that needs parse-stage
  aggregation nodes or parser-level aggregation diagnostics, stop instead of
  maintaining two parallel boundaries. Rationale: dual ownership would make the
  pipeline harder to reason about and would undermine the purpose of this
  conformance-closure task. Date/Author: 2026-03-07 / assistant

## Context and orientation

The current parser flow is:

1. `parse()` in `src/parser/mod.rs` tokenizes source, collects top-level spans,
   gathers top-level `for` semantic rules, builds the CST, and runs
   name-uniqueness validation.
2. `Parsed` in `src/parser/cst_builder/mod.rs` stores:
   - the green tree,
   - the typed root,
   - top-level `for` semantic rules, and
   - parse-time errors.
3. `Rule` in `src/parser/ast/rule.rs` exposes rule-body semantic helpers.
   Aggregation detection, legacy normalization, duplicate checks, and arity
   checks all live here today.

The implementation files most relevant to this change are:

- `src/parser/mod.rs`
- `src/parser/cst_builder/mod.rs`
- `src/parser/ast/rule.rs`
- `src/parser/tests/rules/aggregations.rs`
- `tests/rule_behaviour.rs`

The documents that must be aligned in the same change are:

- `docs/differential-datalog-parser-syntax-spec-updated.md`
- `docs/parser-implementation-notes.md`
- `docs/parser-conformance-register.md`
- `docs/ddlint-design.md`
- `docs/roadmap.md`

## Plan of work

### Stage A: freeze the contract in prose before touching behaviour

Update the normative and design documents so they describe the implemented
boundary precisely.

- In `docs/differential-datalog-parser-syntax-spec-updated.md`, rewrite the
  aggregation sections so they no longer promise a parser-wide `RHSGroupBy`
  node or `__group` rewrite. The spec should say that the parser preserves rule
  body literals in the CST, while rule-body semantic extraction classifies
  aggregation forms and enforces validation when semantic rule terms are
  requested.
- In `docs/parser-implementation-notes.md`, add an explicit pipeline guarantee:
  `parse()` does not perform global aggregation validation;
  `Rule::body_terms()` and `Rule::flattened_body_terms()` do.
- In `docs/ddlint-design.md`, add a short parser-pipeline note clarifying the
  ownership split between CST construction and rule-body semantic helpers.

### Stage B: add failing tests that encode the chosen boundary

Add tests that prove the boundary through observable behaviour, not just
through comments.

- Extend `src/parser/tests/rules/aggregations.rs` with unit tests that parse a
  rule containing duplicate or malformed aggregation forms, assert that
  `parse()` itself reports no aggregation error, and then assert that
  `rule.body_terms()` reports the expected error.
- Add a behavioural test file such as `tests/aggregation_boundary.rs` that
  exercises the public API end-to-end:
  - `parse(src).errors()` stays empty for aggregation misuse,
  - `parse(src).semantic_rules()` stays unchanged,
  - `rule.body_terms()` is the place where aggregation normalization and
    validation become visible.

These tests must fail before the implementation/doc alignment and pass after.

### Stage C: implement the minimal code and doc-comment changes

Make the smallest code changes needed to ensure the repository itself states
the chosen contract clearly.

- Update the `parse()` doc comment in `src/parser/mod.rs` so it does not imply
  aggregation validation or lowering as part of the base parse result.
- Update `Parsed::semantic_rules()` documentation in
  `src/parser/cst_builder/mod.rs` to make clear that this semantic layer is
  currently used for top-level `for` desugaring, not rule-body aggregation.
- Tighten `Rule::body_terms()` documentation in `src/parser/ast/rule.rs` so it
  is explicit that this method is the aggregation-classification boundary.
- Only introduce code changes beyond documentation if the new tests uncover an
  ambiguity or mismatch in observable behaviour.

### Stage D: close the conformance item and roadmap entry

- Update `docs/parser-conformance-register.md` item 9 from `scheduled` to
  `implemented`, with wording that matches the chosen boundary exactly.
- Mark `docs/roadmap.md` item `2.6.2` as done after all validation gates pass.

## Concrete edit list

1. Edit `docs/differential-datalog-parser-syntax-spec-updated.md`.
2. Edit `docs/parser-implementation-notes.md`.
3. Edit `docs/ddlint-design.md`.
4. Edit `docs/parser-conformance-register.md`.
5. Edit `docs/roadmap.md`.
6. Edit `src/parser/mod.rs` doc comment if needed.
7. Edit `src/parser/cst_builder/mod.rs` doc comment if needed.
8. Edit `src/parser/ast/rule.rs` doc comment if needed.
9. Extend `src/parser/tests/rules/aggregations.rs`.
10. Add `tests/aggregation_boundary.rs`.

## Validation commands

Run from the repository root.

```bash
set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-2-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-2-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-2-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-2-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/2-6-2-lint.log
set -o pipefail; make test 2>&1 | tee /tmp/2-6-2-test.log
```

Expected success signal:

```plaintext
All commands exit with status 0. The new unit and behavioural aggregation
boundary tests pass. No markdown, formatting, lint, or workspace test
failures remain.
```

## Acceptance criteria

- The repository has one explicit answer to the aggregation-boundary question.
- The syntax spec, implementation notes, design doc, conformance register, and
  roadmap all describe the same ownership boundary.
- New unit tests prove that aggregation classification and validation occur in
  `Rule::body_terms()`.
- New behavioural tests prove that `parse()` does not currently surface
  aggregation diagnostics or parser-level aggregation semantic rules.
- `docs/parser-conformance-register.md` item 9 is `implemented`.
- `docs/roadmap.md` item `2.6.2` is marked done only after the gates pass.
- `make check-fmt`, `make lint`, and `make test` succeed, with the markdown
  validation steps also green because this change updates documentation.

## Idempotence and recovery

The planned edits are safe to re-run. If the new behavioural tests reveal an
existing parser-level aggregation side effect that contradicts this plan:

1. Stop implementation.
2. Record the contradiction in the `Decision Log` and `Surprises &
   Discoveries` sections.
3. Re-scope the work as a parser-architecture change rather than a conformance
   alignment task.

## Outcomes & Retrospective

This section is intentionally incomplete until implementation finishes. At
completion it must summarize:

- the finalized boundary,
- the exact docs and tests updated,
- whether any code behaviour changed beyond documentation and tests, and
- the final gate results.
