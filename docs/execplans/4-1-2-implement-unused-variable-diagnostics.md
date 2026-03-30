# Implement `unused-variable` diagnostics

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: COMPLETED

## Purpose / big picture

Roadmap item `4.1.2` is the second production lint rule in the initial
correctness catalogue. After this change, `ddlint` exports an `unused-variable`
lint rule (`UnusedVariableRule`) that callers explicitly register in
`CstRuleStore` before running `Runner`. Once registered, the rule emits one
warning for each rule-local binding that never receives a resolved variable use
in the same rule. The wildcard name `_` is treated as an explicit ignore and
must never produce a diagnostic.

For this milestone, a "rule-local binding" means any
`DeclarationKind::RuleBinding` recorded by semantic analysis. Today that
includes:

- variables introduced by a rule head;
- variables introduced by an assignment pattern such as `var x = ...`; and
- variables introduced by a `for`-loop pattern.

Observable success is:

- `ddlint` exports a concrete `unused-variable` rule
  (`UnusedVariableRule`) under `src/linter/rules/correctness/`;
- registering that rule and running `Runner` emits warnings for unused
  head, assignment-pattern, and `for`-pattern bindings;
- `_` and any binding position that semantic analysis intentionally ignores do
  not produce warnings;
- later uses count only when semantic resolution maps them back to the exact
  binding, so unresolved names and shadowed later bindings do not create false
  negatives;
- unit tests cover the semantic query surface and the rule's positive and
  negative cases;
- behavioural tests prove end-to-end diagnostics through `Runner`, including a
  top-level `for` case that exercises parse-time `SemanticRule` analysis;
- `docs/ddlint-design.md` records the final rule contract and any design
  clarification made during implementation;
- `docs/roadmap.md` marks `4.1.2` done only after all quality gates pass; and
- `make check-fmt`, `make lint`, and `make test` all succeed at the end of the
  feature.

## Approval gate

This document is a draft plan only. Do not begin implementation until the user
explicitly approves this ExecPlan or requests revisions and then approves the
revised version.

## Context and orientation

The current repository already contains most of the semantic substrate this
rule needs:

- `src/sema/model.rs` stores owned `Symbol` and `UseSite` records plus
  `Resolution::{Resolved, Unresolved, Ignored}` outcomes.
- `src/sema/traverse.rs` records `DeclarationKind::RuleBinding` symbols for
  rule-head bindings, assignment patterns, and `for` patterns.
- `src/sema/resolve.rs` already treats `_` as `Resolution::Ignored`, and
  `collect_head_binding_names()` / `collect_pattern_binding_names()` already
  avoid declaring `_` as a normal binding symbol.
- `src/sema/variables.rs` records every variable-position use with
  `UseKind::Variable`.
- `src/linter/rules/correctness/unused_relation.rs` is the shipped example of
  a production correctness rule using `declare_lint!`.

Two details matter for this milestone.

First, semantic analysis already enforces rule-order visibility. Head bindings
are visible from the start of the rule, assignment bindings become visible
after their introducing literal, and `for` bindings live only inside the loop
body scope. That means `unused-variable` should trust semantic resolution
instead of re-implementing scope logic inside the rule.

Second, the semantic model currently stores rule-local binding and use spans at
the enclosing rule or literal span, not the exact identifier token span. This
is already an accepted limitation from roadmap item `3.3.1`. The first
`unused-variable` implementation should therefore use the existing stored span
provenance and document that limitation instead of expanding scope into token
recovery work.

The existing design wording is narrower than the roadmap.
`docs/ddlint-design.md` currently says `unused-variable` "Detects variables
bound in a rule head that are not used in the body", while roadmap item `4.1.2`
says "variables defined but not used within a rule" and depends on `3.3.3`,
which explicitly added assignment-pattern and `for`-pattern bindings. This plan
resolves that drift by recommending the broader binding-based rule semantics
and documenting the decision in the design doc.

## Constraints

- Treat `docs/roadmap.md` item `4.1.2`, `docs/ddlint-design.md` section `3.3`,
  and the semantic-model guarantees in `docs/parser-implementation-notes.md` as
  the design basis for this milestone.
- Keep scope limited to implementing `unused-variable`, the additive semantic
  query helpers it needs, its tests, and the required documentation and roadmap
  updates.
- Do not implement `shadowed-variable`, CLI rule discovery, configuration-file
  loading, `miette` rendering changes, or exact identifier-token span recovery
  in this milestone.
- Keep parser grammar and parse-stage diagnostics unchanged unless a genuine
  parser bug blocks the rule and there is no narrower fix.
- Extend the semantic model additively. Existing `RuleCtx`, `Runner`,
  `SemanticModel`, and parser APIs must remain source-compatible.
- Preserve the current owned-data and `Send + Sync` guarantees of
  `SemanticModel`; do not store `rowan` syntax handles inside semantic records.
- Use `declare_lint!` for the production rule unless a documented macro
  limitation makes that impossible.
- Every new Rust module must begin with a `//!` module comment.
- Keep Rust files below 400 lines by splitting rule modules, helpers, and
  tests as needed.
- Update `docs/ddlint-design.md` with the finalized rule semantics, including
  the `_` ignore policy and current span-granularity limitation if it affects
  the rule's user-visible behaviour.
- Mark `docs/roadmap.md` item `4.1.2` done only after all gates pass.
- Run quality gates through Make targets with `set -o pipefail` and `tee`.
- Use en-GB-oxendict spelling in comments and documentation.

## Tolerances (exception triggers)

- Scope: if implementation requires more than 11 changed files or more than
  700 net new lines, stop and re-evaluate the module split before continuing.
- Interface: if the rule cannot be implemented without a breaking change to
  `RuleCtx`, `Runner`, `SemanticModel`, or `declare_lint!`, stop and escalate.
- Semantics: if repository conventions or user feedback require a head-only
  rule instead of the broader rule-binding semantics described here, stop and
  get explicit confirmation before continuing.
- Provenance: if current semantic spans make the emitted diagnostics unusably
  vague and the only coherent fix is exact identifier-token span recovery, stop
  and escalate because that is a larger semantic-model milestone.
- Coverage: if a single CST-node target cannot cover both AST-backed rules and
  parse-time semantic rules cleanly, stop and document the alternatives before
  proceeding.
- Iterations: if targeted tests or Clippy fixes still fail after three focused
  rounds, stop and escalate with the exact failing test names or lint IDs.

## Risks

- Risk: the design doc currently describes a head-only rule, while the roadmap
  and semantic prerequisites support all rule-local bindings. Severity: high.
  Likelihood: high. Mitigation: record the broader contract explicitly in
  `docs/ddlint-design.md` during the same change that ships the rule.

- Risk: a rule that targets only `SyntaxKind::N_RULE` would miss bindings that
  exist only in parse-time `SemanticRule` values produced by top-level `for`
  desugaring. Severity: high. Likelihood: medium. Mitigation: run the rule once
  at the program node and query the full semantic model, or add an equally
  comprehensive alternative that covers both AST rules and semantic rules.

- Risk: the current semantic model records rule-local spans at the enclosing
  rule or literal. Severity: medium. Likelihood: high. Mitigation: document the
  limitation, keep the first implementation additive, and assert only on
  messages and rule names in behavioural tests unless the exact range is made
  precise as part of the milestone.

- Risk: counting any variable use by name would create false negatives in the
  presence of shadowing or unresolved names. Severity: high. Likelihood: high.
  Mitigation: base the rule on resolved `SymbolId` ownership, not string
  matching.

- Risk: `_` is already treated specially in semantic resolution, so a naïve
  rule that scans raw syntax might reintroduce warnings for intentionally
  ignored bindings. Severity: medium. Likelihood: medium. Mitigation: rely on
  semantic symbols and resolution outcomes instead of syntax-only heuristics.

## Surprises & Discoveries

- Observation: the semantic infrastructure already captures all three binding
  origins needed for this rule: rule head, assignment pattern, and `for`
  pattern. Impact: the remaining work is rule policy and query ergonomics, not
  a new semantic-analysis pass.

- Observation: `_` is handled in two places already. Resolution returns
  `Ignored` for uses named `_`, and binding collectors do not declare `_` as a
  normal symbol. Impact: the rule should preserve this contract rather than add
  a second wildcard filter in CST logic.

- Observation: the existing production rule (`unused-relation`) targets a
  declaration node kind, but `unused-variable` must also cover parse-time
  semantic rules that have no dedicated `N_RULE` node. Impact: the cleanest
  first implementation is likely a whole-program pass triggered once from
  `SyntaxKind::N_DATALOG_PROGRAM`.

- Observation: rule-local binding spans are currently coarse by design.
  Impact: the first release should ship correct diagnostics with coarse spans
  rather than silently broadening this milestone into token-span recovery.

- Observation: parse-time semantic rules from top-level `for` lowering still
  count variable uses that appear in the iterable expression. Impact: the
  behavioural top-level `for` coverage needs a genuinely unused loop binding
  such as `for (x in Source(_)) Output(0).`, not `for (x in Source(x)) ...`.

## Decision Log

- Decision: recommend that `unused-variable` cover every
  `DeclarationKind::RuleBinding`, not only head bindings. Rationale: this
  matches the roadmap wording ("defined but not used within a rule"), uses the
  semantic work delivered by `3.3.3`, and avoids inventing a narrower special
  case that later milestones would have to undo. Date/Author: 2026-03-27 /
  Codex.

- Decision: recommend implementing the rule as a single whole-program pass
  triggered from `SyntaxKind::N_DATALOG_PROGRAM`. Rationale: this covers both
  AST-backed rules and parse-time `SemanticRule` scopes from top-level `for`
  desugaring without requiring fragile span-to-rule-node mapping. Date/Author:
  2026-03-27 / Codex.

- Decision: recommend adding a semantic helper equivalent to
  `has_resolved_relation_read()` for rule-binding symbols. Rationale: the rule
  should read as policy code over semantic facts, not as repeated ad hoc scans
  over `uses()`. Date/Author: 2026-03-27 / Codex.

- Decision: recommend keeping the current span granularity for the first
  shipped rule. Rationale: exact identifier-token span recovery is a larger
  semantic-model enhancement than roadmap item `4.1.2` requires. Date/Author:
  2026-03-27 / Codex.

## Proposed design

Add a second production correctness rule alongside `unused-relation`:

- `src/linter/rules/correctness/unused_variable.rs`
- update `src/linter/rules/correctness/mod.rs`

The rule should be exported as `UnusedVariableRule` and defined with
`declare_lint!` using metadata roughly equivalent to:

```rust
name: "unused-variable"
group: "correctness"
level: warn
target_kinds: [SyntaxKind::N_DATALOG_PROGRAM]
```

The rule should run once per parsed program, iterate the semantic model's
rule-binding symbols in stable source order, and emit one diagnostic for each
binding whose `SymbolId` has no resolved variable uses.

Add semantic helpers that make that policy direct and testable. The likely
minimum API is:

```rust
SemanticModel::rule_binding_symbols()
SemanticModel::has_resolved_variable_use(symbol_id)
```

Implement these additively in `src/sema/model.rs`, with any precomputed index
stored in the builder if that keeps rule code simple and deterministic. A
precomputed `HashSet<SymbolId>` for bindings with resolved variable uses is the
closest analogue to the existing relation-read helper and is a reasonable
default unless a simpler helper proves clearer.

The rule must treat usage semantically:

1. A use counts only if it is `UseKind::Variable`.
2. A use counts only if its resolution is `Resolved(symbol_id)` for the exact
   binding being considered.
3. `Unresolved` uses do not count.
4. `Ignored` uses do not count.
5. Shadowed later bindings only satisfy the later binding they resolve to, not
   the earlier one with the same name.

The diagnostic message should be one stable sentence for all binding origins,
for example:

```plaintext
variable `x` is defined but never used in this rule
```

Use the binding symbol's stored span for the diagnostic range. Document in the
design notes that this currently points at the enclosing rule or literal span
for rule-local bindings, not the exact identifier token span.

## Implementation plan

### Milestone 1: add semantic query helpers for rule bindings

Update `src/sema/model.rs` and the semantic builder to expose the smallest
query surface `unused-variable` needs. Prefer a close parallel to the shipped
relation helpers so the rule stays easy to read.

At minimum, add unit coverage in `src/sema/tests.rs` for these cases:

- a head binding used in a later body literal counts as used;
- a head binding that appears only in the head is unused;
- an assignment-pattern binding used in a later literal counts as used;
- an assignment-pattern binding with no later resolved use is unused;
- a `for`-pattern binding used inside the loop body counts as used;
- a `for`-pattern binding that never appears inside the loop body is unused;
- a use outside the `for` body does not count for the loop binding; and
- wildcard positions do not create warning-eligible bindings.

Do not rely on name matching in tests. Assert on `SymbolId`-based resolution or
the semantic helper result so the tests would fail if later shadowing regressed.

### Milestone 2: implement the production rule module

Create `src/linter/rules/correctness/unused_variable.rs` with a module-level
`//!` comment and export it from `src/linter/rules/correctness/mod.rs`.

Implement the rule with `declare_lint!` and whole-program dispatch. In the rule
body:

1. Ignore any node that is not the one canonical `N_DATALOG_PROGRAM` root.
2. Iterate `ctx.semantic_model().rule_binding_symbols()`.
3. Skip any binding symbol that already has a resolved variable use.
4. Emit one `LintDiagnostic` with the binding symbol's span and stable message.

The implementation should not walk the CST to rediscover scopes or uses. All
scope, order, and ignore policy should come from the semantic model.

Add focused unit tests in the rule module covering positive and negative cases
for the shipped rule itself. Reuse the `unused-relation` structure where that
keeps the rule tests easy to read.

### Milestone 3: add behavioural tests through `Runner`

Add `tests/unused_variable_rule.rs` and extend `tests/support.rs` with either
`run_unused_variable_rule()` or a small generic helper shared by both shipped
rules if that stays smaller and clearer than duplicating runner setup.

The behavioural suite should include at least:

1. A head-only binding that warns.
2. A head binding used in the body that does not warn.
3. An unused assignment-pattern binding that warns.
4. A used assignment-pattern binding that does not warn.
5. An unused `for`-pattern binding that warns.
6. A used `for`-pattern binding that does not warn.
7. Wildcard binding positions (`_`) that do not warn.
8. A top-level `for` example that proves the rule also covers parse-time
   semantic rules.
9. A shadowing-shaped example where only the genuinely unused binding warns.

Assert on diagnostic messages and rule names. Only assert exact ranges if the
implementation deliberately tightens span provenance as part of this milestone.

### Milestone 4: update design docs and roadmap

Update `docs/ddlint-design.md` section `3.3` so the catalogue entry matches the
shipped semantics:

- the rule covers all rule-local bindings, not only head bindings;
- `_` is an explicit ignore; and
- the current implementation relies on semantic resolution and existing
  rule-local span provenance.

After the code and tests pass, mark roadmap item `4.1.2` done in
`docs/roadmap.md`.

## Validation plan

Start by writing or updating the targeted tests so the intended failures are
visible before the rule is implemented. Then validate the completed change with
the repository's required gates. Run every command with `set -o pipefail` and
`tee` so truncated terminal output does not hide failures.

Recommended command sequence:

```bash
set -o pipefail; make fmt 2>&1 | tee /tmp/4-1-2-make-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/4-1-2-make-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/4-1-2-make-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/4-1-2-make-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/4-1-2-make-lint.log
set -o pipefail; make test 2>&1 | tee /tmp/4-1-2-make-test.log
```

Expected outcome:

- the new semantic helper tests pass;
- the new rule unit tests pass;
- `tests/unused_variable_rule.rs` passes end to end;
- documentation formatting and linting pass;
- `docs/roadmap.md` shows `4.1.2` as done; and
- the full workspace gates succeed.

Known repository note: if `make test` stalls in the default nextest profile for
reasons unrelated to this change, record that fact in `Surprises & Discoveries`
and verify with `CI=1 make test` while continuing to treat the normal
`make test` gate as the required end-state.

## Progress

- [x] (2026-03-27T00:00Z) Reviewed roadmap item `4.1.2`, the `execplans`
  skill, project memory notes, the semantic model, the shipped
  `unused-relation` rule, and neighbouring ExecPlans.
- [x] (2026-03-27T00:00Z) Drafted this ExecPlan in
  `docs/execplans/4-1-2-implement-unused-variable-diagnostics.md`.
- [x] (2026-03-30T00:00Z) Received user approval to proceed with
  implementation.
- [x] (2026-03-30T00:00Z) Added semantic helper additions for rule-binding
  usage queries.
- [x] (2026-03-30T00:00Z) Implemented `UnusedVariableRule`, exported it, and
  wired it through the shipped correctness-rule surface.
- [x] (2026-03-30T00:00Z) Added semantic helper tests, rule unit tests, and
  behavioural runner coverage, including a top-level `for` semantic-rule case.
- [x] (2026-03-30T00:00Z) Updated `docs/ddlint-design.md` and
  `docs/roadmap.md` to match the shipped rule contract.
- [x] (2026-03-30T00:00Z) Ran `make fmt`, `make markdownlint`, `make nixie`,
  `make check-fmt`, `make lint`, and `make test`.

## Outcomes & Retrospective

- Shipped `UnusedVariableRule` under
  `src/linter/rules/correctness/unused_variable.rs` with whole-program dispatch
  from `SyntaxKind::N_DATALOG_PROGRAM`.
- Added `SemanticModel::rule_binding_symbols()` and
  `SemanticModel::has_resolved_variable_use()` backed by a precomputed set of
  resolved variable-use symbol IDs in the semantic builder.
- The broader rule-binding semantics remained intact: warnings now cover rule
  heads, assignment patterns, and `for`-pattern bindings, while `_` remains an
  explicit ignore.
- No deviation from the planned query surface was required beyond a small
  internal helper that converts stored `Span` values back into `TextRange`
  diagnostics.
- Behavioural coverage now proves end-to-end warnings for unused head,
  assignment, `for`-loop, and top-level `for` semantic-rule bindings, plus the
  shadowing-shaped case where only the genuinely unused binding warns.
- Final validation results:
  - `set -o pipefail; make fmt 2>&1 | tee /tmp/4-1-2-make-fmt.log`
  - `set -o pipefail; make markdownlint 2>&1 | tee /tmp/4-1-2-make-markdownlint.log`
  - `set -o pipefail; make nixie 2>&1 | tee /tmp/4-1-2-make-nixie.log`
  - `set -o pipefail; make check-fmt 2>&1 | tee /tmp/4-1-2-make-check-fmt.log`
  - `set -o pipefail; make lint 2>&1 | tee /tmp/4-1-2-make-lint.log`
  - `set -o pipefail; make test 2>&1 | tee /tmp/4-1-2-make-test.log`
- Follow-up work remains the same as planned: if later milestones need more
  precise ranges, exact identifier-token span recovery should happen as a
  separate semantic-model improvement rather than inside this rule.
