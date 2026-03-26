# Implement `unused-relation` diagnostics

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: Implemented

## Purpose / big picture

Roadmap item `4.1.1` is the first production lint rule in the initial
correctness catalogue. After this change, `ddlint` exports an `unused-relation`
lint rule (`UnusedRelationRule`) that callers must explicitly register in a
`CstRuleStore` before running the `Runner`. Once registered, the rule emits an
`unused-relation` warning for each declared relation that has no resolved
read-like uses anywhere in the analysed program.

For this milestone, "read from" means a resolved relation-position use in a
rule body, a `for` iterable, or a `for` guard. A relation named in a rule head
is a write site, not a read site, so head-only relations must still be warned
about. Observable success is:

- `ddlint` exports a concrete `unused-relation` rule (`UnusedRelationRule`)
  that callers register in `CstRuleStore`; the rule is not enabled by default.
- Running `Runner` with that rule registered emits one warning per unread
  relation declaration and no warnings for relations with at least one resolved
  read.
- The semantic model exposes enough provenance to distinguish relation reads
  from writes without re-walking the concrete syntax tree inside the rule.
- Unit tests cover semantic read-versus-write classification and the rule's
  positive and negative cases.
- Behavioural tests prove end-to-end diagnostics through `Runner`.
- `docs/ddlint-design.md` records the final rule contract and the semantic-use
  provenance added for it.
- `docs/roadmap.md` marks `4.1.1` done only after all quality gates pass.

## Approval gate

Approved by the user on 2026-03-22 when they requested implementation of this
ExecPlan.

## Context and orientation

The current repository already contains the semantic substrate this rule will
build on:

- `src/sema/build.rs` builds one `SemanticModel` from a parsed program.
- `src/sema/builder.rs` records top-level relation, function, and type
  declarations.
- `src/sema/traverse.rs` records relation and variable uses while walking rule
  heads and rule bodies.
- `src/sema/model.rs` stores `Symbol` and `UseSite` records, but today a
  `UseSite` does not record whether it came from a rule head or a read-like
  body position.
- `src/linter/rule.rs`, `src/linter/store.rs`, and `src/linter/runner.rs`
  provide the rule trait surface, dispatch store, and parallel runner.
- `src/linter/macros.rs` provides `declare_lint!`, which should be used for
  the new rule unless a documented blocker appears.

There is no shipped rule-catalogue module yet. Current behavioural tests
register ad hoc rules directly in `CstRuleStore`. This milestone therefore
needs to add the first exported production rule module and corresponding tests,
but it does not need to invent a full Command-Line Interface (CLI)-configured
default ruleset.

The key design gap is semantic provenance. `docs/ddlint-design.md` says
`unused-relation` detects relations "defined but never read from", yet the
current semantic layer records relation uses from both rule heads and rule
bodies with the same `UseKind::Relation`. Counting all relation-position uses
would therefore under-report unused relations by treating writes as reads.

## Constraints

- Treat `docs/ddlint-design.md` section `3.2.1` and section `3.3` plus
  `docs/roadmap.md` item `4.1.1` as the normative design basis for this
  milestone.
- Keep scope limited to implementing `unused-relation`, the additive semantic
  provenance needed for it, its tests, and the required documentation and
  roadmap updates.
- Do not implement `unused-variable`, `shadowed-variable`, command-line
  interface (CLI) rule listing, configuration-file loading, or rich `miette`
  conversion in this milestone.
- Keep parser grammar and parse-stage diagnostics unchanged unless a bug blocks
  the rule and there is no narrower fix.
- Extend the semantic model additively. Existing `RuleCtx`, `Runner`, and
  parser APIs must remain source-compatible.
- Preserve the current owned-data and `Send + Sync` guarantees of
  `SemanticModel`; do not store `rowan` nodes inside semantic records.
- Use `declare_lint!` for the production rule unless a documented limitation
  of the macro makes that impossible.
- Every new Rust module must begin with a `//!` module comment.
- Keep files below 400 lines by splitting rule modules, helpers, and tests as
  needed.
- Update `docs/ddlint-design.md` with any rule-semantics decision taken during
  implementation.
- Mark `docs/roadmap.md` item `4.1.1` done only after all gates pass.
- Run quality gates through Make targets, using `set -o pipefail` and `tee`.

## Tolerances (exception triggers)

- Scope: if implementation requires more than 10 changed files or more than
  650 net new lines, stop and re-evaluate the module split before continuing.
- Interface: if the rule cannot be implemented without a breaking change to
  `RuleCtx`, `Runner`, `SemanticModel`, or `declare_lint!`, stop and escalate.
- Semantics: if the team decides that `output relation` declarations should be
  exempt because of external consumers, stop and get explicit confirmation
  before encoding that policy.
- Provenance: if read-versus-write classification cannot be represented
  additively in `UseSite`, stop and redesign the semantic query surface before
  proceeding.
- Iterations: if targeted tests or Clippy fixes still fail after three focused
  rounds, stop and escalate with the exact failing test names or lint IDs.

## Risks

- Risk: head relation atoms are currently recorded as generic relation uses, so
  a naïve implementation would treat writes as reads and miss real unused
  relations. Severity: high. Likelihood: high. Mitigation: add explicit use
  provenance and write failing tests before adding the rule.

- Risk: the roadmap wording says "declared relations with no usage sites",
  while the design catalogue says "never read from". Severity: medium.
  Likelihood: medium. Mitigation: document and implement the narrower
  read-based rule semantics, because that is the more precise statement and
  avoids counting rule-head writes as reads.

- Risk: `output relation` declarations may represent externally consumed data,
  but the current design documents do not define an exemption. Severity:
  medium. Likelihood: medium. Mitigation: keep the milestone scoped to internal
  program analysis and document that no external-consumer exemption is applied
  unless the user approves a policy change.

- Risk: the current runner API has only per-node and per-token hooks, so a
  rule that repeatedly scans the full semantic model could become noisy or
  awkward to maintain. Severity: low. Likelihood: medium. Mitigation: add small
  semantic query helpers that make the rule implementation direct and testable,
  but avoid introducing a broader precomputed lint-cache layer.

## Decision log

- Decision: treat rule-head relation atoms as write sites, not read sites, for
  `unused-relation`. Rationale: `docs/ddlint-design.md` defines the rule as
  "defined but never read from", and counting heads as reads would make sink
  relations appear used when they are only written. Date/Author: 2026-03-21 /
  Codex.

- Decision: do not add a special exemption for `output relation` declarations
  in this milestone. Rationale: no current design document defines such an
  exemption, and roadmap item `4.1.1` is phrased in terms of declarations plus
  internal usage sites. Date/Author: 2026-03-21 / Codex.

- Decision: expose the first production rule as a normal exported rule type
  that tests register explicitly in `CstRuleStore`, rather than inventing a
  global default ruleset now. Rationale: the current repository has no shipped
  rule-catalogue registration surface, and adding one would broaden scope
  beyond `4.1.1`. Date/Author: 2026-03-21 / Codex.

## Proposed design

Add a small production-rules namespace under `src/linter/` so the rule has a
stable home and future correctness rules can follow the same pattern. A minimal
layout is:

- `src/linter/rules/mod.rs`
- `src/linter/rules/correctness/mod.rs`
- `src/linter/rules/correctness/unused_relation.rs`

Export the new module from `src/linter/mod.rs` so integration tests and later
CLI wiring can import the rule cleanly.

Extend the semantic model with additive relation-use provenance. The shipped
implementation uses a `UseOrigin` enum stored on every `UseSite` with the
following variants:

- `UseOrigin::RelationHead` — relation use from a rule head (write site);
- `UseOrigin::RelationBody` — relation use from a rule-body atom or
  semantic-rule body atom (read site);
- `UseOrigin::ForIterable` — relation use from a `for` iterable expression
  (read site);
- `UseOrigin::ForGuard` — relation use from a `for` guard expression (read
  site); and
- `UseOrigin::Variable` — variable use recorded while traversing expressions.

The `UseOrigin::is_relation_read()` method returns `true` for `RelationBody`,
`ForIterable`, and `ForGuard`, allowing the rule to answer one clear question
without inspecting syntax nodes: "does this relation declaration have any
resolved read-like uses?"

Add semantic helper methods that keep rule code simple. The final names may
change, but the rule should be able to call helpers equivalent to:

```rust
model.relation_symbols()
model.relation_reads()
model.has_resolved_relation_read(symbol_id)
```

If lookup by span is needed to associate an `N_RELATION_DECL` node with its
relation symbol, prefer a small helper such as
`SemanticModel::relation_symbol_at_span(span)` over ad hoc filtering inside the
rule.

Implement the rule itself with `declare_lint!`. It should target
`SyntaxKind::N_RELATION_DECL`, use metadata `name: "unused-relation"`,
`group: "correctness"`, and `level: warn`, then emit one diagnostic per unread
relation declaration with a message equivalent to:

```plaintext
relation `Foo` is declared but never read from
```

The diagnostic span should be the declaration node range, which already matches
the recorded relation declaration span in the semantic model.

The rule must ignore malformed declarations that do not map cleanly to a named
relation symbol. Silent non-emission is preferable to a panic when semantic
facts are incomplete because of parse recovery.

## Implementation plan

### Milestone 1: add semantic use provenance

Update `src/sema/model.rs`, `src/sema/traverse.rs`, and any supporting helpers
so relation uses carry enough provenance to distinguish reads from writes. Keep
the existing `UseKind::Relation` and `UseKind::Variable` split; this milestone
only needs extra origin metadata, not a new top-level use-kind taxonomy.

Adjust semantic-model unit tests in `src/sema/tests.rs` and behavioural tests
in `tests/semantic_scope_resolution.rs` so they assert the new provenance
contract. At minimum, add coverage proving:

- a relation in a rule head is recorded as a write;
- the same relation name in a rule body is recorded as a read; and
- top-level `for` semantic rules preserve the same distinction.

### Milestone 2: add semantic query helpers for the rule

Add focused helper methods on `SemanticModel` that answer the questions
`unused-relation` actually needs. Keep them additive and deterministic. The
goal is that the rule module reads as straightforward policy code rather than a
manually repeated scan over `symbols()` and `uses()`.

Write unit tests for those helpers near the semantic-model tests. Include at
least these cases:

- a relation with one resolved body read returns true for "has resolved read";
- a relation mentioned only in rule heads returns false; and
- an unresolved relation-position use does not count as a read.

### Milestone 3: implement the production rule module

Create the production rule module under `src/linter/rules/correctness/` and
export it through `src/linter/mod.rs`. Use `declare_lint!` for metadata and
dispatch boilerplate.

In the rule body:

1. Read the declaration node's span.
2. Resolve that span to the corresponding relation symbol via a semantic-model
   helper.
3. Ask whether the symbol has any resolved read-like uses.
4. Emit a warning diagnostic only when the answer is no.

Keep helper functions in the same rule module if they are specific to this
rule. If they become reusable across multiple correctness rules, move them to a
small sibling helper module only after the first rule works and the need is
real.

### Milestone 4: add rule-focused tests

Add unit tests close to the rule module and behavioural tests under `tests/`
that run the real parser and runner. Use `rstest` fixtures and parameterized
cases where they reduce repetition.

The minimum coverage set is:

- positive: a declared relation with no reads emits exactly one
  `unused-relation` diagnostic;
- negative: a relation read in a rule body emits no diagnostic;
- head-only write: a relation that appears only in rule heads still emits a
  diagnostic;
- unresolved name safety: an unresolved relation-position use does not mark a
  declaration as used;
- multi-relation ordering: diagnostics are deterministic when several unused
  relations exist.

Use a dedicated behavioural test file, for example
`tests/unused_relation_rule.rs`, rather than burying these cases inside the
generic runner tests.

### Milestone 5: update docs and roadmap

Update `docs/ddlint-design.md` in the semantic-model section and the initial
lint catalogue section so the implemented read-versus-write distinction is
explicit. If the semantic-model contract section already describes relation
uses too loosely, tighten that wording there rather than spreading the rule
semantics across multiple unrelated docs.

If semantic provenance becomes a durable invariant rather than a rule-specific
detail, add a short note to `docs/parser-implementation-notes.md` explaining
that relation use sites now record whether they are reads or writes. This is
the right place for implementation-level semantic invariants that future rules
will rely on.

After all tests and gates pass, change `docs/roadmap.md` item `4.1.1` from
unchecked to done.

## Validation plan

Start with targeted tests that go red before the implementation is complete,
then finish with the full repository gates.

Suggested targeted commands during development:

```bash
set -o pipefail; cargo test sema::tests 2>&1 | tee /tmp/4-1-1-sema-unit.log
set -o pipefail; cargo test --test semantic_scope_resolution 2>&1 | tee /tmp/4-1-1-sema-behaviour.log
set -o pipefail; cargo test unused_relation 2>&1 | tee /tmp/4-1-1-unused-relation-targeted.log
```

Required final commands:

```bash
set -o pipefail; make fmt 2>&1 | tee /tmp/4-1-1-make-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/4-1-1-make-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/4-1-1-make-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/4-1-1-make-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/4-1-1-make-lint.log
set -o pipefail; make test 2>&1 | tee /tmp/4-1-1-make-test.log
```

Acceptance evidence for the rule should include at least one behavioural test
with source equivalent to:

```plaintext
input relation Source(x: u32)
relation Sink(x: u32)
Sink(x) :- Source(x).
```

Expected outcome: `Source` is not warned because it is read in the body; `Sink`
is warned because it is only written in the head.

## Progress

- [x] (2026-03-21 00:00Z) Reviewed roadmap item `4.1.1`, the `execplans`
  skill, current semantic-analysis code, and adjacent ExecPlans.
- [x] (2026-03-21 00:10Z) Identified the key semantic gap: relation uses do
  not yet distinguish reads from writes.
- [x] (2026-03-21 00:20Z) Wrote this draft ExecPlan to
  `docs/execplans/4-1-1-implement-unused-relation-diagnostics.md`.
- [x] (2026-03-22 00:05Z) User approved implementation by requesting execution
  of this plan.
- [x] (2026-03-22 00:40Z) Implemented semantic use-site provenance,
  relation-read query helpers, the exported `unused-relation` rule module, and
  unit plus behavioural coverage.
- [x] (2026-03-22 00:50Z) Updated `docs/ddlint-design.md`,
  `docs/parser-implementation-notes.md`, and `docs/roadmap.md` to reflect the
  shipped contract.
- [x] (2026-03-22 01:25Z) Ran `make fmt`, `make markdownlint`, `make nixie`,
  `make check-fmt`, `make lint`, and `CI=1 make test`; all passed.

## Surprises & discoveries

- Observation: roadmap prerequisites `3.3.2` and `3.3.4` are already complete,
  and the semantic model does record relation declarations and relation uses.
  Impact: the missing work is rule-policy plumbing and provenance, not parser
  or symbol-table construction.

- Observation: `src/sema/traverse.rs` currently records relation uses from rule
  heads via `collect_head_expr`, so the semantic model does not yet encode the
  "read from" language used by the rule catalogue. Impact: `unused-relation`
  cannot be implemented correctly without extending `UseSite`.

- Observation: the current linter module exports engine primitives only; there
  is no production rule namespace yet. Impact: this milestone should introduce
  a small `src/linter/rules/` module tree, but should not broaden into a full
  default ruleset or CLI registry.

- Observation: early experiments with top-level `for` desugaring recorded
  iterable relation reads as semantic-rule body reads rather than a dedicated
  `ForIterable` origin. The shipped contract treats those uses as read-like,
  and tests should assert read-versus-write semantics per the durable rule
  contract rather than overfitting to any particular desugaring detail.
  Historical note: the `ForIterable` origin variant was introduced to
  distinguish iterable positions from rule-body positions.

## Outcomes & retrospective

- Final rule modules:
  `src/linter/rules/mod.rs`, `src/linter/rules/correctness/mod.rs`, and
  `src/linter/rules/correctness/unused_relation.rs`.
- Shipped semantic provenance shape: `UseSite` now carries `UseOrigin` with
  `RelationHead`, `RelationBody`, `ForIterable`, `ForGuard`, and `Variable`.
  `SemanticModel` now exposes `relation_symbols()`,
  `relation_symbol_at_span()`, and `has_resolved_relation_read()`.
- Test coverage:
  `src/sema/tests.rs` covers relation use origins and helper queries;
  `tests/semantic_scope_resolution.rs` covers end-to-end semantic provenance;
  `src/linter/rules/correctness/unused_relation.rs` contains focused rule unit
  tests; and `tests/unused_relation_rule.rs` covers end-to-end diagnostics and
  deterministic ordering through `Runner`.
- Documentation updates:
  `docs/ddlint-design.md` now documents relation read-versus-write provenance
  and the exact `unused-relation` contract;
  `docs/parser-implementation-notes.md` records relation-use provenance as a
  current semantic invariant; and `docs/roadmap.md` marks item `4.1.1` done.
- Passed gate commands:

  ```bash
  set -o pipefail; make fmt 2>&1 | tee /tmp/4-1-1-final-make-fmt.log
  set -o pipefail; make markdownlint 2>&1 | tee /tmp/4-1-1-make-markdownlint.log
  set -o pipefail; make nixie 2>&1 | tee /tmp/4-1-1-make-nixie.log
  set -o pipefail; make check-fmt 2>&1 | tee /tmp/4-1-1-final-check-fmt.log
  set -o pipefail; make lint 2>&1 | tee /tmp/4-1-1-final-lint.log
  set -o pipefail; CI=1 make test 2>&1 | tee /tmp/4-1-1-final-test.log
  ```
