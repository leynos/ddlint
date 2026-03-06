# Implement top-level `for` desugaring into rules

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: COMPLETE

## Purpose / big picture

Roadmap item `2.5.4` is currently open because the syntax spec says top-level
`for` statements desugar into rules, while the parser has no top-level `K_FOR`
entry path and therefore no desugaring behaviour. After this change, parsing a
programme that contains top-level `for` statements will produce a deterministic
set of desugared semantic rules, with explicit diagnostics for unsupported
top-level statement forms. Success is observable through new unit and
behavioural tests, spec/conformance alignment, and green quality gates.

## Constraints

- Preserve lossless concrete syntax tree (CST) behaviour: `Root::text()` must
  still round-trip to the original source text.
- Keep existing explicit rule parsing behaviour unchanged for `N_RULE` nodes and
  `Root::rules()`.
- Implement top-level `for` desugaring as additive functionality exposed through
  a dedicated semantic API, not by injecting synthetic tokens into the CST.
- Reuse existing parser infrastructure (`tokenizer`, span scanners, expression
  parser) and avoid new dependencies.
- Keep modules under 400 lines by splitting helpers into focused files.
- Add unit tests and behavioural tests that fail before implementation and pass
  after implementation.
- Update the normative/design docs when decisions are finalized.

## Tolerances (exception triggers)

- Scope: if implementation needs changes in more than 14 files or more than 700
  net lines, stop and escalate.
- Interface stability: if this work requires changing the signature of
  `Root::rules()` or breaking existing public API contracts, stop and escalate.
- Semantics ambiguity: if the intended `convertStatement` contract cannot be
  derived from repo docs and materially different behaviours are plausible,
  stop and present options.
- Diagnostics parity: if deterministic diagnostics for unsupported top-level
  statement forms cannot be stabilized after two iterations, stop and escalate.
- Validation: if `make test` still fails after two fix attempts, stop and
  escalate.

## Risks

- Risk: `convertStatement` details are underspecified in current local docs.
  Severity: high Likelihood: medium Mitigation: lock a concrete conversion
  contract in `docs/differential-datalog-parser-syntax-spec-updated.md` §6.5
  before writing implementation logic.

- Risk: desugaring could violate CST losslessness if implemented in tree
  construction. Severity: high Likelihood: low Mitigation: keep CST unchanged
  and expose desugared rules via a semantic layer attached to `Parsed`.

- Risk: top-level scanner can misclassify `for` tokens that belong to rule
  bodies or other spans. Severity: medium Likelihood: medium Mitigation: reuse
  exclusion span logic and line-start checks; add scanner unit tests for
  exclusion boundaries.

- Risk: nested control-flow desugaring can introduce rule-order instability.
  Severity: medium Likelihood: medium Mitigation: define and test deterministic
  left-to-right traversal and emitted rule order.

## Progress

- [x] (2026-03-03 00:00Z) Drafted ExecPlan structure and gathered spec, roadmap,
  and parser implementation context.
- [x] Finalize and document top-level `for` conversion contract and unsupported
  statement diagnostics.
- [x] Add failing unit and behavioural tests for top-level `for` desugaring.
- [x] Implement scanner and desugaring pipeline with additive semantic API.
- [x] Update spec/conformance/implementation docs and mark roadmap item `2.5.4`
  done.
- [x] Run validation gates (`make check-fmt`, `make lint`, `make test`).

## Surprises & Discoveries

- `docs/parser-conformance-register.md` item 8 currently links to roadmap item
  `2.6.1`, while the requested implementation target is roadmap item `2.5.4`.
  This cross-reference must be reconciled when closing the feature.
- `collect_rule_spans` only starts candidate parsing at `T_IDENT`, `T_IMPLIES`,
  and `T_AMP`; there is no top-level `K_FOR` rule entry path today.
- Existing `Rule` APIs already flatten rule-body `Expr::ForLoop`, which can
  inform top-level desugaring ordering semantics.

## Decision Log

- Decision: implement actual top-level `for` desugaring (not unsupported-mode
  rejection) to satisfy the explicit `2.5.4` requirement. Rationale: roadmap
  request and syntax spec §6.5 both require desugaring. Date/Author: 2026-03-03
  / assistant

- Decision: keep CST lossless and expose top-level desugared output through an
  additive semantic-rules API. Rationale: synthetic CST rules would break
  round-trip text invariants and blur source-vs-derived node semantics.
  Date/Author: 2026-03-03 / assistant

## Outcomes & Retrospective

Implemented top-level `for` desugaring via `Parsed::semantic_rules()` while
keeping CST `Root::rules()` unchanged and lossless. The previous 2.6.1
unsupported-mode scanner rejection path was removed, tests were rewritten
around semantic desugaring behaviour, and spec/conformance/implementation docs
were aligned with the new contract.

## Context and orientation

Current parser flow is:

1. `parse()` in `src/parser/mod.rs` tokenizes source and calls
   `parse_tokens()` (`src/parser/span_scanner.rs`) to collect top-level spans.
2. `build_green_tree()` (`src/parser/cst_builder/tree.rs`) builds a lossless CST
   from source tokens and span lists.
3. `Root` (`src/parser/ast/root.rs`) exposes typed CST-backed top-level wrappers
   such as imports, relations, and explicit rules.

Top-level `for` is currently missing from this pipeline:

- Spec says top-level `for` rewrites to one or more rules:
  `docs/differential-datalog-parser-syntax-spec-updated.md` §6.5.
- Conformance register item 8 marks this as scheduled:
  `docs/parser-conformance-register.md`.
- Scanner implementation has no top-level `K_FOR` collector:
  `src/parser/span_scanners/rules.rs`.

Testing layout:

- Parser unit tests live under `src/parser/tests/`.
- Behavioural parser tests live under `tests/`.
- Existing for-loop tests are rule-body-centric in `tests/rule_behaviour.rs`.

## Plan of work

Stage A: freeze the conversion contract in docs before coding.

Define exactly which top-level statement forms inside `for` are lowered and how
they map to emitted semantic rules. Document deterministic ordering, handling
of nested `for`, handling of guarded `for`, and diagnostics for unsupported
forms. Do this first in spec and conformance docs so implementation has one
source of truth.

Stage B: add red tests that encode expected behaviour.

Add unit tests for span detection and desugaring logic, and behavioural tests
for end-to-end parser output. Confirm these tests fail before implementation to
prove they are meaningful.

Stage C: implement top-level `for` collection and lowering.

Add a dedicated lowering module that:

- identifies top-level `for` statement spans without conflicting with existing
  declaration/rule spans,
- parses each statement into control-flow expression form,
- recursively lowers to semantic rules with stable ordering, and
- emits span-precise diagnostics for unsupported top-level statement forms.

Keep this as an additive semantic layer and do not mutate CST node assembly.

Stage D: integrate surfaced output and docs.

Expose desugared semantic rules through `Parsed`, wire errors into the existing
parse error stream, update implementation notes/spec/conformance docs, and then
mark roadmap item `2.5.4` done.

Stage E: validation and cleanup.

Run formatting/lint/test gates, confirm green logs, and ensure no unrelated
behaviour regressed.

## Concrete steps

- Step 1: Define and lock desugaring contract.

- Update `docs/differential-datalog-parser-syntax-spec-updated.md` §6.5 with an
  explicit conversion algorithm description and unsupported-form diagnostics.
- Update `docs/parser-conformance-register.md` item 8 wording to reference this
  implementation target and planned API surface.

- Step 2: Add failing tests first.

- Add parser unit tests in `src/parser/tests/` (new
  `src/parser/tests/top_level_for.rs`) covering:
  - simple top-level `for` lowering to one rule,
  - guarded top-level `for`,
  - nested top-level `for`,
  - mixed explicit rule plus top-level `for` ordering,
  - unsupported top-level statement forms diagnostic.
- Add behavioural tests in `tests/top_level_for_desugaring.rs` validating
  end-to-end desugared rule outputs and diagnostics.

- Step 3: Implement scanning and lowering modules.

- Add `src/parser/top_level_for.rs` for:
  - collecting top-level `for` statement spans using token stream and exclusion
    spans from already collected declarations/rules,
  - parsing statement text into expressions,
  - lowering statements to semantic rule records.
- Add semantic rule types in `src/parser/ast/semantic_rule.rs` (or equivalent)
  and re-export from `src/parser/ast/mod.rs`.

- Step 4: Integrate with parser entrypoint.

- Update `src/parser/mod.rs` and `src/parser/cst_builder/mod.rs` so `Parsed`
  stores desugared semantic rules and exposes them via a new accessor (for
  example `Parsed::semantic_rules()`).
- Ensure lowering diagnostics are appended to `Parsed::errors()` with stable
  spans/messages.

- Step 5: Documentation and roadmap updates.

- Update `docs/parser-implementation-notes.md` control-flow section with the new
  top-level lowering pipeline and API.
- Mark `docs/roadmap.md` item `2.5.4` as done once all quality gates pass.
- Reconcile roadmap linkage for conformance register item 8.

- Step 6: Validation commands (run from repository root).

```bash
set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-5-4-markdownlint.log
set -o pipefail; make fmt 2>&1 | tee /tmp/2-5-4-fmt.log
set -o pipefail; make nixie 2>&1 | tee /tmp/2-5-4-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-5-4-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/2-5-4-lint.log
set -o pipefail; make test 2>&1 | tee /tmp/2-5-4-test.log
```

Expected success signal:

```plaintext
All listed commands exit with status 0 and logs contain no failing tests,
warnings promoted to errors, or markdown validation failures.
```

## Validation and acceptance

Acceptance criteria:

- Parsing a source with top-level `for` statements yields deterministic semantic
  rules that match the conversion contract documented in spec §6.5.
- Unsupported top-level statement forms inside desugaring emit clear,
  span-precise diagnostics and parser recovery continues.
- Existing explicit rule parsing (`Root::rules()`) remains unchanged and still
  passes existing behavioural tests.
- New unit tests and behavioural tests for top-level desugaring pass.
- `make check-fmt`, `make lint`, and `make test` succeed.
- `docs/roadmap.md` item `2.5.4` is marked done only after all gates pass.

## Idempotence and recovery

All edits are additive and safe to re-run. If desugaring integration breaks
existing parser behaviour:

1. run the new top-level-for tests alone to isolate failures,
2. temporarily gate semantic API wiring while preserving scanner/lowering unit
   tests, and
3. re-enable integration once explicit-rule behavioural parity is restored.

If a validation command fails, fix the failure and re-run that command, then
re-run the full required validation sequence.

## Artifacts and notes

Capture the following in PR notes or local logs:

- before/after test output for the new top-level `for` cases,
- one example source and its emitted semantic rule list,
- one unsupported-form diagnostic example with span evidence.

Keep snippets short and focused on proving behavioural change.

## Interfaces and dependencies

Target additive interfaces (names may vary if final docs standardize
alternatives):

```rust
pub struct SemanticRule {
    pub head: Expr,
    pub body: Vec<Expr>,
}

impl Parsed {
    pub fn semantic_rules(&self) -> &[SemanticRule];
}
```

Internal lowering entrypoint (crate-private):

```rust
pub(crate) fn lower_top_level_for_rules(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    exclusions: &[Span],
) -> (Vec<SemanticRule>, Vec<Simple<SyntaxKind>>);
```

No new external dependencies are required.

## Revision note

Initial draft created for roadmap item `2.5.4`. This revision establishes the
implementation direction (real desugaring with additive semantic API), captures
constraints and tolerances, and defines concrete test and validation gates for
delivery.
