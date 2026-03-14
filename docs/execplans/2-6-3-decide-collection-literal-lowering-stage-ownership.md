# Decide collection literal lowering stage ownership

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: DRAFT

## Purpose / big picture

Roadmap item `2.6.3` is open because the repository still describes collection
literal lowering in two different ways. The parser already preserves vector and
map literals as raw expression nodes (`Expr::VecLit` and `Expr::MapLit`), and
roadmap item `2.4.4` explicitly says builder-call desugaring is deferred.
However, conformance register item 10 still says the target behaviour is "early
lowering", and the `Expr` doc comments in `src/parser/ast/expr.rs` still
describe immediate desugaring to `vec_with_capacity` and `map_empty` builder
sequences.

After this change, a novice should be able to read the parser docs, call
`parse()` or `parse_expression()`, and know exactly what stage owns collection
literal lowering. Success is observable when the docs agree on one contract,
unit and behavioural tests prove that raw collection literals survive parsing,
all required quality gates pass, and `docs/roadmap.md` item `2.6.3` is marked
done only after those checks succeed.

## Recommended decision

Close item `2.6.3` by standardizing collection literal lowering as a later
lowering responsibility, not a parser responsibility and not part of the
current rule-body semantic helper layer.

Concretely:

- `parse_expression()` continues to produce `Expr::VecLit` and `Expr::MapLit`.
- `parse()` continues to preserve those raw expression forms inside the
  Concrete Syntax Tree (CST)-backed Abstract Syntax Tree (AST) views.
- `Parsed::semantic_rules()` remains limited to top-level `for` desugaring.
- No new collection-literal rewrite helper is introduced in the current parser
  generation.
- Future lowering, if and when it is implemented, should live in the later
  semantic/lowering layer that ADR-001 is preparing for, not in the syntax
  crate boundary.

This is the smallest decision that matches repository reality, preserves the
lossless parser contract, and avoids inventing a second partial semantic stage
for an expression feature that can appear anywhere expressions are allowed.

If implementation uncovers an existing consumer that already requires
builder-sequence lowering during parsing, stop and escalate. That would be a
larger architectural change than the conformance-alignment task captured here.

## Constraints

- Preserve lossless CST behaviour. `Root::text()` and CST-backed wrappers must
  continue to reflect the original source text exactly.
- Do not remove or rename `Expr::VecLit` or `Expr::MapLit`.
- Do not add synthetic builder-sequence nodes, hidden rewrites, or parse-time
  collection lowering to `parse()` or `parse_expression()`.
- Do not widen `Parsed::semantic_rules()` beyond top-level `for` desugaring in
  this task.
- Treat roadmap item `2.4.4` as an existing contract: raw collection literals
  are already part of the parser surface.
- Keep changes narrowly scoped to documentation, doc comments, and tests unless
  the new tests uncover a genuine mismatch in observable behaviour.
- Add both unit tests and behavioural tests that prove the chosen stage
  boundary.
- Record the finalized decision in the relevant design documents, not only in
  the conformance register.
- Mark `docs/roadmap.md` item `2.6.3` done only after docs, tests, and all
  required gates pass.
- Run repository quality gates through Make targets using `set -o pipefail` and
  `tee`.

## Tolerances (exception triggers)

- Scope: if implementing this decision requires changes to more than 9 files of
  code or more than 220 net new code lines, stop and escalate. That would
  indicate accidental drift into implementing lowering rather than documenting
  and testing the boundary.
- Interface: if any public parser API or AST variant signature must change,
  stop and escalate.
- Semantics: if any existing test or downstream caller depends on parser-stage
  builder-call lowering, stop and escalate with concrete file references.
- Architecture: if the only coherent fix requires inventing a new parser-local
  semantic pass for collection literals, stop and escalate.
- Validation: if `make test`, `make lint`, or `make check-fmt` still fails
  after three focused fix attempts, stop and escalate with the failing test
  names or lint messages.
- Runtime: if `make test` does not complete normally and the evidence suggests
  a repository-wide runner issue rather than this change, stop and escalate
  instead of silently substituting a different test profile.

## Risks

- Risk: the repository is already partly aligned, so it is easy to update only
  the most visible docs and leave stale claims elsewhere. Severity: high.
  Likelihood: high. Mitigation: run a repo-wide search for `vec_with_capacity`,
  `map_empty`, `Expr::VecLit`, `Expr::MapLit`, `collection literal lowering`,
  `scheduled literal lowering`, and `builder sequence`, then update every
  contract-bearing hit in the same change.

- Risk: because collection literals are expression-wide, not rule-specific,
  implementers may be tempted to copy the aggregation-helper pattern and create
  an ad hoc semantic helper. Severity: medium. Likelihood: medium. Mitigation:
  keep the decision explicit that the current parser generation has no separate
  collection-lowering helper, and explain why expression-wide lowering belongs
  in a later shared lowering layer.

- Risk: behavioural tests that only use `parse_expression()` could miss a drift
  in the full-program `parse()` contract. Severity: medium. Likelihood: medium.
  Mitigation: add at least one full-program test that proves
  `Parsed::semantic_rules()` stays unchanged and a rule body expression remains
  `Expr::VecLit` or `Expr::MapLit`.

- Risk: `{ expr }` brace groups are an existing extension, so poorly chosen map
  test inputs can accidentally assert the wrong thing. Severity: medium.
  Likelihood: medium. Mitigation: use colon-bearing map examples such as
  `{a: 1}` for boundary tests and keep brace-group coverage separate.

## Progress

- [x] (2026-03-11) Reviewed roadmap item `2.6.3`, conformance register item
  10, the syntax spec, implementation notes, design doc, existing collection
  literal tests, and neighbouring ExecPlans.
- [x] (2026-03-11) Drafted this ExecPlan in
  `docs/execplans/2-6-3-decide-collection-literal-lowering-stage-ownership.md`.
- [ ] Freeze the collection-literal ownership decision in docs and doc
  comments.
- [ ] Add unit tests proving `parse_expression()` preserves raw collection
  literal nodes.
- [ ] Add behavioural tests proving `parse()` does not lower collection
  literals and does not emit collection-specific semantic rules.
- [ ] Mark conformance register item 10 as `implemented`.
- [ ] Mark roadmap item `2.6.3` done.
- [ ] Run `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`,
  `make lint`, and `make test`.

## Surprises & Discoveries

- The main syntax spec is already closer to the implemented behaviour than the
  conformance register suggests. Sections 3.3, 6.4, and 11.6 of
  `docs/differential-datalog-parser-syntax-spec-updated.md` already say vector
  and map literals are preserved today and that lowering is scheduled work.

- The clearest remaining contradiction is code-level documentation, not parser
  behaviour. `src/parser/ast/expr.rs` still says `Expr::VecLit` and
  `Expr::MapLit` desugar to builder sequences even though no such lowering is
  performed by the parser today.

- Roadmap item `2.4.4` already resolved the base parser shape in practice:
  vector and map literals parse as raw AST while builder-call desugaring is
  deferred. Item `2.6.3` is therefore mainly a conformance and ownership
  clarification task.

- The current parser has only two parse-adjacent semantic transforms with
  explicit ownership: top-level `for` desugaring in `Parsed::semantic_rules()`
  and aggregation classification in `Rule::body_terms()`. There is no existing
  collection-literal helper boundary to preserve.

- Existing unit and integration tests already prove expression-level raw-node
  parsing. What is missing is an explicit stage-boundary assertion that
  `parse()` does not lower collection literals or synthesize extra semantic
  rules because of them.

## Decision Log

- Decision: recommend assigning collection literal lowering to a later lowering
  stage, not to the parser and not to the current helper-stage rule analysis.
  Rationale: collection literals can appear anywhere expressions are allowed,
  so a rule-only helper would be an awkward fit, while parser-stage lowering
  would violate the already-implemented raw-AST contract from roadmap item
  `2.4.4`.

- Decision: treat `docs/roadmap.md` item `2.4.4` as binding evidence for the
  intended current-generation parser contract. Rationale: it is already marked
  done, names the exact raw AST variants, and explicitly says builder-call
  desugaring is deferred.

- Decision: update `docs/ddlint-design.md` together with the parser-specific
  docs. Rationale: stage ownership affects ADR-001 crate-split boundaries and
  therefore belongs in the design-level documentation as well as the parser
  notes.

- Decision: prefer tests that assert observable raw-node preservation over
  speculative tests for a future lowering API. Rationale: this roadmap item is
  about closing the current ownership decision, not about designing or
  pre-committing to a future lowering representation.

## Context and orientation

The parser stack relevant to this task is small and already implemented:

1. `src/parser/expression/data_structures.rs` parses `[e1, e2, ...]` into
   `Expr::VecLit` and `{k: v, ...}` into `Expr::MapLit`.
2. `src/parser/ast/expr.rs` defines those AST variants and currently contains
   stale doc comments describing builder-sequence desugaring.
3. `src/parser/mod.rs` defines the base `parse()` contract. It already says
   aggregation classification is outside the base parse pipeline and that
   `Parsed::semantic_rules()` is used for top-level `for`.
4. `src/parser/cst_builder/mod.rs` documents `Parsed::semantic_rules()` as
   top-level `for` output only.
5. `src/parser/tests/collections.rs` and `tests/expression_prefix.rs` already
   prove that expression parsing yields raw collection-literal AST nodes.

The documents that currently carry the ownership story are:

- `docs/roadmap.md`
- `docs/parser-conformance-register.md`
- `docs/differential-datalog-parser-syntax-spec-updated.md`
- `docs/parser-implementation-notes.md`
- `docs/ddlint-design.md`

The likely code files for this change are:

- `src/parser/ast/expr.rs`
- `src/parser/tests/collections.rs`
- `tests/collection_literal_boundary.rs`

If an existing integration test file is a better fit than a new dedicated
behavioural test file, keep the dedicated boundary assertions grouped together
so future maintainers can find them quickly.

## Plan of work

### Stage A: freeze the decision in prose before changing tests

Align every contract-bearing document around one ownership statement:

- In `docs/parser-conformance-register.md`, rewrite item 10 so the spec/target
  behaviour matches the current-generation contract: the parser preserves raw
  collection literal nodes, and later lowering owns any future builder-style
  desugaring. Mark the decision status `implemented` once the rest of this plan
  is complete.
- In `docs/differential-datalog-parser-syntax-spec-updated.md`, keep the
  existing "preserved today" wording but strengthen it so it names the owning
  stage explicitly. Section 6.4 should say that later lowering, not parsing,
  owns collection desugaring.
- In `docs/parser-implementation-notes.md`, add a short pipeline note near the
  existing parser-boundary text stating that collection literal lowering, like
  aggregation classification, is not part of the base `parse()` pipeline.
  Unlike aggregation, it also is not implemented as a current helper-stage
  semantic pass.
- In `docs/ddlint-design.md`, add one sentence in the parser-boundary section
  that collection literal lowering belongs to the future semantic/lowering
  layer expected by ADR-001, rather than the syntax-layer parser.

### Stage B: add failing tests that encode the boundary

Add tests that will fail if collection literal lowering moves into the parser
again.

- Extend `src/parser/tests/collections.rs` with boundary-specific unit tests
  that assert:
  - `parse_expression("[1, 2]")` yields `Expr::VecLit`.
  - `parse_expression("{a: 1}")` yields `Expr::MapLit`.
  - no builder-call-shaped representation appears in the returned expression
    tree.
- Add a behavioural test file such as `tests/collection_literal_boundary.rs`
  that exercises the public parser API end-to-end:
  - `parse("Out(x) :- [1, 2].")` yields exactly one rule.
  - that rule's `body_expressions()` returns a first term matching
    `Expr::VecLit`.
  - `parse("Out(x) :- {a: 1}.")` yields a first body expression matching
    `Expr::MapLit`.
  - both parses leave `Parsed::semantic_rules()` empty, proving collection
    literals do not participate in parse-time semantic lowering today.

These tests should be written first so the implementation follows a red-green
path, even if the expected parser behaviour is already present and only the new
boundary assertions fail initially.

### Stage C: implement the minimal code and doc-comment changes

Make the smallest repository changes needed to express the chosen contract
consistently.

- Update the `Expr::VecLit` and `Expr::MapLit` doc comments in
  `src/parser/ast/expr.rs` so they describe preserved raw literal nodes and
  name later lowering as future work instead of stating that desugaring already
  happens.
- If the new behavioural tests reveal ambiguity in `parse()` or
  `Parsed::semantic_rules()` documentation, tighten the relevant doc comments
  in `src/parser/mod.rs` and `src/parser/cst_builder/mod.rs`.
- Do not introduce any new lowering code unless the tests expose a genuine
  mismatch between documentation and observable behaviour.

### Stage D: close the roadmap item and verify the repository

Once docs, tests, and doc comments agree:

- Mark `docs/roadmap.md` item `2.6.3` done.
- Run the documentation-formatting and validation gates because this task edits
  Markdown:

  ```bash
  set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-3-make-fmt.log
  set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-3-make-markdownlint.log
  set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-3-make-nixie.log
  ```

- Run the required repository quality gates:

  ```bash
  set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-3-make-check-fmt.log
  set -o pipefail; make lint 2>&1 | tee /tmp/2-6-3-make-lint.log
  set -o pipefail; make test 2>&1 | tee /tmp/2-6-3-make-test.log
  ```

Expected success condition for each command: exit code `0`, no failed lint or
test cases, and formatter checks that report no remaining diffs.

## Outcomes & Retrospective

Not yet executed. When implementation completes, replace this section with:

- the final ownership decision in one sentence,
- the files changed,
- the exact tests added,
- the quality gates run and their outcomes,
- any lessons about parser-versus-lowering boundaries that should influence
  later ADR-001 work.
