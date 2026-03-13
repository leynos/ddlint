# Implement symbol-table and scope-resolution passes (roadmap 3.3.1)

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: DRAFT

## Purpose / big picture

Roadmap item `3.3.1` is the first semantic-analysis milestone for `ddlint`. Its
job is to give lint rules an owned, deterministic view of names and bindings
instead of forcing every rule to rediscover that information by walking the
concrete syntax tree (CST) manually.

In this plan, a "symbol table" means an immutable data structure that records
declarations, bindings, and name uses together with their source spans. "Scope
resolution" means deciding which declaration or binding each name use refers to
after accounting for lexical nesting and rule-body evaluation order.

After this milestone lands, a contributor should be able to parse a DDlog
program, build a semantic model once, and ask questions such as:

- which top-level declarations exist;
- which rule-local bindings are in scope at a given literal;
- whether a variable or relation use resolved successfully; and
- where the originating declaration or binding came from.

Observable success is:

- a new semantic-analysis module builds a `Send + Sync` semantic model from
  `Parsed`, `Root`, and parse-time `SemanticRule` values;
- `RuleCtx` exposes that semantic model to lints, and `Runner` computes it once
  per lint run instead of per rule invocation;
- the model records explicit scopes, symbols, bindings, and resolved or
  unresolved name uses with source provenance;
- unit tests cover scope creation, ordered binding visibility, and resolution
  edge cases;
- behavioural tests cover end-to-end semantic analysis on parsed DDlog
  snippets, plus at least one `Runner` path proving semantic context reaches a
  lint rule;
- `docs/ddlint-design.md` records the final semantic-model contract and any
  design corrections made during implementation;
- `docs/roadmap.md` marks item `3.3.1` done after all gates pass; and
- `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`,
  `make lint`, and `make test` all succeed.

## Context and orientation

The current parser and linter surfaces are intentionally CST-first and light on
owned semantic state:

- `src/parser/mod.rs` parses source into `Parsed`.
- `src/parser/cst_builder/mod.rs` stores the `GreenNode`, typed AST `Root`,
  parse-time `SemanticRule` values from top-level `for` desugaring, and parser
  errors.
- `src/parser/ast/root.rs` exposes top-level wrappers such as `relations()`,
  `functions()`, `type_defs()`, and `rules()`.
- `src/parser/ast/rule.rs` exposes helper-stage semantic extraction through
  `body_terms()` and `flattened_body_terms()`.
- `src/linter/rule.rs` defines `RuleCtx`, but today it carries only source
  text, AST root, and per-rule configuration.
- `src/linter/runner.rs` rebuilds a red tree per rule on worker threads and has
  no semantic-model cache.

The roadmap text for `3.3.1` points at `docs/ddlint-design.md §2.3`, but that
section currently describes parser recovery and the parser/linter relationship,
not symbol tables. The practical design basis for this milestone is therefore:

- the roadmap text in `docs/roadmap.md` items `3.3.1` through `3.3.4`;
- the initial lint catalog in `docs/ddlint-design.md §3.3`; and
- the semantic-layer and provenance invariants in
  `docs/parser-implementation-notes.md`.

The later roadmap items make the intended consumers clear:

- `unused-relation` needs top-level declaration and relation-use facts;
- `unused-variable` needs rule-local binding and use facts;
- `shadowed-variable` needs ordered scope and rebinding facts; and
- later style and performance rules need stable provenance back to source.

This milestone should therefore deliver the shared semantic substrate those
rules will query, while keeping later rule-specific reporting logic in `4.*`.

## Constraints

- Keep scope limited to roadmap item `3.3.1`, plus the additive plumbing needed
  to expose the semantic model through `RuleCtx` and `Runner`.
- Do not implement production lint rules from phase `4`.
- Do not broaden parser grammar or change parse-stage diagnostics unless a
  semantic-model bug is impossible to fix otherwise.
- Keep parse-stage aggregation behaviour unchanged: `parse()` must still defer
  rule-body aggregation classification to `Rule::body_terms()` and
  `Rule::flattened_body_terms()`.
- Treat the semantic model as owned data. Do not store `rowan::SyntaxNode` or
  other `!Send` syntax handles inside it.
- Preserve deterministic ordering for scopes, symbols, bindings, and use sites
  so future lint rules and tests can rely on stable iteration order.
- Any `RuleCtx` or `Parsed` API additions must be additive.
- Every new Rust module must start with a `//!` module comment.
- Keep files below 400 lines by splitting model, builder, traversal, and tests
  into focused modules as needed.
- Validate with unit tests and behavioural tests before marking the roadmap
  item done.
- Record the final semantic-model contract in `docs/ddlint-design.md`.
- Use Make targets for final gates and run them with `set -o pipefail` and
  `tee`.
- Use en-GB-oxendict spelling in comments and documentation.

## Tolerances (exception triggers)

- Scope: if implementation needs more than 12 files changed or more than 800
  net new lines, stop and re-evaluate the module split before proceeding.
- Interface: if delivering the semantic model requires a non-additive break to
  `RuleCtx`, `Runner`, `Parsed`, or existing AST wrapper methods, stop and
  escalate.
- Grammar ambiguity: if scope resolution depends on unresolved parser-policy
  questions outside this milestone, such as unsupported index or transformer
  grammar, stop and isolate the dependency before continuing.
- Ownership: if the semantic model cannot stay `Send + Sync` without storing
  `rowan` nodes directly, stop and redesign the provenance layer.
- Ordering: if stable deterministic ordering cannot be preserved without a
  broader data-model rewrite, stop and document the conflict.
- Iterations: if focused test or Clippy fix loops exceed three rounds without a
  clear convergence path, stop and escalate with the exact failing test names
  or lint IDs.

## Risks

- Risk: the current design document reference in the roadmap is stale and could
  mislead the implementation. Severity: medium. Likelihood: high. Mitigation:
  update `docs/ddlint-design.md` during implementation with a dedicated
  semantic-analysis subsection that states the implemented contract plainly.

- Risk: rule-body binding visibility is order-sensitive, especially for
  assignments and `for` loops, and an unordered collector would produce false
  positives for `unused-variable` and `shadowed-variable`. Severity: high.
  Likelihood: high. Mitigation: model rule scopes as ordered facts keyed by the
  literal index at which each binding becomes visible, and write failing tests
  for forward-reference and later-literal shadowing cases first.

- Risk: top-level `for` desugaring already produces `SemanticRule` values, so
  ignoring them would create a split world where some rules are visible to
  syntax consumers but not semantic consumers. Severity: medium. Likelihood:
  medium. Mitigation: feed both AST `Rule` nodes and parse-time `SemanticRule`s
  into the semantic builder from day one, tagging their origin.

- Risk: the AST currently exposes names mostly as strings and helper enums,
  which can encourage reparsing or lossy provenance. Severity: medium.
  Likelihood: medium. Mitigation: carry spans, declaration kinds, rule origin,
  and source-order indices explicitly in the semantic model instead of trying
  to reconstruct them later.

- Risk: computing semantic analysis separately in every worker thread would be
  wasteful and could drift if later code accidentally adds nondeterministic
  collection order. Severity: medium. Likelihood: high. Mitigation: compute the
  semantic model once in `Runner::new`, wrap it in `Arc`, and clone the `Arc`
  into each `RuleCtx`.

## Proposed design

Create a new top-level semantic-analysis module at `src/sema/`. This is the
local precursor to the future `ddlog-sema` crate described in
`docs/parser-implementation-notes.md`. Keep it owned-data-only and split it
into small files, for example:

- `src/sema/mod.rs`: public exports and the top-level build entrypoint;
- `src/sema/model.rs`: `SemanticModel`, identifiers, declaration and use-site
  records, and query helpers;
- `src/sema/scopes.rs`: scope creation, parent links, and ordered visibility
  rules;
- `src/sema/build.rs`: traversal from `Root`, `Rule`, and `SemanticRule` into
  the semantic model; and
- `src/sema/tests/*.rs`: unit coverage split by declaration collection,
  rule-local binding order, and resolution behaviour.

The model should use stable numeric identifiers rather than syntax-node handles:

- `ScopeId`: an opaque index for one lexical scope;
- `SymbolId`: an opaque index for one declaration or binding;
- `DeclarationKind`: at minimum `Relation`, `Function`, `Type`, and
  `RuleBinding`;
- `UseKind`: at minimum `RelationUse` and `VariableUse`;
- `Resolution`: `Resolved(SymbolId)`, `Unresolved`, or `Ignored`.

"Ignored" is important for names that are intentionally not linted as normal
bindings, such as `_`.

Each symbol record should capture enough provenance for later rules:

- canonical name text;
- declaration kind;
- defining scope;
- source span;
- source-order index within the owning scope;
- origin metadata describing whether the symbol comes from a top-level
  declaration, a rule head, an assignment pattern, or a `for` pattern.

Each use-site record should capture:

- the textual name used;
- use kind;
- source span;
- the scope in which the use occurred;
- the literal index or traversal step used for ordered resolution; and
- the final `Resolution`.

The semantic builder should follow two passes over each unit of analysis.

The first pass establishes scopes and declares symbols:

1. Create the program scope.
2. Record top-level relation, function, and type declarations in source order.
3. Create one semantic rule scope per AST `Rule`.
4. Create one semantic rule scope per parse-time `SemanticRule` from top-level
   `for` desugaring, preserving its `SemanticRuleOrigin`.
5. Walk rule heads and body terms in source order, creating child scopes only
   where visibility genuinely nests, starting with rule scopes and `for`-loop
   bodies.

The second pass records name uses and resolves them against the visible symbol
chain for the current ordered position. Use nearest enclosing scope first, then
parents, then the program scope. Record `Unresolved` instead of emitting a
diagnostic in this milestone; lint rules will decide later whether an
unresolved or unused fact is reportable.

For rule-local bindings, use the existing helper-stage rule-body term
extraction rather than reparsing raw literal text:

- rule heads contribute binding candidates from variables found in head atoms;
- `RuleBodyTerm::Assignment` contributes bindings from the left-hand pattern
  after the assignment literal becomes visible;
- `RuleBodyTerm::ForLoop` contributes bindings from the loop pattern inside a
  nested child scope covering the loop body; and
- plain expression terms contribute variable and relation uses but no new
  bindings unless later milestones expand the contract deliberately.

Do not attempt to fully solve every possible nested-expression scope in this
milestone. The implementation should cover only the scope boundaries required
by the immediate roadmap consumers:

- program scope;
- rule scope;
- `for`-loop nested scope; and
- ordered visibility within a rule body.

If implementation shows that closures or match-arm patterns must become
first-class scopes to keep the model sound, document that in `Decision Log` and
only proceed if the change remains additive and within tolerance.

## Integration plan

Expose semantic analysis through additive APIs:

1. Export `pub mod sema;` from `src/lib.rs`.
2. Add a semantic-model accessor to `RuleCtx`, for example
   `semantic_model(&self) -> &SemanticModel`.
3. Change `RuleCtx::new` and `RuleCtx::from_parsed` to accept an
   `Arc<SemanticModel>`.
4. Update `Runner::new` to build `Arc<SemanticModel>` once from the provided
   `Parsed` value and reuse it for every worker-thread context.
5. Keep a direct semantic builder entrypoint available to tests and future
   non-runner consumers.

This milestone should not mutate `Parsed` to cache semantics lazily unless that
becomes clearly simpler during implementation. The current `Parsed` type is a
parser result, and keeping semantic analysis explicit avoids mixing parse-stage
and semantic-stage lifetimes prematurely.

## Testing strategy

Follow red, green, refactor. Add failing tests before implementation, then make
them pass without weakening assertions.

Unit tests should live under `src/sema/tests/` and cover at least:

- program-scope declaration collection for relations, functions, and types;
- resolution preferring the nearest visible binding over outer scopes;
- ordered rule-body visibility where a binding becomes visible only after its
  literal;
- `_` wildcard handling as `Ignored`;
- `for`-loop child-scope behaviour; and
- top-level `for` `SemanticRule` participation in semantic analysis.

Behavioural tests should live in `tests/` and cover at least:

- end-to-end semantic-model construction from real DDlog snippets, asserting on
  stable declaration, scope, and use facts rather than internal builder steps;
- a case proving unresolved names stay recorded as `Unresolved` instead of
  crashing analysis on partially invalid programs; and
- a `Runner` integration case where a synthetic lint rule reads
  `ctx.semantic_model()` and emits a diagnostic based on resolved facts.

If repeated fixtures are needed, use `rstest` fixtures rather than ad-hoc
helper duplication. Keep behavioural inputs short and purpose-built.

## Documentation updates

During implementation, update the following documentation:

- `docs/ddlint-design.md`: add or revise a semantic-analysis subsection that
  defines the semantic model, supported scope boundaries, provenance contract,
  and the decision to keep unresolved names as semantic facts rather than
  immediate diagnostics.
- `docs/parser-implementation-notes.md`: record where the new `src/sema/*`
  module fits into the future `ddlog-sema` crate extraction and note any
  non-obvious invariants discovered while implementing ordered rule-body
  visibility.
- `docs/roadmap.md`: mark `3.3.1` done only after code, tests, docs, and gates
  pass.

## Milestones

### Milestone 1: establish the semantic model skeleton

Add the new `src/sema/` module, define the owned model types, and add a direct
builder entrypoint. Write failing unit tests that describe the expected symbol,
scope, and resolution facts before the builder logic exists.

Acceptance for this milestone is that the model types compile, tests fail for
missing logic rather than missing symbols, and no linter integration has been
attempted yet.

### Milestone 2: implement declaration and scope collection

Build program and rule scopes, record top-level declaration symbols, ingest AST
rules plus parse-time `SemanticRule`s, and assign stable source-order indices.
Write or update failing tests for `for`-loop child scopes and top-level `for`
coverage.

Acceptance for this milestone is that declaration and scope-shape tests pass
while name-use resolution tests still fail in the expected places.

### Milestone 3: implement name-use recording and ordered resolution

Walk rule heads and body terms in source order, record relation and variable
uses, and resolve them through the visible scope chain. Make sure assignment
bindings become visible only after their literal and `_` remains `Ignored`.

Acceptance for this milestone is that all semantic-model unit tests pass and
the model produces deterministic collections for repeated runs.

### Milestone 4: integrate with `RuleCtx` and `Runner`

Thread `Arc<SemanticModel>` through `RuleCtx` and `Runner`, then add
behavioural coverage proving that a lint rule can consume semantic facts
without rebuilding analysis itself.

Acceptance for this milestone is that the end-to-end runner test passes and no
existing linter tests regress.

### Milestone 5: document, gate, and close the roadmap item

Update design and implementation notes, mark roadmap item `3.3.1` done, then
run the full gate suite.

Acceptance for this milestone is that the documentation matches the shipped
code and every required Make target passes.

## Validation commands

Run these commands exactly, capturing output for later review:

```bash
set -o pipefail; make fmt 2>&1 | tee /tmp/3-3-1-make-fmt.log
```

```bash
set -o pipefail; make markdownlint 2>&1 | tee /tmp/3-3-1-make-markdownlint.log
```

```bash
set -o pipefail; make nixie 2>&1 | tee /tmp/3-3-1-make-nixie.log
```

```bash
set -o pipefail; make check-fmt 2>&1 | tee /tmp/3-3-1-make-check-fmt.log
```

```bash
set -o pipefail; make lint 2>&1 | tee /tmp/3-3-1-make-lint.log
```

```bash
set -o pipefail; make test 2>&1 | tee /tmp/3-3-1-make-test.log
```

Expected end state:

- the new unit and behavioural tests pass;
- existing parser and linter tests remain green;
- no Clippy warnings are introduced; and
- Markdown validation passes after the plan and design-doc edits.

## Progress

- [x] (2026-03-13 00:00Z) Review the `execplans` skill, roadmap item `3.3.1`,
  and the current parser and linter surfaces.
- [x] (2026-03-13 00:00Z) Identify the roadmap-to-design documentation drift:
  `docs/roadmap.md` references `docs/ddlint-design.md §2.3`, but that section
  is not about symbol tables or scope resolution.
- [x] (2026-03-13 00:00Z) Draft this ExecPlan for
  `docs/execplans/3-3-1-symbol-table-and-scope-resolution.md`.
- [ ] Write failing unit tests for semantic-model construction and resolution.
- [ ] Implement `src/sema/` model and builder modules.
- [ ] Integrate semantic context into `RuleCtx` and `Runner`.
- [ ] Add behavioural tests covering parsed snippets and runner integration.
- [ ] Update `docs/ddlint-design.md` and `docs/parser-implementation-notes.md`.
- [ ] Mark `docs/roadmap.md` item `3.3.1` done.
- [ ] Run `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`,
  `make lint`, and `make test`.

## Surprises & Discoveries

- Observation: roadmap item `3.3.1` points at `docs/ddlint-design.md §2.3`,
  but that section currently documents parser recovery rather than semantic
  analysis. Impact: the implementation must correct the design documentation as
  part of this milestone instead of treating the existing section reference as
  authoritative.

- Observation: `Runner` currently builds a fresh red tree and `RuleCtx` per
  rule on worker threads, but it has no semantic-analysis cache. Impact: the
  semantic model must be owned and shareable so it can be computed once and
  cloned cheaply via `Arc`.

- Observation: parse-time `SemanticRule` values already exist for top-level
  `for` desugaring. Impact: semantic analysis must include them or document a
  deliberate exclusion, otherwise scope facts will diverge between syntactic
  rules and desugared rules.

## Decision Log

- Decision: implement semantic analysis in a new top-level `src/sema/` module
  rather than burying it under `src/linter/` or `src/parser/ast/`. Rationale:
  this aligns the codebase with the future `ddlog-sema` split, keeps owned
  semantic data distinct from CST wrappers, and avoids coupling the semantic
  model to one consumer. Date/Author: 2026-03-13 / Codex.

- Decision: keep unresolved names as semantic facts (`Unresolved`) instead of
  producing new semantic diagnostics in this milestone. Rationale: roadmap item
  `3.3.1` is infrastructure, not user-facing lint reporting, and this choice
  preserves value on partially invalid files. Date/Author: 2026-03-13 / Codex.

- Decision: compute the semantic model once per runner invocation and share it
  via `Arc` through `RuleCtx`. Rationale: repeated per-rule semantic analysis
  would waste work and create unnecessary opportunities for ordering drift.
  Date/Author: 2026-03-13 / Codex.

- Decision: treat ordered rule-body visibility as part of this milestone's core
  contract. Rationale: later roadmap items for unused and shadowed variables
  depend on that ordering, so a scope system without ordered visibility would
  be misleadingly incomplete. Date/Author: 2026-03-13 / Codex.

## Outcomes & Retrospective

This section is intentionally incomplete until implementation finishes. At
completion it must summarise:

- what semantic model and APIs shipped;
- which scope boundaries are covered;
- what behavioural guarantees were proven by tests;
- what documentation changed; and
- what follow-on work remains for `3.3.2` through `3.3.4`.
