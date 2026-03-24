# Align index declaration grammar to the syntax spec

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: COMPLETE

## Purpose / big picture

Roadmap item `2.6.4` is open because the parser and the syntax specification
describe different index declarations. The current scanner and Abstract Syntax
Tree (AST) tests accept `index Name on Relation(columns)`, while
`docs/differential-datalog-parser-syntax-spec-updated.md` section `5.6` defines
`index Name(field: Type, ...) on Atom`, and the conformance register tracks
that mismatch in item `11`.

After this change, a novice should be able to parse the canonical spec form,
inspect the resulting index node without guessing which grammar is real, and
read one consistent contract across the scanner, tests, and documentation.
Success is observable when:

- `parse("index OrdersByUser(user: UserId, ts: Timestamp) on Orders[user, ts, amt]")`
  returns one index and no parse errors.
- Unit tests and behavioural tests cover the canonical form, invalid legacy
  shorthand, and attribute/name-validation paths.
- The conformance register and design docs describe the same grammar the code
  implements.
- `docs/roadmap.md` item `2.6.4` is marked done only after all required gates
  pass.

## Recommended decision

Close item `2.6.4` by bringing the parser to the spec-side grammar rather than
weakening the spec to match an older shorthand.

The canonical parser contract should become:

- `index Name(field: Type, ...) on Atom`
- The field list is mandatory and uses the same name/type surface as relation
  columns.
- The `on` target accepts the spec atom surface, including constructor and
  bracket forms, plus existing ref/diff/delay adornments where the current atom
  helpers already support them or can be extended narrowly.
- The older `index Name on Relation(columns)` form is rejected with one
  deterministic diagnostic that points at the missing typed field list.

Boundaries for this milestone:

- Keep the repository's current top-level statement terminator behaviour.
  Do not widen this task into a global semicolon-policy change just because the
  Extended Backus-Naur Form (EBNF) examples show `;`.
- Prefer a CST-backed index wrapper that exposes the new grammar directly,
  even if that means reshaping `src/parser/ast/index.rs`. Stable parser APIs do
  not land until roadmap phase `2.8`, so carrying misleading legacy helpers now
  would make Architecture Decision Record (ADR) 001 harder, not easier.
- Reuse existing scanner helpers where possible. `src/parser/lexer_helpers.rs`
  already provides `atom()`, so this work should extend the index scanner
  around that helper instead of inventing a second ad hoc token walker.

If implementation proves that the spec form cannot be supported without a much
larger refactor of shared expression parsing, stop and escalate with concrete
evidence. Do not silently narrow the grammar.

## Constraints

- Align the parser implementation to the normative grammar in
  `docs/differential-datalog-parser-syntax-spec-updated.md` section `5.6`.
- Keep the scope centred on index declarations. Do not fold transformer,
  relation, or legacy-token policy work from roadmap items `2.6.5` through
  `2.6.7` into this change.
- Preserve lossless Concrete Syntax Tree (CST) behaviour. Round-trip tests must
  continue to prove that parsed source text is preserved exactly.
- Keep top-level name-uniqueness and attribute-placement validation working for
  the new index syntax.
- Add both unit tests and behavioural tests before considering the task done.
- Record the final design decision in active design docs, not only in tests or
  the conformance register.
- Mark `docs/roadmap.md` item `2.6.4` done only after docs, tests, and all
  required gates pass.
- Use Make targets for validation and run long commands with
  `set -o pipefail` and `tee`.
- Keep comments and documentation in en-GB-oxendict spelling.

## Tolerances (exception triggers)

- Scope: if the implementation requires edits to more than 14 files or more
  than 320 net new code lines, stop and escalate. That likely means the task is
  drifting into a broader parser-surface redesign.
- Interface: if changes must extend beyond the index AST wrapper, its tests,
  and directly related parser helpers, stop and escalate. Small, explicit
  changes to `src/parser/ast/index.rs` are expected; wider public API churn is
  not.
- Grammar reuse: if `lexer_helpers::atom()` cannot be reused or narrowly
  wrapped for the `on` target, and the only remaining option is to duplicate
  atom grammar by hand, stop and escalate.
- Compatibility: if non-test workspace code depends on
  `Index::relation()` / `Index::columns()` semantics beyond the parser test
  suite, stop, and document those callers before changing the API.
- Validation: if `make check-fmt`, `make lint`, or `make test` still fails
  after three focused fix rounds, stop and escalate with the specific failing
  targets.
- Runner stability: if `make test` hits the known repository-wide nextest stall
  rather than a deterministic regression from this task, capture the evidence
  and escalate instead of silently substituting `CI=1 make test`.

## Risks

- Risk: the spec's `on Atom` surface is richer than the current index scanner,
  and `atom()` does not currently model every adornment on its own. Severity:
  high. Likelihood: medium. Mitigation: begin with red tests that pin the exact
  accepted target forms, then extend the helper narrowly for indexes rather
  than touching unrelated rule parsing.

- Risk: `src/parser/ast/index.rs` currently exposes `relation()` and
  `columns()`, which encode the old shorthand shape. Severity: high.
  Likelihood: high. Mitigation: search for all callers first, then replace the
  misleading accessors with grammar-aligned ones in the same change and update
  all tests atomically.

- Risk: old shorthand examples are scattered through unit tests, behavioural
  tests, CST integration fixtures, and validation tests. Severity: medium.
  Likelihood: high. Mitigation: do a repo-wide search for `index .* on .*(` and
  update all active fixtures in one pass.

- Risk: attribute-placement and name-uniqueness tests may still pass while the
  primary parser tests are stale, because those suites mostly care about
  top-level item discovery rather than index contents. Severity: medium.
  Likelihood: medium. Mitigation: add at least one behavioural test dedicated
  to index grammar, not just validator coverage.

## Progress

- [x] (2026-03-20) Reviewed roadmap item `2.6.4`, conformance register item
  `11`, the syntax spec index grammar, the current scanner in
  `src/parser/span_scanners/indexes.rs`, and the current `Index` wrapper in
  `src/parser/ast/index.rs`.
- [x] (2026-03-20) Reviewed current unit and behavioural coverage in
  `src/parser/tests/indexes.rs`, `src/parser/tests/parser.rs`,
  `tests/attribute_placement.rs`, `tests/name_uniqueness.rs`, and related
  fixtures.
- [x] (2026-03-20) Drafted this ExecPlan in
  `docs/execplans/2-6-4-align-index-declaration-grammar.md`.
- [x] (2026-03-20) Converted parser unit fixtures and specs to the canonical
  index grammar and added legacy-shorthand rejection coverage.
- [x] (2026-03-20) Implemented scanner changes in
  `src/parser/span_scanners/indexes.rs` and reshaped the `Index` wrapper in
  `src/parser/ast/index.rs` around `fields()` and `on_target()`.
- [x] (2026-03-20) Updated behavioural tests and validation fixtures in
  `tests/index_declaration_grammar.rs`, `tests/name_uniqueness.rs`,
  `src/parser/tests/attributes.rs`,
  `src/parser/span_scanners/tests/attribute_tests.rs`,
  `src/parser/tests/cst_integration.rs`, and related helpers.
- [x] (2026-03-20) Updated active documentation and conformance records in
  `docs/parser-conformance-register.md`, `docs/parser-implementation-notes.md`,
  `docs/ddlint-design.md`, and `docs/roadmap.md`.
- [x] (2026-03-20) Ran `make fmt`, `make markdownlint`, `make nixie`,
  `make check-fmt`, `make lint`, and `make test`.
- [x] (2026-03-20) Marked roadmap item `2.6.4` done after all checks passed.

## Surprises & Discoveries

- `src/parser/lexer_helpers.rs` already contains an `atom()` parser that
  accepts scoped names plus parenthesized, bracketed, or braced argument
  blocks. The index scanner is not starting from zero.

- The current `Index` wrapper is very shallow and only exposes the old grammar
  shape: `name()`, `relation()`, and `columns()`. `name()` is used by the
  uniqueness validator, but `relation()` and `columns()` are only referenced in
  parser tests today.

- Behavioural tests are already inconsistent: `tests/attribute_placement.rs`
  uses the spec-style form `index Ix(a: T) on A(a)`, while most parser-facing
  unit tests still use `index I on R(x)`.

- Archive analysis already identified this as an open contract decision. This
  is not a newly discovered requirement; it is an overdue alignment task.

- `Index::on_target()` is easiest to keep deterministic if it returns
  normalized target text with trivia removed rather than preserving formatting
  inside the target. This keeps whitespace-variation tests stable without
  inventing a dedicated typed atom wrapper in this milestone.

## Decision Log

- Decision: recommend aligning code to the syntax spec, not rewriting the spec
  to preserve the shorthand `index Name on Relation(columns)`. Rationale: the
  richer spec grammar is already reflected in the conformance register,
  validation examples, and archive analysis, and it is the better contract to
  freeze before ADR-001.

- Decision: recommend rejecting the old shorthand with one deterministic
  diagnostic rather than carrying both forms silently. Rationale: dual grammar
  support would blur the parser contract immediately before crate extraction
  and create avoidable ambiguity for downstream consumers.

- Decision: keep statement-terminator policy out of scope. Rationale: the real
  mismatch is the index clause shape, not semicolon handling, and widening the
  task would entangle it with unrelated parser conventions.

- Decision: prefer grammar-aligned `Index` accessors over compatibility shims.
  Rationale: roadmap phase `2.8` is where stable parser contracts are frozen;
  before then, clear alignment is more valuable than preserving helpers that
  encode the wrong syntax.

- Decision: expose the `on` target as normalized text through
  `Index::on_target()` instead of adding a new typed atom wrapper. Rationale:
  this roadmap item is about scanner and syntax-surface alignment; returning a
  normalized CST-derived string keeps the contract explicit and bounded without
  expanding into a broader semantic model change.

## Context and orientation

The relevant code and documentation live in a small set of files:

- `src/parser/span_scanners/indexes.rs` now recognizes the canonical
  `index Name(field: Type, ...) on Atom` form, plus a targeted legacy-shorthand
  rejection diagnostic.
- `src/parser/lexer_helpers.rs` already exposes reusable helpers such as
  `ident()`, `inline_ws()`, and `atom()`.
- `src/parser/ast/index.rs` now exposes the grammar-aligned `Index` wrapper
  surface.
- `src/parser/tests/indexes.rs`, `src/parser/tests/programs.rs`,
  `src/parser/tests/specs.rs`, and `src/parser/tests/parser.rs` define the
  current parser-facing test contract.
- `tests/attribute_placement.rs` and `tests/name_uniqueness.rs` provide
  behavioural coverage for top-level validation paths that must remain green.
- `docs/differential-datalog-parser-syntax-spec-updated.md`,
  `docs/parser-conformance-register.md`, `docs/parser-implementation-notes.md`,
  and `docs/ddlint-design.md` carry the active written contract.

The main mismatch today is simple to state:

- Code: `index Name on Relation(columns)`
- Spec: `index Name(field: Type, ...) on Atom`

The implementation should resolve that mismatch in code, tests, and docs in one
atomic change.

## Plan of work

### Stage A: establish the red tests first

Start by changing tests so they describe the intended contract before any
scanner code changes are made.

- Update parser unit fixtures in `src/parser/tests/programs.rs` and
  `src/parser/tests/indexes.rs` to use the canonical spec form.
- Replace `IndexSpec` in `src/parser/tests/specs.rs` so it asserts typed
  fields and the `on` target instead of `relation()` plus raw column strings.
- Add at least one explicit failing unit test for rejected legacy shorthand,
  for example `index Legacy on Orders(user)` producing a deterministic
  diagnostic about the missing typed field list.
- Add at least one unit case that proves the `on` target accepts a bracket
  form, because that is the most visible current spec/example gap.

At the end of Stage A, running the targeted parser index tests should fail for
the right reason: the code still implements the old grammar.

### Stage B: implement the scanner and AST alignment

Update `src/parser/span_scanners/indexes.rs` to parse:

1. `index`
2. index name
3. a non-empty typed field list
4. `on`
5. an atom-shaped target

Use existing helpers where possible:

- Reuse the same surface grammar as relation columns for the typed field list.
- Reuse `lexer_helpers::atom()` for the target shape, adding only the minimal
  extra wrapper needed for any index-specific delay suffix support proven by
  the tests.

Then reshape `src/parser/ast/index.rs` around the new grammar:

- Keep `name()`.
- Replace the old shorthand-oriented accessors with grammar-aligned ones such
  as `fields()` and `on_target()` or an equivalent clearly named pair.
- Document the new accessors with examples that use the canonical syntax.

Do not add semantic lowering in this task. This milestone is about grammar and
observable parser surface, not about turning index targets into a deeper
semantic representation.

### Stage C: align validation and behavioural coverage

Once the scanner parses the new form, update the broader test suite so the rest
of the pipeline uses the same examples.

- Update `src/parser/tests/parser.rs` and any CST integration fixtures that
  still embed the old shorthand.
- Update validator fixtures in `src/parser/tests/attributes.rs`,
  `src/parser/validators/name_uniqueness.rs`, and `tests/name_uniqueness.rs`.
- Keep `tests/attribute_placement.rs` on the canonical spec form and ensure it
  still reports exactly the intended attribute-placement error.
- Add a dedicated behavioural test file if needed, for example
  `tests/index_declaration_grammar.rs`, that proves end-to-end acceptance of
  the canonical form and end-to-end rejection of the legacy shorthand.

### Stage D: align the written contract

After the code and tests agree, update the active documentation in the same
change:

- Mark conformance register item `11` as `implemented` in
  `docs/parser-conformance-register.md`.
- Update any nearby examples and notes in
  `docs/differential-datalog-parser-syntax-spec-updated.md` if implementation
  revealed a narrower accepted target form than the prose currently claims.
- Add an implementation note in `docs/parser-implementation-notes.md`
  explaining which helper owns index target scanning and why the old shorthand
  is now rejected.
- Update `docs/ddlint-design.md` to record the design decision that parser
  contracts should expose the canonical index grammar before ADR-001 crate
  extraction.
- Mark roadmap item `2.6.4` done in `docs/roadmap.md`.

Historical or archived documents do not need bulk rewrites in this milestone
unless they are still linked as active guidance.

## Validation and evidence

Run the full required gates after implementation, capturing logs with `tee`.

```plaintext
set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-4-make-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-4-make-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-4-make-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-4-make-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/2-6-4-make-lint.log
set -o pipefail; make test 2>&1 | tee /tmp/2-6-4-make-test.log
```

Before those full gates, the implementer should also run a focused red/green
loop on the index tests so failures stay attributable to this task.

Observable success criteria:

- Canonical spec-form indexes parse without errors.
- Legacy shorthand is rejected with one deterministic, targeted diagnostic.
- Updated `Index` accessors return the new grammar's data, not inferred
  relation/column fragments from the old form.
- Attribute-placement and duplicate-name behavioural tests remain green with
  canonical index fixtures.
- Docs, conformance register, and roadmap all describe the same grammar.

## Outcomes & Retrospective

The final accepted grammar is the spec-form
`index Name(field: Type, ...) on Atom`. The legacy shorthand
`index Name on Relation(columns)` is now rejected with the deterministic custom
diagnostic
`index declarations require a typed field list '(name: Type, ...)' before 'on'`.

The observable parser contract for indexes is now:

- `Index::name()`
- `Index::fields()`
- `Index::on_target()`

`Index::on_target()` returns normalized target text with trivia removed. This
was sufficient for this milestone and avoided introducing a new typed atom
wrapper before ADR-001 planning.

Tests added or updated:

- `src/parser/tests/indexes.rs`
- `src/parser/tests/programs.rs`
- `src/parser/tests/specs.rs`
- `src/parser/tests/parser.rs`
- `src/parser/tests/attributes.rs`
- `src/parser/tests/cst_integration.rs`
- `src/parser/cst_builder/tree.rs`
- `src/parser/span_scanners/tests/mod.rs`
- `src/parser/span_scanners/tests/attribute_tests.rs`
- `src/parser/validators/name_uniqueness.rs`
- `tests/index_declaration_grammar.rs`
- `tests/name_uniqueness.rs`

Documentation updated:

- `docs/parser-conformance-register.md`
- `docs/parser-implementation-notes.md`
- `docs/ddlint-design.md`
- `docs/roadmap.md`

Validation completed successfully:

- `set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-4-final-make-fmt.log`
- `set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-4-final-make-markdownlint.log`
- `set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-4-final-make-nixie.log`
- `set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-4-final-make-check-fmt.log`
- `set -o pipefail; make lint 2>&1 | tee /tmp/2-6-4-final-make-lint.log`
- `set -o pipefail; make test 2>&1 | tee /tmp/2-6-4-final-make-test.log`

Deferred follow-up: this milestone did not introduce a richer typed index
target wrapper or expand delimiter-recovery coverage for every malformed target
shape. Those remain reasonable future refinements if a later parser surface
needs them.
