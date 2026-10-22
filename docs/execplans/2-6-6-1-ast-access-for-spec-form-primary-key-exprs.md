# 2.6.6.1 typed AST access for spec-form relation primary-key expressions

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (design review applied; awaiting user approval)

Branch: `2-6-6-1-ast-access-for-spec-form-primary-key-exprs`

Roadmap item: `2.6.6.1` in `docs/roadmap.md`.

## Purpose / big picture

`ddlint` is a linter for Differential Datalog (DDlog). Its parser produces a
lossless concrete syntax tree (CST) built with the `rowan` crate. Thin typed
wrappers over that tree, the "AST wrappers", give lint rules and the semantic
model structured access to declarations.

A DDlog `input` relation may declare a primary key. Upstream DDlog writes the
key as a lambda-style binder followed by an expression that computes the key
from a row:

```ddlog
input relation Book(row: BookRow) primary key (row) (row.author, row.title)
```

Here `row` is the binder and `(row.author, row.title)` is the key expression.
Today `ddlint` keeps that trailing expression only as opaque CST text.
`Relation::primary_key()` returns only the binder names (`Some(vec!["row"])`).
A lint rule that wants to inspect the key, for example to check that it
references only its binder, must re-tokenize CST text by hand.

After this change a caller of the public parser API can write:

```rust
let key = relation.primary_key().expect("well-formed clause").expect("clause present");
assert_eq!(key.binder_names(), vec!["row"]);
let expression = key.expression().expect("spec-form expression");
assert_eq!(expression.text(), "(row.author, row.title)");
let expr = expression.parse().expect("valid key expression");
assert_eq!(expr.to_sexpr(), "(tuple (field row author) (field row title))");
```

Every binder and the expression carry source-absolute spans. Parse failures in
the key expression carry source-absolute spans that point inside the
expression, and they never hide the binder names.

Lossless CST round-tripping is unchanged. The parse-time diagnostics
(`Parsed::errors()`) are unchanged for every input that is well formed today.
They change for two classes of malformed input that the scanner currently
mis-handles, both fixed in milestone `EP-M1`:

- a statement terminator `;` after a key clause is swallowed into the key
  expression, together with any declaration that follows on the same line;
- a binder list that is not a comma-separated list of identifiers, such as
  `primary key (x.y)`, is accepted by the scanner even though no binder names
  can be read from it.

The work also closes the outstanding decision recorded in
`docs/adr-002-relation-role-kind-modelling.md`: "Roadmap item `2.6.6.1` must
decide the typed shape for primary-key expression access."

## Constraints

- The CST stays lossless. For every input, `pretty_print(root.syntax())`
  must equal the source text. This plan adds no `SyntaxKind` variant and no
  `ParsedSpans` list, and it leaves `src/parser/cst_builder/tree.rs` unchanged
  (see `Decision log`, D1).
- `Parsed::errors()` gains no diagnostics for malformed key expressions. Key
  expressions are parsed lazily, the same way rule-body expressions are parsed
  lazily through `Rule::body_expressions()` and `Rule::body_terms()` in
  `src/parser/ast/rule.rs`. The only permitted changes to `Parsed::errors()`
  are the two `EP-M1` scanner corrections.
- Binder access must not depend on expression validity. A malformed key
  expression must never make `Relation::primary_key()` return `Err`.
- No production code may panic. `.expect()` and `.unwrap()` are allowed only
  in `#[test]` and `#[scenario]` bodies. Shared fixtures, BDD step definitions,
  and helpers must return `Result` and use `?`, per `docs/developers-guide.md`
  (lines 216 to 221) and Whitaker's `no_expect_outside_tests` lint.
- No Rust source file may exceed 400 lines.
  `src/parser/span_scanners/relations.rs` is at 400 lines and
  `src/parser/ast/relation.rs` at 361, so new logic goes into new submodules.
- Every new module starts with a `//!` comment, including inline `mod tests`
  modules, because Whitaker enforces this.
- The only new dev-dependencies authorized are `googletest`,
  `pretty_assertions`, `rstest-bdd`, and `rstest-bdd-macros`, each with a caret
  requirement. No new runtime dependency is permitted.
- The public API is pre-1.0 with no external consumers, so it changes in
  place. Every caller of `Relation::primary_key()` is updated in the same
  commit, with no compatibility alias, per the ExecPlan skill's prohibition on
  compatibility theatre.
- Historical documents stay untouched. Completed ExecPlans under
  `docs/execplans/`, such as `2-6-6-resolve-relation-form-coverage.md`, are not
  edited.
- Use en-GB-oxendict spelling in comments and documentation.
- Implementation must not begin until the user approves the plan.

## Tolerances (exception triggers)

Count changes with `git diff --shortstat origin/main -- '*.rs'` (net = added
minus deleted). Count files with `git diff --name-only origin/main`, excluding
`docs/`, `Cargo.lock`, and `proptest-regressions/`.

- Scope: stop and escalate if the change touches more than 28 counted files
  or more than 1,100 net Rust lines, tests included.
- CST shape: stop and escalate if any milestone needs a new `SyntaxKind`, a
  new `ParsedSpans` list, or any change to `src/parser/cst_builder/tree.rs`.
  That would reverse D1.
- Parse diagnostics: stop and escalate if an existing assertion on
  `Parsed::errors()` must change. The exceptions are assertions covering the two
  `EP-M1` correction classes, which change as specified in `EP-M1`.
- Scanner: stop and escalate if `EP-M1` needs anything beyond the `;`
  terminator rule and binder-list validation. Examples include multi-line key
  expressions, bracket-body keys, and newline-after-body keys.
- Dependencies: stop and escalate if any dependency is needed beyond the four
  authorized dev-dependencies. Also stop if `rstest-bdd` 0.6 cannot build with
  the repository's `rstest` 0.27.
- Iterations: stop and escalate if a gate still fails after three fix attempts
  in the same milestone.
- Ambiguity: stop and present options if the `EP-M0` probe contradicts
  `Context and orientation` in a way that changes the API shape.

## Risks

- Risk: the `EP-M1` corrections change diagnostics for inputs some user
  currently depends on. Severity: low. Likelihood: low. The affected inputs are
  malformed under spec §5.5. Mitigation: differential tests pin the
  keyed-with-`;` form to the keyless-with-`;` form. A `CHANGELOG.md` entry
  documents the change.
- Risk: the expression parser does not accept some upstream key expression
  form. Severity: medium. Likelihood: low, since `parse_expression` handles
  tuples, field access, calls, and grouping. Mitigation: the `EP-M0` probe
  parses the corpus. Any gap surfaces as an `Err` from
  `PrimaryKeyExpression::parse()`, never as a panic or a failure to read the
  binders, and is recorded as a follow-up.
- Risk: `rstest-bdd` 0.6.0 brings in a large transitive graph (`tokio`,
  `gherkin`, `fluent`, `i18n-embed`, `rust-embed`, `ctor`, `inventory`, and
  `thiserror` 2), and every new crate queues through shared compile admission.
  Severity: low. Likelihood: certain. Mitigation: add the dependency in its own
  commit with a one-scenario smoke test. Its macros emit `#[rstest::rstest]`,
  which resolves against the repository's `rstest` 0.27.
- Risk: googletest's `#[gtest]` wraps the test body in a closure, and the
  repository's Clippy expect-policy specimens (`tests/clippy_expect_policy.rs`)
  do not cover that shape. Severity: low. Likelihood: medium. Mitigation:
  googletest tests return `googletest::Result<()>` and use `verify_that!` with
  `?` rather than `.expect()`. The first red commit proves `make lint` accepts
  the shape.
- Risk: the new proptest module exceeds 400 lines. Severity: low.
  Likelihood: medium. Mitigation: split it up front into a strategies module
  and a properties module (see `EP-M3`).
- Risk: shared compile admission slows the gates. Severity: low.
  Likelihood: high. Mitigation: start each gate once and let it queue. Never
  bypass `build-limits-rustc`.

## Progress

- [x] (2026-10-18) Branch renamed to
  `2-6-6-1-ast-access-for-spec-form-primary-key-exprs`.
- [x] (2026-10-18) Reconnaissance by a Wyvern agent team covering relation
  code, docs, and tests. Upstream DDlog `key_expr` grammar confirmed.
- [x] (2026-10-18) First draft of the ExecPlan written.
- [x] (2026-10-23) Community-of-experts design review completed (structure,
  alternatives, contracts, failure modes, cost, viability), and revisions
  applied. The verdict is "proceed with conditions"; all conditions are folded
  into this revision.
- [x] (2026-10-23) `EP-M0` probe run and transcript recorded in
  `Artefacts and notes`. `GAP-SEMI` confirmed; scratch file deleted.
- [ ] Plan approved by the user.
- [ ] `EP-M1` scanner boundary corrections.
- [ ] `EP-M2` typed primary-key model and accessor.
- [ ] `EP-M3` property and behavioural verification.
- [ ] `EP-M4` documentation, ADR amendment, changelog, and roadmap closure.

## Surprises & discoveries

- Observation: upstream DDlog's key grammar is
  `key_expr = withPos $ KeyExpr nopos <$> (parens varIdent) <*> expr`: exactly
  one binder and a mandatory expression. Evidence: upstream
  `src/Language/DifferentialDatalog/Parse.hs`, retrieved with Firecrawl on
  2026-10-18. Impact: `ddlint`'s grammar, `'(' LcName (',' LcName)* ')' Expr?`,
  is a deliberate superset (spec §5.5). The typed model must allow several
  binders and an absent expression. It must not decide whether a binder names a
  row or a column (D4).
- Observation: upstream also accepts keys on bracket-body relations and on
  the line after the body. `examples/primary_key_and_index.dl` puts
  `primary key (x) (x.name)` on the line after `input relation Person[Person]`.
  Evidence: the example file and the upstream `relation` parser. Impact: none
  here. Bracket-body keys are rejected with D-REL-004, and same-line expression
  scanning was decided in roadmap item `2.6.6`. Both remain out of scope.
- Observation: no parse-time validation runs on rule-body expressions;
  `parse_with_observer` in `src/parser/mod.rs` never calls `parse_expression`.
  Impact: lazy key-expression parsing matches the existing contract.
- Observation (design review): the relation node does not end where the key
  expression ends. `parse_relation_suffix` calls `skip_trivia_end`
  (`src/parser/span_scanners/relations.rs`, around line 192), which pulls
  trailing trivia, including comments on later lines, into the node. Impact:
  the expression is the non-trivia hull of the tokens after the binder list. D1
  is restated accordingly.
- Observation (design review): the scanner validates only that the binder
  list contains some non-trivia token (`relations/cursor.rs`, lines 45 to 50).
  So `primary key (x.y)`, `primary key (1)`, and `primary key (,)` scan without
  errors, yet `primary_key_clause` in `src/parser/ast/parse_utils/relation.rs`
  rejects them. Impact: the scanner and the accessor disagree on which clauses
  are well formed. `EP-M1` closes this gap.
- Observation (design review): `;` is `T_SEMI`, which is neither trivia nor
  newline-bearing. `consume_opaque_primary_key_expr` therefore swallows a `;`
  and everything after it up to the next depth-zero newline, including a whole
  same-line `; output relation S(x: u32)`, which then vanishes from
  `Root::relations()`. The keyless form `input relation R(x: u32);` appears in
  `src/parser/tests/round_trip.rs`. Impact: `EP-M1` is unconditional.
- Observation (design review): `next_token_is_same_line_expression` refuses
  to start an expression at `T_IMPLIES` (`:-`) or `T_DOT`. That keeps a
  same-line rule or fact terminator out of the key, but only checks the first
  token. Impact: expression generators must never start with `.` or `:-`, and
  the reason for the exclusion is to be documented in `EP-M1`.
- Observation (design review): keyed non-input relations and keyed
  bracket-body relations end their CST node at the end of the body
  (`recoverable_relation`). Their clauses are not inside `N_RELATION_DECL`, so
  `primary_key()` returns `Ok(None)` alongside the D-REL-004 or D-REL-006
  diagnostic.

## Decision log

- D1. Decision: implement typed access over the existing tokens of
  `N_RELATION_DECL` without a new CST node. The expression is the non-trivia
  hull of the node's tokens after the binder list, and the last non-trivia
  token of an accepted keyed relation node is the end of the key expression.
  Rationale: the scanner remains the single source of truth for the
  expression's extent, the CST shape is unchanged, and the change stays in
  `src/parser/ast/relation/`. The strongest alternative, raised in the design
  review, is to have the relation scanner also emit the expression's hull span
  into `ParsedSpans::expressions`, so the existing `N_EXPR_NODE` nests inside
  `N_RELATION_DECL`. That would set the boundary by construction and fit
  ADR-001's "typed fields and nodes" goal more closely. It was rejected for now
  because it touches the 400-line relation scanner, `span_scanner.rs` (383
  lines) and `ParsedSpans` ordering validation, and it churns CST shape. It is
  recorded as the migration path for the ADR-001 crate split. The public value
  types own their text (`String`), so a node-backed implementation could adopt
  them unchanged. Date/Author: 2026-10-18, planning agent; revised 2026-10-23
  after design review.
- D2. Decision: change `Relation::primary_key()` to return
  `Result<Option<PrimaryKey>, RelationParseErrors>` rather than add a second
  accessor. Rationale: one clause accessor parses the clause once, keeps the
  binder-independence guarantee (`INV-INDEP`) in one place, and avoids two
  overlapping APIs. The API is pre-1.0 with no external consumers; every caller
  is a test or doctest. Date/Author: 2026-10-18, planning agent; confirmed by
  design review.
- D3. Decision: parse the key expression lazily with
  `PrimaryKeyExpression::parse()`. It returns
  `Result<Expr, Vec<chumsky::error::Simple<SyntaxKind>>>` with source-absolute
  spans. Clause-structure failures keep `RelationParseErrors`. Rationale: this
  matches `Rule::body_expressions()` and keeps located errors for future lint
  diagnostics, which `RelationParseErrors = Vec<String>` would discard. The
  method is named `parse()`, not `expr()`, because it does work and can fail,
  unlike the neighbouring `expression()` getter. The ADR-002 amendment records
  that the ADR-001 crate split should replace the exposed `Simple` type with a
  domain error type. Date/Author: 2026-10-18; renamed 2026-10-23 after design
  review.
- D4. Decision: the syntax layer exposes binders and the expression without
  interpreting them. It does not decide whether a binder names a column or the
  whole row. Rationale: upstream binds the whole row, while the `ddlint` list
  form `primary key (a, b)` reads as columns. Deciding this is semantic-layer
  work, which ADR-001 places in `ddlog-sema`. Date/Author: 2026-10-18.
- D5. Decision: neither Verus nor Kani is used for this item. Conventional
  rstest tables, oracle-based proptest properties, a BDD feature, and an inline
  insta snapshot give proportionate assurance. Rationale: see
  `Verification plan`. The design review endorsed this, noting that the risk
  sits at the scanner, `rowan`, and `chumsky` boundaries, where differential
  properties are more effective than proofs of a small kernel. Date/Author:
  2026-10-18; confirmed 2026-10-23.
- D6. Decision: make `EP-M1` unconditional and give it two corrections. The
  first makes a depth-zero `;` end the key clause, so a keyed relation followed
  by `;` behaves exactly like the keyless relation followed by `;`. The second
  makes the scanner report D-REL-007 for binder lists that are not a non-empty,
  comma-separated list of identifiers. Rationale: without the first, typed
  access would report phantom expressions and lose following declarations.
  Without the second, the scanner and the accessor would disagree about
  well-formedness, so a lint that treats `Err` as "no key" would silently skip
  relations (design review pre-mortem). Both corrections only add diagnostics
  for input that spec §5.5 already rejects. An earlier review option was to drop
  `EP-M1` and let `parse()` report `;` as an error. It was rejected because it
  cannot recover a swallowed same-line declaration. Date/Author: 2026-10-23,
  planning agent after design review; the user can veto this at approval.
- D7. Decision: binders are typed values, `PrimaryKeyBinder { name, span }`,
  not bare strings. Rationale: a lint such as "the key expression references
  only its binder" needs to point at the binder. Adding spans later would break
  a public type just before the ADR-001 crate split. Date/Author: 2026-10-23
  after design review.
- D8. Decision: drop the planned file-based insta snapshot. Use one inline
  `insta::assert_snapshot!` that renders a fixed corpus as
  `binders | text | span | s-expression` in the unit tests. Rationale: the
  repository uses only inline insta snapshots (`src/parser/ast/expr/sexpr.rs`).
  One inline snapshot still guards the three output forms a consumer reads
  against drift. Date/Author: 2026-10-23 after design review.
- D9. Decision: place the scanner split at
  `src/parser/span_scanners/relations/primary_key.rs`, beside the existing
  `relations/cursor.rs` and `relations/preamble.rs`. Rationale: the relation
  scanner already uses a submodule directory, and other scanners'
  `*_support.rs` sibling-file convention does not apply to it. Date/Author:
  2026-10-23.

## Outcomes & retrospective

Not started. Complete this section at each milestone boundary and at completion.

## Context and orientation

The parser pipeline runs in four stages:

1. The tokenizer (`src/tokenizer.rs`) produces `(SyntaxKind, Span)` pairs.
2. The span scanners (`src/parser/span_scanners/`) find the extent of each
   top-level statement.
3. `build_green_tree` (`src/parser/cst_builder/tree.rs`) wraps each extent in
   a CST node.
4. The AST wrappers (`src/parser/ast/`) interpret those nodes on demand.

A "span" is a half-open byte range `start..end` into the source. "Trivia" means
whitespace and comment tokens (`T_WHITESPACE`, `T_COMMENT`). A line comment
token, `//[^\n]*`, does not include its newline. A CRLF line ending is a single
whitespace token.

The relation scanner is `src/parser/span_scanners/relations.rs`, with submodules
`relations/cursor.rs` and `relations/preamble.rs`:

- `parse_relation_suffix` accepts a primary-key clause only on `input`
  relations with record bodies. Other placements produce diagnostics D-REL-004,
  D-REL-006, D-REL-007, and D-REL-008, and the relation node then ends at the
  body.
- `parse_primary_key_clause` consumes `primary`, `key`, and a balanced
  parenthesized binder list. An empty list raises D-REL-007. Then, if
  `next_token_is_same_line_expression` holds, it calls
  `consume_opaque_primary_key_expr`.
- `consume_opaque_primary_key_expr` consumes tokens while tracking delimiter
  depth, until the first newline-containing token at depth zero. That stop
  token can be whitespace, a block comment, or a string literal. If delimiters
  are still open, it returns D-REL-007.
- After the clause, `skip_trivia_end` extends the node over trailing trivia.
- The CST node `N_RELATION_DECL` holds all of these tokens as flat children.
  `primary` and `key` are `T_IDENT` tokens, not keywords.

The AST wrapper is `Relation` in `src/parser/ast/relation.rs`:

- `Relation::primary_key()` (around lines 207 to 242) first checks the body
  with `inspect::inspect_body`.
- It skips the column list with `skip_to_end_of_columns`, which uses
  `extract_delimited` from `src/parser/ast/parse_utils/delimiter.rs`.
- It concatenates the remaining child texts into `rest` and re-tokenizes them.
- It runs `primary_key_clause` from `src/parser/ast/parse_utils/relation.rs`.
  That chumsky parser returns the binder names (it already uses `map_with_span`
  per identifier) and ignores everything after the closing `)`.
- `RelationParseErrors` is `Vec<String>`.
- Inspection helpers live in `src/parser/ast/relation/inspect.rs`, and unit
  tests in `src/parser/ast/relation/tests.rs`.

Expressions are parsed by
`parse_expression(src: &str) -> Result<Expr, Vec<Simple<SyntaxKind>>>` in
`src/parser/expression/pratt.rs`:

- It is re-exported as `ddlint::parser::expression::parse_expression` and
  tokenizes without trivia.
- `Expr` (`src/parser/ast/expr.rs`) derives `PartialEq` but not `Eq`. Its
  s-expression renderer `Expr::to_sexpr` lives in
  `src/parser/ast/expr/sexpr.rs`; for example, `Group` renders as `(group …)`,
  `FieldAccess` as `(field a b)`, and `Tuple` as `(tuple …)`.
- `shift_errors` in `src/parser/span_utils.rs` moves error spans, including a
  `Simple`'s inner `Unclosed` span, by an offset.
- The private `parse_expr_with_offset` in `src/parser/ast/rule_head.rs` trims
  by `char::is_whitespace` (through `trim_byte_range`), parses, and shifts.
  Trimming only whitespace leaves comments in place, so this plan must not
  reuse its trimming step.
- Test builders for expected `Expr` values live in
  `src/test_util/expressions.rs`, for example `var`, `field_access`, `tuple`,
  and `call`.

The current callers of `Relation::primary_key()` are all tests or doctests:

- `src/parser/ast/relation.rs` (module doctest).
- `src/parser/ast/relation/tests.rs` (lines 142, 161).
- `src/parser/ast/relation/inspect.rs` (lines 296, 312).
- `src/parser/tests/relations.rs` (lines 79, 94, 111).
- `src/parser/tests/specs.rs` (line 93, the `RelationSpec::pk` builder used by
  `src/parser/tests/parser.rs`).
- `src/parser/tests/relation_proptest.rs` (line 200).
- `tests/relation_form_grammar.rs` (line 38).

Existing test tooling:

- `Cargo.toml` `[dev-dependencies]` holds `rstest` 0.27, `proptest` 1.6, and
  `insta` 1.47. `googletest`, `pretty_assertions`, and `rstest-bdd` are not yet
  present.
- The repository has no BDD feature files, Kani harnesses, or Verus proofs.
  Kani is installed on the host but has no `Makefile` target. Verus is not
  installed.
- Proptest regression seeds are committed under `proptest-regressions/`.
- `src/parser/tests/` is compiled only under `#[cfg(test)]`, so
  `cargo test --lib <filter>` selects those tests.

The gate commands come from `AGENTS.md` and the `Makefile`: `make fmt`,
`make check-fmt`, `make lint`, `make test`, `make markdownlint`, and
`make nixie`. `make lint` also runs `make spelling`, the Clippy policy tests,
and the Whitaker Dylint suite.

Signposted documentation for the implementer:

- `docs/differential-datalog-parser-syntax-spec-updated.md`: §5.5 relations
  and §10 AST crosswalk.
- `docs/parser-implementation-notes.md`: relation scanner, suffix ownership,
  and expression parser invariants.
- `docs/building-an-error-recovering-parser-with-chumsky.md`: chumsky `Simple`
  errors and span handling.
- `docs/rust-parser-testing-comprehensive-guide.md`: parser test layering,
  property tests, and snapshots.
- `docs/rust-testing-with-rstest-fixtures.md`: fixtures and `#[case]` tables.
- `docs/ddlint-design.md`: §2 on AST wrappers, and ADR-001 layering.
- `docs/complexity-antipatterns-and-refactoring-strategies.md`: keeping the
  new modules small and cohesive.
- `docs/adr-002-relation-role-kind-modelling.md`: the decision this plan
  closes.
- `docs/parser-conformance-register.md`: item 13.
- `docs/developers-guide.md`: lines 216 to 221 on fallible BDD steps and
  fixtures.

Signposted skills:

- `rust-router` first, then:
  - `rust-types-and-apis` for the value types;
  - `rust-errors` for the two error channels;
  - `rust-unit-testing` for rstest, googletest, pretty_assertions, and insta;
  - `proptest` for `EP-M3`;
  - `rust-verification` if the D5 decision is reopened.
- `hexagonal-architecture` for the boundary between the syntax and semantic
  layers.
- `codegraph-mcp` for caller and impact checks.
- `execplans`, `commit-message`, and `en-gb-oxendict-style`.

## Conformance basis

These upstream artefacts are taken at branch base `d1c2571`:

- `docs/roadmap.md` item `2.6.6.1` (`RM-2.6.6.1`).
- `docs/adr-002-relation-role-kind-modelling.md` (status: Proposed):
  - the decision driver "Defer typed primary-key expression access…"
    (`ADR2-DRV`);
  - the `primary_key()` contract paragraph (`ADR2-PK`);
  - the non-goal and risk bullets about primary keys (`ADR2-NG`);
  - "Outstanding decisions" bullet one (`ADR2-OD1`).
- `docs/differential-datalog-parser-syntax-spec-updated.md`:
  - §5.5 `PrimaryKey` production and notes (`SPEC-5.5-PK`);
  - §10 crosswalk (`SPEC-10-REL`).
- `docs/parser-conformance-register.md` item 13 (`PCR-13`).
- `docs/adr-001-parser-crate-split.md`: migration guidance to replace string
  reparsing with typed fields (`ADR1-TYPED`).
- `AGENTS.md`: quality gates and code style (`AGENTS-GATES`).

There is no Terms of Reference document for this item.

Requirements derived for this plan:

- `REQ-PK-EXPR`: a spec-form key expression is available as a typed `Expr`.
- `REQ-PK-BIND`: binders, with spans, remain available and are independent of
  expression validity.
- `REQ-PK-SPAN`: binder, expression, and expression-error spans are
  source-absolute and lie within the clause.
- `REQ-PK-AGREE`: every keyed relation the scanner accepts without
  diagnostics yields `Ok(Some(_))` from `primary_key()`.
- `REQ-PK-LOSSLESS`: CST text and shape are unchanged, and parse-time
  diagnostics are unchanged outside `GAP-SEMI` and `GAP-BINDER`.
- `REQ-PK-DOC`: `ADR2-DRV`, `ADR2-PK`, `ADR2-NG`, and `ADR2-OD1` are
  superseded or closed, and every live document that says "deferred to
  `2.6.6.1`" is updated.
- `GAP-SEMI`: a `;` after a key clause is swallowed into the key expression,
  together with any same-line declaration that follows.
- `GAP-BINDER`: the scanner accepts binder lists that are not identifier
  lists.

Trace links:

```plaintext
SPEC-5.5-PK -> GAP-SEMI -> EP-M1 -> relation_terminators::keyed_semicolon_matches_keyless
SPEC-5.5-PK -> GAP-BINDER -> EP-M1 -> relation_terminators::non_identifier_binders_are_rejected
RM-2.6.6.1 -> REQ-PK-EXPR -> EP-M2 -> relation::primary_key::tests::spec_form_expression_is_typed
RM-2.6.6.1 -> REQ-PK-BIND -> EP-M2, EP-M3 -> relation_primary_key_props::binders_independent_of_expression
ADR1-TYPED -> REQ-PK-SPAN -> EP-M2, EP-M3 -> relation_primary_key_props::parse_errors_lie_within_expression
SPEC-5.5-PK -> REQ-PK-AGREE -> EP-M1, EP-M3 -> relation_primary_key_props::accessor_matches_generated_clause
SPEC-5.5-PK -> REQ-PK-LOSSLESS -> EP-M2 -> existing round-trip tests plus relation_primary_key_props
RM-2.6.6.1 -> EP-M3 -> tests/features/relation_primary_key.feature
ADR2-DRV, ADR2-PK, ADR2-NG, ADR2-OD1, PCR-13, SPEC-10-REL -> REQ-PK-DOC -> EP-M4 -> docs diff and make markdownlint
```

## Verification plan

This section takes a proof-sketch-first approach. It states the abstract model,
the obligations, and the lemmas before fixing the implementation, then assigns
each obligation a proportionate method.

### Abstract model

Model the children of an `N_RELATION_DECL` node after its column list as a
finite token sequence `T = t0 … tn`. Each `ti` has a kind, a text, and an
absolute range `[si, ei)`. Because the CST is contiguous and lossless,
`ei = s(i+1)`. A token is "trivia" if it is whitespace or a comment.

Define the clause parts:

- `k1` is the index of the first non-trivia token. Its text must be
  `primary`; otherwise there is no clause.
- `k2` is the next non-trivia index, whose text is `key`.
- `B = [bo, bc]` is the first balanced parenthesized group after `k2`. `bo`
  is the index of `(` and `bc` is the index of its matching `)`.
- `N` is the binder name tokens inside `B`. The clause is well formed iff the
  non-trivia tokens strictly inside `B` match `ident (',' ident)*`.
- `E` is the non-trivia hull of `t(bc+1) … tn`. It is empty if every such
  token is trivia. Otherwise it runs from the start of the first non-trivia
  token to the end of the last.

The typed result is:

```plaintext
PrimaryKey {
  binders    = [(text(t), range(t)) for t in N],
  span       = [s(k1), end(E) if E is non-empty, else e(bc)),
  expression = Some(text(E), range(E)) if E is non-empty, else None,
}
```

Preconditions: the scanner (after `EP-M1`) reports no diagnostic for the
relation. That guarantees the role is `input`, the body is a record, `B` is
balanced and well formed, and `E`'s tokens are delimiter-balanced and contain
no depth-zero `;` or depth-zero newline-bearing token. Synthetic or malformed
nodes may violate these preconditions. On such nodes the accessor must return
`Err` or `Ok(None)` and must never panic.

### Invariants and lemmas

These are hypotheses about the eventual reasoning, not a fixed proof
decomposition.

- `INV-PART` (lossless partition): `source[range(t)] == text(t)` for every
  binder `t`, and `source[range(E)] == text(E)`. Every binder range lies
  strictly inside `B`. `range(E)` lies within `[e(bc), en)`, so `B` and `E` are
  disjoint and ordered, and the clause span covers both.
- `INV-HULL` (trivia exclusion): when `E` is non-empty, its first and last
  tokens are not trivia, so a trailing same-line comment, or trailing trivia
  absorbed by `skip_trivia_end`, is never part of the expression. Interior
  comments are kept verbatim.
- `INV-AGREE` (scanner–accessor agreement, `REQ-PK-AGREE`): for every relation
  the scanner accepts without diagnostics, `primary_key()` is `Ok(Some(_))`,
  its binders are exactly the names written, and its expression text is exactly
  the expression written. Equivalently, the scanner's acceptance predicate and
  the accessor's well-formedness predicate coincide on scanned input, and the
  scanner's consumed extent ends at `end(E)`.
- `INV-INDEP` (binder independence): the binders depend only on tokens inside
  `B`. Replacing `E` with any other token sequence the scanner accepts, valid
  or invalid as an expression, leaves the binders and the `Ok`-ness of
  `primary_key()` unchanged.
- `INV-SHIFT` (error localization): every error `err` returned by `parse()`
  has `err.span()` within `range(E)`. The end-of-input error is the empty span
  at `end(E)`, which also counts as within. Each span equals the corresponding
  span that `parse_expression(text(E))` reports, shifted by `start(E)`.
- `INV-FIDELITY`: if `parse()` returns `Ok`, the value equals
  `parse_expression(text(E))`.
- `INV-ABSENT`:
  - no clause gives `Ok(None)`;
  - a keyed bracket-body relation or a keyed non-input relation gives
    `Ok(None)`, plus its scanner diagnostic;
  - the list form without an expression gives `Some(key)` with
    `key.expression() == None`.
- `INV-TERM` (`GAP-SEMI`): for any well-formed keyed relation `K` and any
  suffix `S` that starts with `;`, `parse(K + S)` produces the same diagnostic
  messages, the same `Root::relations()` names, and the same top-level
  structure after the relation as `parse(R + S)`. Here `R` is `K` with its key
  clause removed. Spans are compared after adjusting for the removed clause
  length.
- `L-UNIQUE` (lemma): under the preconditions, the first balanced group after
  `key` is unique, so `B` is well defined. A depth counter that starts at one at
  `bo` first returns to zero at exactly one index, `bc`.
- `L-TRIVIA` (lemma): `parse_expression` ignores trivia, because it tokenizes
  with `tokenize_without_trivia`. With `start(E)` at a non-trivia token
  (`INV-HULL`), relative error offsets map to absolute ones by adding
  `start(E)` alone.

### Axioms (trusted, not verified here)

- `AX-ROWAN`: `rowan` preserves token text losslessly, and
  `SyntaxElement::text_range()` is source-absolute for trees built from the
  full source.
- `AX-CHUMSKY`: `parse_expression` reports error spans relative to its input
  string. `shift_errors` adds the offset to every span in each `Simple`,
  including the inner `Unclosed` span, using saturating arithmetic.
- `AX-TOKENIZER`: line-comment tokens exclude the newline, and `\r\n` is a
  single whitespace token.

### Methods per obligation

`INV-PART`, `INV-HULL`, `INV-ABSENT`, and named `INV-SHIFT` cases:

- Method: named rstest cases with googletest matchers. Tests return
  `googletest::Result<()>` and use `verify_that!` with `?`.
- Domain: a finite partition, covering:
  - no clause;
  - list form with one binder, and with several;
  - spec form with and without a trailing line comment;
  - an interior block comment;
  - a leading block comment between `)` and the expression, with a
    hand-computed absolute error span (pre-mortem scenario 3);
  - an expression spanning lines inside open delimiters;
  - end of file without a trailing newline;
  - a keyed bracket body, and a keyed output relation;
  - synthetic malformed nodes: an unclosed binder list and a missing `key`.
- Artefact: `src/parser/ast/relation/primary_key/tests.rs`.
- Evidence: `cargo test --lib relation::primary_key` fails to compile at red,
  because the types do not exist, and passes at green.
- Non-vacuity:
  - every case asserts concrete text and span values;
  - the trailing-comment and leading-comment cases fail if the hull includes
    trivia, or if trimming uses `char::is_whitespace`;
  - the error-span case fails if the shift is dropped.

`INV-AGREE` and `INV-INDEP`:

- Method: proptest, with the generator as the oracle. The properties compare
  accessor output with the generated input, not with values derived from the
  same CST tokens, so they cannot be true by construction.
- Domain:
  - binder lists of one to three identifiers;
  - an optional expression from a small grammar: variables, field access on a
    binder, tuples, calls, and nested groups up to depth three;
  - random inline trivia, including a trailing line comment and line breaks
    only inside open delimiters.
  - Generated expressions never start with `.` or `:-`, and never contain a
    depth-zero `;` or a depth-zero newline-bearing token (multi-line block
    comments and multi-line strings). The scanner deliberately stops at those,
    and that behaviour is covered by the named cases.
  - For `INV-INDEP`, the expression comes from either the valid grammar or an
    invalid mutation that stays balanced and on one line: a stray binary
    operator (`x + * 1`) or a doubled comma inside a tuple (`(x.a,, x.b)`).
    Mutations do not use `@`, which is a valid token.
- Properties:
  - `Parsed::errors()` is empty;
  - `binder_names()` equals the generated names;
  - `expression().map(text)` equals the generated expression text;
  - `source[span] == text` for every binder and for the expression;
  - for `INV-INDEP`, the binders and `Ok`-ness are identical across the
    valid and invalid arms.
- Artefacts:
  - `src/parser/tests/relation_primary_key_strategies.rs`, holding strategies
    and reusing the shape of `GeneratedPrimaryKey` from
    `src/parser/tests/relation_proptest.rs` where practical;
  - `src/parser/tests/relation_primary_key_props.rs`, holding the properties.
- Evidence: `cargo test --lib relation_primary_key_props`.
- Non-vacuity:
  - Class coverage: a plain `#[test]` draws 256 values from each strategy with
    a fixed-seed `proptest::test_runner::TestRunner`. It asserts that the
    with-expression, without-expression, multi-binder, trailing-comment,
    multi-line-delimiter, and invalid-mutation classes all occur. The
    strategies emit an explicit class tag alongside each value for this check.
  - The invalid arm asserts `parse()` is `Err`, so the property is not
    satisfied only by valid expressions.
  - Seeded faults, run once each and reverted, with transcripts recorded in
    `Artefacts and notes`:
    1. include trailing trivia in the hull;
    2. take the binder list from the last balanced group instead of the
       first.
  - After reverting each fault, delete any seed it wrote under
    `proptest-regressions/`.

`INV-SHIFT` and `INV-FIDELITY`:

- Method: proptest plus the named cases above.
- Domain: the invalid-mutation generator, placed after a randomized prefix
  (relation name length, column list, inline trivia), so `start(E)` varies and
  is never zero.
- Properties:
  - every error span lies within `range(E)` and equals the relative span from
    `parse_expression(text)` plus `start(E)`;
  - for valid expressions, `parse()` equals `parse_expression(text())`.
- Non-vacuity: dropping the shift (seeded fault 3) must fail whenever the
  prefix is non-empty, which the generator guarantees.

`INV-TERM`, `GAP-SEMI`, and `GAP-BINDER`:

- Method: named rstest differential cases plus one proptest property.
- Named cases (`EP-M1`):
  - `input relation R(x: u32) primary key (x);` compared with
    `input relation R(x: u32);`;
  - `input relation R(x: R) primary key (x) x.id;` compared with the same
    keyless form;
  - `input relation R(x: u32) primary key (x); output relation S(x: u32)`,
    which must keep `S` in `Root::relations()`;
  - binder lists `(x.y)`, `(1)`, `(,)`, `(x,)`, and `(x y)`, each of which
    must yield exactly one D-REL-007 diagnostic.
- Property: for generated well-formed keyed relations, appending `;` gives
  the same relation names, and diagnostics equal to those of the keyless form
  with `;` appended.
- Artefacts: `src/parser/tests/relation_terminators.rs` (named cases) and the
  property in `relation_primary_key_props.rs`.
- Evidence: the named cases fail before the scanner change and pass after.
- Non-vacuity: the same-line `S` case fails today because `S` is swallowed.

End-to-end behaviour through the public API:

- Method: an `rstest-bdd` feature, with the scenarios listed in `EP-M3`.
- Artefacts: `tests/features/relation_primary_key.feature` and
  `tests/relation_primary_key_bdd.rs`.
- Evidence: the scenarios fail before `EP-M2`, or do not compile, and pass
  after.
- An inline `insta::assert_snapshot!` in
  `src/parser/ast/relation/primary_key/tests.rs` renders the probe corpus as
  `binders | text | span | s-expression` (D8).

### Formal verification decision

Verus is not warranted for this item, and Kani is not used.

The obligations concern agreement between the scanner, `rowan`'s token ranges,
and `chumsky`'s error spans. Each external piece is trusted as an axiom
(`AX-ROWAN`, `AX-CHUMSKY`, `AX-TOKENIZER`). A Verus proof would need models of
those interfaces, so it would prove a model of the integration rather than the
integration itself.

The only pure kernel is the non-trivia hull of the tokens after the first
balanced group: about fifteen lines of straight-line iteration with a single
depth counter. `L-UNIQUE` and `L-TRIVIA` are short arguments recorded above,
not contractual business logic.

Verus is not installed. Wiring it in would mean a toolchain, a `Makefile`
target, and continuous integration (CI) work out of proportion to a syntax
accessor.

The oracle-based properties and the differential `;` property range over the
inputs that matter: layout, binder count, expression validity, and terminators.
Each has a seeded-fault control. A Kani bounded check over token-class
sequences would duplicate that coverage without reaching the `rowan` and
`chumsky` boundary, where the real risk lies.

If the user disagrees, the abstract model above becomes the Verus specification
of a pure `key_expression_hull` function over token-kind slices. A proof
milestone would then run `verus` on that kernel as an explicit acceptance
criterion. That requires a Decision log entry and a new tolerance for toolchain
set-up.

## Plan of work

Stage A (understand; no production code) is `EP-M0`. Stages B (red), C (green),
and D (refactor and documentation) repeat inside each implementation milestone.

Run every gate sequentially in this worktree, teeing its output to
`/tmp/<action>-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out`.

Delete any scratch probe file before running a gate. Repository gates and the
post-turn hook compile every file under `tests/`, and the probe deliberately
uses `eprintln!`.

### `EP-M0` probe (prototype; probe code is never committed)

Write a temporary integration test, `tests/zz_scratch_pk_probe.rs`, that parses
the corpus in `Artefacts and notes`. For each input it prints:

- `Parsed::errors()`;
- the kinds and texts of the root's child nodes (through the public
  `Root::syntax()`, because `AstNode` is crate-private);
- `primary_key()`.

The probe also calls `parse_expression` on each expected key-expression text
and prints its s-expression.

Run it once with `cargo test --test zz_scratch_pk_probe -- --nocapture`. Record
the results in `Artefacts and notes` and `Surprises & discoveries`, then delete
the file.

Acceptance: the transcript is recorded, the scratch file is gone, and
`git status --short` shows only the plan.

### `EP-M1` scanner boundary corrections

Red: create `src/parser/tests/relation_terminators.rs` (with a `//!` doc) and
register it in `src/parser/tests/mod.rs`. Add the `INV-TERM`, `GAP-SEMI`, and
`GAP-BINDER` named cases from `Verification plan`. Run
`cargo test --lib relation_terminators` and observe the failures: the same-line
`S` is missing, and the bad binder lists produce no D-REL-007.

Green:

1. Move `parse_primary_key_clause`, `consume_opaque_primary_key_expr`, and
   `next_token_is_same_line_expression` unchanged from
   `src/parser/span_scanners/relations.rs` into a new submodule,
   `src/parser/span_scanners/relations/primary_key.rs` (D9). Commit this move
   on its own as a refactor once the gates pass.
2. In `next_token_is_same_line_expression`, also reject `T_SEMI`. Add a
   comment explaining that `T_IMPLIES`, `T_DOT`, and `T_SEMI` terminate the
   enclosing statement, so they cannot start a key expression.
3. In `consume_opaque_primary_key_expr`, stop before a depth-zero `T_SEMI`.
4. In `parse_primary_key_clause`, validate the binder list: the non-trivia
   tokens strictly inside the parentheses must match
   `T_IDENT (T_COMMA T_IDENT)*`. Otherwise raise D-REL-007, anchored at the
   list, as the empty-list case already does.
5. Check how the existing tests `relation_invalid` in
   `src/parser/tests/parser.rs` (inputs in `src/parser/tests/programs.rs` lines
   52 to 54) are affected. They use internal relations, so D-REL-006 should
   fire first and they should pass unchanged. Any other change hits the
   parse-diagnostics tolerance.

Refactor, then run all gates. Add a `CHANGELOG.md` `## Unreleased` entry
describing the two diagnostic corrections.

### `EP-M2` typed primary-key model and accessor

Red:

1. Add `googletest = "0.14.3"` and `pretty_assertions = "1.4.1"` to
   `[dev-dependencies]`.
2. Create `src/parser/ast/relation/primary_key/tests.rs` (with a `//!` doc).
   Put the named cases from `Verification plan` there as rstest tables, with
   `#[gtest]` placed before `#[rstest]` and tests returning
   `googletest::Result<()>`. Use matchers such as `eq`, `some`, `none`,
   `elements_are!`, and `property!`.
3. Add the inline insta snapshot (D8).
4. Update every caller listed in `Context and orientation` to the new return
   type:
   - `RelationSpec::pk` in `src/parser/tests/specs.rs` compares
     `binder_names()`;
   - comparisons of whole `PrimaryKey` values and vectors use
     `pretty_assertions::assert_eq` for readable diffs.
5. Compile. Expect compiler error `E0412` reporting that type `PrimaryKey`
   cannot be found, or a mismatched-types error.

Green:

1. Create `src/parser/ast/relation/primary_key.rs` with a `//!` module
   comment. Move `Relation::primary_key()`, `skip_to_end_of_columns`, and
   `first_non_trivia_text` into it as an `impl Relation` block, which shrinks
   `relation.rs`.
2. Define `PrimaryKey`, `PrimaryKeyBinder`, and `PrimaryKeyExpression` as
   specified in `Interfaces and dependencies`.
3. Implement the accessor over CST tokens. This is simpler than the
   `rest`-offset route and needs no chumsky change:
   1. Run the body inspection and `skip_to_end_of_columns` as today.
   2. Collect the remaining child tokens (`SyntaxToken`s) with their absolute
      `text_range()`.
   3. If the first non-trivia token is not `primary`, return `Ok(None)`.
   4. Keep `primary_key_clause` over the re-tokenized `rest` to validate the
      clause and produce today's error messages. Extend its identifier
      `map_with_span` to return `(name, relative span)` pairs, and convert them
      to absolute spans by adding `rest_start`, the start offset of the first
      remaining token.
   5. Locate the binder group's closing `)` in the CST tokens with a depth
      counter (`L-UNIQUE`).
   6. Take the first and last non-trivia tokens after it as the hull of `E`.
   7. Take the clause span from the `primary` token's start to the hull end,
      or to the `)` end when `E` is empty.
4. Implement `PrimaryKeyExpression::parse()` as `parse_expression(&self.text)`
   with `shift_errors(errs, self.span.start)`. Never trim by
   `char::is_whitespace`. The text is already bounded by non-trivia tokens.
5. Re-export the three types from `src/parser/ast/mod.rs`, next to
   `Relation`.
6. Update the module doctest in `relation.rs`, and add doctests to the new
   public items, per `AGENTS.md`. Doctests use `.expect()`, because neither
   error type implements `std::error::Error`.

Refactor: keep functions small, check line counts for `relation.rs` and
`primary_key.rs`, then run all gates.

### `EP-M3` property and behavioural verification

1. Add `relation_primary_key_strategies.rs` and `relation_primary_key_props.rs`
   under `src/parser/tests/`, and register both in `src/parser/tests/mod.rs`.
   Implement the `INV-AGREE`, `INV-INDEP`, `INV-SHIFT`, `INV-FIDELITY`, and
   `INV-TERM` properties and the class-coverage test.
2. Run seeded faults 1 to 3 once each, record the failing transcripts, revert,
   and delete any seed files they wrote.
3. In a separate commit, add `rstest-bdd = "0.6.0"` and
   `rstest-bdd-macros = "0.6.0"` to `[dev-dependencies]`, with a one-scenario
   smoke binding. The dependency tolerance applies here.
4. Write `tests/features/relation_primary_key.feature` (text below) and
   `tests/relation_primary_key_bdd.rs`, with a `//!` doc, step definitions, and
   one `#[scenario]` binding per scenario.
   - Step definitions are fallible: they return `Result` and use `?`, never
     `.expect()`, per `docs/developers-guide.md` lines 216 to 221.
   - State passes between steps through an rstest fixture holding the parsed
     result.

Feature specification for `tests/features/relation_primary_key.feature`:

```gherkin
Feature: Typed access to relation primary-key expressions

  Scenario: Spec-form key exposes a typed tuple expression
    Given the DDlog source "input relation Book(row: BookRow) primary key (row) (row.author, row.title)"
    When the first relation's primary key is inspected
    Then the primary-key binders are "row"
    And the primary-key expression text is "(row.author, row.title)"
    And the primary-key expression renders as "(tuple (field row author) (field row title))"

  Scenario: List-form key has binders but no expression
    Given the DDlog source "input relation R(a: u32, b: u32) primary key (a, b)"
    When the first relation's primary key is inspected
    Then the primary-key binders are "a, b"
    And the primary key has no expression

  Scenario: Trailing comment is not part of the key expression
    Given the DDlog source "input relation R(x: R) primary key (x) x.id // the id"
    When the first relation's primary key is inspected
    Then the primary-key expression text is "x.id"

  Scenario: Malformed key expression keeps binders and reports a located error
    Given the DDlog source "input relation R(x: R) primary key (x) x + * 1"
    When the first relation's primary key is inspected
    Then the primary-key binders are "x"
    And parsing the primary-key expression fails within its span
    And the parse has no parser-level errors

  Scenario: Statement terminator after a key does not hide the next declaration
    Given the DDlog source "input relation R(x: u32) primary key (x); output relation S(x: u32)"
    When the relations are listed
    Then the relation names are "R, S"

  Scenario: Key on an output relation is rejected and not exposed
    Given the DDlog source "output relation R(x: u32) primary key (x)"
    When the first relation's primary key is inspected
    Then the relation has no primary-key clause
    And the parse reports the diagnostic "D-REL-006"

  Scenario: Relation without a key has no primary-key clause
    Given the DDlog source "input relation R(x: u32)"
    When the first relation's primary key is inspected
    Then the relation has no primary-key clause
```

Before writing the steps, validate the exact expected s-expressions, error
behaviour, and diagnostic text against the `EP-M0` probe. If the probe differs,
the probe wins: record the difference in `Surprises & discoveries` and update
the feature text. The D-REL-006 step matches on the diagnostic message defined
in `src/parser/diagnostics/`. Confirm its exact wording there.

### `EP-M4` documentation, ADR amendment, changelog, roadmap closure

1. In `docs/adr-002-relation-role-kind-modelling.md`:
   - supersede the `ADR2-DRV` decision driver explicitly;
   - replace the `ADR2-PK` paragraph with the `PrimaryKey` shape;
   - update the `ADR2-NG` non-goal and risk bullets;
   - add a dated "Amendment: primary-key expression access (2.6.6.1)" section
     recording D1 to D9 and the invariants that shape the API (`INV-INDEP`,
     `INV-SHIFT`, `INV-AGREE`);
   - remove `ADR2-OD1` from "Outstanding decisions".
2. In `docs/differential-datalog-parser-syntax-spec-updated.md`:
   - rewrite the §5.5 note about deferred typed access;
   - state that a binder list must be identifiers, and that a `;` ends the
     clause;
   - update the §10 crosswalk to `primaryKey?: { binders, expr? }`.
3. In `docs/parser-conformance-register.md`, update item 13's "Current
   behaviour" and remove the deferral sentence.
4. In `docs/parser-implementation-notes.md`, update the relation section that
   mentions `2.6.6.1`. Document the lazy parse, the absolute spans, the
   non-trivia hull, and the terminator rule.
5. In `docs/developers-guide.md`:
   - update the `relation.rs` entry, documenting `PrimaryKey`,
     `PrimaryKeyBinder`, and `PrimaryKeyExpression`;
   - add their reuse policy: they are the only supported way to read key
     clauses, and lint rules must not re-tokenize relation text;
   - document the googletest convention (`#[gtest]` before `#[rstest]`,
     returning `googletest::Result<()>`);
   - document the rstest-bdd convention: feature files under
     `tests/features/`, fallible steps, and `#[scenario]` bindings.
6. In `docs/users-guide.md`, replace the "deferred" sentence, and describe the
   two diagnostic corrections from `EP-M1`.
7. In `docs/ddlint-design.md` §2, add one sentence pointing to the ADR-002
   amendment.
8. In `CHANGELOG.md`, add the `Relation::primary_key()` return-type change
   under `## Unreleased` → `### Breaking changes`, if `EP-M1` has not already
   added it.
9. Confirm `docs/contents.md` lists this ExecPlan (added with the plan).
10. In `docs/roadmap.md`, mark `2.6.6.1` done.
11. Run `make fmt`, `make markdownlint`, and `make nixie`.

## Milestones and plateaus

`EP-M0`: probe findings recorded, and the plan updated.

- Requirements and gaps: confirms `GAP-SEMI` and `GAP-BINDER`, and validates
  the corpus.
- Acceptance evidence: the probe transcript in `Artefacts and notes`.
- Conformance check, one line per item:
  - no code change;
  - design still followed;
  - upstream assumptions re-checked against the probe output;
  - no interface, dependency, trust-boundary, or format change;
  - trace links current.
- Recovery: re-run the probe; delete `tests/zz_scratch_pk_probe.rs` if a run
  aborts.
- Remaining gaps: everything else.
- Compatibility decision: none.

`EP-M1`: a keyed relation followed by `;` behaves like a keyless one, binder
lists must be identifiers, and the relation scanner is split under the line cap.

- Requirements and gaps: `GAP-SEMI`, `GAP-BINDER`, and the scanner half of
  `REQ-PK-AGREE`.
- Acceptance evidence: `relation_terminators` cases fail before and pass
  after, and the full gates pass.
- Conformance check, one line per item:
  - the only diagnostic changes are the two corrections;
  - scanner suffix ownership stays private to `span_scanners/relations`;
  - no interface or dependency change;
  - no format change.
- Recovery: `git revert` the milestone commits in reverse order.
- Remaining gaps: typed access.
- Compatibility decision: none.

`EP-M2`: the typed API exists and every caller uses it.

- Requirements and gaps:
  - `REQ-PK-EXPR` and `REQ-PK-BIND`;
  - `REQ-PK-SPAN`, at example level;
  - `REQ-PK-LOSSLESS`, through the existing round-trip tests.
- Acceptance evidence: the new unit tests and updated tests pass, and
  `make check-fmt`, `make lint`, and `make test` pass.
- Conformance check, one line per item:
  - public API change limited to `Relation::primary_key()` and the three new
    types;
  - no CST change;
  - no new runtime dependency.
- Recovery: revert the milestone commit. The callers and the API change
  together, so no partial state exists.
- Remaining gaps: generated-input evidence and the BDD feature.
- Compatibility decision: none. The API is pre-1.0 with no external
  consumers.

`EP-M3`: properties, the class-coverage test, and the BDD scenarios pass, and
each seeded fault was seen to fail.

- Requirements and gaps: `REQ-PK-AGREE`, `REQ-PK-SPAN`, and `REQ-PK-BIND` at
  property level.
- Acceptance evidence: the test names in `Conformance basis`, plus the
  seeded-fault transcripts.
- Conformance check: dev-dependencies are limited to the four authorized.
- Recovery: revert the milestone commits.
- Remaining gaps: documentation.
- Compatibility decision: none.

`EP-M4`: documentation reconciled, and the roadmap item done.

- Requirements and gaps: `REQ-PK-DOC`.
- Acceptance evidence:
  - `rg -n "2\.6\.6\.1" docs --glob '!docs/execplans/**'` shows only the
    roadmap entry (now done) and the ADR-002 amendment;
  - no "deferred" wording remains in the ADR-002, developers' guide, users'
    guide, conformance register, or implementation notes passages listed in
    `EP-M4`;
  - `make markdownlint` and `make nixie` pass.
- Conformance check: the ADR-002 amendment matches the shipped API.
- Recovery: revert the documentation commits.
- Remaining gaps: none.
- Compatibility decision: none.

## Concrete steps

Run everything from the worktree root,
`/home/leynos/.lody/repos/github---leynos---ddlint/worktrees/6d7d7a04-94f2-4639-a88c-55e3de2c2b68`.

Focused red-green loops:

```sh
cargo test --lib relation_terminators 2>&1 | tee /tmp/test-focused-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
cargo test --lib relation::primary_key 2>&1 | tee /tmp/test-focused-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
cargo test --lib relation_primary_key_props 2>&1 | tee /tmp/test-focused-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
cargo test --test relation_primary_key_bdd 2>&1 | tee /tmp/test-focused-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
```

Expected results:

- at the `EP-M2` red stage: compiler error `E0412` reporting that type
  `PrimaryKey` cannot be found;
- at green: `test result: ok.` with the new case count.

At each milestone boundary, run the gates sequentially:

```sh
make fmt 2>&1 | tee /tmp/fmt-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
make check-fmt 2>&1 | tee /tmp/check-fmt-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
make lint 2>&1 | tee /tmp/lint-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
make test 2>&1 | tee /tmp/test-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
make markdownlint 2>&1 | tee /tmp/markdownlint-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
make nixie 2>&1 | tee /tmp/nixie-ddlint-2-6-6-1-ast-access-for-spec-form-primary-key-exprs.out
```

Prefer delegating the gate sequence to the `scrutineer` agent, then read the
cited logs on failure. Commit after each green milestone, following the
`AGENTS.md` commit conventions.

## Validation and acceptance

The work is done when all of the following hold:

1. Parsing
   `input relation Book(row: BookRow) primary key (row) (row.author, row.title)`
   gives `primary_key() == Ok(Some(key))`, where:
   - `key.binder_names() == ["row"]`;
   - the binder span covers `row` inside the parentheses;
   - `key.expression().map(|e| e.text())` is `Some("(row.author, row.title)")`;
   - `parse().map(|e| e.to_sexpr())` is
     `Ok("(tuple (field row author) (field row title))")`.
2. Parsing `input relation R(x: R) primary key (x) x + * 1` gives:
   - no `Parsed::errors()`;
   - binder names `["x"]`;
   - a `parse()` result of `Err` whose every span lies inside the expression
     span.
3. Parsing `input relation R(x: u32) primary key (x); output relation S(x: u32)`
   lists relations `R` and `S`, with the same diagnostics as the keyless form.
4. Parsing `input relation R(x: u32) primary key (x.y)` gives exactly one
   D-REL-007 diagnostic.
5. `make check-fmt`, `make lint`, `make test`, `make markdownlint`, and
   `make nixie` pass. Each new test named in `Conformance basis` fails before
   its milestone's change (or fails to compile for missing types) and passes
   after.
6. Each seeded fault in `Verification plan` was run once, failed for the
   intended reason, and was reverted, with transcripts recorded below.

Quality criteria:

- Tests: all of `make test`.
- Verification: the `INV-*` obligations discharged by the methods above;
  Verus and Kani are not used (D5).
- Lint: `make lint` passes, including Clippy, Whitaker, and spelling.
- Performance: no requirement; key expressions are parsed only on request.
- Security: no new trust boundary.

## Idempotence and recovery

Every step can be re-run. The probe file is temporary and must be deleted
before any gate runs. Seeded faults are reverted immediately, and their
regression seeds deleted. Each milestone is one or more commits, which can be
reverted independently in reverse order. If a gate is interrupted, re-run it;
do not start a second, concurrent build.

## Artefacts and notes

Probe corpus for `EP-M0`, with the expectations to confirm:

```plaintext
input relation Book(row: BookRow) primary key (row) (row.author, row.title)
  -> expression (tuple (field row author) (field row title))
input relation R(a: u32, b: u32) primary key (a, b)
  -> no expression
input relation R(x: u32) primary key (x);
  -> GAP-SEMI: today the expression is ";"; after EP-M1 it is None
input relation R(x: u32) primary key (x) (x.a,\n x.b)\noutput relation S(x: u32)
  -> expression spans two lines; S listed separately
input relation R(x: R) primary key (x) x.y // c
  -> expression "x.y"
input relation R(x: R) primary key (x)\n  x.y
  -> expression None; x.y scanned separately
input relation R(x: R) primary key (x) hash(x.a)\r\n
  -> call expression; CRLF is trivia
input relation R(x: R) primary key (x) x + * 1
  -> parse() Err, binders ["x"]
input relation R(x: R) primary key (x) x.y /* c */
  -> expression "x.y"
```

Probe transcript (2026-10-23,
`cargo test --test zz_scratch_pk_probe -- --nocapture` against `d1c2571`,
condensed; `pk` is today's binder-only `primary_key()`):

```plaintext
SRC "input relation R(x: u32) primary key (x);\n"
  errors=[]  N_RELATION_DECL="input relation R(x: u32) primary key (x);\n"  pk=Ok(Some(["x"]))
SRC "input relation R(x: u32);\n"
  errors=[]  N_RELATION_DECL="input relation R(x: u32)"  pk=Ok(None)
SRC "input relation R(x: R) primary key (x) x.y // c\n"
  errors=[]  N_RELATION_DECL="… primary key (x) x.y // c\n"  pk=Ok(Some(["x"]))
SRC "input relation R(x: R) primary key (x)\n  x.y\n"
  errors=[Unexpected at 41..45]  N_RELATION_DECL="… primary key (x)\n  "  pk=Ok(Some(["x"]))
SRC "input relation R(x: R) primary key (x) (x.a,\n x.b)\noutput relation S(x: u32)\n"
  errors=[]  two N_RELATION_DECL nodes (R with the two-line key, then S)
SRC "input relation Book(row: BookRow) primary key (row) (row.author, row.title)\n"
  errors=[]  pk=Ok(Some(["row"]))
SRC "input relation R(x: R) primary key (x) x + * 1\n"
  errors=[]  pk=Ok(Some(["x"]))
SRC "input relation R(x: R) primary key (x) hash(x.a)\r\n"
  errors=[]  N_RELATION_DECL includes the trailing "\r\n"
SRC "input relation R(x: R) primary key (x) x.y /* c */\n"
  errors=[]  N_RELATION_DECL includes " /* c */\n"
EXPR "(row.author, row.title)" -> Ok("(tuple (field row author) (field row title))")
EXPR "x + * 1"   -> Err([custom "unexpected token: T_STAR" at 4..5,
                         custom "missing right-hand side for operator" at 2..3])
EXPR "hash(x.a)" -> Ok("(call hash (field x a))")
EXPR "(x.name)"  -> Ok("(group (field x name))")
EXPR "x.y"       -> Ok("(field x y)")
EXPR ";"         -> Err([custom "unexpected token: T_SEMI" at 0..1])
```

Probe conclusions:

- `GAP-SEMI` is confirmed. With a key, the `;` and the trailing newline sit
  inside `N_RELATION_DECL`, and the opaque expression is `;`. Without a key,
  the node ends before the `;`, which stays a root-level token, and no
  diagnostic is raised. `EP-M1` must reproduce the keyless behaviour.
- A key expression on the line after the binder list is not silently lost.
  The scanner leaves `x.y` outside the relation, and the parse reports an error
  at `41..45`. This is the `2.6.6` same-line rule and stays out of scope.
- Trailing trivia, CRLF and comments included, is inside the relation node,
  so `INV-HULL` trimming is required.
- `parse_expression` returns errors not sorted by position (`4..5` before
  `2..3`). Tests must compare error-span sets, or sort them, rather than assume
  an order.
- Every expected s-expression in the feature file matches the probe.

## Interfaces and dependencies

Define these items in `src/parser/ast/relation/primary_key.rs` and re-export
them from `src/parser/ast/mod.rs`:

```rust
/// A `primary key (binders) expr?` clause on an input relation.
#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKey { /* binders: Vec<PrimaryKeyBinder>, span: Span, expression: Option<PrimaryKeyExpression> */ }

impl PrimaryKey {
    /// Binders from the parenthesized list, in source order.
    #[must_use]
    pub fn binders(&self) -> &[PrimaryKeyBinder];
    /// Binder names in source order; convenience over `binders()`.
    #[must_use]
    pub fn binder_names(&self) -> Vec<&str>;
    /// Source span from `primary` to the end of the clause, excluding trailing trivia.
    #[must_use]
    pub fn span(&self) -> Span;
    /// The trailing key expression, when the spec form is used.
    #[must_use]
    pub fn expression(&self) -> Option<&PrimaryKeyExpression>;
}

/// One binder name inside `primary key ( ... )`.
#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKeyBinder { /* name: String, span: Span */ }

impl PrimaryKeyBinder {
    #[must_use]
    pub fn name(&self) -> &str;
    /// Source-absolute span of the binder identifier.
    #[must_use]
    pub fn span(&self) -> Span;
}

/// The trailing expression of a spec-form primary key.
#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKeyExpression { /* text: String, span: Span */ }

impl PrimaryKeyExpression {
    /// Source text bounded by its first and last non-trivia tokens.
    #[must_use]
    pub fn text(&self) -> &str;
    /// Source-absolute span of `text()`.
    #[must_use]
    pub fn span(&self) -> Span;
    /// Parse the expression on demand.
    ///
    /// # Errors
    /// Returns expression parse errors whose spans are source-absolute and lie
    /// within `span()`.
    pub fn parse(&self) -> Result<Expr, Vec<chumsky::error::Simple<SyntaxKind>>>;
}

impl Relation {
    /// # Errors
    /// Returns [`RelationParseErrors`] when a clause is present but its binder
    /// list or the relation body is malformed; never because of the key
    /// expression.
    pub fn primary_key(&self) -> Result<Option<PrimaryKey>, RelationParseErrors>;
}
```

The types derive `PartialEq` but not `Eq`, so they can later cache an `Expr` or
wrap a CST node without breaking callers. They own their text (`String`), so a
later `N_EXPR_NODE`-backed implementation (D1) can keep the same signatures.

Reuse policy:

- The three types are syntax-layer value types, owned by
  `src/parser/ast/relation/`.
- Lint rules and `src/sema` read primary keys only through
  `Relation::primary_key()`.
- No other module re-tokenizes relation suffix text.
- Binder interpretation (row or column) and any key-expression scope belong
  to the semantic layer (D4).

Existing helpers are reused rather than duplicated:

- `parse_expression` (`src/parser/expression/pratt.rs`);
- `shift_errors` (`src/parser/span_utils.rs`);
- `extract_delimited` (`src/parser/ast/parse_utils/delimiter.rs`);
- `primary_key_clause` (`src/parser/ast/parse_utils/relation.rs`), extended
  to return binder spans;
- `tokenize_with_trivia`.

No new shared helper is introduced. The trim step of `parse_expr_with_offset`
is deliberately not reused, because it trims whitespace but not comments.

New dev-dependencies, all with caret requirements:

- `googletest = "0.14.3"`;
- `pretty_assertions = "1.4.1"`;
- `rstest-bdd = "0.6.0"`;
- `rstest-bdd-macros = "0.6.0"`.

## Revision note

- 2026-10-18: first draft, based on Wyvern reconnaissance and upstream grammar
  research.
- 2026-10-23: applied the community-of-experts design review.
  - Restated D1 as the non-trivia hull, and recorded the `N_EXPR_NODE`
    alternative as the crate-split migration path.
  - Made `EP-M1` unconditional and added binder-list validation (D6).
  - Renamed `expr()` to `parse()`, added binder spans (D7), and dropped `Eq`.
  - Replaced the tautological agreement property with oracle-based
    properties, a deterministic class-coverage test, and seed-file hygiene.
  - Required fallible BDD steps and googletest `Result` tests.
  - Replaced the file snapshot with an inline one (D8).
  - Added `CHANGELOG.md` and explicit ADR-002 driver supersession, and
    excluded historical ExecPlans from the documentation grep.
  - Raised the tolerances and defined how they are counted.

  The remaining work is unchanged in order; `EP-M1` now always runs.
