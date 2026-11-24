# 2.6.9 align primary-key clauses and import aliases with upstream

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (redesigned for the upstream-fidelity policy; awaiting approval)

Branch: `2-6-9-enforce-lc-name-case-restriction-on-primary-key-binders`

Roadmap item: `2.6.9` in `docs/roadmap.md`.

## Purpose / big picture

`ddlint` lints programs written for VMware's Differential Datalog (DDlog). Its
span scanner finds declarations in the token stream and reports most
declaration errors; typed accessors over the concrete syntax tree (CST) then
expose each declaration to lint rules.

A DDlog relation may declare a primary key. Upstream reads the clause as
`primary key`, one parenthesized *binder* (a lowercase variable standing for
the row), and a required expression that computes the key from that row:

```haskell
key_expr = withPos $ KeyExpr nopos <$> (parens varIdent) <*> expr
```

ddlint accepts three forms that upstream rejects: a capitalized binder, several
binders, and no expression. After this change each reports `D-REL-007`:

```ddlog
input relation A(x: u32) primary key (x) x.id    // accepted
input relation B(x: u32) primary key (Foo) Foo.x // D-REL-007 at Foo
input relation C(a: u32) primary key (a, b) a    // D-REL-007 at (
input relation D(x: u32) primary key (x)         // D-REL-007 at )
```

Import aliases need the opposite correction. The specification requires a
lowercase alias, but upstream accepts any case and a scoped path, and an
upstream program uses `import tinyset as TS`. ddlint will accept
`import foo as Baz` and `import foo as a::b`, and `Import::alias()` will return
the whole alias.

## Constraints

- Implementation begins only after explicit user approval.
- Upstream DDlog at commit `ca5ded823dc2c01bc33aac44e526c12d2fed03a6` is the
  authority. No intentional superset or subset is recorded.
- Violations reuse `D-REL-007` and its canonical message. No new code or
  message is added.
- ddlint is unreleased. Callers and tests change together; there are no shims,
  migration diagnostics, migration-guide entries, or breaking-change framing.
- Authorized public API change: `PrimaryKey` exposes `binder()` and a
  non-optional `expression()`; `binders()` and `binder_names()` are removed.
- No dependency, configuration, build, or CI change.
- Source files stay at or below 400 lines; every module starts with `//!`;
  Whitaker's macro-aware cognitive complexity limit is 9. Test assertions carry
  diagnostics. Production code does not use `expect`.
- Do not edit completed ExecPlans.
- Out of scope: transformer declarations (2.6.16), where keys may be placed
  (2.6.17), and `;` at statement level (2.6.10).

## Tolerances (exception triggers)

Compare with `git merge-base HEAD origin/main`, excluding `docs/execplans/`.
Count net lines (additions minus deletions) with `git diff --numstat`. Enforce
each category independently. Code is `*.rs`, `*.dl`, and `*.feature`;
configuration is Cargo, Make, CI, and lint settings; documentation is other
Markdown. Files under `proptest-regressions/` are excluded but must be reviewed.

- Code: stop beyond 38 files or 900 net lines.
- Documentation: stop beyond 14 files or 350 net lines.
- Configuration: stop on any change.
- Public API: stop if more than the authorized change is needed.
- Scanner: stop if statement-boundary or `;` handling must change.
- Gates: stop after three unsuccessful fix rounds.
- Time: stop if a milestone exceeds four active hours.
- Ambiguity: stop if evidence challenges a decision below.

Budget rationale: 25 code files mention `primary key`, and expression-less
fixtures must be migrated rather than deleted. Imports add three files, helper
consolidation three, and new modules and tests about six, giving about 37.
Migration is mostly one-for-one, so new tests dominate net lines. Plan 2.6.6.1
set 1,100 net Rust lines and finished at 2,062; 2.6.8 set 1,600 and finished at
3,120. Both overran on tests. This redesign needs fewer tests because the
grammar is now fixed-size.

## Risks

- Multi-line declarations. Upstream style puts `primary key` on its own line,
  but `skip_line()` skips only the line on which the declaration starts.
  Severity: medium. Likelihood: high. A rejected second-line clause may be
  rescanned and report twice. `EP-M0` measures this before design is fixed.
- Expression on a later line. Upstream ignores newlines; ddlint reads an
  expression only on the same line as `)`. Requiring one would reject valid
  input unless the search crosses newlines (`D5`). Severity: medium.
  Likelihood: high without `D5`.
- Fixture migration hides a regression. Severity: medium. Likelihood: medium.
  Migrate in its own commit, changing only key text and expected values.

## Progress

Times are commit times in UTC, taken from git.

- [x] (2026-11-23 17:17Z) Reconnaissance and first draft committed.
- [x] (2026-11-23 17:34Z) First expert review applied.
- [x] (2026-11-24) User set the upstream-fidelity policy; plan redesigned from
  the upstream source.
- [ ] Plan approved by the user.
- [ ] `EP-M0` probe current behaviour.
- [ ] `EP-M1` align the clause and accessor.
- [ ] `EP-M2` property and fault evidence.
- [ ] `EP-M3` align import aliases.
- [ ] `EP-M4` consolidate case predicates.
- [ ] `EP-M5` documentation and closure.

## Surprises & discoveries

- All 16 keys in the 126 upstream `.dl` files use one lowercase binder and an
  expression. None puts the expression on a later line than `)`, though the
  grammar permits it.
- Upstream uses an uppercase alias, `tinyset as TS`. No upstream import ends in
  `;`, and upstream has no top-level `;`.
- `_` is reserved upstream, as in ddlint, so `primary key (_)` is malformed.
- `examples/primary_key_and_index.dl` puts a key on a bracket-bodied relation.
  This is valid upstream, but ddlint's scanner reports `D-REL-004` (2.6.17).
- Upstream `Validate.hs` lines 274 to 277 reject keys on non-`input` relations,
  streams, and multisets. ddlint enforces only the first.
- Upstream transformer names are `ucScopedIdentifier`, with typed `->` outputs.
  Register item 12 and roadmap item 2.6.5 implemented the opposite.
- Corrections to earlier drafts of this plan: `Progress` times were invented;
  upstream quotations came from an LLM-extracted page rather than the source;
  the scanner's control flow was misdescribed; requested `googletest` and
  `pretty_assertions` usage was missing; and the claim that length-7
  enumeration exceeded the automaton bound was wrong (six plus six states needs
  length 10). One seeded fault also targeted a test that could not detect it.
  The redesign removes the automaton argument.

## Decision log

- D1. Match `key_expr` exactly: one `LcName` binder, then a required expression.
  Rejected: enforcing case alone, which would preserve two known deviations in
  the same production. User policy, 2026-11-24.
- D2. Reuse `D-REL-007` for all three forms. The span identifies the fault.
- D3. Anchor at the binder when the group holds exactly one identifier of the
  wrong case; at `(` for any other malformed group, as now; and at `)` when the
  expression is missing.
- D4. Represent the contract in the type: one binder and one expression.
  Rejected: keeping a vector and an option that can never vary.
- D5. Look for the expression across newlines. If the next significant token is
  on a later line and can begin a declaration (a declaration keyword, `#`, or a
  `UcName`), report the expression missing. This wrongly rejects a later-line
  expression starting with an uppercase constructor. The corpus contains none;
  record it as a defect in the register, not as an intentional subset. The
  proper fix is grammar-driven expression extent.
- D6. Import aliases follow `modname`: any case, optionally scoped.
- D7. Use no Verus or Kani; see `Formal verification decision`. Requires user
  approval.
- D8. One crate-private `ident_case` module owns `is_lc_name` and `is_uc_name`.
- D9. Transformer alignment is decided (user, 2026-11-24). It replaces a
  different production, scanner, and accessor, so it is delivered as 2.6.16.
- D10. A malformed clause reports `D-REL-007` whatever the relation's role or
  body, as malformed lists already do. Upstream parses before it validates
  roles. This reverses the first review's `D10`: a capitalized binder on an
  `output` relation changes from `D-REL-006`, with the relation exposed, to
  `D-REL-007`, with it dropped. It also avoids a two-stage verdict.

## Outcomes & retrospective

Not started.

## Context and orientation

Inspected at `main` commit `d8343aec` plus this branch's documentation. Find
symbols with `grep -rn '<symbol>' src tests`.

`parse_relation_suffix` in `src/parser/span_scanners/relations/suffix.rs`
checks for a bracket-wrapped clause (`D-REL-008`), a bracket body
(`D-REL-004`), and a non-`input` role (`D-REL-006`). The last two call
`parse_primary_key_clause(...)?` first, so a malformed clause already wins.
`parse_primary_key_clause` checks the group with `is_binder_identifier_list`,
which reads token kinds only, then reads an optional expression using
`next_token_is_same_line_expression` and `consume_opaque_primary_key_expr`. The
latter stops at a depth-zero `;` or newline.

`src/parser/ast/relation/primary_key/clause.rs` re-tokenizes the suffix and
parses binders with `primary_key_clause` in
`src/parser/ast/parse_utils/relation.rs`. No production code outside this
module calls `binders()` or `binder_names()`.

`src/parser/span_scanners/imports.rs` reads a one-identifier alias with no end
anchor. `Import::alias()` in `src/parser/ast/import.rs` returns the first
identifier after `as`.

Private case helpers exist in `span_scanners/transformers.rs`,
`pattern/mod.rs`, and `expression/qualified.rs`, all under `src/parser/`.

Tests use `rstest` 0.28, `rstest-bdd` 0.6, `googletest` 0.14, `proptest` 1.6,
`insta` 1.47, and `pretty_assertions` 1.4. The `cli/` crate does not call the
parser, so `ddlint::parse` is the end-to-end boundary.

Relevant documents: the syntax specification, sections 2.2, 5.2, and 5.5; the
conformance register, items 12 and 13; `docs/parser-implementation-notes.md`;
`docs/ddlint-design.md`; `docs/users-guide.md`; `docs/developers-guide.md`;
ADR-0002; `docs/building-an-error-recovering-parser-with-chumsky.md`;
`docs/rust-parser-testing-comprehensive-guide.md`;
`docs/rust-testing-with-rstest-fixtures.md`; and
`docs/complexity-antipatterns-and-refactoring-strategies.md`. Skills:
`execplans`, `rust-router`, `rust-unit-testing`, `proptest`,
`rust-verification`, `hexagonal-architecture`, and `codegraph-mcp`. The case
and binder policies are pure functions; scanner and accessor adapt to them.

## Conformance basis

No Terms of Reference exists. The basis is roadmap 2.6.9, specification
sections 2.2, 5.2, and 5.5, register item 13, ADR-0002, and upstream `Parse.hs`
lines 110 to 121, 146, 149, 287 to 289, and 334 to 360.

- `REQ-BINDER`: exactly one `LcName` binder.
- `REQ-EXPR`: an expression is required.
- `REQ-AGREE`: scanner and accessor accept the same clauses.
- `REQ-ALIAS`: aliases follow `modname`.
- `REQ-DOCS`: live documents state the upstream contract.

```plaintext
Parse.hs key_expr -> spec 5.5 -> REQ-BINDER, REQ-EXPR -> EP-M1 -> primary_key_clause_contract
ADR-0002 INV-AGREE -> REQ-AGREE -> EP-M1, EP-M2 -> scanner_and_accessor_agree
Parse.hs modname -> spec 5.2 -> REQ-ALIAS -> EP-M3 -> imports::alias_forms
```

ADR-0002's amendment describes compound binders and an optional expression.
Approval of this plan authorizes a dated superseding amendment.

## Verification plan

### Abstract model

Let `g` be the non-trivia tokens in the binder group and `e` the first
significant token after it. `LC(t)` means `t` matches
`^(_|[a-z])[A-Za-z0-9_]*$`.

```plaintext
Binder(g) = Ok(t)     if g = [T_IDENT t] and LC(t)
          = NonLc(t)  if g = [T_IDENT t] and not LC(t)
          = Malformed otherwise
Clause    = accepted  iff Binder(g) = Ok and e can start an expression (D5)
```

The group must be balanced; an unclosed group keeps its current diagnostic.

### Invariants, lemma, and axioms

- `INV-BINDER`: every exposed binder satisfies `LC`.
- `INV-EXPR`: every exposed key has expression text. The text may still fail
  `parse()`, so ADR-0002 `INV-INDEP` holds for accepted clauses.
- `INV-AGREE`: scanner and accessor accept the same clauses.
- `INV-ONE`: a rejected clause yields exactly one diagnostic and no relation,
  and the next declaration is scanned.
- `INV-ALIAS`: `alias()` returns the full alias in any case.
- `INV-REFAC`: consolidation changes no classification.
- `L-LEX`: for text matching `[A-Za-z_][A-Za-z0-9_]*`, `LC` is equivalent to a
  first character that is `_` or ASCII lowercase.
- `AXIOM-LEXER`: `T_IDENT` is that pattern minus keywords and `_`.
- `AXIOM-UPSTREAM`: the pinned source defines the language. Upstream's `lower`
  and `letter` are Unicode-aware; ddlint's lexer is ASCII-only. That deviation
  belongs to the lexer and is recorded in the register.

### Methods per obligation

- Contract (`INV-BINDER`, `INV-EXPR`, `INV-ONE`): an `rstest` table through
  `ddlint::parse`, using `googletest` matchers for diagnostics and
  `pretty_assertions` for values, plus one inline `insta` snapshot of code,
  span, and text. Accepted rows: `(row) row.id`, `(_x) _x`, `(rowId) rowId.k`,
  an expression on the next line, `primary key` on its own line, and comments
  inside the group. Rejected rows: `(Foo) Foo.x`, `(a, b) a`, `(x,) x`, `() x`,
  `(_) x`, `(type) x`, `(Self) x`, no expression at end of file, no expression
  before a keyword declaration, and before a rule. Each rejected row has a
  following declaration that must survive. `output` and bracket rows pin `D10`.
- Binder function: enumerate all sequences up to length four over `a`, `_b`,
  `Cd`, comma, comment, `_`, and `type` (2,801 cases) against an oracle built
  from the symbols' labels. The function's result depends only on the first two
  non-trivia tokens, so this covers every case of that shape; review confirms
  the final code keeps that shape.
- Agreement: a `proptest` over labelled binder groups and expressions compares
  the scanner with the accessor on hand-built CSTs. Expected results come from
  labels. Names are kept only when they lex to one `T_IDENT`; a deterministic
  run asserts that each class occurs.
- `L-LEX` and `INV-REFAC`: labelled `proptest` cases in `ident_case`; existing
  transformer, pattern, and qualified-call tests stay unchanged.
- Aliases: an `rstest` table for `baz`, `Baz`, `_baz`, `a::b`, `A::b`, and the
  incomplete `a::`.
- End to end: the BDD feature and one `parse_with_observer` row.
- Non-vacuity: temporarily ignore case, accept two binders, skip the expression
  check, remove the accessor check, and truncate a scoped alias. Each named
  test must fail; revert each fault before committing.

### Formal verification decision

Verus is not warranted. The clause contract has no loop invariant, arithmetic,
or unbounded state: it inspects a group of fixed size and one following token.
A Verus theorem about `Binder` would restate its definition. The risk lies in
integration between the lexer, scanner, CST, and accessor, which tests exercise
directly. The Verus lane also checks standalone files under `verus/`, so a
proof would cover a transcription. Kani is not configured, and enumeration
already covers the function. No `make verus` gate applies.

### Suggested BDD features

Extend `tests/features/relation_primary_key.feature`. Replace its list-form
scenario; put new bindings in `tests/primary_key_contract_bdd.rs` if the
existing 340-line harness would exceed 400.

```gherkin
Scenario: A capitalized binder is rejected at the binder
  Given the DDlog source "input relation R(x: u32) primary key (Foo) Foo.x"
  When the relations are listed
  Then the relation names are ""
  And the parse reports only "D-REL-007" at "Foo"

Scenario: A second binder is rejected
  Given the DDlog source "input relation R(a: u32) primary key (a, b) a"
  When the relations are listed
  Then the parse reports only "D-REL-007" at "("

Scenario: A key without an expression is rejected
  Given the DDlog source "input relation R(x: u32) primary key (x)"
  When the relations are listed
  Then the parse reports only "D-REL-007" at ")"
```

## Plan of work

### `EP-M0` probe current behaviour

With a deleted scratch example, record `errors()`, names, and accessor results
for: scoped aliases; a trailing `;` after an import; a rejected clause on the
line after its relation header; an expression on a later line; and a multi-line
expression. Go if a rejected second-line clause reports once and loses no later
declaration. Otherwise stop: recovery must change, which needs approval.

### `EP-M1` align the clause and accessor

Red: add the contract table and BDD scenarios; confirm that the three rejected
forms are accepted today. Green: add `src/parser/ident_case.rs` and
`src/parser/primary_key_binder.rs`; replace `is_binder_identifier_list`;
require the expression (`D5`); use the binder function in the accessor; delete
`primary_key_clause`; change `PrimaryKey` (`D4`). Migrate fixtures in a
separate commit within the milestone. Update specification sections 2.2 and 5.5
and register item 13 in the same change.

### `EP-M2` property and fault evidence

Add enumeration and agreement tests, run each seeded fault, and record results.

### `EP-M3` align import aliases

Red: the alias table. Green: reuse the module-path parser for the alias and
collect the full alias in `Import::alias()`. Correct section 5.2, removing
`LcName` and the trailing `';'`, and add a register entry. If today's scanner
accepts a trailing `;`, record that under 2.6.10.

### `EP-M4` consolidate case predicates

Replace the private helpers with `ident_case` in a separate refactor commit.
Each argument must be the text of one token matching the identifier pattern.

### `EP-M5` documentation and closure

Update the users' guide, implementation notes, developers' guide (including the
reuse policy for both new modules), repository layout, design document,
changelog, and contents. Append the ADR-0002 amendment. Correct register item
12's conformance claim. Record the bracket-body, stream, Unicode, `;`, and `D5`
defects in the register. Mark roadmap 2.6.9 done and this plan `COMPLETE`.

## Milestones and plateaus

Each milestone ends with all gates passing and is safe to stop at.
Compatibility decision for each: none, because there is no user base.

- `EP-M0`: recorded transcript and go/no-go. Nothing to revert.
- `EP-M1`: `REQ-BINDER`, `REQ-EXPR`, `REQ-AGREE`. Check that only the authorized
  API changed and ADR-0002's other invariants hold. Revert its commits.
- `EP-M2`: test-only evidence. Revert independently.
- `EP-M3`: `REQ-ALIAS`. Check the specification against `modname`. Revert its
  commits.
- `EP-M4`: `INV-REFAC`, with no test edits. Revert independently.
- `EP-M5`: `REQ-DOCS`. Search live documents for stale compound-binder,
  optional-expression, and lowercase-alias claims.

## Concrete steps

From the repository root, after each milestone, sequentially:

```sh
set -o pipefail
for gate in fmt check-fmt lint test markdownlint nixie; do
  make "$gate" 2>&1 |
    tee "/tmp/$gate-ddlint-$(git branch --show-current).out" || break
done
```

Focused: `cargo test --lib primary_key` and
`cargo test --test relation_primary_key_bdd`. Red: the rejected rows report
`expected one D-REL-007, got []`. Green: all pass.

## Validation and acceptance

The four declarations and two imports in `Purpose` behave as annotated. Every
seeded fault fails its named test. All gates pass. `make verus` is not required.

## Idempotence and recovery

Steps are repeatable. Delete the scratch example and revert seeded faults
before committing; check with `git status` and `git diff`.

## Artefacts and notes

Upstream evidence, commit `ca5ded823dc2c01bc33aac44e526c12d2fed03a6`:

```plaintext
110  ccnLCDef = ccnDef{T.identStart = lower <|> char '_'}
146  varIdent = lcIdentifier
149  modIdent = identifier
287  imprt = ... reserved "import" <*> modname <*> (... reserved "as" *> modname)
289  modname = ModuleName <$> modIdent `sepBy1` reservedOp "::"
360  key_expr = withPos $ KeyExpr nopos <$> (parens varIdent) <*> expr
```

## Interfaces and dependencies

No new dependencies. `ident_case` provides `is_lc_name(&str) -> bool` and
`is_uc_name(&str) -> bool`; it owns case policy for the parser.
`primary_key_binder` provides one function returning the binder span or a
`Malformed` or `NonLc(Span)` fault; only the scanner and accessor call it.
`PrimaryKey` exposes `binder()`, `expression()`, and `span()`.

## Revision note

- 2026-11-23: first draft, then first expert review.
- 2026-11-24: redesigned after the user made upstream fidelity standing policy.
  Compound binders and optional expressions are now rejected; the list
  recognizer and automaton argument are gone; `D10` is reversed; transformer
  and key-placement alignment become 2.6.16 and 2.6.17; and the errors listed
  under `Surprises & discoveries` are corrected.
