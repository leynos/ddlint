# 2.6.9 enforce the `LcName` case restriction on primary-key binders

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (design review applied; awaiting user approval)

Branch: `2-6-9-enforce-lc-name-case-restriction-on-primary-key-binders`

Roadmap item: `2.6.9` in `docs/roadmap.md`.

## Purpose / big picture

`ddlint` is a linter for Differential Datalog (DDlog). Its parser produces a
lossless concrete syntax tree (CST) built with the `rowan` crate. A first pass,
the "span scanner", walks the token stream and decides which byte ranges form
each top-level declaration; it also emits most declaration-level diagnostics.
Thin typed wrappers over the CST, the "AST wrappers" or "accessors", then give
lint rules structured access to each declaration.

A DDlog `input` relation may declare a primary key. The key starts with a
parenthesized list of "binders": names that stand for the whole row while the
optional key expression is evaluated. A binder is a row variable, not a column
name:

```ddlog
input relation Book(row: BookRow) primary key (row) (row.author, row.title)
input relation R(a: u32, b: u32) primary key (a, b)
```

The syntax specification spells each binder as `LcName`: an identifier whose
first character is a lowercase ASCII letter or `_`, matching
`^(_|[a-z])[A-Za-z0-9_]*$`. Upstream DDlog applies the same case class: its
`key_expr` parser reads `parens varIdent`, and `varIdent` is the lowercase
identifier class. (Upstream takes exactly one binder and requires an
expression; ddlint's compound binder lists and optional expression are recorded
supersets in specification section 5.5 and are not changed here.) Today the
ddlint scanner accepts any identifier token, so this is accepted silently:

```ddlog
input relation R(x: u32) primary key (Foo)
```

After this change, that line reports exactly one diagnostic,
`D-REL-007: unexpected or malformed primary key clause`, whose span covers
`Foo`, and the relation is not exposed, which is the existing recovery for every
`D-REL-007`. Binders such as `row`, `_x`, and `rowId` remain valid; only the
first character is case-restricted. Clauses that already fail for another
reason keep their current diagnostic: a structurally malformed list still
reports `D-REL-007` at its `(`, a key on a non-`input` relation still reports
`D-REL-006`, and a key on a bracket-bodied relation still reports `D-REL-004`.

The roadmap item also asks for the same decision on import aliases
(`import foo::bar as baz`). The specification currently spells the alias as
`LcName`, but upstream DDlog parses the alias with `modname`, a `::`-separated
path of identifiers of any case. Enforcing `LcName` would reject valid upstream
programs, so this plan keeps alias acceptance case-independent, corrects the
specification, and pins the behaviour with regression tests. It also settles
whether a scoped alias such as `as a::b` is accepted or silently truncated.

Observable success: `make test` runs new tests that fail before the change and
pass after it; the conformance register's item 13 no longer lists the binder
gap; specification section 5.2 states the upstream alias rule; and roadmap item
2.6.9 is checked.

## Constraints

- Implementation begins only after the user explicitly approves this plan,
  including the formal-verification decision `D7`.
- No new diagnostic code and no new message text. The case violation reuses
  `DiagnosticCode::RelationMalformedPrimaryKey` (`D-REL-007`) and its canonical
  `RELATION_MALFORMED_PRIMARY_KEY_ERROR` text in
  `src/parser/error_messages.rs`. The frozen `D-REL-001` to `D-REL-008` set in
  `src/parser/diagnostics/code.rs` must not change.
- Existing scanner diagnostics stay byte-for-byte unchanged for every input
  that does not contain a non-`LcName` binder in an otherwise accepted clause.
  In particular, the role and body checks (`D-REL-004`, `D-REL-006`,
  `D-REL-008`) keep precedence over the case check (`D10`).
- No public API signature changes. `Relation::primary_key()`, `PrimaryKey`,
  `PrimaryKeyBinder`, `PrimaryKeyExpression`, and `Import::alias()` keep their
  signatures. Rustdoc may gain postcondition statements written in plain text
  (no intra-doc links to crate-private items, because `private_intra_doc_links`
  is denied). New helpers are `pub(crate)` or narrower.
- Recovery for a rejected clause is unchanged: exactly one `D-REL-007`, the
  scanner skips the rest of the line, and no `N_RELATION_DECL` node is
  produced. ADR-0002 invariants `INV-INDEP`, `INV-SHIFT`, `INV-AGREE`, and
  `INV-TERM` must keep holding.
- No dependency, `Cargo.toml`, `Cargo.lock`, `Makefile`, or CI workflow
  changes. No new verification tooling (Kani, Verus targets, or crates).
- ddlint is pre-release: update callers and tests in place, add no migration
  diagnostics, no migration-guide entry, and no "breaking change" changelog
  framing.
- Repository limits: every source file stays at or below 400 lines (Whitaker
  `module-max-lines`); every new module, including `#[cfg(test)]` modules,
  starts with a `//!` comment; `self_named_module_files` is denied, so a module
  with children uses `mod.rs`. Files already near the cap must not grow:
  `src/parser/tests/relations/mod.rs` (392 lines),
  `src/parser/tests/relation_primary_key_strategies.rs` (384), and
  `tests/relation_primary_key_bdd.rs` (340; new scenarios go in a new harness).
- Workspace lints that bind test code too (`Cargo.toml` lines 70 to 134 and
  `clippy.toml`): `print_stdout`, `indexing_slicing`,
  `integer_division_remainder_used`, `result_large_err`, and
  `cognitive_complexity` (threshold 9) are denied; `too-many-lines-threshold`
  is 70 and `excessive-nesting-threshold` is 4. Tests report counts through
  assertion messages, build sequences without `/`, `%`, or `[]`, and split long
  test bodies into helpers.
- Every `assert!`, `assert_eq!`, and `assert_ne!` carries a diagnostic message.
  Tests prefer `expect(...)` to `unwrap()`. Production code uses no `expect`.
  Rustdoc examples on crate-private items use `rust,ignore`, as
  `src/parser/lexer_helpers.rs` does.
- Comments and documentation use en-GB-oxendict spelling.
- Completed ExecPlans in `docs/execplans/` are historical and must not be
  edited.
- Transformer declaration grammar (register item 12) is out of scope even
  though this plan records an upstream divergence there (`D9`).

## Tolerances (exception triggers)

Comparison base: `git merge-base HEAD origin/main`. All counts exclude every
file under `docs/execplans/`, including this plan. Counts measure net lines
(additions minus deletions) per category with `git diff --numstat`. Each
category is enforced independently; spare capacity in one category never
offsets another.

Classification rules (whole-file):

- Code: `*.rs` anywhere, and `tests/features/*.feature` (executable
  behavioural specifications).
- Configuration: `Cargo.toml`, `Cargo.lock`, `Makefile`, `.github/**`,
  `typos*.toml`, `clippy.toml`, `rust-toolchain*`, `.cargo/**`.
- Documentation: `docs/**/*.md` except `docs/execplans/**`, `CHANGELOG.md`,
  `README.md`.
- Generated exclusion: files under `proptest-regressions/` are excluded from
  line counts, but any new file there must be reviewed and recorded in
  `Surprises & discoveries`.

Counting commands, run from the repository root:

```sh
base=$(git merge-base HEAD origin/main)
git diff --numstat "$base" -- '*.rs' 'tests/features/*.feature' \
  | awk '{a+=$1; d+=$2; n++} END {printf "code: %d files, net %d\n", n, a-d}'
git diff --numstat "$base" -- 'docs/*.md' 'docs/**/*.md' CHANGELOG.md README.md \
  ':(exclude)docs/execplans/**' \
  | awk '{a+=$1; d+=$2; n++} END {printf "docs: %d files, net %d\n", n, a-d}'
git diff --numstat "$base" -- Cargo.toml Cargo.lock Makefile '.github/**' \
  'typos*.toml' clippy.toml 'rust-toolchain*' '.cargo/**' \
  | awk '{a+=$1; d+=$2; n++} END {printf "config: %d files, net %d\n", n, a-d}'
```

Limits:

- Code: stop and escalate beyond 24 files or 1,100 net lines.
- Documentation: stop and escalate beyond 14 files or 380 net lines.
- Configuration: stop and escalate on any change (budget 0 files, 0 lines).
  `make spelling` regenerating `typos.toml` is the one foreseeable exception;
  if it happens, stop and follow the `AGENTS.md` rule of committing an
  unrelated regeneration in a separate base pull request.
- Interface: stop if any public signature must change, or if a new
  `DiagnosticCode` variant or message appears necessary.
- Dependencies: stop if any new crate or tool is needed.
- Scanner scope: stop if enforcing the binder rule requires changing scanner
  recovery (`skip_line` behaviour), the `;` terminator rule, or the precedence
  of `D-REL-004`, `D-REL-006`, and `D-REL-008` over the case check.
- Accessor scope: stop if moving the accessor onto the shared recognizer
  changes `PrimaryKey` results (binders, spans, expression) for any clause the
  scanner accepts, as shown by the existing primary-key tests.
- Import scope (`EP-M3`): stop if the probe shows behaviour other than the two
  outcomes anticipated by `D11` (clean rejection or silent truncation of a
  scoped alias).
- Refactor scope (`EP-M4`): stop and leave a call site unconsolidated if its
  argument is not the text of exactly one token matched by the lexer's `Ident`
  regex, recording the reason.
- Test runtime: if the new library tests together add more than 1.5 seconds to
  `cargo test --lib` (measured from the `finished in` line of a focused run),
  reduce the cross-check enumeration bound by one and record the change. No
  test asserts on elapsed time.
- Iterations: stop if a gate still fails after three fix rounds.
- Time: stop if any milestone takes more than four hours of active work.
- Ambiguity: stop if evidence found during implementation challenges `D3`,
  `D6`, `D10`, or `D11`.

Budget rationale. Production code: a pure `ident_case` module (about 60 lines
with rustdoc and unit tests), a shared binder-list recognizer (about 130
lines), scanner integration in `suffix.rs` (about 30 net lines), accessor
integration replacing the `chumsky` binder parser (about 20 net lines, with
deletions in `parse_utils/relation.rs` offsetting additions in `clause.rs`),
and the conditional scoped-alias fix (about 25 lines). Tests: about 220 lines
for the recognizer's product exploration, specification automaton, and
cross-check; about 160 lines of `rstest` rows and one inline snapshot; about
180 lines of property strategies and properties; about 150 lines for the new
BDD feature and harness; about 60 lines of import tests; about 30 lines across
observability and hand-built CST tests. That totals roughly 1,000 lines over
about 22 files; the limits leave about 10% contingency. The `EP-M4` refactor is
net negative.

Historical evidence: 2.6.5 (the transformer-case precedent) set 10 files and
260 lines and did not overrun; 2.6.6.1 set 1,100 net Rust lines and finished at
2,062, with the excess in tests (its decision `D13`); 2.6.8 set 1,600 code
lines and finished at 3,120. Those overruns were missed test scope rather than
approved growth, so this plan itemizes test volume. An earlier draft of this
plan set 16 files and 900 lines; design review showed the draft already needed
17 files and moved the recognizer and BDD harness into new files, so the limits
above replace it before approval.

## Risks

- Risk: an existing test, fixture, example, or proptest strategy generates an
  uppercase-initial binder and starts failing. Severity: low. Likelihood: low.
  Mitigation: reconnaissance found none (the binder strategy is
  `prop::sample::select(vec!["a", "b", "c"])` in
  `src/parser/tests/relation_primary_key_strategies.rs` line 162); `make test`
  in `EP-M1` confirms.
- Risk: moving the accessor from its `chumsky` binder parser to the shared
  recognizer changes accessor results for accepted clauses. Severity: medium.
  Likelihood: low. Mitigation: the accessor-scope tolerance; the existing
  primary-key unit, property, and BDD tests must pass unchanged.
- Risk: the accessor's error text for structurally malformed hand-built CSTs
  changes (from `chumsky` "expected … found …" renderings to the canonical
  `D-REL-007` text). Severity: low. Likelihood: high. Mitigation: accepted;
  only hand-built CSTs reach that path, ddlint is pre-release, and `D4` records
  it.
- Risk: users read "unexpected or malformed primary key clause" on `Id` as a
  column-name complaint. Severity: low. Likelihood: medium. Mitigation: the
  users' guide row and example explain that a binder is a lowercase row variable
  (`D2`).
- Risk: the property test fails intermittently because a generated name lexes
  as a keyword, or a span is located by searching the source and hits the
  relation name. Severity: medium. Likelihood: medium if unmitigated.
  Mitigation: names are filtered by lexing to exactly one `T_IDENT`, offsets
  are computed while rendering, and coverage runs use
  `TestRunner::deterministic()`.
- Risk: consolidating the case helpers in `EP-M4` changes pattern or
  qualified-call behaviour. Severity: medium. Likelihood: low. Mitigation: lemma
  `L-LEX` and the refactor-scope tolerance; existing tests stay green.
- Risk: the scoped-alias probe in `EP-M3` reveals a third behaviour.
  Severity: low. Likelihood: low. Mitigation: the import-scope tolerance stops
  the work for a decision.

## Progress

- [x] (2026-11-23 18:00Z) Branch renamed; Lody session titled.
- [x] (2026-11-23 18:20Z) Reconnaissance completed with a Wyvern agent team
  (scanner and AST, imports and transformers, verification and documentation
  infrastructure) and upstream grammar lookup.
- [x] (2026-11-23 18:50Z) Initial draft written and committed.
- [x] (2026-11-23 19:45Z) Community-of-experts design review (structure and
  contracts; alternatives and viability; failure modes and cost) applied; see
  the revision note.
- [x] (2026-11-23 20:05Z) Draft PR #472 opened for plan review.
- [ ] Plan approved by the user.
- [ ] `EP-M1` shared recognizer and binder case enforcement.
- [ ] `EP-M2` property tests, observability row, and seeded-fault evidence.
- [ ] `EP-M3` import-alias decision pinned and specification corrected.
- [ ] `EP-M4` consolidate the case-class helpers (refactor).
- [ ] `EP-M5` documentation, register, roadmap, and delivery closure.

## Surprises & discoveries

- Observation: upstream DDlog import aliases accept any case and a scoped
  path. Evidence: upstream `Parse.hs` (fetched 2026-11-23) defines
  `imprt = Import nopos <$ reserved "import" <*> modname <*> (option
  (ModuleName []) $ reserved "as" *> modname)`,
  ``modname = ModuleName <$> modIdent `sepBy1` reservedOp "::"``, and
  `modIdent = identifier`. Impact: enforcing `LcName` on aliases would reject
  valid upstream DDlog, so `D6` corrects the specification instead.
- Observation: ddlint's import scanner matches the alias as a single
  identifier with no end anchor. Evidence:
  `src/parser/span_scanners/imports.rs` lines 36 to 45 build
  `just(K_AS).padded_by(ws).ignore_then(ident_parser)`, and `parse_and_record`
  records whatever prefix matched. Impact: `import foo as a::b` very probably
  records `import foo as a` and leaves `::b` to other scanners, a silent
  truncation that specification section 13 forbids. `EP-M3` probes this and
  `D11` decides the fix.
- Observation: specification section 5.2 ends `Import` with `';'`, but neither
  upstream `imprt` nor the ddlint scanner consumes a semicolon, and no ddlint
  test writes one. Impact: examples in this plan omit `;`; the `EP-M3` probe
  records what a trailing `;` does, and the register's import entry notes it.
- Observation: upstream transformer names are uppercase, not lowercase.
  Evidence: upstream `Parse.hs` defines `transIdent = ucScopedIdentifier`, and
  `lib/graph.dl` declares `extern transformer SCC(...) -> (SCCLabels: ...)`.
  ddlint's section 5.4 and scanner require a lowercase transformer name and a
  `:` output list. Impact: the roadmap item's model ("the way transformer names
  already do") is itself a ddlint-only rule. It does not change this plan,
  because the binder rule is independently grounded in upstream `varIdent`, but
  `EP-M5` records the divergence (`D9`).
- Observation: upstream accepts primary keys on bracket-bodied relations,
  while ddlint rejects them with `D-REL-004`. Evidence: the upstream `relation`
  parser calls `key_expr` in both body branches. Impact: none for this item;
  noted in register item 13 by `EP-M5`.
- Observation: `Aggregate`, `FlatMap`, `Inspect`, and `Self` lex as keyword
  tokens, and `_` alone lexes as `K_UNDERSCORE`. Evidence: `keyword_kind` in
  `src/tokenizer.rs`. Impact: `primary key (Self)` and `primary key (_)` are
  structurally malformed (anchored at `(`), not case violations; `EP-M1` pins
  both.
- Observation: specification section 2.2 defines `LcScopedIdentifier` but
  never defines `LcName` or `UcName`, although sections 5.2 to 5.12 use them.
  Impact: `EP-M1` adds the definitions.
- Observation: CodeGraph MCP failed to connect during planning, so
  reconnaissance used text search. Impact: none on the plan's facts, which were
  verified by reading the cited files at revision `7adbd8bc`.

## Decision log

- D1. Decision: enforce `LcName` on primary-key binders rather than record an
  intentional superset. Rationale: the specification, the conformance register,
  and the roadmap describe acceptance of `primary key (Foo)` as a gap, and
  upstream `key_expr` uses `varIdent`, the lowercase class. Only the case class
  is aligned with upstream; compound binders and the optional expression remain
  recorded supersets. Rejected alternative: document a case superset, which
  would legitimize input upstream rejects. Date/Author: 2026-11-23, planning
  agent; confirmed by design review.

- D2. Decision: report the violation as `D-REL-007` with its canonical message;
  add neither a new code nor a case-specific message. Rationale: the roadmap
  names `D-REL-007`; `DiagnosticCode` maps one canonical message per code
  through `message_text()`; the register's stable-surface rule says message
  text is not an identifier; and the span (`D3`) shows which binder is wrong.
  The users' guide carries the explanation. A more specific message or fix hint
  belongs with roadmap item 2.8.1, the post-split diagnostic contract. Rejected
  alternatives: a new `D-REL-009`, and a second `D-REL-007:` message text.
  Date/Author: 2026-11-23, planning agent; confirmed by design review.

- D3. Decision: anchor a case-violation `D-REL-007` at the first binder whose
  name is not `LcName`. Structural malformation is decided first and keeps its
  existing anchor, the binder list's opening `(`. Rationale: existing
  `D-REL-007` emissions already anchor at the offending token (for example
  `primary value` anchors at `value`, pinned in
  `tests/parser_observability.rs`), as does the transformer-name diagnostic.
  Structure-first keeps every existing scanner diagnostic unchanged. The
  "unchanged" claim covers scanner diagnostics only; accessor error text for
  hand-built CSTs may change (`D4`). Rejected alternative: always anchor at
  `(`. Date/Author: 2026-11-23, planning agent; confirmed by design review.

- D4. Decision: replace the accessor's `chumsky` binder parsing with one
  shared, pure binder-list recognizer used by both the scanner and the
  accessor. Rationale: ADR-0002 `INV-AGREE` requires the two to accept the same
  binder lists. With one recognizer, agreement holds by construction,
  hand-built CSTs cannot expose uppercase binders, the accessor stops depending
  on `chumsky` error-merging behaviour for this check, and the recognizer can
  be written as an explicit finite-state step function whose correctness for
  all lengths is established by exhaustive product exploration (`D7`). The
  accessor's error text for structurally malformed hand-built CSTs becomes the
  canonical `D-REL-007` text; accessor results for accepted clauses do not
  change. Rejected alternatives: scanner-only enforcement (silently weakens
  `INV-AGREE`), and two implementations with a `chumsky` `try_map` plus an
  agreement test (the draft design; it needs a `result_large_err` expectation,
  relies on `chumsky` error merging, and offers only bounded agreement
  evidence). Date/Author: 2026-11-23, adopted from design review (Wafflecat's
  alternative).

- D5. Decision: add one crate-private module, `src/parser/ident_case.rs`, with
  `is_lc_name` and `is_uc_name` implementing the full section 2.2 patterns, and
  make it the single owner of identifier case-class predicates. `EP-M4`
  replaces the three private lowercase helpers and the one uppercase helper in
  `src/parser/span_scanners/transformers.rs`, `src/parser/pattern/mod.rs`, and
  `src/parser/expression/qualified.rs`, in a separate refactor commit.
  Rationale: the change needs the predicate in the shared recognizer and the
  repository already has four private copies; full-pattern predicates are
  correct for any string, whereas the first-character copies are correct only
  for identifier-shaped tokens (`L-LEX`). Moving both case classes keeps the
  ownership policy true on the day it is written. Rejected alternative: a fifth
  private copy. Date/Author: 2026-11-23, planning agent; `is_uc_name` added by
  design review.

- D6. Decision: do not enforce a case class on import aliases; correct
  specification section 5.2 to the upstream rule and pin the behaviour with
  tests. Rationale: upstream parses the alias as `modname`, any-case
  identifiers separated by `::`. `Import::alias()` has no non-test consumer,
  `qualified.rs` checks only a call path's final segment, and import
  name-uniqueness does not depend on case, so nothing relies on lowercase
  aliases. This is a specification correction, not an intentional superset.
  Rejected alternative: enforce `LcName` for symmetry with binders.
  Date/Author: 2026-11-23, planning agent; confirmed by design review.

- D7. Decision (requires explicit user approval): no Verus proof and no Kani
  harness for this item. The recognizer's correctness for all input lengths is
  discharged by exhaustive exploration of the product of the production step
  function and a specification automaton, inside an ordinary `cargo test`.
  Rationale: see `Formal verification decision`. Date/Author: 2026-11-23,
  planning agent; strengthened by design review.

- D8. Decision: the case-independence rows in `EP-M3` are characterization
  tests and pass on first run; their red evidence is a seeded fault
  (temporarily rejecting uppercase aliases) that must turn them red. Rationale:
  `D6` preserves behaviour; the tests guard the decision against a later "fix".
  Date/Author: 2026-11-23, planning agent.

- D9. Decision: record, but do not act on, the transformer divergence from
  upstream (uppercase `transIdent`, `->` outputs). `EP-M5` adds one
  known-limitation sentence to register item 12, adjusts its "matches section
  5.4" wording so it does not imply upstream agreement, and adds one unchecked
  roadmap item to decide transformer alignment with upstream, in a separate
  documentation commit. Rationale: it follows the existing pattern of roadmap
  items 2.6.10 to 2.6.15 ("decide alignment with upstream") and is a separate
  grammar decision with its own blast radius. The roadmap line is proposed for
  approval with this plan. Date/Author: 2026-11-23, planning agent; scope
  confirmed by design review.

- D10. Decision: the case check applies only to clauses that would otherwise
  be accepted (an `input` relation with a record body). The scanner computes
  the binder-list verdict where it does today, rejects structurally malformed
  lists there (unchanged), and defers a case violation until after the role and
  body checks, so `output relation R(x: u32) primary key (Foo)` keeps reporting
  `D-REL-006` with `R` exposed, and `input relation R[T] primary key (Foo)`
  keeps reporting `D-REL-004`. Rationale: the draft's clause-first rejection
  would have changed those codes and dropped the relation, contradicting the
  scanner-scope tolerance; a user fixes the role or body first and then sees
  `D-REL-007` for the binder, one actionable problem at a time. Rejected
  alternative: report `D-REL-007` first, as structural errors do, at the cost
  of changing existing codes and dropping relations. Date/Author: 2026-11-23,
  adopted from design review.

- D11. Decision (conditional on the `EP-M3` probe): if `import foo as a::b`
  is silently truncated today, accept a scoped alias as upstream does, by
  parsing the alias with the same path parser as the module path and making
  `Import::alias()` return the full alias text (for example `Some("a::b")`). If
  it is already rejected with a diagnostic, keep that behaviour and record the
  scoped alias as a known subset gap in the register. Rationale: silent
  truncation violates specification section 13 ("MUST NOT silently accept
  constructs that are not part of this specification"); accepting the upstream
  form is the smallest fix that also matches upstream, keeps the
  `Option<String>` signature, and removes the need to document a truncation.
  Date/Author: 2026-11-23, adopted from design review.

## Outcomes & retrospective

Not started. To be completed at each milestone and at closure.

## Context and orientation

Inspected base revision: `7adbd8bcba7ad139c6cb26dfcce59c66da826634`
(`codify brace blocks and adopt upstream map literals (2.6.8) (#408)`). Line
numbers below refer to that revision; relocate symbols with
`grep -rn '<symbol>' src tests`.

### Pipeline

`ddlint::parse(src)` in `src/parser/mod.rs` tokenizes with the `logos` lexer in
`src/tokenizer.rs`, runs the span scanners in `src/parser/span_scanners/`,
builds the CST, and returns `Parsed`, whose `errors()` holds
`chumsky::error::Simple<SyntaxKind>` diagnostics. `parse_with_observer()`
additionally reports diagnostic records to a `ParseObserver`; codes are derived
from the `D-REL-xxx:` message prefix by `DiagnosticCode::from_message` in
`src/parser/diagnostics/code.rs`.

The lexer's identifier rule is `#[regex(r"[A-Za-z_][A-Za-z0-9_]*")] Ident`
(`src/tokenizer.rs` line 23). `keyword_kind` maps reserved spellings to `K_*`
kinds, including the capitalized `Aggregate`, `FlatMap`, `Inspect`, and `Self`,
and `_` alone to `K_UNDERSCORE`. `primary` and `key` are not keywords; the
relation scanner matches them by text.

### Relation scanner (owner of the binder rule)

`collect_relation_spans` in `src/parser/span_scanners/relations/mod.rs` (279
lines) calls `parse_relation_decl`, which calls `parse_relation_suffix` in
`src/parser/span_scanners/relations/suffix.rs` (237 lines). In
`parse_relation_suffix`, `parse_primary_key_clause(...)?` runs first; then the
bracket-body check returns `recoverable_relation(D_REL_004)` and the role check
returns `recoverable_relation(D_REL_006)` (lines 63 to 71); the accepting
branch is at line 73. `recoverable_relation` keeps the relation in the CST.

`parse_primary_key_clause` (lines 83 to 113) consumes `primary` and `key`,
records `binder_list_span = current_span(...)` (the opening `(` token), runs
`parse_balanced_block` to the matching `)`, then:

```rust
if !has_column || !is_binder_identifier_list(tokens, list_start, *cursor) {
    return Err(Box::new(custom_error(&binder_list_span, D_REL_007)));
}
```

`is_binder_identifier_list` (lines 193 to 225) is a two-state recognizer over
token kinds: it skips trivia (`T_WHITESPACE`, `T_COMMENT`, via
`cursor::is_trivia`), accepts `T_IDENT` when an identifier is expected and
`T_COMMA` otherwise, and rejects everything else, including a trailing comma.
It never reads token text, so `Foo` passes. `!has_column` is redundant with an
empty-list rejection. `D_REL_007` is
`DiagnosticCode::RelationMalformedPrimaryKey.message_text()`, aliased at
`relations/mod.rs` line 22. Any `Err` from the clause reaches `handle_candidate`
(`relations/mod.rs` lines 97 to 100), which pushes the error and calls
`st.skip_line()` (`src/parser/span_collector.rs` lines 56 to 59); no relation
span is recorded. An unclosed list (`primary key (Foo`) reports `unclosed ')'`
from `parse_balanced_block` before any binder check.

### Accessor (typed AST)

`Relation::primary_key()` in `src/parser/ast/relation/primary_key/mod.rs` (349
lines) calls `interpret_suffix` in
`src/parser/ast/relation/primary_key/clause.rs` (206 lines). It re-tokenizes
the suffix text (`tokenize_with_trivia(&rest)`), returns `Ok(None)` unless the
first non-trivia text is `primary`, calls `parse_binder_list(&rest)` (lines 124
to 134), and locates the binder list's closing `)` with
`binder_group_close(elements)` to bound the expression. `parse_binder_list` runs
`primary_key_clause(rest)` from `src/parser/ast/parse_utils/relation.rs` (82
lines), a `chumsky` 0.9 parser that matches the `primary` and `key` keywords
through `keyword` (which carries `#[expect(clippy::result_large_err)]`) and
binds each `T_IDENT` to `(name, span)` without a case check.
`primary_key_clause` has no other caller. Errors become `RelationParseErrors`
(`Vec<String>`) through `render_parse_error` in
`src/parser/ast/relation/mod.rs`; a `SimpleReason::Custom` message is returned
verbatim. Hand-built CST tests live in
`src/parser/ast/relation/primary_key/tests.rs` (354 lines).

### Import aliases

`collect_import_spans` in `src/parser/span_scanners/imports.rs` (55 lines)
parses `import`, a `module_path` (identifiers separated by `::`), and an
optional `just(K_AS).padded_by(ws).ignore_then(ident_parser)` alias, then
`parse_and_record` records the matched span
(`src/parser/span_scanners/utils.rs` lines 38 to 56). `Import::alias()` in
`src/parser/ast/import.rs` (99 lines) returns the first `T_IDENT` after `K_AS`;
`Import::path()` collects `T_IDENT` and `T_COLON_COLON` tokens. Tests in
`src/parser/tests/imports.rs` (56 lines) use only lowercase aliases and no
trailing `;`. No uppercase alias exists in any test, fixture, example, or
document.

### Existing case helpers (all private)

- `is_lowercase_ident` in `src/parser/span_scanners/transformers.rs`
  (lines 276 to 281), first-character check, called with the text of a strict
  `T_IDENT` (lines 179 to 182).
- `is_lowercase_ident` and `is_uppercase_ident` in `src/parser/pattern/mod.rs`
  (lines 332 to 344; file 347 lines), first-character checks, called at lines
  163, 230, and 300 with the text of one token that may be `T_IDENT` or one of
  `K_FLATMAP`, `K_AGGREGATE`, `K_INSPECT`.
- `is_lowercase_identifier_segment` in `src/parser/expression/qualified.rs`
  (lines 42 to 47), already the full `LcName` pattern over a path segment.

### Tests and tooling

Dev-dependencies (`Cargo.toml` lines 34 to 46): `rstest` 0.28, `rstest-bdd` and
`rstest-bdd-macros` 0.6.0, `insta` 1.47.2, `proptest` 1.6.0, `trybuild`,
`googletest` 0.14.4, `pretty_assertions` 1.4.1. Primary-key coverage lives in
`src/parser/tests/relation_terminators.rs` (160 lines; `custom_error_messages`
helper and `non_identifier_binders_are_rejected`),
`src/parser/tests/relation_primary_key_props.rs` (303 lines; deterministic
coverage via `TestRunner::deterministic()` in `draw_classes`),
`src/parser/tests/relation_primary_key_strategies.rs` (384 lines), and
`tests/relation_primary_key_bdd.rs` (340 lines) with
`tests/features/relation_primary_key.feature`. `tests/parser_observability.rs`
(317 lines) has a `relation_diagnostics_emit_stable_code_and_span` table (lines
11 to 50). Inline `insta` snapshots are used in
`src/parser/tests/block_expression_snapshots.rs` and
`src/parser/ast/relation/primary_key/tests.rs`; there are no `.snap` files. A
recent full library run took about one second for 939 tests.

Verification tooling: `make verus` runs the pinned `prover-tools` Verus lane on
the file named by `VERUS_FLAGS` (default `verus/smoke/main.rs`, the only proof);
`.github/workflows/proofs.yml` triggers only on `verus/**`, `tools/verus/**`,
the `Makefile`, and itself. The lane is outside `make test` and `make lint`.
Kani is not configured. The `cli/` crate does not invoke the parser, so the
public end-to-end boundary is the library API (`ddlint::parse`,
`parse_with_observer`) exercised from `tests/`.

Gates: `make check-fmt`, `make lint` (interrogate, Clippy, Whitaker, spelling),
`make test` (all targets plus doctests), `make markdownlint`, `make nixie`.

### Budget survey

Comparable plans:
`docs/execplans/2-6-5-align-transformer-declaration-grammar.md` (the case-rule
precedent; 10 files and 260 lines, no overrun),
`docs/execplans/2-6-6-1-ast-access-for-spec-form-primary-key-exprs.md` (28
files and 1,100 lines; finished at 2,062 lines, excess in tests, escalated as
`D13`), and `docs/execplans/2-6-8-decide-brace-group-extension-policy.md` (code
1,600 lines, finished at 3,120; documentation 1,200 lines, finished at 835).
The repeated overruns were missed test scope, not approved scope growth, so
this plan itemizes test volume in `Tolerances`.

### Related documentation and skills

- `docs/differential-datalog-parser-syntax-spec-updated.md`, sections 2.2, 5.2,
  5.5, 9, 12, 13, and 14.
- `docs/parser-conformance-register.md`, items 12 and 13 and the stable
  diagnostic compatibility surface.
- `docs/adrs/adr-0002-relation-role-kind-modelling.md`, the primary-key
  amendment and its invariants.
- `docs/parser-implementation-notes.md`, `docs/ddlint-design.md` (section 2),
  `docs/users-guide.md` ("Relation declarations", "Relation diagnostics"),
  `docs/developers-guide.md` ("Boundary rules", "Parser test helpers",
  "Property-test ownership", "Behaviour-driven scenarios", "Formal
  verification"), `docs/repository-layout.md`.
- `docs/building-an-error-recovering-parser-with-chumsky.md`,
  `docs/rust-parser-testing-comprehensive-guide.md`,
  `docs/rust-testing-with-rstest-fixtures.md`,
  `docs/complexity-antipatterns-and-refactoring-strategies.md`.
- Skills: `execplans`, `rust-router`, `rust-state-machines` (the recognizer's
  step-function shape), `rust-unit-testing`, `proptest`, `rust-verification`,
  `hexagonal-architecture` (the case policy and recognizer are pure domain
  logic with no I/O; scanner and accessor are adapters over token streams),
  `en-gb-oxendict-style`, `commit-message`, `pr-creation`.

## Conformance basis

Upstream artefacts, inspected at revision `7adbd8bc`:

- Roadmap: `docs/roadmap.md` item 2.6.9.
- Normative grammar: `docs/differential-datalog-parser-syntax-spec-updated.md`
  section 2.2 (case classes), section 5.2 (`Import`), section 5.5
  (`PrimaryKey ::= 'primary' 'key' '(' LcName (',' LcName)* ')' Expr?`),
  section 13 (no silent acceptance).
- Register: `docs/parser-conformance-register.md` items 12 and 13.
- ADR: `docs/adrs/adr-0002-relation-role-kind-modelling.md` (primary-key
  amendment; `INV-INDEP`, `INV-SHIFT`, `INV-AGREE`, `INV-TERM`).
- Upstream DDlog: `vmware/differential-datalog`, `master`,
  `src/Language/DifferentialDatalog/Parse.hs` and `lib/graph.dl`, fetched
  2026-11-23.
- No Terms of Reference document exists for this item.

Requirements:

- `REQ-BINDER-LC`: a primary-key binder whose name is not `LcName`, in a
  clause that is otherwise accepted, is rejected with exactly one `D-REL-007`.
- `REQ-BINDER-SPAN`: that diagnostic's span is the first offending binder;
  every other existing scanner diagnostic is unchanged.
- `REQ-AGREE`: scanner and accessor accept exactly the same binder lists.
- `REQ-ALIAS-DECIDED`: the import-alias case question and the scoped-alias
  behaviour are decided, recorded in the specification, and pinned by tests.
- `REQ-DOCS`: the specification, register, users' guide, implementation
  notes, developers' guide, ADR-0002, changelog, and roadmap reflect the
  outcome.

Gaps:

- `GAP-BINDER-CASE`: `is_binder_identifier_list` and `primary_key_clause`
  accept any `T_IDENT` (register item 13 known limitation).
- `GAP-ALIAS-SPEC`: specification section 5.2 claims an unenforced `LcName`
  alias that contradicts upstream.
- `GAP-ALIAS-SCOPED`: a scoped alias is probably truncated silently.

Trace links:

```plaintext
roadmap 2.6.9 -> spec 5.5 -> REQ-BINDER-LC, GAP-BINDER-CASE -> EP-M1 -> relation_binder_case::non_lc_binders_are_rejected
roadmap 2.6.9 -> spec 5.5 -> REQ-BINDER-SPAN -> EP-M1 -> relation_binder_case::binder_case_diagnostics_snapshot
ADR-0002 INV-AGREE -> REQ-AGREE -> EP-M1 -> binder_list::tests::production_machine_simulates_specification
roadmap 2.6.9 -> spec 5.5 -> REQ-BINDER-LC -> EP-M2 -> relation_binder_case_props::binder_case_verdict_matches_labels
roadmap 2.6.9 -> spec 5.2, 13 -> REQ-ALIAS-DECIDED, GAP-ALIAS-SPEC, GAP-ALIAS-SCOPED -> EP-M3 -> imports::alias_case_is_not_restricted
roadmap 2.6.9 -> REQ-DOCS -> EP-M5 -> make markdownlint, make nixie, roadmap checkbox
```

## Verification plan

### Abstract model

Let the binder list be the token sequence strictly between the `(` and its
matching `)` after `primary key`. Map each token to a class:

- `Lc`: a `T_IDENT` whose text satisfies `LC`;
- `NonLc`: a `T_IDENT` whose text does not satisfy `LC`;
- `Comma`: `T_COMMA`;
- `Trivia`: `T_WHITESPACE` or `T_COMMENT`;
- `Other`: any other kind (including `K_UNDERSCORE`, keyword kinds such as
  `K_SELF` or `K_TYPE`, `T_DOT`, `T_NUMBER`).

Here `LC(t)` holds when `t` matches `^(_|[a-z])[A-Za-z0-9_]*$`. Let `strip(w)`
delete every `Trivia`. Define the verdict `V(w)`:

- `Malformed` when `strip(w)` does not match `Id (Comma Id)*`, where `Id` is
  `Lc` or `NonLc`;
- `NonLcBinder(s)` otherwise, when some identifier is `NonLc`, where `s` is
  the span of the first such token;
- `WellFormed(spans)` otherwise, where `spans` are the identifier spans in
  order.

Required behaviour, for every relation whose clause is balanced:

- `Malformed`: the scanner reports exactly one `D-REL-007` spanning the `(`
  and exposes no relation, before any role or body check (unchanged).
- `NonLcBinder(s)` on an `input` relation with a record body: exactly one
  `D-REL-007` spanning `s`; no relation.
- `NonLcBinder(s)` on a non-`input` or bracket-bodied relation: the existing
  `D-REL-006` or `D-REL-004` diagnostic and recovery, unchanged (`D10`).
- `WellFormed(spans)` on an accepted relation: no diagnostic;
  `PrimaryKey::binder_names()` returns the texts at `spans`.
- The accessor returns `Ok` for the clause exactly when `V(w)` is
  `WellFormed`, and otherwise `Err` containing the `D-REL-007` text.

Precondition: the list is balanced; an unbalanced list reports
`parse_balanced_block`'s own error first, unchanged. Malformed input before the
list (for example `primary value`) keeps its existing behaviour.

### Invariants and lemmas

- `INV-VERDICT` (`REQ-BINDER-LC`, `REQ-BINDER-SPAN`): the scanner's outcome
  and diagnostic span equal those dictated by `V(w)` and `D10`, for every
  binder list.
- `INV-CASE` (`REQ-BINDER-LC`): every binder exposed by
  `Relation::primary_key()` satisfies `LC`, whether the CST came from `parse()`
  or was built by hand.
- `INV-ONE` (preserved): a rejected clause yields exactly one diagnostic in
  total, no relation node, and the next line is scanned normally.
- `INV-AGREE` (preserved and strengthened, `REQ-AGREE`): for every `w`, the
  scanner accepts if and only if the accessor accepts. With one shared
  recognizer this reduces to `L-SIM` plus the fact that both adapters pass the
  same token range.
- `INV-PRECEDENCE` (`D10`): for every `w`, the scanner's code on a non-`input`
  or bracket-bodied relation is unchanged by this plan.
- `INV-ALIAS` (`REQ-ALIAS-DECIDED`): `import p as a` is accepted and
  `Import::alias()` returns `a` verbatim for any identifier text `a`, whatever
  its case; and `D11`'s chosen behaviour holds for `as a::b`.
- `INV-REFAC` (`EP-M4`): transformer-name, pattern, and qualified-call
  classification are unchanged after consolidation.
- `L-SIM`: the production step function simulates the specification
  automaton: for every reachable pair of states and every token class, the two
  transitions agree on whether a non-`LcName` binder is first seen, and at
  every reachable pair the two verdict kinds agree. By induction on the length
  of `w`, the production fold and the specification automaton then produce the
  same verdict kind and the same first-offender position for every `w`.
- `L-SPEC`: the specification automaton recognizes `V`: it accepts exactly
  the class sequences whose stripped form is `Id (Comma Id)*`, and marks the
  first `NonLc`. This is the hand-derived link from the EBNF to the automaton.
- `L-STRUCT-FIRST`: if `strip(w)` is malformed, `V(w)` is `Malformed`
  irrespective of identifier case.
- `L-FOLD`: the production fold records the span of the token on which the
  step function first reports a non-`LcName` binder, and the identifier spans
  in order; nothing else.
- `L-LEX`: for every string `t` matching the lexer's `Ident` regex
  `[A-Za-z_][A-Za-z0-9_]*`, `LC(t)` holds if and only if the first character of
  `t` is `_` or ASCII lowercase, and the `UcName` pattern holds if and only if
  the first character is ASCII uppercase. This connects the full-pattern
  predicates to the first-character helpers they replace, for any token whose
  text the lexer matched with that regex (identifiers and keyword spellings
  alike).

### Axioms (trusted, not verified here)

- `AXIOM-LEXER`: `logos` 0.13 tokenizes exactly the strings matching
  `[A-Za-z_][A-Za-z0-9_]*` that are not keywords as `T_IDENT`, keyword
  spellings as `K_*`, and `_` alone as `K_UNDERSCORE`. Boundary: the
  public-`parse()` property test in `EP-M2` exercises the real lexer, and the
  property generator uses the lexer only to discard keyword spellings.
- `AXIOM-SIMULATION`: the standard result that if a deterministic machine
  simulates another on every reachable state pair, the two agree on every input
  sequence (proved by induction on sequence length). It is used, not re-proved,
  to lift `L-SIM`'s finite check to all lengths.
- `AXIOM-UPSTREAM-PK`: upstream `key_expr = parens varIdent` with
  `varIdent = lcIdentifier` (identifier start `_` or lowercase).
- `AXIOM-UPSTREAM-IMPORT`: upstream alias is `modname`, any-case identifiers
  separated by `::`.

### Methods per obligation

- Obligation: `L-SIM` (and through it `INV-AGREE` and the verdict part of
  `INV-VERDICT`) for all input lengths.
  - Method: exhaustive exploration of the product automaton in an ordinary
    `cargo test`: a breadth-first search over pairs
    `(production_state, specification_state)` from the two initial states,
    taking every token class from each reachable pair, asserting
    transition-output agreement and verdict-kind agreement at every reachable
    pair.
  - Rationale: both machines are finite (the production state type is an enum
    of a few phases plus a "non-`LcName` seen" flag), so the search terminates
    after visiting every reachable pair, and `AXIOM-SIMULATION` extends the
    result to every sequence. This checks the production `step` function
    itself, not a transcription.
  - Domain: all reachable pairs (expected at most a few dozen) times the five
    token classes.
  - Artefact: `src/parser/binder_list/tests.rs`,
    `production_machine_simulates_specification`.
  - Evidence: the test passes; it fails before the green step of `EP-M1`
    because the recognizer does not yet exist.
  - Non-vacuity: the test asserts that every specification state and every
    verdict kind (`Malformed`, `NonLcBinder`, `WellFormed`) is reached, and
    reports the visited-pair count in its assertion messages. Seeded faults,
    each reverted before committing: (a) the step function treats `NonLc` as
    `Lc`; (b) it records the last rather than the first offender; (c) it lets
    a trailing `Comma` end in `WellFormed`. Each must fail with the offending
    pair and class named.
- Obligation: `L-SPEC` (the specification automaton encodes `V`).
  - Method: review of the hand derivation recorded in the test module's `//!`
    comment, plus a bounded cross-check: every class sequence of length 0 to 7
    over the five classes (97,656 sequences) is run through the specification
    automaton and through a direct implementation of `V` (strip trivia, split
    on `Comma`, check each segment, then find the first `NonLc`), and the
    verdicts must match.
  - Rationale: the direct `V` implementation is structurally unlike an
    automaton, so agreement is independent evidence. Both machines are small
    (the specification automaton has six states and the minimal machine for
    `V` no more), so length 7 exceeds the `n1 + n2 - 2` bound within which two
    inequivalent deterministic machines must already disagree.
  - Artefact: `src/parser/binder_list/tests.rs`,
    `specification_machine_matches_ebnf_oracle`. Sequences are built by
    iterative extension with `Vec::extend` and `.get()`, never by index
    arithmetic.
  - Non-vacuity: assert each verdict kind occurs, and that a `Malformed`
    sequence containing `NonLc` exists (`L-STRUCT-FIRST`).
- Obligation: `L-FOLD` and the span part of `INV-VERDICT`.
  - Method: bounded enumeration over concrete tokens through
    `parse_primary_key_clause` (made `pub(super)` for its sibling test
    module), comparing the returned outcome and span with the direct `V`
    oracle.
  - Domain: every sequence of length 0 to 4 over seven concrete symbols:
    `T_IDENT "a"`, `T_IDENT "_b"`, `T_IDENT "Cd"`, `T_COMMA`,
    `T_COMMENT "/*c*/"`, `K_UNDERSCORE "_"`, and `K_TYPE "type"` (2,801
    sequences), framed as `primary key (` … `)`, with tokens and spans
    constructed directly so adjacent identifiers stay distinct tokens.
    `K_UNDERSCORE` and `K_TYPE` have identifier-like text, so a fault that
    checks text instead of kind is caught.
  - Artefact: `src/parser/span_scanners/relations/binder_list_framing_tests.rs`.
  - Non-vacuity: assert that `NonLcBinder` at the first and at a later
    position, `Malformed`, and `WellFormed` are all reached. Seeded fault (d):
    classify by text instead of kind; it must fail on `(_)` or `(type)`.
- Obligation: `INV-VERDICT`, `INV-ONE`, and `INV-CASE` through the public
  boundary over a broad name space.
  - Method: `proptest` property over `ddlint::parse`.
  - Domain: one to four binders, each from a generator-labelled class:
    `Lower` (`[a-z][A-Za-z0-9_]{0,6}`), `Underscore` (`_[A-Za-z0-9_]{1,6}`),
    `Upper` (`[A-Z][A-Za-z0-9_]{0,6}`). A name is kept only if the lexer turns
    it into exactly one `T_IDENT` (`AXIOM-LEXER`); this discards keyword
    spellings such as `as`, `on`, `Self`, and `Inspect`. Lists are rendered
    with `", "` separators, an optional trailing expression, and a second
    relation on the next line; every binder's byte offset is recorded while
    rendering.
  - Artefacts: `src/parser/tests/relation_binder_case_strategies.rs` and
    `src/parser/tests/relation_binder_case_props.rs`.
  - Evidence: the expected verdict comes from the class labels, never from
    `is_lc_name`: all `Lower`/`Underscore` means accepted with the exact
    names; otherwise exactly one diagnostic in total, `D-REL-007`, at the
    recorded offset of the first `Upper` binder, and only the second relation
    is exposed.
  - Non-vacuity: a deterministic run (`TestRunner::deterministic()`, as
    `draw_classes` does) asserts that every class, a first-binder `Upper`, and
    a later-binder `Upper` are generated, and counts keyword discards before
    filtering, asserting they stay under 5%.
- Obligation: named contract cases for `REQ-BINDER-LC`, `REQ-BINDER-SPAN`,
  `INV-PRECEDENCE`, and `INV-ONE`.
  - Method: `rstest` table plus one inline `insta` snapshot of rendered
    diagnostics (`code`, byte span, sliced source text) for a small
    multi-relation program.
  - Artefact: `src/parser/tests/relation_binder_case.rs`.
  - Rejected rows (exactly one diagnostic in total, `D-REL-007`, at the
    stated span, no relation): `(Foo)` at `Foo`; `(a, Bad)` at `Bad`;
    `(Foo, Bar)` at `Foo`; `(Row) Row.id` on `input relation R(row: T)` at
    `Row` (the expression does not rescue the binder); a multi-line list
    `(a,\n  Foo)` at `Foo`; `(a, // c\n  Foo)` at `Foo`.
  - Structural rows (span at `(`, unchanged): `(Foo,)`, `(Self)`,
    `(Inspect)`, `(_)`, `(type)`.
  - Precedence rows (`D10`): `output relation R(x: u32) primary key (Foo)`
    reports `D-REL-006` and exposes `R`; `relation R(x: u32) primary key
    (Foo)` reports `D-REL-006`; `input relation R[T] primary key (Foo)`
    reports `D-REL-004` and exposes `R`.
  - Recovery rows: `"input relation R(x: u32) primary key (Foo)\noutput
    relation S(x: u32)"` exposes only `S`; `"input relation R(x: u32) primary
    key (Foo); output relation S(x: u32)"` exposes no relation (the same-line
    successor is skipped, as for every `D-REL-007`); `primary key (Foo` reports
    only the unclosed-delimiter error.
  - Accepted rows (no errors, exact `binder_names()`): `(_x)`, `(rowId)`,
    `(a_B9)`, `(a, _b, cD)`.
- Obligation: `INV-CASE` for hand-built CSTs.
  - Method: named unit tests building a relation CST with binders `Foo` and
    `a, Bad`; `primary_key()` must return `Err` whose only entry is the
    `D-REL-007` text.
  - Artefact: `src/parser/ast/relation/primary_key/tests.rs` (354 lines; keep
    it at or below 400, otherwise a new sibling `case_tests.rs`).
- Obligation: observability of the new diagnostic.
  - Method: one new row in `relation_diagnostics_emit_stable_code_and_span`
    in `tests/parser_observability.rs`:
    `"input R(id: u32) primary key (Id)\noutput S(id: u32)"`,
    `RelationMalformedPrimaryKey`, span `30..32`.
- Obligation: `INV-ALIAS` and `D11`.
  - Method: `rstest` table in `src/parser/tests/imports.rs` covering `as baz`,
    `as Baz`, `as BAZ`, and `as _baz` (no errors, exact `alias()`), plus the
    `as a::b` row whose expectation `D11` fixes after the probe.
  - Non-vacuity: seeded fault (reject uppercase aliases in
    `collect_import_spans`) must fail the `Baz` and `BAZ` rows.
- Obligation: `L-LEX` and `INV-REFAC`.
  - Method: `proptest` over `is_lc_name` and `is_uc_name` with
    generator-labelled classes, plus a property that, for strings from
    `[A-Za-z_][A-Za-z0-9_]{0,8}`, each predicate agrees with its
    first-character rule; existing transformer, pattern, and qualified-call
    tests stay green unchanged.
  - Artefact: the `#[cfg(test)]` module in `src/parser/ident_case.rs`.
  - Non-vacuity: generated classes include the empty string, digit-led
    strings, strings containing `-` or `:`, and non-ASCII letters, all expected
    false for both predicates.
- Obligation: ADR-0002 invariants and the existing contract.
  - Method: existing tests in `relation_terminators.rs`,
    `relation_primary_key_props.rs`, `primary_key/tests.rs`, and the
    `relation_primary_key` BDD feature stay green without edits.

### Formal verification decision

Verus is not warranted for this item, and Kani is not adopted; this needs the
user's explicit approval because the brief asks for Verus where warranted.

The obligations concern one finite-state recognizer of a regular language over
token classes. Writing the recognizer as an explicit step function lets an
ordinary unit test explore every reachable state of its product with a
specification automaton, so `L-SIM` is checked exhaustively for the production
code and lifted to all input lengths by a standard induction
(`AXIOM-SIMULATION`). That is the same unbounded guarantee a Verus proof would
give, obtained on the code users run.

A Verus proof would be weaker here, not stronger. The repository's Verus lane
verifies standalone files under `verus/`, not the production crate, so a proof
would establish the property for a transcription and still need a
correspondence test. It would also require configuration changes (a new
`VERUS_FLAGS` target in the `Makefile` and a widened trigger in
`.github/workflows/proofs.yml`), which this item's zero-configuration budget
excludes; tooling that links proofs to production code belongs to roadmap item
3.3.6. The remaining candidate lemmas, `L-LEX` and the predicates' agreement
with their regular expressions, would only restate definitions. Kani would
check production code, but it is not configured, would re-check only a bound,
and is costly on the parser's dependencies; the product exploration already
covers every state. The precedent is 2.6.6.1 decision `D5` and ADR-0006, where
the user approved the same substitution.

Residual gaps: `L-SPEC` rests on a reviewed hand derivation plus a bounded
cross-check; `AXIOM-SIMULATION` is a textbook result, not mechanized. Revisit
trigger: if roadmap item 3.3.6 delivers proof tooling over production crates,
`L-SIM` is a natural first Verus target. Because no Verus milestone exists, no
`make verus` acceptance criterion is added.

### Suggested BDD features

Add a new feature file, `tests/features/relation_binder_case.feature`, bound by
a new harness, `tests/relation_binder_case_bdd.rs`, because
`tests/relation_primary_key_bdd.rs` (340 lines) cannot absorb these scenarios
and new steps under the 400-line cap. The harness has its own small world
(source, parsed result) and steps worded distinctly from the existing feature:

```gherkin
Feature: Primary-key binders must be lowercase row variables

  Scenario: Capitalized binder is rejected at the binder
    Given the DDlog program "input relation R(x: u32) primary key (Foo)"
    When the program is parsed
    Then no relation is exposed
    And exactly one diagnostic "D-REL-007" is reported at "Foo"

  Scenario: A capitalized later binder is reported at that binder
    Given the DDlog program "input relation R(a: u32, b: u32) primary key (a, Bad)"
    When the program is parsed
    Then no relation is exposed
    And exactly one diagnostic "D-REL-007" is reported at "Bad"

  Scenario: Underscore-led and camel-case binders are accepted
    Given the DDlog program "input relation R(x: u32, y: u32) primary key (_x, rowId)"
    When the program is parsed
    Then the first relation's binders are "_x, rowId"
    And no diagnostic is reported

  Scenario: A key on an output relation still reports the role error first
    Given the DDlog program "output relation R(x: u32) primary key (Foo)"
    When the program is parsed
    Then the relation names are "R"
    And exactly one diagnostic "D-REL-006" is reported at "output relation R(x: u32) primary key (Foo)"
```

"Exactly one diagnostic … is reported at" asserts that the parse has one error
in total, that it carries the code, and that its span slices the source to the
quoted text. The fourth scenario's expected span is whatever span
`recoverable_relation` reports today (the relation span); the implementer
confirms it at the red stage and adjusts the quoted text if needed. All four
scenarios trace to `REQ-BINDER-LC` and `EP-M1`; the first two also trace to
`REQ-BINDER-SPAN`, and the fourth to `INV-PRECEDENCE` (`D10`).

## Plan of work

### `EP-M1` shared recognizer and binder case enforcement

Assigned: `REQ-BINDER-LC`, `REQ-BINDER-SPAN`, `REQ-AGREE`, `GAP-BINDER-CASE`;
decisions `D1` to `D5` and `D10`.

Red. Add `src/parser/tests/relation_binder_case.rs` (declared in
`src/parser/tests/mod.rs`) with the `rstest` rows and inline snapshot, the
hand-built CST accessor tests, the new BDD feature and harness, and the
recognizer's test module stub `src/parser/binder_list/tests.rs` referencing the
not-yet-written `step` function. Run the focused commands in `Concrete steps`
and confirm the failures are for the intended reasons: `(Foo)` and `(a, Bad)`
produce no diagnostic, the hand-built accessor returns `Ok`, and the recognizer
test does not compile because `binder_list` does not exist. Precedence,
structural, and accepted rows already pass; record that as characterization of
`D10`.

Green.

1. Create `src/parser/ident_case.rs` (declared `mod ident_case;` in
   `src/parser/mod.rs`) with a `//!` comment stating its scope: identifier case
   classes from specification section 2.2; pure, no I/O; the single owner of
   case-class predicates in the parser. It provides
   `pub(crate) fn is_lc_name(text: &str) -> bool` and
   `pub(crate) fn is_uc_name(text: &str) -> bool`, each with a `rust,ignore`
   rustdoc example and unit tests.
2. Create `src/parser/binder_list/mod.rs` (declared `mod binder_list;` in
   `src/parser/mod.rs`), the shared recognizer, with a `//!` comment stating
   its scope (the binder-list grammar of specification section 5.5; pure; the
   only binder-list recognizer, used by the relation scanner and the
   primary-key accessor). It provides:

   ```rust
   pub(crate) enum BinderTokenClass { Lc, NonLc, Comma, Trivia, Other }
   pub(crate) fn classify(kind: SyntaxKind, text: &str) -> BinderTokenClass;

   #[derive(Debug, Clone, Copy, PartialEq, Eq)]
   pub(crate) struct BinderListState { /* phase, non-LcName seen */ }
   impl BinderListState {
       pub(crate) const START: Self;
       pub(crate) fn step(self, class: BinderTokenClass) -> Self;
       pub(crate) fn verdict_kind(self) -> BinderVerdictKind;
   }

   #[derive(Debug, Clone, PartialEq, Eq)]
   pub(crate) enum BinderListVerdict {
       WellFormed(Vec<Span>),
       Malformed,
       NonLcBinder(Span),
   }
   pub(crate) fn recognize(tokens: &[(SyntaxKind, Span)], src: &str) -> BinderListVerdict;
   ```

   `recognize` takes the tokens strictly between the parentheses, folds `step`
   over their classes, records identifier spans and the span of the token on
   which the "non-`LcName` seen" flag first becomes true, and maps the final
   state to a verdict. The phases are those of the specification automaton
   derived from the EBNF (`Start`, `AfterBinder`, `AfterComma`, `Dead`). Keep
   each function within the cognitive-complexity threshold of 9.
3. In `src/parser/span_scanners/relations/suffix.rs`, delete
   `is_binder_identifier_list` and the redundant `has_column` test. Make
   `parse_primary_key_clause` `pub(super)` and have it return the clause end
   together with an `Option<Span>` holding the first non-`LcName` binder:
   `Malformed` still returns the existing `Err` at `binder_list_span`;
   `NonLcBinder(span)` returns `Ok` with `Some(span)`. In
   `parse_relation_suffix`, keep the bracket and role checks as they are; only
   in the accepting branch, if a non-`LcName` span is present, return
   `Err(custom_error(&span, D_REL_007))` (`D10`).
4. In `src/parser/ast/relation/primary_key/clause.rs`, change
   `parse_binder_list` to locate the `primary` and `key` keywords and the
   binder list's parentheses in the re-tokenized suffix (reusing the
   delimiter-matching approach of `binder_group_close`), call
   `binder_list::recognize`, and build binders from `WellFormed` spans;
   `Malformed` and `NonLcBinder` return `Err(vec![D_REL_007.to_owned()])`.
   Delete `primary_key_clause` from `src/parser/ast/parse_utils/relation.rs` if
   it has no remaining caller (keep `keyword` only if still used).
5. Add plain-text postconditions to the rustdoc of `PrimaryKey::binders` and
   `PrimaryKeyBinder::name`: every binder name starts with a lowercase ASCII
   letter or `_` (`LcName`, specification section 2.2).
6. Update the specification: define `LcName` and `UcName` in section 2.2;
   in section 5.5 remove the "does not enforce" sentence and state the anchor
   and precedence rules. Update register item 13: remove the binder known
   limitation, describe the rule, and list the new test files. (The register's
   maintenance rule requires these in the same change.)

Refactor. Tidy names and rustdoc; confirm file sizes; run the focused tests,
then the full gates.

### `EP-M2` property tests, observability row, and seeded-fault evidence

Assigned: `INV-VERDICT`, `INV-CASE`, `INV-ONE`, `L-SPEC`, `L-FOLD`.

Add `src/parser/binder_list/tests.rs`'s
`specification_machine_matches_ebnf_oracle` cross-check (if not already
completed in `EP-M1`), the framing enumeration in
`src/parser/span_scanners/relations/binder_list_framing_tests.rs` (declared
`#[cfg(test)] mod binder_list_framing_tests;` in `relations/mod.rs`), the
strategies and property in
`src/parser/tests/relation_binder_case_strategies.rs` and
`src/parser/tests/relation_binder_case_props.rs`, and the observability row.
Property helpers return `Result<_, TestCaseError>`, following the developers'
guide. Because `EP-M1` is already green, this milestone's red evidence is the
seeded faults (a) to (d): apply each, observe the named test fail with a
counterexample, revert, and record the transcripts in `Artefacts and notes`.
Measure the runtime of the new library tests against the 1.5-second tolerance.

### `EP-M3` import-alias decision pinned and specification corrected

Assigned: `REQ-ALIAS-DECIDED`, `GAP-ALIAS-SPEC`, `GAP-ALIAS-SCOPED`; decisions
`D6`, `D8`, and `D11`.

1. Probe. Add the `as a::b` row and a trailing-`;` row to the new import
   table with provisional expectations, run them, and record the actual errors,
   spans, and `alias()` values in `Artefacts and notes`.
2. Decide per `D11`. If truncated silently, write the red expectation
   (`alias()` is `Some("a::b")`, no errors), then make the alias parser in
   `collect_import_spans` reuse the module-path parser and make
   `Import::alias()` collect `T_IDENT` and `T_COLON_COLON` tokens after `K_AS`,
   as `path()` does. If rejected with a diagnostic, pin that and record the
   subset gap.
3. Add the case-independence rows and run the seeded fault.
4. Update specification section 5.2:
   `Import ::= 'import' ScopedPath ('as' ScopedPath)?` (or `('as' Ident)?` if
   `D11` keeps rejection), with the upstream rule and decision in prose, and
   resolve the trailing `';'` according to the probe. Add a register entry for
   import aliases (new item 23, status `implemented`) and remove the
   import-alias sentence from item 13.

### `EP-M4` consolidate the case-class helpers

Assigned: `INV-REFAC`, `L-LEX`; decision `D5`. Separate refactor commit.

Replace `is_lowercase_ident` in `src/parser/span_scanners/transformers.rs`,
`is_lowercase_ident` and `is_uppercase_ident` in `src/parser/pattern/mod.rs`,
and `is_lowercase_identifier_segment` in `src/parser/expression/qualified.rs`
with `crate::parser::ident_case::{is_lc_name, is_uc_name}`. At each site,
confirm the argument is the text of exactly one token matched by the lexer's
`Ident` regex (`T_IDENT` or a keyword kind such as `K_FLATMAP`); `L-LEX` then
guarantees identical results. The qualified-call site already uses the full
pattern. All existing tests must pass unchanged.

### `EP-M5` documentation, register, roadmap, and delivery closure

Assigned: `REQ-DOCS`; decision `D9`.

- `docs/users-guide.md`: in "Relation declarations", state that a binder is a
  row variable that must start with a lowercase letter or `_`, with
  `primary key (Foo)` as a rejected example, the anchor rule, and the
  precedence rule; in "Relation diagnostics", widen the `D-REL-007` cause to
  "Stray or malformed `primary key` clause, including a binder that does not
  start with a lowercase letter or underscore". Near the `as` note, state that
  import aliases are not case-restricted and, per `D11`, whether scoped aliases
  are accepted.
- `docs/parser-implementation-notes.md`: describe the shared recognizer, its
  two adapters, the anchor and precedence rules, and the case predicates.
- `docs/developers-guide.md`: under "Boundary rules", record
  `src/parser/ident_case.rs` as the single owner of identifier case-class
  predicates and `src/parser/binder_list/` as the single binder-list
  recognizer, with their reuse policy (crate-private, pure, call them instead
  of adding local checks); under "Property-test ownership", record the
  binder-case strategies file beside the primary-key one; under
  "Behaviour-driven scenarios", list the new feature.
- `docs/repository-layout.md`: add the two modules and the new test files to
  the parser ownership map.
- `docs/adrs/adr-0002-relation-role-kind-modelling.md`: append a dated
  amendment: `INV-AGREE` now includes the binder case class and holds by
  construction through the shared recognizer; `D-REL-007` anchors at the first
  non-`LcName` binder; role and body diagnostics keep precedence.
- `docs/ddlint-design.md`: one sentence in section 2 that binders are checked
  by the shared recognizer, referencing the ADR-0002 amendment.
- `docs/parser-conformance-register.md`: item 12 gains the transformer
  known-limitation sentence and corrected wording (`D9`); item 13 notes the
  upstream bracket-body primary-key acceptance as an observation.
- `docs/roadmap.md`: check item 2.6.9 with a short "Done:" summary; add the
  approved transformer-alignment item (`D9`) in a separate commit.
- `CHANGELOG.md`: under "Unreleased", "Changed": primary-key binders must be
  `LcName`, and `primary key (Foo)` reports `D-REL-007` at the binder; record
  the `D11` outcome for scoped import aliases.
- `docs/contents.md`: the ExecPlan index entry was added with the draft plan;
  check that its title still matches.

Search for stale claims with
`grep -rn "does not enforce\|LcName\|2\.6\.9" docs --include=*.md | grep -v docs/execplans`
and fix every live hit. Mark this plan `COMPLETE` after the gates pass and the
outcomes section is written.

## Milestones and plateaus

- `EP-M1`: binders are case-checked through one shared recognizer.
  - Requirements and gaps: `REQ-BINDER-LC`, `REQ-BINDER-SPAN`, `REQ-AGREE`,
    `GAP-BINDER-CASE`.
  - Acceptance evidence: the `relation_binder_case` tests, hand-built accessor
    tests, `production_machine_simulates_specification`, and the new BDD
    scenarios fail before and pass after; existing primary-key tests pass
    unchanged; `make check-fmt`, `make lint`, and `make test` pass.
  - Conformance check:
    - spec 5.5 satisfied for binders;
    - ADR-0002 invariants still hold (existing tests green);
    - `D-REL-004`, `D-REL-006`, and structural `D-REL-007` unchanged;
    - no new public interface, code, message, or dependency;
    - no trust-boundary or persisted-format change;
    - register item 13 and spec sections 2.2 and 5.5 updated in the same
      commit.
  - Recovery: revert the milestone commits; no data or format migration.
  - Remaining gaps: property and seeded-fault evidence (`EP-M2`), aliases
    (`EP-M3`).
  - Compatibility decision: none (pre-release).
- `EP-M2`: verification evidence complete for binders.
  - Requirements: `INV-VERDICT`, `INV-CASE`, `INV-ONE`, `L-SPEC`, `L-FOLD`.
  - Acceptance evidence: cross-check, framing enumeration, and property pass;
    seeded-fault transcripts recorded; runtime within tolerance; gates pass.
  - Conformance check: tests exercise production code; no tooling added.
  - Recovery: test-only commit, safe to revert.
  - Compatibility decision: none.
- `EP-M3`: alias decision pinned.
  - Requirements and gaps: `REQ-ALIAS-DECIDED`, `GAP-ALIAS-SPEC`,
    `GAP-ALIAS-SCOPED`.
  - Acceptance evidence: probe transcript recorded; alias table passes;
    seeded fault fails it; spec 5.2 and register updated; gates pass.
  - Conformance check: case behaviour unchanged; scoped-alias behaviour per
    `D11`; spec aligned with upstream.
  - Recovery: revert the commits.
  - Compatibility decision: none.
- `EP-M4`: one owner for case-class predicates.
  - Requirements: `INV-REFAC`.
  - Acceptance evidence: no test changes needed; gates pass; searching `src`
    for the definitions of `is_lowercase_ident`, `is_uppercase_ident`, and
    `is_lowercase_identifier_segment` (for example with
    `grep -rn "fn is_lowercase_ident" src`) finds none, or only sites recorded
    as unconsolidated.
  - Recovery: revert the refactor commit independently of the others.
  - Compatibility decision: none.
- `EP-M5`: documentation and roadmap closed.
  - Requirements: `REQ-DOCS`.
  - Acceptance evidence: `make markdownlint` and `make nixie` pass;
    stale-claim search clean; roadmap 2.6.9 checked.
  - Compatibility decision: none.

## Concrete steps

Working directory: the repository root. Use `set -o pipefail` for every `tee`
pipeline. Logs go to `/tmp/<action>-ddlint-<branch>.out`.

`EP-M1`:

```plaintext
Red:   cargo test --lib relation_binder_case
       -> FAILED: (Foo) expected exactly one D-REL-007 at Foo, got []
Red:   cargo test --test relation_binder_case_bdd
       -> scenario "Capitalized binder is rejected at the binder" fails
Red:   cargo test --lib binder_list
       -> error[E0432]: unresolved import (module not yet written)
Green: the same three commands -> all pass
Check: cargo test --lib relation_primary_key && cargo test --test relation_primary_key_bdd
       -> pass unchanged
```

`EP-M2`:

```plaintext
Green:  cargo test --lib binder_list -> ok; note the "finished in" time
Fault:  each of (a) to (d) applied in turn -> the named test fails with the
        offending state pair, class, or sequence; revert after each
Green:  cargo test --lib relation_binder_case_props -> ok
Green:  cargo test --test parser_observability -> ok
```

`EP-M3`:

```plaintext
Probe: cargo test --lib imports -> record as a::b and trailing ; outcomes
Green: cargo test --lib imports -> ok
Fault: temporarily reject uppercase aliases -> Baz and BAZ rows fail; revert
```

Gate sequence after each milestone, sequentially (delegated to the `scrutineer`
agent where available):

```sh
set -o pipefail
make fmt
make check-fmt 2>&1 | tee /tmp/check-fmt-ddlint-$(git branch --show-current).out
make lint 2>&1 | tee /tmp/lint-ddlint-$(git branch --show-current).out
make test 2>&1 | tee /tmp/test-ddlint-$(git branch --show-current).out
make markdownlint 2>&1 | tee /tmp/markdownlint-ddlint-$(git branch --show-current).out
make nixie 2>&1 | tee /tmp/nixie-ddlint-$(git branch --show-current).out
```

## Validation and acceptance

- `ddlint::parse("input relation R(x: u32) primary key (Foo)")` returns one
  error whose message is
  `D-REL-007: unexpected or malformed primary key clause` and whose span slices
  the source to `Foo`, and no relations.
- `ddlint::parse("input relation R(x: u32, y: u32) primary key (_x, rowId)")`
  returns no errors and `binder_names()` is `["_x", "rowId"]`.
- `ddlint::parse("input relation R(x: u32) primary key (Foo,)")` reports one
  `D-REL-007` spanning the `(`.
- `ddlint::parse("output relation R(x: u32) primary key (Foo)")` reports
  `D-REL-006`, as before, and exposes `R`.
- `ddlint::parse("import foo::bar as Baz")` returns no errors and `alias()` is
  `Some("Baz")`; `import foo as a::b` behaves as `D11` decides.

Quality criteria:

- Tests: `make test` passes, including the new tests named above.
- Verification: `L-SIM` discharged exhaustively over reachable product states;
  `L-SPEC` cross-checked to length 7; `L-FOLD` enumerated to length 4;
  `INV-CASE`, `INV-ALIAS`, and `INV-REFAC` discharged by named and property
  tests; each seeded fault observed failing and reverted.
- Lint and format: `make check-fmt`, `make lint`, `make markdownlint`, and
  `make nixie` pass.
- Performance: the new library tests add at most 1.5 seconds.

## Idempotence and recovery

All steps are re-runnable. Seeded faults are applied and reverted within one
working-tree session and never committed; confirm with `git diff` before each
commit. If proptest writes a file under `proptest-regressions/`, keep it only
if it records a genuine, fixed failure, and record it in
`Surprises & discoveries`. Each milestone is one or more commits that can be
reverted independently; `EP-M4` in particular is isolated so it can be dropped
without affecting behaviour.

## Artefacts and notes

Reconnaissance evidence, revision `7adbd8bc`:

```plaintext
suffix.rs:63-73  parse_primary_key_clause(...)? runs before the D-REL-004 and D-REL-006 checks
suffix.rs:104    if !has_column || !is_binder_identifier_list(tokens, list_start, *cursor) {
suffix.rs:215    (true, SyntaxKind::T_IDENT) => { ... }          // no text check
relation.rs:64   let binder = just(SyntaxKind::T_IDENT)        // no text check
imports.rs:36    just(SyntaxKind::K_AS).padded_by(ws).ignore_then(ident_parser)
```

Upstream DDlog `Parse.hs` (fetched 2026-11-23):

```plaintext
key_expr = withPos $ KeyExpr nopos <$> (parens varIdent) <*> expr
varIdent = lcIdentifier <?> "variable name"
imprt = Import nopos <$ reserved "import" <*> modname
          <*> (option (ModuleName []) $ reserved "as" *> modname)
modname = ModuleName <$> modIdent `sepBy1` reservedOp "::"
modIdent = identifier <?> "module name"
transIdent = ucScopedIdentifier <?> "transformer name"
```

Implementation transcripts are added here as milestones complete.

## Interfaces and dependencies

No new dependencies. At the end of `EP-M1`:

- `src/parser/ident_case.rs`: `pub(crate) fn is_lc_name(text: &str) -> bool`
  and `pub(crate) fn is_uc_name(text: &str) -> bool`. Owner: the parser. Reuse
  policy: every parser site that classifies an identifier's case calls this
  module; local first-character checks are not added. Pure, no allocation, no
  I/O.
- `src/parser/binder_list/mod.rs`: `BinderTokenClass`, `classify`,
  `BinderListState` (`START`, `step`, `verdict_kind`), `BinderListVerdict`, and
  `recognize`, all `pub(crate)`, with the derives shown in `EP-M1`. Owner: the
  parser. Reuse policy: the relation scanner and the primary-key accessor are
  its only callers; neither may re-implement binder-list recognition. Pure;
  `recognize` allocates only the binder-span vector.
- `src/parser/span_scanners/relations/suffix.rs`: `parse_primary_key_clause`
  becomes `pub(super)` (for its sibling test module) and returns the clause end
  with an optional first non-`LcName` binder span.
- `src/parser/ast/parse_utils/relation.rs`: `primary_key_clause` is removed if
  it has no remaining caller.
- Public API: unchanged signatures; rustdoc postconditions added to
  `PrimaryKey::binders` and `PrimaryKeyBinder::name`. `Import::alias()` keeps
  `Option<String>`; under `D11`'s acceptance outcome it may return scoped text.

## Revision note

- 2026-11-23: initial draft from Wyvern reconnaissance and upstream grammar
  lookup.
- 2026-11-23: design review applied. Three panel reviews (structure and
  contracts; alternatives and viability; failure modes and cost) produced these
  changes:
  - adopted a single shared binder-list recognizer for scanner and accessor
    (`D4` rewritten), replacing the `chumsky` `try_map` design;
  - replaced the unsound `L-FINITE` small-scope argument and the 137,257-case
    enumeration with an exhaustive product-automaton check (`L-SIM`), an EBNF
    cross-check (`L-SPEC`), and a short framing enumeration (`L-FOLD`);
  - added `D10`, keeping `D-REL-004` and `D-REL-006` precedence over the case
    check, after review showed the draft would have changed those codes and
    dropped relations;
  - added `D11` and an `EP-M3` probe for the likely silent truncation of
    scoped import aliases;
  - added `is_uc_name` to `D5`, restated `L-LEX` for keyword-kind tokens, and
    moved the new BDD scenarios to their own harness under the 400-line cap;
  - recorded lint constraints (`private_interfaces`, `print_stdout`,
    `indexing_slicing`, `integer_division_remainder_used`,
    `cognitive_complexity`, `private_intra_doc_links`), keyword and
    multi-line edge cases, an observability row, and seeded faults (a) to
    (d);
  - corrected D1's upstream-agreement wording, made `D7` an explicit
    approval item, and raised the budgets to 24 code files and 1,100 lines,
    and 14 documentation files and 380 lines.

  Remaining work and milestone order are otherwise unchanged; implementation
  still awaits approval.
