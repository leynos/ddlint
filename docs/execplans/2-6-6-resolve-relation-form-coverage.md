# Resolve relation form coverage (role/kind/bracket variants)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`,
and `Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: DRAFT (post design review)

## Purpose / big picture

Roadmap item `2.6.6` is open because the scanner, the typed Abstract Syntax
Tree (AST) wrapper, and the normative grammar disagree on what a relation
declaration actually is. Today the scanner in
`src/parser/span_scanners/relations.rs` only dispatches on three opening
keywords (`input`, `output`, and a bare `relation`), and the `Relation` wrapper
in `src/parser/ast/relation.rs` only surfaces `is_input()` and `is_output()`.
The conformance register tracks this as item `13` with status `scheduled`, and
the syntax spec in `docs/differential-datalog-parser-syntax-spec-updated.md`
§5.5 documents a much larger surface
(`Role × Kind × {paren-body, bracket-body}` with an optional `PrimaryKey`).

After this change, a maintainer should be able to read one role/kind/body
grammar, parse any spec-conformant relation declaration, inspect it through a
typed AST that distinguishes role, kind, and body form, and get deterministic
diagnostics for invalid combinations. Success is observable when:

- parser, unit tests, behavioural tests, and active docs describe the same
  grammar;
- all permitted role × kind × body-form combinations parse without error;
- ill-ordered preambles (`relation input ...`) and incompatible
  body+clause pairs produce stable diagnostics;
- `make check-fmt`, `make lint`, `make markdownlint`, `make nixie`, and
  `CI=1 make test` pass; and
- `docs/roadmap.md` item `2.6.6` is marked done only after those gates
  pass and conformance register item `13` is flipped to `implemented`.

## Spec deltas from upstream DDlog

Firecrawl research against the canonical `vmware-archive/differential-datalog`
repository (Parse.hs and the language reference, last substantive change
2022-07-23) shows that the local spec doc §5.5 diverges from upstream in five
places. These deltas must be resolved as part of the milestone because they
materially change what the parser should accept.

<!-- markdownlint-disable MD013 MD060 --><!-- Table rows are intentionally kept
intact for reviewability. -->

| #   | Local spec §5.5                                              | Upstream DDlog (authoritative)                                                                                 | Recommended action                                                                                                                                                                              |
| --- | ------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| D1  | `Role ::= 'input' \| 'output' \| 'internal'`                 | No `internal` keyword; "internal" is the *absence* of `input`/`output`                                         | Update spec: drop `internal` from the keyword list; treat absence of a role keyword as the implicit internal role. AST exposes this via `RelationRole::Internal` plus `role_keyword_present()`. |
| D2  | `PrimaryKey ::= '[' 'primary' 'key' '(' LcName ')' Expr ']'` | `primary key (var_name) expr` — no surrounding brackets, single lambda binder, restricted to `input` relations | Update spec: drop the outer brackets; clarify the binder is a single lambda-style variable followed by an expression; record the input-only static check.                                       |
| D3  | Each `RelationDecl` terminates with `';'`                    | No trailing `;`; declarations end where the next one begins                                                    | Update spec: drop the mandatory `';'` from `RelationDecl`. Defer scanner work — current scanner already does not require `;`.                                                                   |
| D4  | No `&` ref form                                              | `Role? Kind? '&'? UcName ...` declares `Ref<...>` element type                                                 | Add `'&'?` to the grammar in this milestone. Scanner records the marker; AST exposes `is_ref()`.                                                                                                |
| D5  | No relation-level `#[...]` attributes                        | Relations accept attribute prefixes (`relAttrs` in Parse.hs)                                                   | Cross-reference §9; confirm `src/parser/span_scanners/attributes.rs` already allows the relation role and kind keyword set.                                                                     |

<!-- markdownlint-enable MD013 MD060 -->

These corrections land in Milestone 5 alongside conformance-register updates.
They are listed here so design reviewers can challenge the direction before the
implementation milestones start.

## Recommended decision

Close item `2.6.6` by widening the parser to the upstream-conformant surface
(the local §5.5 grammar after deltas D1–D5 are applied) and introducing typed
`RelationRole` and `RelationKind` enums plus a `RelationBody` distinction. The
canonical parser contract becomes:

```text
RelationDecl ::= Attrs? Role? Kind? '&'? UcName Body PrimaryKey?
Body         ::= '(' Fields? ')' | '[' Type ']'
Role         ::= 'input' | 'output'
Kind         ::= 'relation' | 'stream' | 'multiset'
Fields       ::= Field (',' Field)* ','?
Field        ::= LcName ':' Type
PrimaryKey   ::= 'primary' 'key' '(' LcName ')' Expr    -- record body only
```

`Role` and `Kind` may each be absent. The typed AST models both axes
symmetrically with three-variant enums and parallel "keyword present"
predicates:

- `pub enum RelationRole { Input, Output, Internal }` — `Internal` is
  the default when no role keyword is present;
- `pub enum RelationKind { Relation, Stream, Multiset }` — `Relation`
  is the default when no kind keyword is present;
- `role_keyword_present() -> bool` and
  `kind_keyword_present() -> bool` distinguish defaulted axes from
  source-present ones for callers that need exact lossless fidelity.

Symmetry between the role and kind APIs is deliberate: the previous draft used
`Option<RelationRole>` on the role axis and a defaulted enum on the kind axis,
but that asymmetry forced every consumer to learn two patterns for one concept.
Both axes now use the enum-plus-predicate shape. See Decision Log entry 2 for
the rationale.

The `'&'` ref marker is recognised by the scanner and exposed as
`Relation::is_ref()`. The bracket body carries a single element type as an owned
`String` because the type may span multiple CST tokens. Bracket bodies cannot
combine with a `PrimaryKey` clause.

Spec-form lambda-style primary keys (`primary key (id) Expr`) are recognised at
scan time and their verbatim token text is preserved in the CST; typed access
to the expression body is deferred behind roadmap follow-up `2.6.6.1`. **Before
any scanner refactor lands, run the Milestone 0 spike below to confirm that the
end of an opaque PK expression can be reliably located without expression-level
parsing.** If the spike fails, stop and escalate before any other milestone
starts.

The input-only primary-key static check (diagnostic D-REL-006) is in-scope for
this milestone and is not deferred. Deferring it would prevent conformance
register item `13` from flipping to `implemented`, which is the headline
outcome the milestone exists to deliver.

## Constraints

- Align the active written contract in
  `docs/differential-datalog-parser-syntax-spec-updated.md` §5.5 to the final
  parser behaviour. No drift between scanner, AST, register, and spec.
- Keep the scope centred on relation declarations. Do not fold legacy-
  token policy (`2.6.7`) or brace-group extension policy (`2.6.8`) into this
  change.
- Preserve lossless Concrete Syntax Tree (CST) behaviour. Round-trip
  tests must continue to prove that parsed source text is preserved exactly.
- Preserve the existing `Relation::is_input()` and `Relation::is_output()`
  public surface. Layer the new typed accessors on top; do not remove the
  legacy ones until phase `2.8`.
- Add both unit tests and behavioural tests before considering the task
  done. Use `rstest` for parameterised matrices and `rstest-bdd` for any
  behavioural scenarios that need step-style coverage.
- Consider `proptest` for the role × kind × body-form combinatoric
  matrix; if used, keep the property simple (round-trip plus role/kind
  preservation) and bounded.
- Update active documentation
  (`docs/parser-conformance-register.md`, `docs/parser-implementation-notes.md`,
  `docs/ddlint-design.md`, `docs/users-guide.md`, `docs/developers-guide.md`)
  and, where the typed AST surface is substantive, add an ADR.
- File-size limit ≤ 400 lines per source file (`AGENTS.md`).
- All comments and docs in en-GB Oxford spelling.
- Caret-only dependency requirements; no panics in non-test code; lints
  not silenced except as a tightly-scoped last resort.
- Use Make targets for validation and run long commands with
  `set -o pipefail` and `tee`.

## Tolerances (exception triggers)

- Scope: if the implementation requires edits to more than 18 files or
  more than 500 net new code lines, stop and escalate.
- Interface: if the only coherent fix requires removing `is_input()` or
  `is_output()` from the public AST, stop and escalate.
- Compatibility: if non-test workspace code depends on the bare-relation
  dispatch shape in `relations.rs` beyond the scanner itself, document the
  callers before changing behaviour.
- Tokenizer: any non-keyword identifier collision (e.g. tests using
  `internal` as an identifier) blocks the tokenizer change. Resolve before
  extending the dispatcher.
- Validation: if `make check-fmt`, `make lint`, or `make test` still
  fails after three focused fix rounds, stop and escalate with the failing
  target names and logs.
- Complexity: if the new scanner preamble parser exceeds cyclomatic
  complexity 10 or function length 60 lines, refactor into helpers before
  proceeding (see `docs/complexity-antipatterns-and-refactoring-strategies.md`).

## Rejected forms (with diagnostics)

<!-- markdownlint-disable MD013 --><!-- Diagnostic table messages stay on one
row for stable review. -->

| Rejected form                                         | Diagnostic ID | Message                                                                                | Recovery                                                    |
| ----------------------------------------------------- | ------------- | -------------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| Kind keyword before role keyword                      | D-REL-001     | `relation role keyword (input/output) must precede the kind keyword`                   | consume both preamble keywords, then skip to next newline   |
| Two role keywords (`input output R(...)`)             | D-REL-002     | `at most one role keyword (input, output) is permitted`                                | skip to next newline                                        |
| Two kind keywords (`stream multiset R(...)`)          | D-REL-003     | `at most one kind keyword (relation, stream, multiset) is permitted`                   | skip to next newline                                        |
| Bracket form combined with primary key clause         | D-REL-004     | `bracket-form relations cannot declare a primary key clause`                           | accept the relation; drop the primary key tokens to newline |
| Empty bracket body (`R[]`)                            | D-REL-005     | `bracket-form relations require a single element type between '[' and ']'`             | skip the declaration (do not record a relation node)        |
| Primary key on non-`input` relation                   | D-REL-006     | `primary key clauses are only valid on input relations`                                | accept the relation; drop the primary key tokens to newline |
| Stray or malformed `primary` token                    | D-REL-007     | `unexpected or malformed primary key clause` (retain current scanner wording)          | skip to next newline (existing behaviour)                   |
| Spec-form bracket primary key (`[ primary key ... ]`) | D-REL-008     | `bracket-wrapped primary key clauses are not supported; remove the surrounding '['/']` | accept the relation; drop the bracket block to its `]`      |

<!-- markdownlint-enable MD013 -->

Per-diagnostic recovery paths are normative for this milestone: tests must
assert both the diagnostic and the resulting CST state (relation recorded or
not, neighbour declaration boundary preserved).

## Test matrix

Drive a parameterised matrix in `src/parser/tests/relations.rs`. Each row is one
`#[case(...)]`. Body forms abbreviate as `()` for record and `[]` for bracket.

| #   | Role   | Kind     | Body | Ref | Primary key | Expected |
| --- | ------ | -------- | ---- | --- | ----------- | -------- |
| 1   | absent | absent   | `()` | no  | absent      | accept   |
| 2   | absent | absent   | `()` | no  | present     | accept   |
| 3   | absent | absent   | `[]` | no  | absent      | accept   |
| 4   | input  | absent   | `()` | no  | present     | accept   |
| 5   | output | absent   | `()` | no  | absent      | accept   |
| 6   | input  | relation | `()` | no  | present     | accept   |
| 7   | output | relation | `()` | no  | absent      | accept   |
| 8   | absent | relation | `[]` | no  | absent      | accept   |
| 9   | input  | stream   | `()` | no  | absent      | accept   |
| 10  | output | stream   | `[]` | no  | absent      | accept   |
| 11  | absent | stream   | `()` | no  | absent      | accept   |
| 12  | input  | multiset | `()` | no  | present     | accept   |
| 13  | absent | multiset | `()` | no  | absent      | accept   |
| 14  | output | multiset | `[]` | no  | absent      | accept   |
| 15  | input  | absent   | `()` | yes | absent      | accept   |
| 16  | output | relation | `[]` | yes | absent      | accept   |

Rejection rows (literal source on the left, expected diagnostic on the right):

| #   | Source                                | Expected         |
| --- | ------------------------------------- | ---------------- |
| 17  | `relation input R(...)`               | reject D-REL-001 |
| 18  | `input output R(...)`                 | reject D-REL-002 |
| 19  | `stream multiset R(...)`              | reject D-REL-003 |
| 20  | `input R[u32] primary key (id)`       | reject D-REL-004 |
| 21  | `input R[]`                           | reject D-REL-005 |
| 22  | `output R(x: u32) primary key (x)`    | reject D-REL-006 |
| 23  | `input R(x: u32) [primary key (x) x]` | reject D-REL-008 |

Total: 23 parameterised cases. Add four further behavioural cases in
`tests/relation_form_grammar.rs` covering multi-line declarations, attribute
placement, comment-laden preambles, and recovery after a malformed preamble.

## Decisions to be made and documented

- Introduce `RelationRole` and `RelationKind` enums on the typed AST,
  modelled symmetrically: `pub enum RelationRole { Input, Output, Internal }`
  (`Internal` is the default when no keyword is present);
  `pub enum RelationKind { Relation, Stream, Multiset }` (`Relation` is the
  default when no keyword is present). `Relation::role() -> RelationRole`,
  `Relation::role_keyword_present() -> bool`,
  `Relation::kind() -> RelationKind`,
  `Relation::kind_keyword_present() -> bool`, `Relation::is_ref() -> bool`,
  `Relation::body() -> RelationBody`,
  `Relation::element_type() -> Option<String>`. Keep `is_input()` /
  `is_output()` as derived helpers backed by `role()`; do not deprecate yet
  (phase `2.8` owns API freeze). Annotate both helpers with a rustdoc comment
  along the lines of "Derived from `role()`; prefer `role()` in new code." so
  the transitional intent is visible at the call site.
- Bracket form `Name[Type]` modelled as
  `pub enum RelationBody { Fields(Vec<(String, String)>), ElementType(String) }`.
  `columns()` continues to return an empty vector for the bracket form for
  backwards compatibility. The enum is retained (rather than the alternative of
  two `Option<...>` accessors) so exhaustive `match` sites cannot drift if a
  future body form is added.
- Primary-key conformance: scanner continues to accept the existing
  `primary key (id, ...)` shape unchanged; spec-form bracket-wrapped primary
  keys are rejected with D-REL-008; the upstream lambda-style
  `primary key (id) Expr` body is recognised as opaque text in this milestone,
  with typed access deferred. Track follow-up as roadmap item `2.6.6.1`. The
  opaque-text boundary is gated by the Milestone 0 spike below.
- Tokenizer: no new keyword for `internal` (upstream does not reserve
  it). `K_STREAM` and `K_MULTISET` already exist (`src/language.rs`,
  `src/tokenizer.rs`) and are threaded into the relation dispatcher. Confirm no
  fixture relies on `stream`/`multiset` as identifiers, since those *are*
  reserved. Documented in Decision Log.
- CST node shape: keep the existing `N_RELATION_DECL` node. Reserved
  variants `N_RELATION_ROLE` and `N_RELATION_SEMANTICS` in `src/language.rs`
  stay unused for this milestone (avoids a coordinated CST consumer change).
- Scanner refactor strategy: extract the preamble into a small state-
  machine helper modelled on `parse_index_decl` in
  `src/parser/span_scanners/indexes.rs`, returning a typed `Preamble` struct
  (`role`, `kind`, `is_ref`, name span, body open delimiter). This isolates the
  role/kind ordering check from body parsing and keeps cyclomatic complexity
  bounded.

### ADR recommendation

Add ADR-002 covering the introduction of `RelationRole`, `RelationKind`, and
`RelationBody`. Rationale: first multi-axis typed classification on the AST;
sets precedent for index and transformer classification; and ADR-001 (parser
crate split) needs to point at a settled relation surface. The ADR documents:

- the enum shapes and their `Display`/`FromStr` policy;
- the enum-plus-predicate "unmarked = implicit internal" choice;
- the decision to keep `is_input()` / `is_output()` as derived helpers;
- the deferred typed access to spec-form primary keys; and
- the spec deltas D1–D5 above and how the local spec is reconciled with
  upstream.

## Risks and mitigations

- Risk: spec-doc rewrite (deltas D1–D5) is rejected by review on the
  grounds of stability. Severity: medium. Likelihood: medium. Mitigation:
  produce the spec changes as a separate commit inside Milestone 5 and request
  explicit reviewer sign-off; if rejected, document the divergence in the
  conformance register instead of changing the spec.
- Risk: token-class change (none expected, but threading
  `K_STREAM`/`K_MULTISET` may expose fixtures using those words as
  identifiers). Severity: low. Likelihood: low. Mitigation: search fixtures
  before Milestone 2; add tokenizer-level recovery tests.
- Risk: the preamble parser grows unwieldy. Severity: medium.
  Likelihood: medium. Mitigation: use the index-scanner helper pattern; enforce
  the complexity tolerance above.
- Risk: bracket-form rejection of primary key is too strict if a fixture
  depends on it. Severity: low. Likelihood: low. Mitigation: upstream is
  unambiguous (PrimaryKey only on record body); if a fixture is found, treat it
  as a fixture bug.
- Risk: downstream lint rules (phase 4) use `is_input()` / `is_output()`
  and are unaware of role distinctions. Severity: low. Likelihood: high.
  Mitigation: keep both helpers; provide `role()` as the canonical path
  forward; document the migration in `docs/developers-guide.md`.
- Risk: diagnostic-stability requirement under ADR-001. Severity:
  medium. Likelihood: low. Mitigation: pin diagnostic message text by string in
  tests and enumerate the IDs in the conformance register.

## Milestones

Each milestone lands as a single atomic commit and CodeRabbit review, except
where explicitly split below.

### Milestone 0 — Opaque-PK preservation spike

Time-boxed to two hours. Outcome decides whether the milestone proceeds.

Activities:

- Lift three representative upstream programs containing
  `primary key (id) <expr>` clauses (e.g. `redist.dl`, tutorial examples) into
  `tests/fixtures/relation_pk_spike/`.
- Write a throwaway scanner-level probe that recognises the clause as
  balanced bracket/paren text terminated at the next top-level newline outside
  any open delimiter, and asserts the source-text round-trip.
- Confirm that the next declaration boundary is reachable without
  expression-level parsing in every case.

Exit criteria:

- All three fixtures round-trip with no spurious diagnostics → proceed
  to Milestone 1.
- Any fixture fails → stop and escalate; this milestone cannot ship as
  scoped.

Rollback: the spike fixtures are thrown away regardless of outcome; no
production code is touched.

### Milestone 1 — Tokenizer audit and groundwork

Files touched: `src/tokenizer.rs` (no keyword additions — confirm and document);
`src/parser/span_scanners/attributes.rs` (confirm `K_STREAM`/`K_MULTISET` are
accepted as attribute targets; add coverage tests where missing).

Tests added: `tokenizer.rs` regression tests confirming `internal` continues to
tokenize as an identifier (`T_IDENT`), since upstream does not reserve it; and
that `stream` / `multiset` (which *are* keywords in the existing tokenizer)
remain `K_STREAM` / `K_MULTISET`.

Docs updates: none beyond this ExecPlan's `Progress` section.

Rollback: if a fixture or downstream consumer is found that depends on
`internal` being a reserved keyword, revert this milestone's tokenizer change
as a single commit, raise an issue documenting the consumer, and fall back to
extending the dispatcher with explicit `identifier == "internal"` matching
inside the relation scanner only. The plan can still close on that path; the
cost is a slightly less clean tokenizer surface.

CodeRabbit gate: PR description must call out the deliberate non-reservation of
`internal` and the rollback strategy above.

### Milestone 2 — Scanner refactor, preamble parser, bracket form, diagnostics

Files touched: `src/parser/span_scanners/relations.rs` (rewrite dispatch around
a `parse_role_kind_preamble` helper; add bracket-form body handling; add the
eight new diagnostics D-REL-001 through D-REL-008);
`src/parser/span_scanners/mod.rs` (optional helper module export);
`src/parser/ast/parse_utils/relation.rs` (extract a `relation_body` parser that
accepts either paren or bracket forms; keep `relation_columns()` available for
callers).

New files: none expected unless `relations.rs` would exceed 400 lines, in which
case split the preamble helper into
`src/parser/span_scanners/relations/preamble.rs` and re-export.

Tests added (in `src/parser/tests/relations.rs`):

- the full 23-case matrix above as `parses_relation_form_matrix`,
  asserting acceptance/rejection and (for rejects) the diagnostic ID and
  recovery state only — AST accessors are *not* asserted here;
- `recovers_after_malformed_preamble`;
- `parses_multiline_role_kind_preamble`.

Test-matrix ownership rule: Milestone 2 authors `parses_relation_form_matrix`
asserting accept/reject + diagnostic + CST recovery. Milestone 3 extends the
same rows with AST accessor assertions (`role()`, `kind()`, etc.). The two
passes share the same `#[rstest]` table by parameterising on a
`RelationExpectation` struct.

Docs updates: none in this milestone.

CodeRabbit gate: PR must include the diagnostic table and confirm cyclomatic
complexity of the new preamble parser stays under 10.

### Milestone 3 — Typed AST surface

Files touched: `src/parser/ast/relation.rs` (introduce `RelationRole`,
`RelationKind`, `RelationBody`; add `role()`, `role_keyword_present()`,
`kind()`, `kind_keyword_present()`, `is_ref()`, `body()`, `element_type()`;
re-express `is_input()` / `is_output()` via `role()`);
`src/parser/tests/specs.rs` (extend `RelationSpec` to assert role, kind, ref,
and body form); `src/parser/tests/relations.rs` (extend the existing matrix
with AST accessor columns per the test-matrix ownership rule above).

New files: `src/parser/ast/relation/inspect.rs`,
`src/parser/tests/relation_proptest.rs`, and the checked-in proptest regression
seed under `proptest-regressions/parser/tests/relation_proptest.txt`.

Tests added:

- unit tests in `src/parser/ast/relation.rs` for `role()`, `kind()`,
  `is_ref()`, and `element_type()`;
- `proptest` generator and property in
  `src/parser/tests/relation_proptest.rs` over the role × kind × body × ref ×
  pk space (96 cells before pruning invalid combinations). Property: for any
  generated valid declaration, `parse()` succeeds, CST text equals input text,
  and `role()` / `kind()` / `*_keyword_present()` / `is_ref()` / `body()` match
  the generator inputs. The proptest is required, not optional: hand-picked
  accepts cover ~17% of the space, which is insufficient for an ADR-001-frozen
  contract.

Docs updates: `docs/ddlint-design.md` adds a short subsection naming the new
enums and pointing at ADR-002; `docs/developers-guide.md` records the
convention that `role()` is canonical and `is_input()` / `is_output()` are
derived ergonomics helpers.

CodeRabbit gate: PR must reference the ADR-002 draft.

### Milestone 4 — Primary-key conformance decision

Files touched: `src/parser/span_scanners/relations.rs` (recognise the spec-form
bracket-wrapped primary key clause only to reject it with D-REL-008; recognise
the upstream lambda-style body as opaque text preserved in the CST);
`src/parser/ast/relation.rs` (extend `primary_key()` semantics to continue
returning record-form key column names; add a guarded TODO pointing at follow-up
`2.6.6.1`).

Tests added: matrix cases 20, 22, and 23 prove rejections;
`preserves_spec_form_primary_key_text` asserts source-text round-trip; optional
behavioural test in `tests/relation_form_grammar.rs` for an input-only static
check (D-REL-006), gated behind the implementer's judgement on milestone scope.

Docs updates: `docs/parser-implementation-notes.md` documents the deferred
typed access; add roadmap follow-up `2.6.6.1` (do not mark done) tracking typed
primary-key expression access.

CodeRabbit gate: PR must justify the deferral in the description.

### Milestone 5 — Spec reconciliation, ADR, and user-facing docs

Lands as three independent commits so partial reviewer pushback on any one
slice does not bounce the rest.

#### Commit 5a — Spec deltas D1–D5

- `docs/differential-datalog-parser-syntax-spec-updated.md` §5.5 —
  apply deltas D1–D5 (drop `internal` keyword; correct `PrimaryKey` shape; drop
  mandatory `';'`; add `'&'?` ref marker; cross-reference attribute placement
  with §9);
- `docs/parser-conformance-register.md` — rewrite item `13` and flip
  status to `implemented`;
- `docs/parser-implementation-notes.md` — describe the preamble parser
  and the role/kind/body modelling.

#### Commit 5b — ADR-002

- new `docs/adr-002-relation-role-kind-modelling.md`, explicitly
  cross-referencing ADR-001 so the freeze handshake is documented;
- `docs/ddlint-design.md` — short subsection on the typed AST surface
  pointing at ADR-002.

#### Commit 5c — User-facing docs

- `docs/users-guide.md` — document the new accepted forms and the eight
  new diagnostics;
- `docs/developers-guide.md` — internal convention for `role()` and
  `kind()` and the transitional status of `is_input()` / `is_output()`.

Tests added: none in this milestone.

CodeRabbit gate per commit: ensure `make markdownlint` and `make nixie` pass on
each slice independently.

### Milestone 6 — Verification and close-out

Activities:

- Run `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`,
  `make lint`, and `CI=1 make test`, each with `set -o pipefail` and `tee` to
  `/tmp/2-6-6-<target>.log`.
- Add a behavioural test file `tests/relation_form_grammar.rs` mirroring
  the style of `tests/index_declaration_grammar.rs`, covering:
  - canonical record form with role+kind preserved;
  - canonical bracket form with role preserved;
  - bracket+primary-key rejection (D-REL-004);
  - bracket-wrapped primary-key rejection (D-REL-008);
  - multi-line and attribute-prefixed forms.
- Optional `proptest` over the role × kind × body-form generator with
  the property "round-trip text equals input and parsed `role()`/`kind()` match
  the input keywords". If complexity is prohibitive, defer.
- After gates pass, mark `docs/roadmap.md` item `2.6.6` done.

CodeRabbit gate: final review focuses on conformance register status,
documentation completeness, and ADR-002.

## Edge cases to cover

- Whitespace and trivia between role and kind keywords (spaces, tabs,
  comments).
- Multi-line declarations spanning the preamble and the body.
- Bracket form with and without an attribute prefix.
- `relation stream` and `relation multiset` orderings: rejected as
  D-REL-001 (Role precedes Kind by the spec); document that the bare `relation`
  keyword cannot precede `stream` or `multiset`.
- Attribute placement (`#[...]`) on each form, cross-referencing
  `src/parser/span_scanners/attributes.rs` and §9. Confirm the attribute
  scanner already permits `K_INPUT`, `K_OUTPUT`, `K_RELATION`, `K_STREAM`, and
  `K_MULTISET`; verify `&` after attributes still parses.
- Recovery after a malformed preamble: the scanner discards through the
  next newline boundary, matching documented behaviour.
- Comments between keywords in the preamble must continue to work.
- Bare `relation` with no role and no kind remains valid (matrix row 1).
- `&` placement: after any role/kind preamble but before the name
  (`input & R(x: u32)`).

## Definition of done

- [ ] Milestone 0 PK-preservation spike passes for all three fixtures.
- [ ] Tokenizer audit confirms no keyword churn; `internal` remains
      `T_IDENT`.
- [ ] Attribute scanner accepts the relation-keyword set used by the new
      preamble parser.
- [ ] Scanner accepts all 16 valid (role, kind, body, ref) combinations
      from the matrix.
- [ ] Scanner rejects the seven invalid combinations with the documented
      diagnostics and per-diagnostic recovery state.
- [ ] Typed AST exposes `role()`, `role_keyword_present()`, `kind()`,
      `kind_keyword_present()`, `is_ref()`, `body()`, and
      `element_type()`; legacy `is_input()` / `is_output()` preserved
      and documented as derived.
- [ ] `proptest` over the role × kind × body × ref × pk space passes
      the round-trip + accessor-preservation property.
- [ ] Round-trip tests still pass; CST text equals input text.
- [ ] `tests/relation_form_grammar.rs` exercises the public `parse()`
      entry point.
- [x] `docs/parser-conformance-register.md` item `13` is `implemented`.
- [ ] `docs/differential-datalog-parser-syntax-spec-updated.md` §5.5
      updated with deltas D1–D5;
      `docs/parser-implementation-notes.md`, `docs/ddlint-design.md`,
      `docs/users-guide.md`, and `docs/developers-guide.md` updated.
- [ ] `docs/adr-002-relation-role-kind-modelling.md` added.
- [x] Roadmap follow-up `2.6.6.1` recorded for typed primary-key
      expression access.
- [ ] `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`,
      `make lint`, and `CI=1 make test` all pass.
- [ ] `docs/roadmap.md` item `2.6.6` marked done.

## Validation commands

Run from the repository root, capturing output with `tee`:

```shell
set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-6-make-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-6-make-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-6-make-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-6-make-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/2-6-6-make-lint.log
set -o pipefail; CI=1 make test 2>&1 | tee /tmp/2-6-6-make-test.log
```

Successful completion means all six commands exit with status `0`, the new
relation matrix and behavioural tests pass, the conformance register is
flipped, and the roadmap item can be closed.

## Progress

- [x] (2026-05-29) Reviewed roadmap item `2.6.6`, conformance register
      item `13`, the syntax spec relation grammar, the relation scanner,
      the typed AST wrapper, existing parser and behavioural tests, and
      the upstream DDlog grammar via firecrawl.
- [x] (2026-05-29) Drafted this ExecPlan and ran the Logisphere
      pre-implementation review. Incorporated revisions: enum-symmetric
      role/kind APIs, Milestone 0 PK-preservation spike, Milestone 5
      split into three commits, mandatory `proptest`, per-diagnostic
      recovery, explicit rejected alternatives in Decision Log.
- [x] (2026-06-16) Ran Milestone 0 (PK-preservation spike). Cloned
      upstream DDlog into `/tmp/ddlog-spike`, copied three representative
      fixtures into `/tmp/relation_pk_spike`, and ran the delimiter-depth probe
      logged at `/tmp/2-6-6-milestone-0-pk-spike.log`. The probe found clause
      boundaries for `primary key (x) (x.author, x.title)`,
      `primary key (row) (row.id)`, and `primary key (x) ()`, and each next
      declaration boundary was reachable.
- [x] (2026-06-16) Landed Milestone 1 (tokenizer audit). Added tokenizer
      regression coverage confirming `internal` remains `T_IDENT` while
      `stream` and `multiset` remain `K_STREAM` and `K_MULTISET`. Added
      attribute scanner coverage for `input`, `output`, bare `relation`,
      `stream`, and `multiset` relation targets.
- [x] (2026-06-16) Landed Milestone 2 (scanner refactor + diagnostics).
      Replaced the keyword-only relation span scanner with a cursor parser
      that accepts role/kind/ref preambles, record bodies, bracket bodies, and
      opaque primary-key expressions. Added the relation form matrix and
      recovery diagnostics in `src/parser/tests/relations.rs`; updated
      attribute unit and behavioural fixtures to the canonical `stream R(...)`
      and `multiset R(...)` forms. Split cursor helpers into
      `src/parser/span_scanners/relations/cursor.rs` and preamble parsing into
      `src/parser/span_scanners/relations/preamble.rs` to keep source files
      under the 400-line limit. Focused parser coverage passed with
      `cargo test parser::tests --lib`; final deterministic gates passed:
      `make fmt`, `make check-fmt`, `make markdownlint`, `make nixie`,
      `make lint`, and `CI=1 make test`. CodeRabbit then reported four
      concerns: preamble cognitive complexity, missing internal helper docs in
      `preamble.rs` and `cursor.rs`, and an implicit delimiter contract. Fixed
      them by extracting `PreambleState`, documenting the `pub(super)` helper
      surface, and guarding invalid close delimiters with D-REL-007; all
      gates passed again after the fixes.
      A follow-up CodeRabbit pass found that the delimiter-contract
      `debug_assert!` still left a silent release fallback. The branch now
      returns D-REL-007 instead of using `unreachable!()`, preserving the
      repository's no-production-panics policy while making contract violations
      non-silent. A third CodeRabbit pass completed with zero findings.
- [x] (2026-06-16) Landed Milestone 3 (typed AST surface + proptest).
      Added `RelationRole`, `RelationKind`, and `RelationBody` to
      `src/parser/ast/relation.rs`, exposed role/kind keyword-presence
      predicates, ref-marker access, body access, and bracket element-type
      access, and re-expressed `is_input()` / `is_output()` through `role()`.
      Extended the relation matrix to assert AST accessors and added
      `src/parser/tests/relation_proptest.rs` over role × kind × body × ref ×
      pk combinations. The property found a real scanner gap for bare
      `& R(...)`; the scanner now dispatches line-start `&` and the regression
      seed is checked in under `proptest-regressions/`. Updated
      `docs/ddlint-design.md` and `docs/developers-guide.md` with the relation
      AST conventions. Deterministic gates passed before each CodeRabbit
      review: `make fmt`, `make check-fmt`, `make markdownlint`, `make nixie`,
      `make lint`, and `CI=1 make test`. CodeRabbit reported documentation
      gaps in the relation inspection helpers, an Oxford-spelling conflict in
      a comment, a dead branch in the proptest, and an import simplification.
      Fixed the valid issues; the spelling conflict was resolved by avoiding
      the disputed word. A final CodeRabbit pass completed with zero findings.
- [x] (2026-06-16) Landed Milestone 4 (primary-key decision). Confirmed that
      Milestone 2 already implemented the scanner-side D-REL-004, D-REL-006,
      D-REL-008, and opaque primary-key suffix preservation. Added
      `preserves_spec_form_primary_key_text` to pin the remaining contract:
      `primary key (row) (row.author, row.title)` round-trips in CST text while
      `Relation::primary_key()` continues to expose only `row`. Documented the
      deferred typed expression accessor in `src/parser/ast/relation.rs`,
      `docs/parser-implementation-notes.md`, and roadmap follow-up `2.6.6.1`.
      Deterministic gates passed (`make fmt`, `make check-fmt`,
      `make markdownlint`, `make nixie`, `make lint`, and
      `CI=1 make test`), then CodeRabbit completed with zero findings.
- [x] (2026-06-16) Landed Milestone 5a (spec deltas D1–D5). Updated syntax
      spec §5.5 to drop `internal` as a keyword, drop mandatory relation
      semicolons, add declaration `&`, model bracket bodies, and document
      input-only spec-form primary keys without bracket wrapping. Flipped
      conformance-register item `13` to `implemented` and expanded
      `docs/parser-implementation-notes.md` with the relation preamble scanner
      and typed AST surface. Deterministic doc gates passed (`make fmt`,
      `make check-fmt`, `make markdownlint`, and `make nixie`), then
      CodeRabbit completed with zero findings.
- [ ] (YYYY-MM-DD) Landed Milestone 5b (ADR-002).
- [ ] (YYYY-MM-DD) Landed Milestone 5c (user-facing docs).
- [ ] (YYYY-MM-DD) Landed Milestone 6 (verification + close-out).

## Surprises & Discoveries

- The existing parser test labelled "internal" in
  `src/parser/tests/relations.rs` actually exercises the bare
  `relation UserSession(...)` form, not an explicit `internal` keyword. This
  matches upstream (no `internal` keyword) and informs the
  `Option<RelationRole>` AST choice.
- The local spec doc §5.5 lists `internal` as a keyword and wraps the
  `PrimaryKey` clause in brackets. Both diverge from upstream DDlog
  (`vmware-archive/differential-datalog`, Parse.hs as of 2022-07-23). The
  deltas table above resolves the divergence.
- `K_STREAM` and `K_MULTISET` already exist as `SyntaxKind` variants
  (`src/language.rs`) but are not consumed by the relation scanner. Threading
  them through is a single-point change in the dispatcher rather than a
  tokenizer change.
- `N_RELATION_ROLE` and `N_RELATION_SEMANTICS` already exist as reserved
  variants but are not produced anywhere. They remain reserved for a future CST
  refinement.
- The relation scanner uses a `chumsky`-based span recogniser whereas
  the index scanner uses a hand-rolled cursor walker. Aligning on the
  index-scanner style for the new preamble parser keeps complexity predictable
  and matches the pattern landed in `2.6.4`.
- The ADR recommendation still mentioned `Option<RelationRole>` from an
  earlier draft even though the accepted API shape is the enum-plus-predicate
  model. Corrected the plan before implementation to keep the ADR guidance
  aligned with the Decision Log.
- A plain `primary key` text search is not sufficient for the scanner because
  upstream examples contain comments such as `Example: primary key.` before the
  declaration. The production scanner should only enter PK handling after a
  relation body has already been accepted and the stream is positioned at a
  candidate clause.
- The Milestone 0 probe supports the planned opaque-expression strategy:
  tracking `()`, `[]`, and `{}` delimiter depth until the next zero-depth
  newline is enough for the three sampled upstream forms, including tuple and
  unit expressions.
- Milestone 1 required no production tokenizer or attribute scanner changes:
  `internal` was already absent from `KEYWORDS`, and
  `src/parser/span_scanners/attributes.rs` already accepted `K_INPUT`,
  `K_OUTPUT`, `K_RELATION`, `K_STREAM`, and `K_MULTISET`.
- The original test matrix row for a role-less primary-key relation conflicted
  with D-REL-006 and the upstream delta that restricts primary keys to `input`
  relations. The implementation follows D-REL-006: role-less relations are
  implicit internal relations, so primary-key clauses on them are rejected.
- The scanner refactor exceeded the single-file limit when implemented in one
  module. Extracting balanced-block and trivia cursor helpers into
  `src/parser/span_scanners/relations/cursor.rs`, then extracting preamble
  parsing into `src/parser/span_scanners/relations/preamble.rs`, kept the
  scanner files below the 400-line limit (`relations.rs` 365 lines,
  `relations/cursor.rs` 206 lines, `relations/preamble.rs` 194 lines).
- The CST builder already wraps lexer `N_ERROR` tokens as error nodes, but the
  parser did not report a corresponding diagnostic unless another scanner also
  failed nearby. Milestone 2 now appends parser-level lexer diagnostics after
  scanner diagnostics so invalid-token parses fail deterministically without
  changing the precedence of more specific rule diagnostics.
- `tests/attribute_placement.rs` still used the old `stream relation` and
  `multiset relation` forms. Updating those behavioural fixtures to
  `stream R(...)` and `multiset R(...)` aligned them with the Milestone 2
  grammar and made the full `CI=1 make test` gate pass.
- CodeRabbit's complexity finding was correct: `parse_preamble` had become a
  compact but nested state machine. Extracting `PreambleState` preserved the
  diagnostic order and cursor advancement while making D-REL-001 through
  D-REL-003 validation points easier to inspect.
- The second CodeRabbit pass recommended `unreachable!()` for an invalid close
  delimiter in `consume_open_for_close`. That would have introduced a
  production panic, so the implementation instead returns D-REL-007 from the
  invalid-contract arm.
- The Milestone 3 `proptest` generator discovered that `& R(id: u32)` was
  valid under the planned `Role? Kind? '&'? UcName Body` grammar but was not a
  scanner candidate. Dispatching line-start `T_AMP` and allowing the preamble
  parser to stop before `&` fixed the gap.
- `Relation::element_type()` returns `Option<String>` rather than
  `Option<&str>`. Bracket element types can span multiple CST tokens, so an
  owned string avoids returning a borrowed value assembled from temporary token
  text.
- CodeRabbit suggested replacing the proptest's dead missing-relation branch
  with `expect()`, but `make lint` proved that `expect_used` is denied in that
  macro-expanded test context. Replacing the branch with `remove(0)` after the
  length assertion cleared both the review concern and the lint gate.
- CodeRabbit gave contradictory spelling advice on "parenthesized" versus
  "parenthesised". The repository instructions require Oxford spelling, but the
  final comment avoids the disputed word entirely so the style question does
  not keep resurfacing in reviews.
- Milestone 4 did not need new scanner code. The relation suffix scanner
  already preserves same-line opaque primary-key expressions after the binder
  block; the missing piece was regression coverage and documentation of the
  typed-access boundary.

## Decision Log

1. Extend the parser to the upstream-conformant surface rather than
   trimming the spec down to the legacy three-keyword dispatcher. The spec is
   the normative contract; the scanner is the implementation deficit.
2. Model `RelationRole` as a three-variant enum (`Input | Output | Internal`)
   with `Internal` as the default-when-absent, and pair it with
   `role_keyword_present() -> bool`. Mirrors `RelationKind` /
   `kind_keyword_present()`. **Rejected alternative:** `Option<RelationRole>`
   with no `Internal` variant. Reason for rejection: the asymmetry with the
   kind axis forced every consumer to learn two patterns for one concept; the
   upstream archive is unlikely to ever reserve `internal`, so the hedge bought
   no future value.
3. Proceed past Milestone 0 using delimiter-depth opaque primary-key clause
   preservation. The spike showed that the expression boundary can be found
   without expression parsing for the sampled upstream declarations. The
   production implementation must avoid comment false positives by only looking
   for `primary key` after a relation body has parsed successfully.
4. Keep Milestone 1 as coverage-only groundwork. The audit confirmed the
   intended tokenizer and attribute-target behaviour already exists, so adding
   production code would only churn stable surfaces.
5. Treat role-less primary-key declarations as invalid under D-REL-006. The
   matrix row that listed them as accepted was superseded by the explicit
   upstream-conformance decision that primary keys are input-only.
6. Split cursor movement and balanced-block scanning out of `relations.rs`
   rather than keeping the whole scanner in one file. This preserves the
   file-size constraint and keeps grammar policy separate from token walking.
7. Surface lexer `N_ERROR` tokens as parser diagnostics after scanner
   diagnostics. The CST already preserves error nodes; appending diagnostics
   makes invalid-token parses fail deterministically while preserving existing
   first-error expectations from more specific scanners.
8. Treat `relations/cursor.rs` and `relations/preamble.rs` as internal
   interfaces that need Rustdoc despite `pub(super)` visibility. CodeRabbit's
   helper-doc findings match the project rule that internally facing
   conventions should be documented for maintainers.
9. Return a parser diagnostic rather than panicking when
   `consume_open_for_close` is called with a non-closing delimiter. **Rejected
   alternative:** use `unreachable!()` as suggested by CodeRabbit. Reason for
   rejection: this repository forbids production panics; D-REL-007 is a
   non-silent failure path that preserves that policy.
10. Support bare ref relations (`& R(...)`) in the scanner. The grammar permits
    the `&` marker after an absent role and absent kind; `proptest` found this
    gap once the role × kind × ref space was generated.
11. Return an owned `String` from `Relation::element_type()`. **Rejected
    alternative:** return `Option<&str>` as the original plan stated. Reason
    for rejection: bracket element types may be composed from several CST
    tokens, and the wrapper has no stable single borrowed slice to return.
12. Avoid `expect()` in `relation_proptest.rs` despite CodeRabbit's suggestion.
    Clippy's `expect_used` lint applies in the property test expansion, so the
    implementation removes the single relation from the vector after asserting
    its length instead.
13. Apply spec deltas D1–D5 to the local spec doc in Commit 5a, ADR-002
   in Commit 5b, and user-facing docs in Commit 5c, rather than bundling them.
   Partial reviewer pushback on any slice does not bounce the others.
14. Keep `is_input()` and `is_output()` in this milestone, annotated as
   derived. **Rejected alternative:** deprecate or remove them now under
   ADR-002. Reason for rejection: phase `2.7` has not yet locked the public API
   and phase 4 lint rules already use them; churning them twice (now and at
   freeze time) is wasted effort.
15. Retain `RelationBody` as a two-variant enum (`Fields | ElementType`).
   **Rejected alternative:** expose `fields() -> Option<...>` and
   `element_type() -> Option<...>` directly. Reason for rejection: exhaustive
   `match` is the cheapest way to guarantee that a future third body form (e.g.
   union) cannot silently slip past consumers.
16. Include the `&` ref-relation form (delta D4) in this milestone.
   **Rejected alternative:** defer to a follow-up. Reason for rejection: it is
   the only delta that adds surface area; landing it alongside D1–D3 keeps the
   spec-reconciliation commit cohesive.
17. Defer typed access to spec-form (lambda-style) primary keys; only
   preserve their text in the CST. Record follow-up as `2.6.6.1`. The Milestone
   0 spike gates this decision.
18. Enforce D-REL-006 (primary key only on `input` relations) inside
   this milestone, not as a follow-up. Deferring it would prevent conformance
   register item `13` from flipping to `implemented`.
19. Promote `proptest` from optional (Milestone 6) to required
   (Milestone 3). Hand-picked accepts cover ~17 % of the role × kind × body ×
   ref × pk space; an ADR-001-frozen contract needs better.
20. Recommend ADR-002 for the role/kind/body modelling and the
    spec-delta reconciliation, explicitly cross-referencing ADR-001.

## Outcomes & Retrospective

To be completed once the milestones land. Capture: the final accepted grammar,
the final typed AST signatures, the set of diagnostic strings, and any deferred
follow-ups (notably `2.6.6.1` for typed primary-key expression access).

## References & resources

Skills (load via the appropriate routers):

- `rust-router` → `rust-types-and-apis` for `RelationRole` /
  `RelationKind` enum design, newtype/typestate considerations, and the
  `Option<RelationRole>` convention.
- `rust-router` → `rust-errors` for diagnostic shape, message stability,
  and `Simple<SyntaxKind>` versus `thiserror` boundaries.
- `arch-crate-design` for ADR-001 alignment and follow-up scoping.
- `nextest` for running test subsets such as
  `cargo nextest run -E "test(parses_relation_form_matrix)"`.
- `execplans` (this skill) for living-document maintenance.
- `firecrawl` for any further upstream grammar verification.

Local documentation:

- `docs/parser-implementation-notes.md`
- `docs/parser-conformance-register.md`
- `docs/differential-datalog-parser-syntax-spec-updated.md` §5.5, §9
- `docs/ddlint-design.md`
- `docs/complexity-antipatterns-and-refactoring-strategies.md`
- `docs/rust-parser-testing-comprehensive-guide.md`
- `docs/rust-testing-with-rstest-fixtures.md`
- `docs/building-an-error-recovering-parser-with-chumsky.md`
- `docs/adr-001-parser-crate-split.md`

Prior art:

- `docs/execplans/2-6-4-align-index-declaration-grammar.md`
- `docs/execplans/2-6-5-align-transformer-declaration-grammar.md`

Upstream authority (last touched 2022-07-23, archived 2022-11-29):

- `vmware-archive/differential-datalog`,
  `src/Language/DifferentialDatalog/Parse.hs`
- `vmware-archive/differential-datalog`,
  `doc/language_reference/language_reference.md`
- `vmware-archive/differential-datalog`, `doc/tutorial/tutorial.md`
