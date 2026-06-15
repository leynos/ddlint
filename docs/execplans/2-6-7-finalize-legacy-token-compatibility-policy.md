# Finalise legacy token compatibility policy

This ExecPlan (execution plan) is a living document. The sections
`Constraints`, `Tolerances`, `Risks`, `Progress`, `Surprises & Discoveries`,
`Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work
proceeds.

Status: BLOCKED — scope tolerance exceeded during implementation audit

## Purpose / big picture

Roadmap item `2.6.7` and parser conformance register item `14` are open because
five legacy token classes — `typedef`, `as`, the legacy type names (`bigint`,
`bit`, `double`, `float`, `signed`), `#`, and `<=>` — are recognized by the
tokenizer but receive inconsistent treatment in the parser. Spec section `9.1`
sketches an intent ("reject with an error and fix hint") but stops short of a
closed policy matrix, and spec section `2.3` lists `#` and `<=>` as
"reserved but not part of the grammar" even though `#` is the load-bearing
sigil for the existing attribute syntax `#[...]` and `as` is the load-bearing
rename keyword for `import X as Y`. A novice reading the spec today therefore
cannot tell which uses of these tokens are valid, which are silently accepted,
and which are rejected.

After this change, a novice should be able to read one short policy table,
predict the parser's behaviour for every legacy token, and either get a
deterministic diagnostic with a fix hint or have the spec confirm the token's
remaining legitimate role. Success is observable when:

- the parser, unit tests, behavioural tests, and active docs all describe the
  same closed policy for every token in scope;
- attempting to use `typedef`, `<=>`, a bare `#` (one not followed by `[`), or
  any of the legacy type names in a source program produces a stable,
  deterministic diagnostic carrying a token-specific message and a fix hint;
- the existing roles of `as` (in `import` aliases) and `#` (as the attribute
  sigil) are explicitly preserved in code, spec, and tests;
- `make check-fmt`, `make lint`, `make markdownlint`, `make nixie`, and
  `CI=1 make test` pass after the implementation change; and
- `docs/roadmap.md` item `2.6.7` is marked done only after those checks
  succeed.

## Recommended decision

Close item `2.6.7` by adopting the "reservation without semantics" pattern
borrowed from Rust's reserved-keyword tier and TypeScript's per-token
diagnostics: keep every token kind in the lexer so spans remain precise, and
reject every use that has no production at the parser stage with a per-token,
deterministic diagnostic including a fix hint. Where a token already has a
load-bearing parser use, the spec is amended to record that use rather than
the parser being weakened to match the older intent.

The closed policy matrix becomes:

- `typedef` (`K_TYPEDEF`) — **reject at parse**. Emit
  `` `typedef` is a legacy DDlog keyword; use `type` instead ``. The current
  scanner in `src/parser/span_scanners/typedefs.rs` accepts these declarations
  today; it must be changed to record a deterministic legacy-keyword error and
  skip the malformed line through the shared scanner-recovery utility rather
  than producing a `TypeDef` AST node.
- `as` (`K_AS`) — **keep as the import alias keyword and the future cast
  keyword**. Spec section `9.1`'s "reject as a keyword" wording is corrected
  because upstream DDlog used `as` in both roles and the current ddlint parser
  already depends on it for `Import` aliases (`src/parser/ast/import.rs`,
  `src/parser/span_scanners/imports.rs`). Spec section `5.2` is updated so the
  `Import` production records the alias clause.
- Legacy type names `bigint`, `bit`, `double`, `float`, `signed` (`K_BIGINT`,
  `K_BIT`, `K_DOUBLE`, `K_FLOAT`, `K_SIGNED`) — **reject at parse when used in
  type position**. Each token gets a fix hint naming the modern sized type
  (`bigint → use a sized integer such as i64 or u64`,
  `bit → use unsigned sized types such as u32`,
  `double → use f64`, `float → use f32`, `signed → use the signed sized
  integer types such as i32`). Tokens encountered outwith a type position
  still produce one deterministic message that names the token, so an
  identifier such as `let bigint = ...` cannot silently shadow the keyword.
- `#` (`T_HASH`) — **keep as the attribute sigil; reject bare uses**. A
  `T_HASH` followed by `T_LBRACKET` enters the existing attribute scanner in
  `src/parser/span_scanners/attributes.rs` unchanged. Every other occurrence
  is rejected with `` `#` is reserved; only `#[...]` attribute syntax is
  accepted``. Spec section `2.3` is updated so `#` is listed as a special
  attribute-prefix token and the "reserved but not part of the grammar" line
  is narrowed to bare uses.
- `<=>` (`T_SPACESHIP`) — **reject at parse with a fix hint**. Emit
  `` `<=>` was reserved upstream but has no semantics in DDlog; remove it``.
  The Pratt expression layer in `src/parser/expression/` is the natural site
  to detect and recover.

All diagnostics flow through a new module
`src/parser/reserved_tokens.rs`. The module hosts the per-token message
constants, a single classification predicate
`rejection_for(kind: SyntaxKind) -> Option<&'static str>` that returns the
message text when the kind is a rejected reserved token (and `None`
otherwise), and a thin constructor that builds a
`Simple<SyntaxKind>::custom` from a span plus a message. Every enforcement
site (the four named below) routes through this predicate so the five
messages cannot drift between scanners. The module name uses
"reserved tokens" rather than "legacy tokens" because `as` is also a legacy
token but is *kept*; the rejected set is more precisely described as
"reserved without semantics" in the Rust-tier sense.

The parser emits these as `Simple<SyntaxKind>::custom` failures so they
appear in `Parsed::errors` like every other deterministic parser diagnostic
and benefit from the existing `into_owned`/clone machinery. The message
constants are `pub(crate)` and the messages themselves are *not* a public
contract: their wording may change in any release. Tests import the
constants by name so test breakage flags any drift. If a future milestone
exposes structured diagnostic codes for IDE tooling, the codes — not the
messages — become the public contract; this plan does not introduce such
codes.

This is the smallest decision that simultaneously (1) closes the conformance
delta in code, (2) reconciles the contradictions inside the spec, and (3)
avoids breaking the already-shipping `import X as Y` and `#[attr]` surfaces.

If implementation reveals a workspace consumer that depends on parsing
`typedef`, `bigint`, `bit`, `double`, `float`, `signed`, bare `#`, or `<=>` as
valid grammar (e.g., a parser test asserting that a `typedef` declaration
produces a `TypeDef` AST node, or a fixture using `bigint` as a type name),
update that consumer in the same change. The plan budgets for this in the
`typedef` migration step. If any such consumer cannot be updated within the
scope tolerance below, stop and escalate.

## Constraints

- Align the active written contract in
  `docs/differential-datalog-parser-syntax-spec-updated.md` sections `2.3`,
  `5.2`, and `9.1` to the final parser behaviour.
- Keep the scope centred on the five token classes named above. Do not fold
  relation form, brace-group, or wider terminator policy work from roadmap
  items `2.6.6` and `2.6.8` into this change.
- Preserve the existing AST surface for valid programs. Specifically, the
  `Import` typed wrapper keeps its `alias` accessor and the attribute parser
  keeps consuming `T_HASH` `T_LBRACKET` sequences without behaviour change.
- Replace the `typedef`-accepting code path with a deterministic rejection
  path; do not leave a hidden second entrypoint that still produces a
  `TypeDef` AST node from `typedef ...`.
- Token kinds in `src/tokenizer.rs` remain unchanged; this milestone is a
  parser policy decision, not a lexer redesign.
- Add both unit tests in `src/parser/tests/` and behavioural tests in
  `tests/` covering each token's reject path, plus regression coverage that
  proves `as` aliases and `#[attr]` placements still parse.
- Update the relevant design documents, not only the conformance register:
  `docs/differential-datalog-parser-syntax-spec-updated.md`,
  `docs/parser-conformance-register.md`,
  `docs/parser-implementation-notes.md`, `docs/ddlint-design.md`,
  `docs/users-guide.md` (because programs that previously parsed now produce
  errors), `docs/developers-guide.md` (because there is a new internal
  reserved-token diagnostic convention), and `CHANGELOG.md` (or the closest
  equivalent migration-notes file, so adopters scanning a release note find
  the breaking changes without having to browse the users' guide).
  The canonical home for the per-token policy table is spec section `9.1`;
  the developers' guide references that section rather than duplicating the
  table.
- Mark `docs/roadmap.md` item `2.6.7` done only after all documentation,
  implementation, and validation steps succeed.
- Run repository quality gates through Make targets using `set -o pipefail`
  and `tee` so truncated logs cannot hide regressions.
- Keep comments and documentation in en-GB-oxendict spelling.

## Tolerances (exception triggers)

- Scope: if the implementation requires edits to more than 14 files or more
  than 360 net new code lines, stop and escalate. That likely means policy
  drift has pulled in adjacent grammar work.
- Interface: if the only coherent fix requires removing
  `Import::alias()`, reshaping the attribute scanner, or changing the public
  `Parsed` surface, stop and escalate.
- Compatibility: if non-test workspace code depends on parsing `typedef`,
  bare `#`, `<=>`, or any of the legacy type names as valid syntax, stop and
  document those callers before changing behaviour.
- Grammar: if updating spec sections `2.3`, `5.2`, or `9.1` leaves an
  unavoidable contradiction with sections `4` (operator table) or `5.5`–`5.8`,
  stop and escalate with concrete section references instead of silently
  widening the task.
- Diagnostics: every reject path must route through
  `reserved_tokens::rejection_for` and the shared constructor. If any
  enforcement site emits a `Simple<SyntaxKind>::custom` for a rejected
  reserved token without going through the predicate, stop and refactor
  before adding more sites.
- Visibility: the per-token message constants stay `pub(crate)`. If any
  consumer outwith the crate needs to match against the strings, stop and
  introduce structured diagnostic codes rather than widening visibility.
- Validation: if `make check-fmt`, `make lint`, or `make test` still fails
  after three focused fix rounds, stop and escalate with the specific failing
  targets.
- Runner stability: if `make test` hits the known repository-wide nextest
  stall rather than a deterministic regression from this task, capture the
  evidence and escalate instead of silently substituting a different profile.

## Risks

- Risk: removing `typedef` acceptance breaks existing parser tests that
  positively assert a `TypeDef` AST node from `typedef ...` input, and may
  break sample programs under `tests/` or `examples/`. Severity: high.
  Likelihood: high. Mitigation: audit `src/parser/tests/parser.rs` lines
  272–293 and run `rg "typedef\b" -t md -t rs` before changing the scanner;
  migrate any `typedef`-shaped fixtures to `type` form in the same commit
  that introduces the rejection.
- Risk: legacy type names such as `float` are common identifiers in
  user-authored fixtures and could cause unrelated test failures when the
  rejection lands. Severity: medium. Likelihood: medium. Mitigation: grep for
  each legacy type name across the workspace before tightening the parser,
  and update the spec's "reserved words" example list in section `2.3` so
  contributors understand the constraint.
- Risk: the spec is internally inconsistent: section `2.3` says `#` is
  reserved-but-not-grammar while section `9.1` says only legacy tokens
  outside the grammar are rejected, and the attribute grammar in section
  `5.1` already uses `#[...]`. Severity: high. Likelihood: high. Mitigation:
  resolve the contradiction in one atomic spec change in stage C so reviewers
  see the corrected grammar as a single coherent contract.
- Risk: emitting one `Simple<SyntaxKind>::custom` per token without a shared
  helper risks message drift between scanners and tests. Severity: medium.
  Likelihood: medium. Mitigation: centralize the five messages and their fix
  hints in a single module (`src/parser/reserved_tokens.rs`) and
  reference the same constants from scanners and tests.
- Risk: the `<=>` operator can appear in user code at lex points where the
  Pratt parser is not active (e.g., inside a relation row when expression
  parsing has bailed out). Severity: medium. Likelihood: low. Mitigation:
  add a span-level fallback in the top-level span scanner so a stray
  `T_SPACESHIP` outwith expression context still emits the deterministic
  diagnostic.
- Risk: changing parser behaviour silently breaks downstream linter rules
  that assumed a `TypeDef` AST node would be present for legacy fixtures.
  Severity: medium. Likelihood: low. Mitigation: search `src/linter/` and
  `src/sema/` for any reliance on `typedef`-shaped fixtures before merging,
  and add one explicit regression test under `src/linter/tests/` (or the
  closest existing home) that feeds a `typedef`-only programme through the
  linter and asserts the rejection diagnostic appears in `Parsed::errors`
  *and* that no linter rule produces a false-negative pass on the file.

## Progress

- [x] (2026-05-29) Reviewed roadmap item `2.6.7`, parser conformance register
  item `14`, the syntax spec section `9.1`, parser-implementation-notes,
  the tokenizer, every parser site that mentions one of the five token kinds,
  and the existing diagnostic catalogue
  (`src/parser/error_messages.rs`).
- [x] (2026-05-29) Researched upstream DDlog parser treatment in
  `vmware/differential-datalog` and reserved-keyword diagnostic patterns in
  rustc, TypeScript, and Swift to ground the recommended decision.
- [x] (2026-05-29) Drafted this ExecPlan in
  `docs/execplans/2-6-7-finalize-legacy-token-compatibility-policy.md`.
- [x] (2026-05-29) Ran the Logisphere community-of-experts review on the
  draft, recorded a verdict of "proceed with conditions", and revised the
  plan: renamed the policy module to `reserved_tokens`, pinned the
  message constants to `pub(crate)`, added a single classification
  predicate routed through every enforcement site, recorded the
  workspace audit counts, committed to a linter / sema regression test,
  added a `CHANGELOG.md` update, and recorded the rejected
  lex-stage-rejection alternative in the Decision Log.
- [ ] Add red tests in `src/parser/tests/reserved_tokens.rs` for each
  reject path and regression tests for the preserved `as` and `#[...]`
  uses.
- [ ] Implement the reserved-token diagnostic module and update the four
  scanners (`typedefs.rs`, the Pratt expression layer, the top-level span
  scanner, and the type-position scanners), routing every reject site
  through the shared predicate.
- [ ] Update active documentation: spec sections `2.3`, `5.2`, and `9.1`;
  the conformance register; parser-implementation-notes; ddlint-design;
  users' guide; developers' guide.
- [ ] Run `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`,
  `make lint`, and `CI=1 make test`, capturing each log under `/tmp`.
- [ ] Mark roadmap item `2.6.7` done and close the conformance register entry
  as `implemented`.
- [x] (2026-06-16) Resumed implementation on branch
  `2-6-7-finalize-legacy-token-compatibility-policy`, loaded `leta`,
  `execplans`, `rust-router`, `rust-unit-testing`, and `rust-errors`, and
  re-read the plan and active documentation index before editing code.
- [x] (2026-06-16) Re-ran the workspace token audit before stage A and found
  that completing the plan as written will require more than the current
  14-file scope tolerance because documentation updates, parser/test changes,
  and owned example migrations together exceed that threshold.

## Surprises & Discoveries

- The current parser already *accepts* `typedef` and exposes the result via
  `root.type_defs()` in `src/parser/tests/parser.rs` (lines 272–282). Closing
  this conformance item therefore requires changing existing positive test
  assertions, not just adding new reject tests.
- The spec is internally inconsistent on `#`: section `2.3` declares it
  reserved-but-not-grammar while the attribute grammar (section `5.1`) and
  the parser both depend on `#[...]`. The same contradiction does not appear
  for `<=>`, which has no live use anywhere in the parser.
- Upstream DDlog treats `as` as a real keyword in both `import X as Y` and
  the `expr as type` cast. Section `9.1`'s "reject as a keyword" wording
  therefore conflicts with both upstream prior art and the live ddlint
  parser; the only safe reading is that section `9.1` was written before the
  `Import` alias clause was preserved.
- The diagnostic catalogue (`src/parser/error_messages.rs`) is currently
  two constants long and was added only for transformer-specific messages.
  It sits alongside the new `src/parser/reserved_tokens.rs`; both are
  parser-internal diagnostic homes and the developers' guide notes the
  distinction (per-feature messages vs the reserved-token policy module).
- Workspace audit counts (pre-implementation): `\btypedef\b` appears 9
  times across 7 `examples/*.dl` fixture files and 0 times in `*.rs` source
  programmes (the `.rs` matches are tokenizer-table entries and code
  identifiers, not parsed input). The legacy type-name family
  `\b(typedef|bigint|bit|double|float|signed)\b` totals 22 matches across
  9 `examples/*.dl` files; sampling those files is required during stage A
  to distinguish identifier-position uses (none expected after the
  upstream-aligned grammar) from type-position uses that will need
  migration. `<=>` does not appear in any `.rs` source programme outwith
  the tokenizer itself, and a manual scan of `examples/*.dl` is still
  required for completeness. These counts confirm the scope tolerance
  (≤14 files, ≤360 net LOC) is realistic provided the `.dl` examples are
  updated alongside the parser change.
- On 2026-06-16, `docs/repository-layout.md`, referenced by the general
  repository-orientation guidance, was absent. Orientation used
  `docs/contents.md` and `leta files` instead.
- On 2026-06-16, a refined audit of parseable `.dl` examples found legacy
  tokens in seven owned examples:
  `examples/left_join_by_negation.dl`,
  `examples/tuple_destructuring.dl`, `examples/paths_excluding.dl`,
  `examples/reachability.dl`, `examples/functions_and_match.dl`,
  `examples/extern_transformer_decl.dl`,
  `examples/hello_join.dl`, `examples/ref_and_intern.dl`, and
  `examples/primary_key_and_index.dl`. `typedef` appears in seven examples;
  `bit<N>` appears in three examples. No `<=>`, `bigint`, `double`,
  `float`, `signed`, or bare `#` use was found in the example programmes.
- No `CHANGELOG.md` exists in the repository. The closest existing
  migration-notes file found by `rg --files` is
  `docs/differential-datalog-parser-syntax-spec-migration-plan.md`, but
  using it as release-facing migration notes would be a new documentation
  role and should be a conscious decision.

## Decision Log

- Decision: adopt Rust's "reserved keyword" pattern — lex into named token
  kinds, reject at the parser stage with a per-token diagnostic plus fix
  hint — rather than dropping the token kinds from the lexer. Rationale:
  preserves precise spans, keeps editor highlighters working, mirrors
  upstream DDlog's `reservedNames`/`reservedOpNames` model, and matches
  rustc, TypeScript, and Swift's stage choice.
- Decision: keep `as` as a live keyword for the `Import` alias clause and
  for the future cast operator, and update spec section `9.1` to record
  this. Rationale: upstream DDlog used `as` in both roles, the current
  ddlint parser already depends on it for `Import`, and removing it would
  break any programme that uses `import foo::bar as baz`.
- Decision: keep `#` as the attribute sigil and reject only bare uses (a
  `T_HASH` not immediately followed by `T_LBRACKET`). Rationale: aligns
  with upstream DDlog's `#[...]` attribute syntax, preserves
  `src/parser/span_scanners/attributes.rs` unchanged for its happy path,
  and resolves the section `2.3` vs section `5.1` spec contradiction by
  narrowing rather than widening the rejection rule.
- Decision: replace `typedef` acceptance with deterministic rejection and
  migrate any positive fixtures in the same change. Rationale: the spec
  explicitly directs users to `type`, and continuing to silently accept
  `typedef` while documenting it as a legacy keyword would leave the
  conformance register stuck at `scheduled` indefinitely.
- Decision: centralize the five diagnostic messages and fix hints in a
  single Rust module rather than copy-pasting them across scanners.
  Rationale: keeps wording aligned with tests, prevents drift, and matches
  the pattern already established by `error_messages.rs` for the
  transformer messages.
- Decision: this milestone does not touch the wider top-level statement
  terminator policy, the relation-form work in `2.6.6`, or the brace-group
  decision in `2.6.8`. Rationale: those items are tracked separately and
  folding them in here would push past the scope tolerance.
- Decision: rejected — lex-stage rejection (mapping the rejected tokens
  directly to `N_ERROR` in the tokenizer). Rationale: the parser would lose
  the named kind it needs to produce a structural fix hint (e.g., "did you
  mean `type Foo = u32`?"), recovery would degrade because every error
  becomes the same `N_ERROR` shape, and editor highlighters would lose
  their per-keyword colouring for legacy tokens. Parse-stage rejection
  preserves the kind, mirrors rustc, TypeScript, and Swift, and adds only
  a single classification predicate to the parser. The simpler lex-stage
  approach was considered and is intentionally not adopted.
- Decision: name the module `reserved_tokens`, not `legacy_tokens`.
  Rationale: `as` is also a legacy DDlog token but is *kept*, so placing
  it under a "legacy" banner would mislead future contributors. The
  rejected set is precisely the "reserved without semantics" tier from the
  Rust reserved-keyword model, so the module name reflects that pattern.
- Decision: cut `typedef` over immediately rather than running a
  one-release deprecation warning. Rationale: the workspace audit shows
  `typedef` use is confined to `examples/*.dl` fixtures owned by this
  repository, so a single coordinated migration is cheaper than carrying a
  deprecation cycle and the conformance register entry stuck at
  `scheduled`. If subsequent adopter feedback shows external programmes
  depend on `typedef`, the policy module can grow a warning variant
  without changing the public surface.
- Decision pending user direction: the plan's 14-file scope tolerance is
  exceeded before implementation begins. Completing the plan exactly as
  written appears to require at least parser code, new parser tests, a
  behavioural test, a linter regression, seven active documentation or
  migration-note files, the roadmap, this ExecPlan, and up to seven owned
  example migrations. Options are:
  1. raise the file-count tolerance and proceed with the complete plan;
  2. keep the tolerance and narrow the milestone by deferring owned example
     migration or some documentation updates, accepting that the repository
     may temporarily contain invalid examples or incomplete public contract
     text;
  3. split the work into multiple explicitly scoped commits or follow-up
     ExecPlans while keeping this branch blocked at the parser-policy
     boundary.
  The cleanest option is to raise the tolerance because the extra files are
  not scope creep; they are direct consequences of the already-approved
  acceptance criteria.

## Outcomes & Retrospective

To be completed once implementation lands and gates pass.

## Context and orientation

The relevant code and documents are concentrated in a small set of files. A
novice with only this ExecPlan and the current working tree should be able to
locate each one without prior context.

- `src/tokenizer.rs` — defines `K_TYPEDEF`, `K_AS`, `K_BIGINT`, `K_BIT`,
  `K_DOUBLE`, `K_FLOAT`, `K_SIGNED`, `T_HASH`, and `T_SPACESHIP`. The
  `KEYWORDS` `phf` map (lines around `120`–`195`) and the `Token` enum
  (lines around `60`–`112`) are the source of truth for the token names
  used elsewhere.
- `src/parser/span_scanners/typedefs.rs` — currently the *acceptance* path
  for `typedef`. The scanner consumes the keyword via `handle_typedef` and
  records a top-level span for downstream `TypeDef` AST construction.
- `src/parser/span_scanners/imports.rs` and `src/parser/ast/import.rs` —
  consume `K_AS` to record the optional alias on `Import` AST nodes. Both
  files must stay green; `as` is *not* being rejected.
- `src/parser/span_scanners/attributes.rs` — consumes `T_HASH` `T_LBRACKET`.
  The happy path stays unchanged; the new bare-`#` rejection lives in the
  top-level span scanner so it sees uses outwith attribute context.
- `src/parser/expression/` — the Pratt parser and infix table. A stray
  `T_SPACESHIP` reaches this layer when expression parsing is active and is
  the natural site to detect and recover.
- `src/parser/error_messages.rs` — current per-feature diagnostic
  catalogue (two transformer-specific constants today). This module is
  left in place and not extended; reserved-token messages live in a
  sibling module so per-feature and policy-level diagnostics stay
  visually distinct.
- `src/parser/reserved_tokens.rs` — new module added by this milestone.
  Hosts the five `pub(crate)` message constants, the
  `rejection_for(kind) -> Option<&'static str>` predicate, and the
  `Simple<SyntaxKind>::custom` constructor that every enforcement site
  shares.
- `src/parser/tests/parser.rs` (lines 272–293) — currently asserts that
  `typedef` declarations parse into `TypeDef` nodes; these assertions are
  inverted in stage A.
- `src/parser/tests/` — feature-specific unit tests. A new file
  `src/parser/tests/reserved_tokens.rs` is the natural home for the five
  token-specific reject suites.
- `tests/` — `tests/attribute_placement.rs` and `tests/name_uniqueness.rs`
  show the canonical shape for behavioural unhappy-path coverage. A new
  `tests/reserved_token_rejection.rs` files the end-to-end coverage.
- `docs/differential-datalog-parser-syntax-spec-updated.md` — spec sections
  `2.3`, `5.2`, and `9.1` are updated in stage C. The grammar table in
  section `5.2` gains the alias clause; section `2.3` narrows the bare-`#`
  rejection; section `9.1` records the final per-token policy table.
- `docs/parser-conformance-register.md` — item `14` is rewritten and marked
  `implemented` once code, tests, and spec agree.
- `docs/parser-implementation-notes.md` — gains a short parser-boundary
  note describing the reserved-token diagnostic module and the fact that
  rejection happens at parse rather than lex.
- `docs/ddlint-design.md` — gains one sentence in the parser-contract
  section recording the per-token reservation policy by name.
- `docs/users-guide.md` — gains a short note describing the new errors a
  user may see when upgrading legacy DDlog input.
- `docs/developers-guide.md` — gains a short note describing the internal
  reserved-token diagnostic convention so future contributors emit
  messages through the same module.

At the start of this work, the live contradictions are:

- Spec: "`typedef`: not supported; emit an error with a fix hint" (section
  `9.1`).
- Code: `typedef` is accepted and produces a `TypeDef` AST node
  (`src/parser/span_scanners/typedefs.rs`, `src/parser/tests/parser.rs`).
- Spec: "`as`: not a keyword in the updated grammar" (section `9.1`).
- Code: `as` is the import alias keyword
  (`src/parser/span_scanners/imports.rs`, `src/parser/ast/import.rs`).
- Spec: "Reserved but not part of the grammar: `#`, `<=>`" (section `2.3`).
- Code: `#` is the attribute sigil (`src/parser/span_scanners/attributes.rs`)
  and `<=>` is tokenized but never consumed.
- Spec: legacy type names "not in the grammar" (section `9.1`).
- Code: legacy type names are tokenized into keyword kinds but never
  consumed; no diagnostic is emitted today.

The implementation resolves each of these mismatches atomically in code,
tests, and docs.

## Plan of work

### Stage A: audit the workspace and establish red tests for the chosen policy

Begin with a workspace audit so the migration scope is known before any
parser change lands. Run `rg` across `*.rs`, `*.md`, `*.dl`, and `*.ddlog`
for `\btypedef\b`, each legacy type name, bare `#`, and `<=>`. Append the
per-file results to the Surprises section so reviewers see the migration
list. The pre-implementation counts already recorded there are the
starting baseline.

Then change tests so they describe the chosen grammar before touching
scanner logic.

- Add `src/parser/tests/reserved_tokens.rs` containing rstest cases for
  every reject path:
  - `typedef Foo = u32;` produces a deterministic legacy-keyword diagnostic
    and no `TypeDef` AST node.
  - `1 <=> 2` inside an expression context produces the deterministic
    spaceship-rejection diagnostic.
  - `# foo` (bare hash, not `#[...]`) produces the deterministic bare-hash
    diagnostic.
  - Each legacy type name appearing in a type position (e.g.,
    `type Foo = bigint;`) produces the matching deterministic diagnostic
    with the modern-type fix hint.
- Invert the existing positive assertions in
  `src/parser/tests/parser.rs` lines 272–293 so `typedef` inputs are
  expected to fail with the new diagnostic message rather than succeed.
- Add a regression case in the same file asserting that
  `import foo::bar as baz` still parses cleanly and that
  `Import::alias()` returns `Some("baz")`.
- Add a regression case asserting that `#[attribute] type Foo = u32;` still
  parses cleanly and produces both an attribute and a `TypeDef`.
- Add a parser-level integration assertion in `src/parser/tests/parser.rs`
  and the shared fixtures (`src/parser/tests/programs.rs`,
  `src/parser/tests/specs.rs`) so the broader parser suite encodes the same
  contract.

At the end of stage A the new unit suite should fail for the right reason
(rejection diagnostics not yet emitted) and the regression cases should still
pass (because `as` and `#[...]` paths have not changed yet).

### Stage B: implement deterministic grammar handling

Adjust the parser so the chosen contract is enforced explicitly and
recoverably.

- Add `src/parser/reserved_tokens.rs` with five `pub(crate)` message
  constants, the `rejection_for(kind: SyntaxKind) -> Option<&'static str>`
  predicate, and a `reserved_token_error(span, message) ->
  Simple<SyntaxKind>` constructor. The module doc string explains the
  "reservation without semantics" pattern and links to spec section `9.1`.
- Refactor `src/parser/span_scanners/typedefs.rs` so `handle_typedef`
  calls `reserved_tokens::rejection_for(K_TYPEDEF)`, builds the diagnostic
  through the shared constructor, and skips the line via the shared
  span-recovery utility rather than producing a `TypeDef` span.
- Extend the top-level span scanner (`src/parser/span_scanner.rs` or the
  appropriate dispatcher in `src/parser/span_scanners/`) so every token
  drawn from the stream is first classified via
  `reserved_tokens::rejection_for`. If it returns `Some(message)`, the
  scanner records the diagnostic and skips the offending line. Bare
  `T_HASH` (a `T_HASH` not immediately followed by `T_LBRACKET`) is
  classified explicitly because the attribute-prefix case must still reach
  the attribute scanner unchanged.
- Extend the Pratt expression layer in `src/parser/expression/infix.rs`
  (or `pratt.rs`) so `T_SPACESHIP` and any legacy type-name keyword
  encountered inside an expression context routes through the same
  predicate and recovers to the next infix boundary.
- Extend the type-position scanners (the bodies of `TypeDef`, parameter
  lists, and field declarations) so the five legacy type names route
  through the same predicate, producing the matching diagnostic with the
  modern-type fix hint. No site constructs its own
  `Simple<SyntaxKind>::custom` for a rejected reserved token; every site
  goes through `reserved_tokens::reserved_token_error`.
- Keep recovery behaviour aligned with existing scanners so subsequent
  declarations still parse cleanly.

### Stage C: align documentation and public contract text

Once parser and tests agree, update every active document that carries the
grammar contract.

- In `docs/differential-datalog-parser-syntax-spec-updated.md`:
  - Section `2.3`: list `#` as a special attribute-prefix token, retain
    `<=>` in the reserved list, and narrow the "reject" sentence so it
    covers `<=>` and bare `#` only.
  - Section `5.2`: extend the `Import` production to include the optional
    `('as' UcName)?` alias clause already implemented by the parser.
  - Section `9.1`: replace the loose prose with a closed policy table
    enumerating each token, the reject site, the diagnostic message
    template, and the fix hint.
- In `docs/parser-conformance-register.md` item `14`: rewrite the
  current/spec/target paragraphs so they match the final policy, then mark
  the entry `implemented`.
- In `docs/parser-implementation-notes.md`: add a short parser-boundary
  note describing the reserved-token diagnostic module and explaining
  why rejection happens at parse rather than lex.
- In `docs/ddlint-design.md`: add one sentence in the parser-contract
  section recording the per-token reservation policy and naming the
  reserved-token diagnostic module.
- In `docs/users-guide.md`: add a short subsection ("Legacy DDlog tokens")
  describing what programmes encounter when porting from upstream DDlog,
  with one line per token and its replacement.
- In `docs/developers-guide.md`: add a short subsection
  ("Reserved-token diagnostics") describing the convention so future
  contributors route messages through `src/parser/reserved_tokens.rs`,
  and linking to spec section `9.1` as the canonical policy table rather
  than duplicating it.
- In `CHANGELOG.md` (or the closest migration-notes file): add a
  "Breaking changes" entry naming each newly rejected token, the
  replacement, and a one-line example of the diagnostic users will see.

### Stage D: add behavioural coverage for end-to-end parser behaviour

Add behavioural tests in `tests/` so the public `parse()` entrypoint proves
the decision, not just the internal unit suites.

- Add `tests/reserved_token_rejection.rs` with one rstest case per token
  exercising a complete programme through `parse()` and asserting the
  presence of the matching diagnostic in `Parsed::errors`.
- Include a behavioural test in the same file that asserts
  `import foo::bar as baz` still produces an `Import` AST node with the
  expected alias.
- Include a behavioural test asserting that `#[attribute] type Foo = u32;`
  still produces both an attribute and a `TypeDef` and that the absence of
  attribute brackets after `#` produces the bare-hash diagnostic.
- Add one explicit linter / sema regression test (under
  `src/linter/tests/` or `src/sema/tests/`, whichever already hosts
  similar coverage) that feeds a `typedef Foo = u32;`-only programme
  through the linter pipeline and asserts (a) the rejection diagnostic
  appears in `Parsed::errors` and (b) no existing linter rule produces a
  false-negative clean pass on the file. This guards against the
  Doggylump pre-mortem scenario where a rule silently misses now-error
  input because the `TypeDef` AST node it depended on has disappeared.

### Stage E: validation and close-out

After implementation and documentation updates:

1. Run `make fmt`, `make markdownlint`, and `make nixie` because this
   change updates Markdown.
2. Run `make check-fmt`, `make lint`, and `make test`.
3. Only after those gates pass, update `docs/roadmap.md` to mark item
   `2.6.7` done.
4. Update this ExecPlan's `Progress`, `Decision Log`, and
   `Outcomes & Retrospective` sections.
5. Run `coderabbit review --agent` and clear every concern before closing
   the PR for review.

## Concrete steps

1. Audit the workspace for every legacy token in scope, sample each match
   to distinguish identifier-position from grammar-position uses, and
   record the per-file migration list at the bottom of the Surprises
   section. The Progress entry for stage A is the recorded list.
2. Add red tests under `src/parser/tests/reserved_tokens.rs` for every
   reject path, and add regression cases for the preserved `as` alias and
   `#[...]` attribute paths.
3. Invert the `typedef`-acceptance assertions in
   `src/parser/tests/parser.rs` (lines 272–293) and adjust any shared
   fixtures under `src/parser/tests/programs.rs` and
   `src/parser/tests/specs.rs` that depended on them. Migrate any
   `examples/*.dl` fixtures that use rejected tokens to the modern
   equivalents in the same commit.
4. Add `src/parser/reserved_tokens.rs` with the five `pub(crate)`
   diagnostic constants, the `rejection_for` predicate, and the
   `reserved_token_error` constructor that yields a
   `Simple<SyntaxKind>::custom` carrying the matching message.
5. Rewrite `handle_typedef` in `src/parser/span_scanners/typedefs.rs` so
   it routes through `reserved_tokens::rejection_for(K_TYPEDEF)` plus
   `reserved_token_error` and skips the line via the shared span-recovery
   utility. Remove or redirect any helper that previously assumed
   `typedef` would produce a `TypeDef` span.
6. Extend the top-level span scanner so every drawn token is classified
   via `reserved_tokens::rejection_for` and rejected through the shared
   constructor. Add the bare-`T_HASH` lookahead so attribute uses still
   reach the attribute scanner unchanged.
7. Extend the Pratt expression layer in `src/parser/expression/` so
   `T_SPACESHIP` and any legacy type-name keyword inside an expression
   context routes through the same predicate and recovers to the next
   infix boundary.
8. Extend the type-position scanners so the five legacy type names route
   through the same predicate, producing the modern-type fix hint. No
   site constructs its own `Simple<SyntaxKind>::custom` for a rejected
   reserved token.
9. Add behavioural tests in `tests/reserved_token_rejection.rs` covering
   each token's reject path plus the preserved `as` and `#[...]` uses.
10. Add the linter / sema regression test described under stage D so no
    rule silently misses now-error legacy-keyword input.
11. Update active documentation (spec sections `2.3`, `5.2`, `9.1`;
    conformance register item `14`; parser-implementation-notes;
    ddlint-design; users' guide; developers' guide; `CHANGELOG.md` or
    the closest migration-notes file).
12. Run the Make targets listed under validation and fix any failures.
13. Mark roadmap item `2.6.7` done and close the conformance register
    entry as `implemented`.

## Validation commands

Run the following from the repository root, capturing output with `tee`:

```shell
set -o pipefail; make fmt 2>&1 | tee /tmp/2-6-7-make-fmt.log
set -o pipefail; make markdownlint 2>&1 | tee /tmp/2-6-7-make-markdownlint.log
set -o pipefail; make nixie 2>&1 | tee /tmp/2-6-7-make-nixie.log
set -o pipefail; make check-fmt 2>&1 | tee /tmp/2-6-7-make-check-fmt.log
set -o pipefail; make lint 2>&1 | tee /tmp/2-6-7-make-lint.log
set -o pipefail; CI=1 make test 2>&1 | tee /tmp/2-6-7-make-test.log
```

Successful completion means all six commands exit with status `0`, the new
reserved-token unit and behavioural suites pass, the `as` alias and `#[...]`
attribute regression cases still pass, and the roadmap entry can be closed
without leaving the conformance register in `scheduled` state.

## Interfaces and dependencies

This plan does not introduce new external dependencies. The new internal
surface is constrained as follows.

In `src/parser/reserved_tokens.rs`, expose only crate-local items. The
message constants and the classification predicate are `pub(crate)` because
the diagnostic strings are not a public contract; tests inside the crate
import the constants by name so any wording change forces test breakage.
Use `&'static str` throughout — every message is statically known and
fits the `Simple<SyntaxKind>::custom` payload without allocation.

```rust
pub(crate) const RESERVED_TYPEDEF_ERROR: &str =
    "`typedef` is a legacy DDlog keyword; use `type` instead";
pub(crate) const RESERVED_SPACESHIP_ERROR: &str =
    "`<=>` was reserved upstream but has no semantics in DDlog; remove it";
pub(crate) const RESERVED_BARE_HASH_ERROR: &str =
    "`#` is reserved; only `#[...]` attribute syntax is accepted";
pub(crate) const RESERVED_BIGINT_ERROR: &str =
    "`bigint` is a legacy type name; use a sized integer such as `i64` or `u64`";
pub(crate) const RESERVED_BIT_ERROR: &str =
    "`bit` is a legacy type name; use an unsigned sized integer such as `u32`";
pub(crate) const RESERVED_DOUBLE_ERROR: &str =
    "`double` is a legacy type name; use `f64`";
pub(crate) const RESERVED_FLOAT_ERROR: &str =
    "`float` is a legacy type name; use `f32`";
pub(crate) const RESERVED_SIGNED_ERROR: &str =
    "`signed` is a legacy type name; use a signed sized integer such as `i32`";

/// Returns the matching diagnostic message when `kind` is a reserved
/// token that must be rejected by the parser, or `None` otherwise. The
/// bare-`#` case is *not* classified here because rejection depends on
/// the following token (`T_LBRACKET` is the attribute prefix); the
/// top-level span scanner handles that lookahead directly and calls
/// `reserved_token_error` with `RESERVED_BARE_HASH_ERROR` when the
/// lookahead fails.
pub(crate) fn rejection_for(
    kind: crate::SyntaxKind,
) -> Option<&'static str>;

/// Constructs the deterministic `Simple<SyntaxKind>::custom` carried by
/// `Parsed::errors`. Every enforcement site routes through this helper so
/// the message wording stays single-sourced.
pub(crate) fn reserved_token_error(
    span: crate::Span,
    message: &'static str,
) -> chumsky::error::Simple<crate::SyntaxKind>;
```

Routing every enforcement site through `rejection_for` plus
`reserved_token_error` keeps the five messages aligned across the four
scanner locations. A constraint in the section above forbids any site
from constructing its own `Simple<SyntaxKind>::custom` for a rejected
reserved token.

The preserved interfaces are:

- `crate::parser::ast::Import::alias()` keeps returning the `as`-bound
  identifier for `import X as Y` programmes.
- `crate::parser::span_scanners::attributes::*` keeps consuming `T_HASH`
  `T_LBRACKET` sequences unchanged.
- `crate::Parsed::errors` keeps its `Vec<Simple<SyntaxKind>>` shape; the
  new diagnostics simply appear in that vector alongside existing parser
  diagnostics.

## Idempotence and recovery

Each implementation step is a small, reviewable diff and is safe to repeat:
applying the same edit twice has no effect. If a step fails mid-way, revert
the staged changes with `git restore --staged --worktree <files>` and rerun
from the previous concrete step. The validation Make targets are themselves
idempotent.

## Related documentation and skills

A novice continuing this work should consult, in roughly this order:

- `docs/parser-conformance-register.md` item `14` for the open delta.
- `docs/differential-datalog-parser-syntax-spec-updated.md` sections `2.3`,
  `5.2`, and `9.1` for the normative grammar being changed.
- `docs/parser-implementation-notes.md` for the parser-boundary conventions
  used elsewhere in the scanner stack.
- `docs/building-an-error-recovering-parser-with-chumsky.md` for the
  chumsky idioms used by the existing diagnostic constructors.
- `docs/rust-parser-testing-comprehensive-guide.md` and
  `docs/rust-testing-with-rstest-fixtures.md` for the test-shape conventions
  expected by the existing suites.
- `docs/ddlint-design.md` for the broader parser-contract story.
- `docs/complexity-antipatterns-and-refactoring-strategies.md` if any
  scanner refactor grows past the file-length threshold during stage B.

Relevant skills the implementing agent should load:

- `rust-router`, then `rust-errors` for the `Simple<SyntaxKind>::custom`
  shape and message hygiene, and `rust-types-and-apis` if the diagnostic
  helper grows beyond constants and one function.
- `nextest` for the test runner conventions used by `CI=1 make test`.
- `commit-message` and `pr-creation` to land the change.

## Revision note

This plan was initially drafted on 2026-05-29 and immediately revised
after a Logisphere community-of-experts review on the same day.

What changed in the post-review revision:

- The policy module was renamed from `legacy_tokens` to
  `reserved_tokens` (Pandalump): `as` is also a legacy token but is
  kept, so the more precise name avoids confusion.
- A single classification predicate `rejection_for(kind)` was added and
  every enforcement site now routes through it (Pandalump): the previous
  draft risked message drift between the four scanner locations.
- The message constants were pinned to `pub(crate)` and the messages
  themselves were declared *not* a public contract (Telefono): tests
  import the constants by name so any drift breaks the tests.
- The workspace audit was pre-quantified in the Surprises section
  (Wafflecat): the counts confirm the scope tolerance is realistic and
  show that `typedef` migration is confined to `examples/*.dl` fixtures.
- An explicit linter / sema regression test was added under stage D
  (Doggylump): guards against the most likely pre-mortem scenario where
  a rule silently misses now-error input.
- A `CHANGELOG.md` (or equivalent migration-notes) update was added to
  the documentation step (Dinolump): adopters scanning a release note
  are more likely to find the breaking changes there than in the users'
  guide alone.
- The Decision Log gained explicit entries recording (a) why the cheaper
  lex-stage rejection was considered and rejected, (b) why the module is
  named `reserved_tokens`, and (c) why `typedef` is cut over immediately
  rather than via a deprecation cycle.

Why these revisions: each addresses a 🟡 or 💡 finding from the
Logisphere review. None changes the structural choice (parse-stage
rejection, named token kinds preserved) — they sharpen the policy and
its enforcement so a novice can follow the plan to completion without
relying on tacit knowledge.

Effect on remaining work: the workspace audit step is now explicit at
the top of stage A, the predicate constraint binds every enforcement
site identically, and the test plan now commits to one extra regression
test under the linter / sema suite. The overall scope budget (≤14 files,
≤360 net LOC) is unchanged because the new module is small and the new
regression test is a single rstest case.

Subsequent revisions must append a short note describing what changed,
why, and how it affects the remaining work, and must update the Status
field above.
