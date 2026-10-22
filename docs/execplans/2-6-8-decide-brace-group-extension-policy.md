# Codify brace-delimited block expressions (roadmap 2.6.8)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT

This plan must be approved before implementation begins. Silence is not
approval. Three decisions are marked "requires user approval" in
`Decision log`; approving the plan approves them unless the approver says
otherwise.

## Purpose / big picture

Roadmap item 2.6.8 asks for a decision on the brace-group extension `{ expr }`:
either codify it or remove it with migration diagnostics. Today the expression
parser accepts `{ expr }` and silently stores it as `Expr::Group`, the same
node as `( expr )`. The syntax specification defines a statement-level
`Block ::= '{' Statement* '}'` (section 5.10, used for function and closure
bodies in section 5.3), but it defines no expression-level brace form, no
separator rule, no meaning for `{}`, and no rule for telling braces apart from
map literals. Section 13's compatibility-policy bullet forbids silently
accepting constructs the specification does not define. Conformance register
item 15 records the gap as `scheduled`.

This plan decides to codify. Curly braces around expressions are not a ddlint
invention: upstream Differential Datalog (DDlog) parses `{ e1; e2; … }` as a
core expression term (`braces eseq`; see `docs/haskell-parser-analysis.md`,
"Expression grammar"), uses it for every braced `if`, `else`, `for`, closure,
and match-arm body, and documents it as "a new block (scope)". Removing it
would reject valid DDlog and would break ddlint's own braced `if` and `for`
parsing. What is wrong today is the shape of the extension, not its existence:
it is unspecified, it conflates braces with parentheses, it cannot represent a
trailing semicolon, it lets an `if` or `for` inside braces swallow the following
`;`, and it treats `{}` as an empty map where DDlog treats it as an empty
block.

After this change a ddlint user can write `{ x }`, `{ a; b }`, `{ a; }`,
`{ if c { a } else { b }; d }`, and `{}` anywhere an expression is allowed and
get a typed `Expr::Block` node whose statements and value are explicit.
Non-empty map literals `{k: v, …}` keep working. Malformed braces such as
`{ a, b }`, `{ a; b, c }`, or `{ a;; b }` produce deterministic, documented
diagnostics. The specification, the conformance register, the users' guide, the
design document, and an Architectural Decision Record (ADR) state the same
contract. Success is observable by running `make test` and seeing the new
block-expression unit, property, behavioural, snapshot, and public-API
integration suites pass, and by reading register item 15 with status
`implemented` and roadmap item 2.6.8 ticked.

## Recommended decision

Each part has a stable identifier used throughout this plan.

`BG-D1` (codify): `{ … }` is a normative block expression. ddlint keeps
accepting it and the specification defines it. Rejecting it with migration
diagnostics was considered and rejected; see `Decision log`.

`BG-D2` (distinct node): a block is `Expr::Block(BlockExpr)`, never
`Expr::Group`, which is reserved for parentheses. `BlockExpr` has two public
fields: `statements: Vec<Expr>`, the items followed by `;`, and
`value: Option<Box<Expr>>`, the final item when it is not followed by `;`.
`value == None` means the block yields unit, `()`. Upstream has no block node
(a one-item `eseq` returns the item itself), so this choice rests on what a
linter needs: the source shape must survive parsing.

`BG-D3` (separator, not operator): inside braces `;` is a separator handled by
the block parser, not the `BinaryOp::Seq` infix operator. A trailing `;` is
accepted. An empty item (`{ ; }`, `{ a;; b }`) is rejected. The infix `Seq`
operator is unchanged outside braces (`BG-D9`).

`BG-D4` (empty braces): `{}` is the empty block, whose value is unit. It is no
longer an empty map literal; a map literal needs at least one `key: value`
entry. Upstream DDlog has no brace map literal at all (its map literals are
`[k -> v, …]` and must be non-empty) and reads `{}` as `()`.

`BG-D5` (disambiguation): after `{`, the first item is parsed with `:` as a
terminator, exactly as today. A following `:` selects a map literal; `;` or `}`
selects a block; `,` produces a separator diagnostic. Consequently `{ x: T }`
stays a map literal, and a block whose first item is a type ascription must be
written `{ (x: T) … }`. Later items are parsed normally, so `{ a; x: T }` is a
block whose value is an ascription. This is a documented divergence from
upstream, caused by ddlint's own `{k: v}` map syntax.

`BG-D6` (struct-literal guard): the "struct-literal guard" is the rule that, in
an `if` condition or a `for` guard, an identifier followed by `{` is not read
as a struct literal, because the `{` starts the body. Today the guard also
reports an error when the identifier is followed by `{}` or by `{ ident :`.
After this change `ident {}` is reported only when another `{` follows it (as in
`if x == Foo{} { a }`), so `if ready {} else { go() }` parses, while the
`{ ident :` heuristic is kept unchanged.

`BG-D11` (separator terminates tails): while block items are parsed, a scoped
parser state ("`;` terminates") makes the infix loop stop at `;`. The state is
kept through prefix-construct tails that would otherwise restart at binding
power 0 (`if`/`else` branches, `for` bodies, closure bodies, `return` values,
and operator right-hand sides) and cleared inside every nested delimiter
(`( … )`, call arguments, `[ … ]` and bit slices, nested `{ … }`, struct
fields, the `match` scrutinee and arms, and the `for ( … )` header). Without it,
`{ if c { a } else { b }; d }` would parse as a single `if` whose `else`
branch is `Seq({ b }, d)`. The state mirrors the existing colon-terminates and
struct-literal states in the Pratt parser and is absent outside braces, so no
parse outside braces changes.

Explicitly out of scope, recorded rather than changed:

- `BG-D7`: no new `DiagnosticCode`. Messages become constants in
  `src/parser/error_messages.rs`; stable codes for expression diagnostics wait
  for roadmap item 2.8.1.
- `BG-D8`: top-level `for` lowering keeps rejecting braced bodies through its
  existing head diagnostic. Upstream accepts them; the gap is recorded under
  register item 8 and scheduled with `BG-D9`.
- `BG-D9`: the `Seq` operator outside braces (`( a; b )`, unbraced `for`
  bodies, call arguments, vector items, struct fields, match-arm bodies, and
  map values such as `{a: x; y}`) is recorded as new register item 16 and
  scheduled as new roadmap item 2.6.9, together with braced top-level `for`
  bodies.
- `BG-D10`: `var` declarations inside blocks remain unsupported and keep
  their existing "unexpected token: K_VAR" diagnostic. Blocks open no semantic
  scope until they do.
- `BG-D12`: upstream `[k -> v, …]` map literals remain unsupported; recorded
  as new register item 17 and scheduled as new roadmap item 2.6.10.

## Constraints

- Do not change the meaning of any existing `DiagnosticCode` (`D-REL-001` to
  `D-REL-008`) or `DiagnosticCategory`; the conformance register freezes them.
- Do not change parenthesized forms: `( e )` must still yield `Expr::Group`,
  `()` must still yield `Expr::Tuple(vec![])`, and `(a, b)` must still yield
  `Expr::Tuple`.
- Do not change non-empty map literal parsing or its colon-terminating key
  mode (`{a and b: 1}` must still be a one-entry map, and `{a: x; y}` must
  still parse as today, a map whose value is `Seq(x, y)`).
- Do not change the binding power or associativity of any infix operator,
  including `BinaryOp::Seq`, and do not change any parse outside braces.
- Do not change top-level `for` lowering semantics, the reserved-token policy
  in `src/parser/reserved_tokens.rs`, or the relation, index, or transformer
  scanners.
- Keep the parser a pure core: no input/output, logging, metrics, or global
  state. No new port or adapter is introduced; the semantic layer and the
  linter keep depending inward on the AST.
- New production code must not panic: no `expect`, `unwrap`, `unreachable!`,
  `todo!`, or slice indexing. (Clippy denies `expect`, `unwrap`, and indexing;
  it does not deny `unreachable!`, which existing semantic helpers use, so this
  plan's rule is stricter than the lint.)
- No Rust source file may exceed 400 lines; every module, including test
  modules, needs a `//!` comment (Whitaker enforces both).
- Use only the shared default Cargo cache; never bypass `RUSTC_WRAPPER`
  compile admission.
- Every commit passes the gates for its change type; a red test is observed
  and recorded, never committed on its own.
- Documentation follows `docs/documentation-style-guide.md` and en-GB Oxford
  spelling (`-ize`, `parenthesized`); `typos.toml` is generated and must not be
  hand-edited.

## Tolerances (exception triggers)

- Scope: stop and escalate if implementation changes more than 45 files in
  total (documentation included) or adds more than 1,600 net lines of Rust
  (production and test code, excluding `proptest-regressions/` seed files).
- Interface: the only planned public API changes are the new `Expr::Block`
  variant, the new `BlockExpr` type and its methods, the `{}` reinterpretation,
  and a new `test_util::block` helper behind the existing `test-support`
  feature. Stop and escalate if any other public signature in `ddlint::parser`,
  `ddlint::sema`, or `ddlint::linter` must change.
- Dependencies: the plan adds exactly four direct development dependencies,
  `rstest-bdd`, `rstest-bdd-macros`, `googletest`, and `pretty_assertions`.
  Their transitive trees (for example `tokio`, `gherkin`, `inventory`) are
  accepted. Stop and escalate before adding any other direct dependency, any
  runtime dependency, or a `build.rs`.
- Compatibility: if `rstest-bdd` 0.6 macros do not work with `rstest` 0.27,
  or their expansions cannot pass `make lint`, stop and present the options
  (pin an older `rstest-bdd`, upgrade `rstest`, or express the behavioural
  scenarios as plain `rstest` integration tests) rather than choosing silently.
- Blast radius: if reinterpreting `{}` changes the expected result of more
  than 12 existing `#[case]` rows or test functions beyond those enumerated in
  `Validation and acceptance`, stop and report.
- Grammar: if any input outside braces changes its parse, or any existing
  accepted input other than those enumerated in `Validation and acceptance`
  becomes rejected, stop and escalate.
- Iterations: if a gate still fails after three fix attempts, stop and
  escalate with the log path.
- Time: if any milestone runs more than eight hours past its first
  `Progress` timestamp, stop and re-plan.

## Risks

- Risk: reinterpreting `{}` (`BG-D4`) silently changes the meaning of code
  that relied on ddlint's previous empty-map reading. Severity: medium.
  Likelihood: low, because upstream DDlog has no brace map literal and ddlint
  is unreleased (version 0.1.0, no Git tags). Mitigation: approval gate;
  `CHANGELOG.md` breaking-change entry advising `map_empty()`, the upstream
  library function for an empty map; users' guide note.

- Risk: `BG-D11` misses a tail or fails to clear the state in a delimiter,
  so `;` is swallowed in one construct or wrongly rejected in another.
  Severity: high. Likelihood: medium. Mitigation: the state is scoped through
  one helper that saves and restores it (like
  `with_struct_literals_suspended`); the unit table and the property generator
  include every tail form and every delimiter; seeded faults remove propagation
  for one tail at a time.

- Risk: narrowing the struct-literal guard (`BG-D6`) weakens diagnostics for
  `if x == Foo{} …`. Severity: low. Likelihood: medium. Mitigation: keep the
  trigger when another `{` follows, pin the newly accepted and still-rejected
  inputs in one table, and document the remaining limitation
  (`if c { x: 1 } else …` is still rejected by the kept heuristic).

- Risk: `rstest-bdd` 0.6 generated code, or `#[gtest]` bodies, may trip
  Clippy or Whitaker (`expect_used`, `no_expect_outside_tests`) because step
  functions are not test functions. Severity: medium. Likelihood: medium.
  Mitigation: EP-M2 proves the harness, including a `#[gtest]` test and
  fallible steps, under `make lint` before any parser change.

- Risk: semantic consumers silently ignore `Expr::Block` through wildcard
  arms (`relation_name` in `src/sema/resolve.rs`, `classify_expression` and
  `classify_for_body_with_aggregation_tracking` in
  `src/parser/ast/rule/classification.rs`, `collect_lowered_terms` in
  `src/parser/top_level_for/lowering.rs`), or panic through inner
  `unreachable!` helpers in `src/sema/resolve.rs` and `src/sema/variables.rs` if
  `Block` is routed into a dispatch group without a matching helper arm.
  Severity: high. Likelihood: medium. Mitigation: dedicated outer `Expr::Block`
  arms, per-consumer unit tests that construct blocks directly, and the
  metamorphic property in `Verification plan`.

- Risk: assignments inside a block (`{ x = f(y); x }`) bind `x` in the
  enclosing scope because blocks open no semantic scope (`BG-D10`). Severity:
  low. Likelihood: high. Mitigation: this matches today's behaviour for
  `( x = f(y); x )`; the ADR and users' guide state it, and scoped `var`
  support is where block scopes will be introduced.

- Risk: `make test` has previously hit Rayon `EAGAIN` thread exhaustion on
  this host. Severity: low. Likelihood: medium. Mitigation: rerun once with
  `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1` and record the rerun.

## Progress

- [x] (2026-10-18T01:00Z) Branch renamed to
  `2-6-8-decide-brace-group-extension-policy`.
- [x] (2026-10-18T01:30Z) Reconnaissance of parser code, specification,
  documentation, prior ExecPlans, test tooling, and upstream DDlog grammar.
- [x] (2026-10-18T02:00Z) First draft written and committed.
- [x] (2026-10-18T03:30Z) Four-member expert review (grammar and parser,
  verification and testing, API and architecture, delivery and documentation)
  completed.
- [x] (2026-10-23T09:00Z) Draft revised to address every review finding.
- [ ] Draft plan pushed and opened as a draft pull request.
- [ ] Plan approved by the user.
- [ ] EP-M1: decision recorded (ADR-004, design document, register items
  16 and 17, roadmap items 2.6.9 and 2.6.10).
- [ ] EP-M2: behavioural and assertion harness proven with existing
  behaviour.
- [ ] EP-M3: block expressions implemented test-first, with specification,
  register, users' guide, changelog, and roadmap aligned.
- [ ] EP-M4: developers' guide, repository layout, and plan close-out.

## Surprises & discoveries

- Observation: upstream DDlog treats braces as a core block expression.
  Evidence: `term = elhs <|> parens … <|> braces eseq <|> term'` and `eseq` in
  upstream `src/Language/DifferentialDatalog/Parse.hs` (quoted in
  `docs/haskell-parser-analysis.md`); the upstream tutorial's "Control flow"
  section states that `{ var x: u32 = y + 1; x }` and
  `{ var x: u32 = y + 1; x; }` differ, the latter yielding `()`. Impact: the
  roadmap's "remove" option would reject valid DDlog; the decision is to codify.

- Observation: ddlint's braced `if`, `else`, `for`, closure, and match-arm
  bodies all go through the brace-group path. Evidence:
  `src/parser/expression/control_flow.rs` and `parse_closure_literal` parse
  those bodies with `parse_expr(0)`, which dispatches `{` to
  `parse_brace_or_map_literal` (`src/parser/expression/prefix.rs`). Impact:
  removal is not viable.

- Observation: prefix-construct tails restart at binding power 0, so inside a
  block an `if`, `for`, closure, or `return` would swallow the separating `;` as
  `BinaryOp::Seq`. Evidence: `Seq` has `l_bp` 0
  (`src/parser/ast/precedence.rs`); `parse_if_clause`, the `for` body, the
  `return` value, and closure bodies call `parse_expr(0)`. Found by the expert
  review. Impact: added `BG-D11`; parsing items at a raised binding power alone
  is insufficient.

- Observation: the first-item continuation in
  `parse_brace_group_continuation` never does anything. Colon-terminating mode
  differs from normal mode only at `:`, and `:` always selects the map branch.
  Evidence: `src/parser/expression/infix.rs`. Impact: the continuation is
  dropped, and `BG-D5` says "first item" explicitly.

- Observation: `{}` parses as `Expr::MapLit(vec![])` today, and
  `if c {} else { d }` reports "struct literal syntax is not allowed in this
  context". Evidence: `parse_brace_or_map_literal` and `parse_ident_expression`
  in `src/parser/expression/data_structures.rs`. Impact: `BG-D4` and `BG-D6`.

- Observation: upstream map literals are `[k -> v, …]`; ddlint's `{k: v}`
  form is itself a ddlint extension, and upstream accepts braced top-level
  `for` statement bodies. Evidence: `emap_literal` and `parseBlockStatement` in
  upstream `Parse.hs`. Impact: `BG-D8` and `BG-D12` record and schedule both
  deltas.

- Observation: the repository has no Verus, Kani, `rstest-bdd`, `googletest`,
  or `pretty_assertions` setup, no `.feature` files, and no `build.rs`;
  `cargo-mutants` is installed on this host. Evidence: `Cargo.toml`, repository
  search, `~/.cargo/bin/cargo-mutants`. Impact: EP-M2 adds the harness;
  `Verification plan` explains the prover decision and uses `cargo-mutants` for
  systematic seeded faults.

- Observation: `ddlint`'s command-line binary is a placeholder that exits with
  status 1. Evidence: `src/main.rs`. Impact: end-to-end coverage is a
  public-API integration test through `ddlint::parse`, `ddlint::sema::build`,
  and the linter `Runner`.

## Decision log

- Decision: codify `{ … }` as a block expression (`BG-D1`) rather than remove
  it. Rationale: upstream DDlog defines it as a core term, ddlint's own
  control-flow bodies depend on it, and removal would need migration
  diagnostics for every braced `if` and `for` body in real DDlog programs.
  Section 13's policy is satisfied by specifying the form. Date/Author:
  2026-10-18, planning agent.

- Decision: `Expr::Block(BlockExpr)` with `statements` plus optional `value`
  (`BG-D2`). Rationale: a linter must tell braces from parentheses; the
  `statements`/`value` split makes "a block's value is its last item unless it
  ends in `;`" structural, so no flag can contradict the value. A separate
  struct (as with `MatchArm`) is justified because it carries the canonical
  traversal methods `items()` and `into_items()` and the constructor
  `from_items`. Rejected alternatives: keep `Expr::Group` (loses fidelity,
  cannot represent `{ a; }`); `Block(Box<Expr>)` over a `Seq` chain (cannot
  represent `{}` or `{ a; }` without inventing unit nodes); a flat `items` list
  plus `has_trailing_semicolon` flag (admits the invalid state "empty block
  ending in `;`"). `#[non_exhaustive]` is not added to `Expr` or `BlockExpr`:
  no AST type uses it today, and the ADR defers exhaustiveness to an AST-wide
  decision at roadmap item 2.7.1 or 2.8.1. Date/Author: 2026-10-18, revised
  2026-10-23 after API review, planning agent.

- Decision: `{}` is an empty block (`BG-D4`). Rationale: upstream parity, and
  real DDlog writes `if (c) {} else { … }`. Alternative left to the approver:
  keep `{}` as an empty map, codify only non-empty blocks, keep `BG-D6`'s
  current guard, and document `{}` as a ddlint-specific divergence; upstream
  programs using `{}` as an empty body would then be parsed incorrectly.
  Date/Author: 2026-10-18, planning agent. Requires user approval.

- Decision: add a scoped "`;` terminates" parser state (`BG-D11`). Rationale:
  it is the smallest change that makes block items stop at `;` inside
  prefix-construct tails without changing any parse outside braces. Rejected
  alternative: parse every tail at `BP_SEQ + 1` unconditionally, which would
  change unbraced `for` bodies and parenthesized sequences, violating
  `Constraints` and pre-empting roadmap item 2.6.9. Date/Author: 2026-10-23,
  planning agent, from the grammar and verification reviews.

- Decision: schedule the residual deltas instead of leaving them unscheduled:
  register item 16 and roadmap item 2.6.9 (`Seq` outside braces and braced
  top-level `for` bodies); register item 17 and roadmap item 2.6.10 (upstream
  `[k -> v]` map literals). Rationale: the specification's source-of-truth
  policy requires every recorded delta to be scheduled in `docs/roadmap.md`,
  and roadmap 2.7 requires 2.6 contract decisions to be closed or explicitly
  bounded. Date/Author: 2026-10-23, planning agent. Requires user approval,
  because it adds roadmap items.

- Decision: no Verus or Kani lane for this item. Rationale, per obligation, is
  in `Verification plan` ("Where Verus and Kani are and are not warranted").
  Date/Author: 2026-10-23, planning agent. Requires user approval, because
  `AGENTS.md` asks for Verus proofs of introduced lemmas and this plan
  introduces lemma sketches it discharges by other means.

- Decision: record ADR-004 (`docs/adr-004-block-expression-policy.md`).
  Rationale: the decision fixes a public AST variant, a normative grammar
  production, a deliberate upstream divergence (`{ x: T }`), and a breaking
  change (`{}`) just before the ADR-001 crate split; ADR-002 is the precedent
  for AST-shape ADRs. The ADR covers `BG-D1` to `BG-D6`, `BG-D11`, and the AST
  shape; `BG-D7` to `BG-D10` and `BG-D12` appear under its non-goals.
  Date/Author: 2026-10-18, revised 2026-10-23, planning agent.

- Decision: messages are constants, not new diagnostic codes (`BG-D7`).
  Rationale: the stable-surface rules in the register say message text is not
  an identifier and codes are additive; reserved-token messages from 2.6.7 are
  the uncoded precedent; expression diagnostics in rule bodies already flow
  through the observer as category `rule` with no code, so no
  `DiagnosticCategory` or observability change is needed. Date/Author:
  2026-10-23, planning agent.

## Outcomes & retrospective

Not started. Complete this section at each milestone boundary and at
completion, comparing the result against `Purpose / big picture`.

## Context and orientation

ddlint is a Rust linter for DDlog. Its parser lives under `src/parser/`. A
tokenizer (`src/tokenizer.rs`, using `logos`) produces `SyntaxKind` tokens
(`src/language.rs`). Span scanners under `src/parser/span_scanners/` find
top-level declarations, and a concrete syntax tree (CST, a lossless tree that
keeps every token) is built with `rowan`. Expressions are not stored as typed
CST nodes. Instead an abstract syntax tree (AST, a typed tree without
punctuation) is built on demand by a handwritten Pratt parser, a
precedence-climbing parser driven by a table of binding powers (numbers that
decide how tightly an operator holds its operands).

Files that matter:

- `src/parser/expression/pratt.rs`:
  `parse_expression(src) -> Result<Expr, Vec<Simple<SyntaxKind>>>`, the public
  expression entry point; `parse_expr(min_bp)`; `parse_map_key_expr`, which
  parses with `:` as a terminator; and the scoped parser states
  (`with_struct_literals_suspended`, `with_struct_literal_activation`) that
  `BG-D11` imitates.
- `src/parser/expression/infix.rs`: `parse_infix` and
  `parse_infix_with_colon_mode`, the infix loop.
- `src/parser/expression/prefix.rs`: dispatches `T_LBRACE` to
  `parse_brace_or_map_literal`.
- `src/parser/expression/data_structures.rs` (310 lines):
  `parse_brace_or_map_literal`, `parse_map_entries`,
  `parse_brace_group_continuation`, `parse_ident_expression` (the
  struct-literal guard), `parse_parenthesized_expr`, and
  `parse_closure_literal`.
- `src/parser/expression/control_flow.rs`: `if`, `match`, `return`, and `for`
  parsing; `parse_if_condition` and the `for` guard activate the struct-literal
  guard.
- `src/parser/expression/postfix.rs`: call arguments and bit slices.
- `src/parser/ast/precedence.rs`: binding powers; `BP_SEQ` (private) is the
  lowest; `;` is the left-associative infix `BinaryOp::Seq`.
- `src/parser/ast/expr.rs` (320 lines): the public `Expr` enum (24 variants,
  not `#[non_exhaustive]`), including `Group(Box<Expr>)` and `MapLit`.
- `src/parser/ast/expr/sexpr.rs`: `Expr::to_sexpr()`, an S-expression
  (Lisp-style prefix) rendering used for compact test expectations.
- Consumers that match on `Expr`: `src/sema/resolve.rs` (`relation_name`
  and two dispatch groups), `src/sema/variables.rs` (two dispatch groups),
  `src/sema/traverse.rs` (calls `relation_name` for heads, `for` iterables, and
  guards), `src/parser/ast/rule/classification.rs` (`classify_expression`,
  `classify_for_body_with_aggregation_tracking`), and
  `src/parser/top_level_for/lowering.rs` (`collect_lowered_terms`).
- `src/parser/error_messages.rs`: crate-private contract message constants;
  `src/test_util/mod.rs` re-exports selected ones for integration tests.
- `src/test_util/expressions.rs`: `test-support` helpers such as `var`,
  `call`, and `map_lit`.

Upstream DDlog, quoted from `docs/haskell-parser-analysis.md` and the upstream
`Parse.hs`:

```haskell
term  =  elhs
     <|> (withPos $ eTuple <$> (parens $ commaSepEnd expr))
     <|> braces eseq
     <|> term'

-- Semicolon-separated expressions with an optional extra semicolon in the end.
eseq = do
    p1 <- getPosition
    me <- optionMaybe expr
    p2 <- getPosition
    case me of
       Nothing -> return $ E $ ETuple (p1, p2) []
       Just e -> do extra_semi <- optionMaybe semi
                    if isNothing extra_semi
                       then return e
                       else do suffix <- eseq
                               return $ E $ ESeq (fst $ pos e, snd $ pos suffix) e suffix
```

In upstream, `;` is not an expression operator, and `if`, `for`, and closure
tails are `term`s, so `{ if c { a } else { b }; d }` has two items.

Terms used in this plan: a "block" is a brace-delimited expression
`{ s1; …; sn; v }`; its "statements" are the items followed by `;`; its "value"
is the optional final item not followed by `;`; "unit" is the empty tuple `()`.
A "tail" is the part of a prefix construct parsed after its keyword or header
(an `if` branch, a `for` body, a closure body, a `return` value). A "seeded
fault" is a deliberate bug introduced temporarily to prove a test can fail.
"Non-vacuity" means evidence that a check can fail when the code is wrong. A
"metamorphic property" relates the outputs of several related inputs instead of
checking one output against a fixed answer.

## Conformance basis

Upstream artefacts and identifiers:

- `docs/roadmap.md` item 2.6.8 at commit `d1c2571` (`RM-2.6.8`); this plan
  adds items 2.6.9 (`RM-2.6.9`) and 2.6.10 (`RM-2.6.10`).
- `docs/parser-conformance-register.md` item 8 (`REG-8`), item 15 (`REG-15`),
  and its maintenance rules; this plan adds items 16 (`REG-16`) and 17
  (`REG-17`).
- `docs/differential-datalog-parser-syntax-spec-updated.md`: "Status and
  source-of-truth policy" (`SPEC-POLICY`), section 3.3 (`SPEC-3.3`), section 4
  (`SPEC-4`), sections 5.3 and 5.10 (`SPEC-5.10`), section 9 (`SPEC-9`),
  section 10 (`SPEC-10`), section 11.6 (`SPEC-11.6`), section 12 (`SPEC-12`),
  section 13's compatibility-policy bullet (`SPEC-13-COMPAT`), and section 14.
- `docs/haskell-parser-analysis.md`, "Expression grammar" (`UP-ESEQ`).
- `docs/ddlint-design.md` section 2 (parsing pipeline), section 2.4 (crate
  split), and section 6 (testing strategy).
- `docs/adr-001-parser-crate-split.md` (`ADR-001`: the crate that will own
  the expression AST is decided in roadmap 2.7.1, and new node variants then
  need joint approval) and `docs/adr-002-relation-role-kind-modelling.md`.
- Governing standards: `AGENTS.md`, `docs/documentation-style-guide.md`,
  `docs/developers-guide.md`, `docs/repository-layout.md`.

No Terms of Reference document exists for ddlint; none is invented.

Trace links:

```plaintext
SPEC-13-COMPAT + UP-ESEQ -> RM-2.6.8 -> REG-15 -> BG-D1 -> EP-M1 -> docs/adr-004-block-expression-policy.md
SPEC-POLICY -> BG-D9/BG-D8 -> EP-M1 -> REG-16 + REG-8 note + RM-2.6.9
SPEC-POLICY -> BG-D12 -> EP-M1 -> REG-17 + RM-2.6.10
BG-D2 -> EP-M3 -> V-L1, V-L2 -> ast::expr::block::tests
BG-D3, BG-D11 -> EP-M3 -> V-L3, V-I1, V-I2 -> block_expression_proptest + block_expressions table
BG-D4, BG-D5 -> EP-M3 -> V-L4 -> classifies_every_follow_token + block_expressions table
BG-D6 -> EP-M3 -> struct_literal_guard table in tests/expression_compound.rs
BG-D2 consumers -> EP-M3 -> V-L5, V-I4 -> tests/block_expression_semantics.rs
BG-D8 -> EP-M3 -> tests/top_level_for_desugaring.rs braced-body cases
BG-D1..BG-D6 -> EP-M3 -> SPEC-3.3/3.4/4/5.10/9/10/11.6/12/14 text + REG-15 implemented + RM-2.6.8 done
behavioural contract -> EP-M3 -> tests/features/block_expressions.feature
```

## Verification plan

### Abstract model

The specification of a block is independent of ddlint's token stream. An
abstract block is a finite sequence of expressions `items = [e1, …, en]`
(`n ≥ 0`) with a flag `t` ("ends in `;`"), subject to the well-formedness
condition `t ⇒ n ≥ 1`. Its value is `en` when `n ≥ 1` and `t` is false, and
unit otherwise. Its concrete syntax is `{`, then `e1; e2; …; en`, then `;` when
`t` holds, then `}`.

The representation is `BlockExpr { statements, value }`. The representation
relation `R` maps a well-formed abstract block to a `BlockExpr`:

- if `t` is false and `n ≥ 1`: `statements = [e1, …, e(n-1)]` and
  `value = Some(en)`;
- otherwise: `statements = [e1, …, en]` and `value = None`.

Its executable forms are
`BlockExpr::from_items(items, ends_with_semicolon) -> Option<BlockExpr>`
(returning `None` exactly for the ill-formed input "empty and ends in `;`"),
and the inverse pair `BlockExpr::items()` (all items in source order) and
`BlockExpr::ends_with_semicolon()`
(`value.is_none() && !statements.is_empty()`).

### Invariants

- `V-I1` (no swallowed separator): inside a block item, every `Seq` node lies
  beneath a delimiter node nested within that item (`Group`, `Tuple`, `VecLit`,
  `MapLit`, `Struct`, `Apply`/`Call` arguments, `Match` arms, or a nested
  `Block`). In particular no item root, and no `if`/`else` branch, `for` body,
  closure body, or `return` value reachable from an item without crossing a
  delimiter, is a `Seq`.
- `V-I2` (order preservation): the left-to-right source order of block items
  equals the order of `BlockExpr::items()`.
- `V-I3` (brace/parenthesis separation): the parser never produces
  `Expr::Group` for a brace-delimited form and never produces `Expr::Block` for
  a parenthesized form.
- `V-I4` (semantic transparency): for a rule body containing `( s1; …; sn )`
  and the same rule body containing `{ s1; …; sn }`, the semantic observation
  is equal, where the observation is the multiset of
  `(name, UseKind, Resolution, span)` from `ddlint::sema::build(&parse(src))`
  plus the linter `Runner` diagnostics. (Spans line up because `{` and `(` have
  the same width.) The domain excludes items ending in an open tail, whose
  parenthesized and braced parses legitimately differ under `BG-D11`.
- `V-I5` (totality without panic): for any token sequence beginning with `{`,
  `parse_expression` returns `Ok` or `Err` without panicking, and every error
  span satisfies `start <= end <= src.len()`.
- `V-I6` (no change outside braces): for any expression source containing no
  `{`, the parse is identical before and after this change.

### Lemmas (proof sketch)

These are hypotheses about the proof structure; implementation may refine or
split them.

- `V-L1` (representation is a bijection): `R` is a bijection between
  well-formed abstract blocks and `BlockExpr` values. Sketch: by cases on
  `value`. If `value = Some(v)`, the unique preimage is
  `(statements ++ [v], false)`, well-formed because its length is at least one.
  If `value = None` and `statements` is empty, it is `([], false)`. If
  `value = None` and `statements` is non-empty, it is `(statements, true)`. The
  three images are disjoint and each map is injective on lists. Executable
  statement: `from_items(b.items(), b.ends_with_semicolon()) == Some(b)` for
  every `b`, and for every well-formed `(items, t)`, `from_items(items, t)` is
  `Some(b)` with `b.items() == items` and `b.ends_with_semicolon() == t`.
  Scope: the type has no junk state for the flag; `V-I1` is a parser property,
  not a type property.
- `V-L2` (value semantics): the abstract value of `b` is `b.value` (with
  `None` meaning unit), and `{ a; }` and `{ a; () }` both have value unit
  although their representations differ (`([a], None)` and `([a], Some(()))`).
  Sketch: immediate from the case split in `V-L1`.
- `V-L3` (parse correspondence): let each item source `si` be drawn from the
  generator's item grammar (identifiers, integer literals, `+`, `and`, calls,
  tuples, parenthesized sequences, nested blocks, map literals, and the tails
  `if`/`else`, `for`, closure, and `return`), with the first item additionally
  fully consumed by `parse_map_key_expr` (no top-level `:`). Then parsing the
  concrete syntax of `([s1, …, sn], t)` yields
  `Expr::Block(from_items([ê1, …, ên], t))`, where `êi` is the expected AST
  built independently by the generator. Sketch: induction on `n`. Each item
  parse stops exactly at the next `;` or `}` because (a) the infix loop runs
  with the "`;` terminates" state set, (b) every tail inherits that state, and
  (c) every delimiter clears it and restores it on exit; the separator loop
  consumes exactly one `;` per item.
- `V-L4` (disambiguation is total and deterministic): define
  `classify_block_follow(position, kind)` for `position` in {opening, first
  item, later item, after separator} and `kind` in all `SyntaxKind` values plus
  end of input. After `{`: `}` gives the empty block, `;` gives the empty-item
  diagnostic, end of input gives unclosed, and anything else starts the first
  item. After the first item: `:` gives map; `;` gives separator; `}` gives end;
  `,` gives the map-or-block separator diagnostic; end of input gives the
  unclosed-brace diagnostic; anything else gives the item-separator diagnostic.
  After a later item: `;` gives separator; `}` gives end; end of input gives
  unclosed; anything else, including `,`, gives the item-separator diagnostic.
  After a `;`: `}` gives end, `;` gives the empty-item diagnostic, end of input
  gives unclosed, and anything else starts a later item. Sketch: case analysis
  over a finite set.
- `V-L5` (consumer semantics): each consumer's rule for `Expr::Block` is
  fixed as follows, and transparency (`V-I4`) follows by structural induction
  over `Expr`, one case per consumer:
  - `relation_name(Block b)` is `relation_name(v)` when `b.statements` is
    empty and `b.value` is `Some(v)`, and `None` otherwise (matching today's
    `Group(Seq(…))` result);
  - the binding and use collectors in `src/sema/resolve.rs` and
    `src/sema/variables.rs` visit `b.items()` in order through a dedicated
    outer `Expr::Block` arm, opening no scope;
  - `classify_for_body_with_aggregation_tracking` flattens `b.into_items()`
    in order, recursing into nested `Block`, `Group`, and `Seq`;
  - `classify_expression` keeps a bare rule-body `{ A(x); B(x) }` as one
    `RuleBodyTerm::Expression(Block)`, as it keeps `( A(x); B(x) )` today;
  - `collect_lowered_terms` keeps a braced top-level `for` body as an
    unsupported head (`BG-D8`).

### Axioms

- `V-A1`: `logos` tokenizes `{`, `}`, `;`, `,`, and `:` as `T_LBRACE`,
  `T_RBRACE`, `T_SEMI`, `T_COMMA`, and `T_COLON`; existing tokenizer tests
  cover this.
- `V-A2`: `chumsky::error::Simple::custom` stores the span and message given
  to it unchanged.
- `V-A3`: `proptest` explores generated inputs as documented and replays
  failures from its regression file.
- `V-A4`: upstream DDlog semantics are as stated in its tutorial and
  `Parse.hs`; ddlint does not execute DDlog, so this axiom only fixes the
  intended meaning of `value`.
- `V-A5`: `rstest-bdd` 0.6 binds each scenario to a generated `rstest` test
  and embeds the feature file with `include_bytes!`, so Cargo rebuilds when the
  feature file changes without a `build.rs` (verified in its `codegen/tracking`
  module during review; EP-M2 confirms it in this crate).

### Obligations and methods

`V-L1` and `V-L2`:

- Method: type-level construction plus a `proptest` round-trip property on
  `from_items`, `items`, and `ends_with_semicolon`, plus a three-row `rstest`
  table.
- Rationale: the bijection is enforced by the shape of `BlockExpr`; the
  executable content is `from_items` and the two inverse methods.
- Domain: item lists of length 0 to 8 built from distinct variables `x0`,
  `x1`, …, crossed with both flag values.
- Artefact: unit tests in `src/parser/ast/expr/block.rs`.
- Evidence: `cargo test --all-features --lib block_expr_round_trip` passes.
- Non-vacuity: the table has one witness per case, including the rejected
  `([], true)`; seeded faults "return `Some` for `([], true)`" and "put the
  last statement into `value` when the flag is set" must each fail.

`V-L3`, `V-I1`, `V-I2`, `V-I3`:

- Method: `proptest` property tests against an independent oracle, plus a
  named `rstest` table.
- Rationale: the obligation ranges over unbounded item counts and nested
  expressions; sampling with shrinking against the real parser gives evidence
  about the shipped code, which a proof over a model would not.
- Domain: the generator produces pairs `(source, expected Expr)`. It first
  picks a class, then builds a block of that class
  (`class_strategy().prop_flat_map(block_of_class)`), so every class is
  constructed, never filtered. Classes: empty; one value; one statement with
  trailing `;`; two or more items with and without trailing `;`; nested depth
  two or more (built directly); first item a nested block or map; an item
  ending in each tail (`if`/`else`, `if` without `else`, `else if`, `for`,
  closure, `return`); a parenthesized sequence item. Items use distinct
  identifiers so that reordering is always observable. The expected AST is
  built with `src/test_util` helpers and the new `block` helper, and the source
  is printed from the generated tree, so the oracle does not reuse the parser
  under test. A secondary check compares each item with `parse_expression(si)`.
- Artefact: `src/parser/tests/block_expression_proptest.rs`, with the
  generator in `src/parser/tests/block_expression_proptest/strategy.rs` to stay
  under 400 lines; regression seeds in
  `proptest-regressions/parser/tests/block_expression_proptest.txt` are
  committed if created.
- Evidence: `cargo test --all-features --lib block_expression_proptest`
  passes with 256 cases per property. With about 12 evenly weighted classes,
  the chance that a run misses a class is about `12 × (11/12)^256`, which is
  below `10^-8`.
- Class coverage evidence: `generator_covers_every_class` draws 256 values
  with `TestRunner::deterministic()` through `strategy.new_tree(&mut runner)`
  and `current()`, counts each class, and asserts every count is at least 5;
  the counts are recorded in `Artefacts and notes`.
- Non-vacuity: the red run fails because braces produce `Expr::Group`.
  Seeded faults that must each fail a property: reversing `statements`; setting
  `value = Some(last)` for `{ a; }`; parsing items without the "`;` terminates"
  state (violates `V-I1` for tail classes); not propagating the state into
  `for` bodies only; returning `Expr::Group` for `{ e }` (violates `V-I3`).

`V-I6`:

- Method: characterization. Before any parser change, record the
  `to_sexpr()` rendering and error list of every expression in the existing
  expression fixtures that contains no `{`
  (`src/parser/tests/expression/fixtures/`); after the change, assert they are
  identical.
- Artefact: `src/parser/tests/block_expressions/no_brace_regression.rs`
  (an inline `insta` snapshot taken in EP-M2, unchanged in EP-M3).
- Non-vacuity: the snapshot covers `Seq` in parentheses, unbraced `for`
  bodies, and call arguments; seeded fault "set the `;` terminates state at the
  top level" must change it.

`V-L4`:

- Method: exhaustive enumeration plus a whole-input table.
- Rationale: the classifier's domain is finite. A unit test iterates every
  `SyntaxKind` from `0` to `N_ERROR as u16` (asserting that
  `SyntaxKind::from_u16` covers the range without gaps), plus end of input, for
  both positions. This completely checks the classifier, but it largely
  restates its `match`; the stronger evidence is the whole-input table, which
  exercises each outcome through the real parser and checks the exact message
  and span.
- Artefact: `classifies_every_follow_token` in
  `src/parser/expression/block.rs`; the whole-input table in
  `src/parser/tests/block_expressions.rs`.
- Evidence: `cargo test --all-features --lib classifies_every_follow_token`.
- Non-vacuity: the test asserts every `BlockFollow` variant is produced by
  at least one input; seeded fault "map `T_COMMA` after a later item to
  separator" must fail the table.

`V-L5` and `V-I4`:

- Method: per-consumer unit tests that construct `Expr::Block` by hand, plus
  a `proptest` metamorphic property over rule bodies.
- Rationale: wildcard arms make a missing `Block` arm silent, so each
  consumer is tested directly; the metamorphic property catches a consumer that
  diverges from its `Group` treatment.
- Domain: rule bodies `R(x) :- S(x), <wrapped>.` where `<wrapped>` is
  `( items )` or `{ items }` and the items are atoms, conditions, and
  assignments from the generator (tail classes excluded, see `V-I4`).
- Artefacts: unit tests beside each consumer (`src/sema/resolve.rs`,
  `src/sema/variables.rs`, `src/parser/ast/rule/classification.rs`, and
  `src/parser/top_level_for/lowering.rs` test modules) and
  `tests/block_expression_semantics.rs`.
- Evidence: the observations are equal for all generated cases.
- Red evidence: the per-consumer unit tests fail in the red step because the
  consumer arms are stubs; the metamorphic property passes before the change
  (braces and parentheses both produce `Group` today) and is therefore a
  characterization guard whose non-vacuity comes from seeded faults.
- Non-vacuity: seeded faults "drop `value` in `relation_name`", "skip
  `statements` in the use collector", and "route `Block` into a dispatch group
  whose inner helper has no `Block` arm" (which must panic and fail the test)
  must each be rejected.

`V-I5`:

- Method: two `proptest` properties with at least 2,048 cases each.
- Domain (structural fuzz): space-separated token sequences of length 1 to 20
  starting with `{`, over `{`, `}`, `(`, `)`, `;`, `,`, `:`, `a`, `b`, `1`, `+`,
  `if`, `else`, `for`, `in`, `return`, `|`, and `Foo`, so the struct-literal
  guard, every tail, and the `;` state are reached.
- Domain (differential): space-separated sequences over `{`, `}`, `;`, `,`,
  `:`, `a`, `1`, `+` only, compared with a small reference recognizer written
  in the test from the grammar in this plan, which predicts accept or reject
  and, when accepted, block, map, or group at the root.
- Artefact: `src/parser/tests/block_expression_proptest.rs`.
- Non-vacuity: both properties assert that `Ok` and `Err` each occur at
  least 100 times; the differential property gives evidence for `V-L4` on
  rejected inputs, which no other test covers; seeded fault "treat `;` after
  `;` as a separator" must fail it.

Systematic seeded faults: after green, run
`cargo mutants --file src/parser/expression/block.rs --file src/parser/ast/expr/block.rs`
and record surviving mutants in `Artefacts and notes`; each survivor is either
killed by a new test or justified in `Decision log`.

### Where Verus and Kani are and are not warranted

No Verus proof and no Kani harness are planned. The reasons differ by
obligation:

- `V-L1` and `V-L2` are pure statements about a data type and are within
  Verus's supported subset, so a proof is technically possible. It would prove
  a three-case triviality over `Vec`: the property follows from the shape of
  `BlockExpr` and the case split in `from_items`, and the round-trip property
  test checks the executable functions directly over lengths 0 to 8, which
  covers every case of the split. A Verus toolchain lane (none exists in this
  repository) would add infrastructure without adding assurance.
- `V-L3`, `V-I1`, and `V-I2` concern the correspondence between a grammar
  and ddlint's handwritten, `Vec`-backed token-stream parser, which uses
  `chumsky` error types and mutable parser state. A Verus proof would have to
  verify a re-implementation of that parser, establishing a property of a model
  rather than of the shipped code.
- `V-L4` has a finite domain and is checked exhaustively by enumeration,
  which is complete for the classifier.
- Kani gains nothing: there is no `unsafe` code, arithmetic, or bounded state
  machine, and `Expr` is heap-allocated and recursive.

None of these is contractual business logic. If later work introduces a
scope-resolution invariant over unbounded nesting (for example scoped `var`
declarations inside blocks), a Verus model should be reconsidered; ADR-004
records that trigger. If the approver prefers a Verus proof of `V-L1`
regardless, the plan would add a milestone EP-M3a creating a `verus/` proof
crate that proves `V-L1` over a generic `Seq<T>` model of `from_items`,
`items`, and `ends_with_semicolon`, with a `make verus` target and the command
`make verus` as an extra acceptance criterion.

Residual gap: property tests sample `V-L3`; they do not prove it for every
input. The class-first generator, independent oracle, class-coverage counts,
and seeded-fault and mutation evidence are the mitigation.

## Plan of work

### Stage A: understand and propose (complete when the plan is approved)

No code changes. This plan is the output.

### Stage B: decision record (EP-M1)

Write `docs/adr-004-block-expression-policy.md` from the template in
`docs/documentation-style-guide.md` ("Architectural decision records"). Its
Status line reads `Accepted (2026-MM-DD): codify brace blocks as Expr::Block`,
dated on approval. Include the context (`SPEC-13-COMPAT`, `SPEC-5.10`,
`UP-ESEQ`, register item 15), the decision (`BG-D1` to `BG-D6`, `BG-D11`, and
the `BlockExpr` shape), an "Architectural rationale" that records how the proof
obligations shaped the design (no flag field because of `V-L1`; the classifier
extracted as a pure function because of `V-L4`; the scoped `;` state because of
`V-L3`), the rejected options, non-goals (`BG-D7` to `BG-D10`, `BG-D12`),
consequences (including that ADR-001's joint-approval rule will govern later
changes to `BlockExpr`), and the Verus trigger.

Add ADR-004 and this ExecPlan to `docs/contents.md`. In a separate commit, add
the missing ADR-003 entry to `docs/contents.md`.

In `docs/ddlint-design.md` section 2, after the 2.6.7 paragraph, add a two- or
three-sentence summary of the block-expression policy that cites ADR-004,
following the ADR-002 paragraph's pattern.

In `docs/parser-conformance-register.md`, rewrite item 15's "Spec/target
behaviour" to state the decision, leaving "Current behaviour (code)" unchanged
and status `scheduled`. Add a sentence to item 8 recording that upstream
accepts braced top-level `for` bodies and that roadmap 2.6.9 decides them. Add
item 16, "Sequence operator outside braces", status `decision needed`, listing
every context where `Seq` appears outside braces (`( a; b )`, unbraced `for`
bodies, call arguments, vector items, struct fields, match-arm bodies, and map
values such as `{a: x; y}`). Add item 17, "Upstream bracket map literals
`[k -> v]`", status `decision needed`.

Use the `mapsplice` skill to insert into `docs/roadmap.md`, after 2.6.8:

```plaintext
- [ ] 2.6.9. Decide sequence-operator policy outside braces, including
  braced and sequenced top-level `for` bodies. See
  docs/parser-conformance-register.md items 8 and 16.
- [ ] 2.6.10. Decide support for upstream bracket map literals
  (`[k -> v, …]`). See docs/parser-conformance-register.md item 17.
```

### Stage C: behavioural and assertion harness (EP-M2)

Add to `[dev-dependencies]` in `Cargo.toml`: `rstest-bdd = "0.6.0"`,
`rstest-bdd-macros = "0.6.0"`, `googletest = "0.14.3"`, and
`pretty_assertions = "1.4.1"`. Do not enable the `rstest-bdd` `no-inventory`
feature (it switches to a build-script registry).

Create `tests/features/block_expressions.feature` containing only the first
scenario ("A map literal parses as a map"), and
`tests/block_expressions_bdd.rs` with a `//!` comment, a `ScenarioState` (an
`rstest-bdd` derive that groups per-scenario values) holding `Slot`s (cells
that steps fill and read) for the source and the parse outcome, and step
functions that return `Result<(), String>` and read slots with `.get()` and
`ok_or`, never `expect`. Add one `googletest` test,
`#[gtest] #[rstest] #[case(…)]` in that order (`#[gtest]` first, per the
`googletest` README's interoperability note), using `.expect("…")` in its body
and `pretty_assertions::assert_eq!`, over existing map-literal behaviour. Add
the `V-I6` no-brace characterization snapshot.

Run all gates. If the harness cannot pass `make lint`, follow `Tolerances`.

### Stage D: red tests, then implementation (EP-M3)

Red (observed and recorded, not committed): first add the types so tests
compile: `src/parser/ast/expr/block.rs` with `BlockExpr`, `from_items`, `items`,
`into_items`, `ends_with_semicolon`, and `Default`; the `Expr::Block` variant;
the `(block …)` S-expression arm; the `block` helper in
`src/test_util/expressions.rs`; the new message constants; and consumer arms in
the exhaustive matches written as stubs that ignore the block. Then add the
tests in `Validation and acceptance`: the unit and whole-input tables, the
classifier test against a stub classifier, the property tests, the remaining
eight BDD scenarios, the snapshot table, the per-consumer unit tests, the
metamorphic property, and the top-level `for` braced-body pins. Run the focused
commands and record that they fail for the intended reasons ("expected Block,
found Group", "expected empty block, found MapLit([])", "expected Seq-free
item, found IfElse{else: Seq(…)}", and the consumer stubs' wrong results).

Green: create `src/parser/expression/block.rs` with `BlockFollow`,
`ItemPosition`, `classify_block_follow`, `parse_block_or_map_literal` (replacing
`parse_brace_or_map_literal`), `parse_block_items`, and `finish_block`; move
`parse_map_entries` there too. Add `pub(crate) const BLOCK_ITEM_MIN_BP` to
`src/parser/ast/precedence.rs` instead of exposing `BP_SEQ`. Add the "`;`
terminates" state to the Pratt parser beside the struct-literal state, with
`with_semicolon_terminating` and `with_semicolon_terminating_cleared` helpers,
and make `parse_infix` stop at `T_SEMI` while it is set. Apply the clearing
helper in every delimiter listed in `BG-D11`. Drop the first-item continuation.
Update `prefix.rs` dispatch. Narrow the struct-literal guard in
`parse_ident_expression` (`BG-D6`). Fill in the consumer arms per `V-L5`.
Update the existing tests whose expectations change (listed in
`Validation and acceptance`). Run the focused commands until they pass, then
all gates, then commit red and green together.

Refactor: keep each new function small and single-purpose, confirm no file
exceeds 400 lines, rerun the focused tests and all gates, and commit.

Seeded faults and mutation run: apply each seeded fault in `Verification plan`
in the working tree, run the named test, record the failure, and restore with
`git restore`; run `cargo mutants` as described. Record all evidence in
`Artefacts and notes`.

Alignment commit, in the same milestone because the register's maintenance
rules require behaviour, register, roadmap, and specification to change
together:

- Specification: add section 3.4 "Block expressions" with the grammar below
  and the disambiguation and struct-literal-guard text; in section 3.3 state
  that map literals need at least one entry and that `{}` is a block; in
  section 4 state that `;` inside braces is a separator, not an operator; in
  section 5.10 replace `Block ::= '{' Statement* '}'` with a reference to
  `BlockExpr` and state that ddlint parses function and closure bodies, braced
  `if`/`for` bodies, and block expressions with the same production; add the
  five diagnostics to section 9 and the malformed examples `{ a, b }`,
  `{ a; b, c }`, and `{ a;; b }` to section 12; add a `{}` and a
  `{ if c { a } else { b }; d }` example to section 11.6; in section 10 map both
  `SBlock` and a new `EBlock` to `Expr::Block`, noting that ddlint has no
  separate statement AST; add a section 14 change-log bullet.
- Register item 15: "Current behaviour (code)" describes `Expr::Block` and
  `src/parser/expression/block.rs`; status `implemented`.
- `docs/parser-implementation-notes.md`: replace "This behaviour is an
  implementation extension" in the struct-literal guard paragraph with the
  `BG-D6` rule and a pointer to section 3.4 and ADR-004; add an "Expression
  parser invariants" entry for `BLOCK_ITEM_MIN_BP`, the `;` terminates state,
  `classify_block_follow`, and `V-I1` to `V-I3`; note that control-flow bodies
  are now `Expr::Block`; add `block.rs` to the file index; note on
  `Expr::MapLit` that the parser no longer produces an empty map.
- `docs/users-guide.md`: a "Block expressions" section with examples, the
  `{}`, `{ (x: T) }`, and no-scope notes.
- `CHANGELOG.md` under `## Unreleased`: `### Breaking changes`, "`{}` is now
  an empty block, not an empty map literal; write `map_empty()` for an empty
  map", and a new `### Added`, "Block expressions `{ a; b }` with typed
  `Expr::Block` nodes".
- `docs/roadmap.md`: mark 2.6.8 done.

```ebnf
BlockExpr  ::= '{' BlockBody? '}'
BlockBody  ::= Expr (';' Expr)* ';'?
```

Disambiguation text for section 3.4: "After `{`, the first item is parsed with
`:` as a terminator. A following `:` makes the form a map literal (section 3.3);
`;` or `}` makes it a block; `,` is an error. Therefore `{ x: T }` is a map
literal, and a block whose first item is an ascription is written
`{ (x: T) … }`. `{}` is the empty block, whose value is `()`. Inside a block,
`;` ends the current item even when the item ends with an `if`, `for`, closure,
or `return` tail."

### Stage E: close-out (EP-M4)

Update `docs/developers-guide.md`: the parser module structure entry for
`src/parser/expression/block.rs` and `src/parser/ast/expr/block.rs`; a
"Boundary rules" entry stating that `BlockExpr::items()` and `into_items()` are
the only sanctioned traversal order, that consumers must handle `Expr::Block`
and `Expr::Group` in dedicated arms, and that `BLOCK_ITEM_MIN_BP` and the `;`
terminates state are owned by the expression parser; a "Parser test helpers"
entry for `test_util::block`; and the behavioural-test conventions (step
functions fallible, `#[gtest]` before `#[rstest]`, `googletest` matchers for
shape checks and `pretty_assertions` for whole-AST equality). Update
`docs/repository-layout.md` "Test layout" with `tests/features/` and the rule
that each `.feature` file is bound by one `tests/*_bdd.rs` file. Update
`docs/ddlint-design.md` section 6 with the behavioural-test layer
(`rstest-bdd`) and assertion libraries. Reconcile `Outcomes & retrospective`
and set this plan to `COMPLETE`.

### Abstraction policy compliance

`AGENTS.md` requires a sweep, a re-use policy, and a recorded decision for each
new abstraction.

- Sweep (done during planning, to be repeated before EP-M3): no existing
  block type, follow-token classifier, or `;`-aware item loop exists.
  `parse_comma_separated_expressions` was considered for generalization and
  rejected, because a trailing `,` is ignored while a trailing `;` changes the
  value. There is no `group()` test helper; `block(statements, value)` follows
  the `return_expr` and `if_expr` helper convention.
- Re-use policy: `BlockExpr::items()` and `into_items()` are the canonical
  traversal; consumers must not concatenate the fields by hand.
  `classify_block_follow` is private to `src/parser/expression/block.rs` with
  one caller. `BLOCK_ITEM_MIN_BP` is owned by `precedence.rs`. The `rstest-bdd`
  steps and `ScenarioState` are file-local to `tests/block_expressions_bdd.rs`
  until a second feature needs them, at which point they move to
  `tests/support/`.
- Record: EP-M4's developers' guide entries above.

## Milestones and plateaus

Every milestone ends with `make check-fmt`, `make lint`, and `make test`
passing, plus `make markdownlint` and `make nixie` when documentation changed,
with logs at `/tmp/<gate>-ddlint-<branch>.out`.

### EP-M1: decision recorded

- Outcome: ADR-004 exists and is indexed; the design document summarizes it;
  register items 8, 15, 16, and 17 and roadmap items 2.6.9 and 2.6.10 are in
  place; item 15 is still `scheduled`.
- Requirements: `RM-2.6.8` (decision half), `REG-15`, `BG-D8`, `BG-D9`,
  `BG-D12`, `SPEC-POLICY`.
- Acceptance evidence: all gates pass; ADR-004 has the template's required
  sections.
- Conformance check:
  - Decision text matches `BG-D1` to `BG-D12`.
  - No code changed.
  - Upstream evidence cited from an in-tree document.
  - No public interface or dependency change.
  - No persisted-format change.
  - Trace links in this plan point at the created files.
- Recovery: documentation-only; revert the commits to retry.
- Remaining gaps: the code still behaves as before.
- Compatibility decision: none.

### EP-M2: behavioural and assertion harness

- Outcome: the four development dependencies are present; one BDD scenario,
  one `#[gtest]` test, and the `V-I6` characterization snapshot pass over
  existing behaviour.
- Requirements: the test-strategy requirement for `rstest-bdd`,
  `googletest`, `pretty_assertions`, and `insta`.
- Acceptance evidence: `make test` output includes
  `a_map_literal_parses_as_a_map` and the `#[gtest]` test passing; `make lint`
  passes with the harness present.
- Conformance check:
  - Only the four named direct dependencies added; no `build.rs`.
  - No production code changed.
  - Trace links current.
- Recovery: revert the `Cargo.toml`, `Cargo.lock`, and test-file commit.
- Remaining gaps: no block behaviour yet.
- Compatibility decision: none.

### EP-M3: block expressions implemented and aligned

- Outcome: `Expr::Block(BlockExpr)` is produced for every brace-delimited
  non-map form; `{}` is an empty block; block items stop at `;` through tails;
  diagnostics are constants; consumers handle blocks; specification, register
  item 15 (`implemented`), implementation notes, users' guide, changelog, and
  roadmap 2.6.8 (done) describe the behaviour.
- Requirements: `BG-D1` to `BG-D6`, `BG-D8`, `BG-D11`, `SPEC-13-COMPAT`,
  `REG-15`, `RM-2.6.8`.
- Acceptance evidence: every test in `Validation and acceptance` passes;
  seeded-fault, class-coverage, and mutation evidence recorded in
  `Artefacts and notes`.
- Conformance check:
  - `V-L1` to `V-L5` and `V-I1` to `V-I6` have passing artefacts.
  - Parses outside braces, non-empty maps, reserved tokens, and top-level
    `for` behave as before.
  - Only the planned public interface change occurred.
  - No dependency change beyond EP-M2.
  - Register, roadmap, and specification updated in the same milestone.
  - Trace links current.
- Recovery: commits are green (red and green together), refactor, then
  alignment; revert to the last green commit to retry.
- Remaining gaps: developers' guide and repository layout.
- Compatibility decision: none. `Expr` is a pre-1.0, unreleased API (version
  0.1.0, no tags), so callers are updated in place; no alias preserves the old
  brace-to-`Group` behaviour.

### EP-M4: close-out

- Outcome: developers' guide, repository layout, and design-document testing
  section agree with the implementation; plan marked `COMPLETE`.
- Requirements: documentation obligations in `AGENTS.md`.
- Acceptance evidence: all gates pass.
- Conformance check:
  - Every upstream artefact in `Conformance basis` reconciled.
  - No unrecorded deviation.
  - No public interface or dependency change in this milestone.
  - Trace links current.
- Recovery: documentation-only.
- Remaining gaps: roadmap 2.6.9 and 2.6.10 by design.
- Compatibility decision: none.

## Concrete steps

All commands run from the repository root of a checkout of the branch. Gates
run sequentially, never in parallel, each captured with `tee`:

```sh
BRANCH=$(git branch --show-current)
set -o pipefail
make fmt 2>&1 | tee "/tmp/fmt-ddlint-${BRANCH}.out"
make check-fmt 2>&1 | tee "/tmp/check-fmt-ddlint-${BRANCH}.out"
make lint 2>&1 | tee "/tmp/lint-ddlint-${BRANCH}.out"
make test 2>&1 | tee "/tmp/test-ddlint-${BRANCH}.out"
make markdownlint 2>&1 | tee "/tmp/markdownlint-ddlint-${BRANCH}.out"
make nixie 2>&1 | tee "/tmp/nixie-ddlint-${BRANCH}.out"
```

`make fmt` may reformat unrelated Markdown; restore unrelated churn with
`git restore <path>` before committing. If `make test` fails with
`Resource temporarily unavailable`, rerun it once as
`RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1 make test`.

Focused red/green commands for EP-M3:

```sh
cargo test --all-features --lib block_expr_round_trip
cargo test --all-features --lib block_expressions 2>&1 | tee "/tmp/red-block-ddlint-${BRANCH}.out"
cargo test --all-features --lib block_expression_proptest
cargo test --all-features --lib classifies_every_follow_token
cargo test --all-features --test block_expressions_bdd
cargo test --all-features --test block_expression_semantics
cargo test --all-features --test expression_prefix
cargo test --all-features --test top_level_for_desugaring
```

Expected red output (abridged) before the parser change:

```plaintext
---- parser::tests::block_expressions::parses_blocks::case_01_single_value stdout ----
assertion failed: `(left == right)`
Diff < left / right > :
<Group(
>Block(
```

Expected green output:

```plaintext
test result: ok. N passed; 0 failed; 0 ignored
```

Commit after each green step with an imperative subject of 50 characters or
fewer and a body wrapped at 72 columns.

## Validation and acceptance

Behavioural specification, `tests/features/block_expressions.feature` (scenario
1 lands in EP-M2, the rest in EP-M3). "Parsing fails with" means that some
reported error's message equals the quoted text exactly. Item lists are given
as data tables and compared by parsing each expected item with
`parse_expression`, never by comparing text.

```gherkin
Feature: Block expressions
  Brace-delimited expressions are blocks whose value is their final item
  unless the block is empty or ends with a semicolon.

  Scenario: A map literal parses as a map
    Given the expression source "{a: 1, b: 2}"
    When the expression is parsed
    Then the result is a map literal with 2 entries

  Scenario: Semicolons separate block statements in order
    Given the expression source "{ a; b; c }"
    When the expression is parsed
    Then the result is a block with statements
      | item |
      | a    |
      | b    |
    And the block value is "c"

  Scenario: A trailing semicolon makes the block value unit
    Given the expression source "{ a; }"
    When the expression is parsed
    Then the result is a block with statements
      | item |
      | a    |
    And the block has no value

  Scenario: Empty braces are an empty block
    Given the expression source "{}"
    When the expression is parsed
    Then the result is a block with no statements
    And the block has no value

  Scenario: An if expression inside a block does not swallow the separator
    Given the expression source "{ if c { a } else { b }; d }"
    When the expression is parsed
    Then the result is a block with statements
      | item                  |
      | if c { a } else { b } |
    And the block value is "d"

  Scenario Outline: Braces and parentheses stay distinct
    Given the expression source "<source>"
    When the expression is parsed
    Then the result is a <form>

    Examples:
      | source     | form                |
      | ( x )      | parenthesized group |
      | { x }      | block               |
      | { x: T }   | map literal         |
      | { (x: T) } | block               |

  Scenario Outline: Malformed blocks are rejected
    Given the expression source "<source>"
    When the expression is parsed
    Then parsing fails with "<message>"

    Examples:
      | source       | message                                                   |
      | { a, b }     | expected ':' for a map literal, or ';' or '}' for a block |
      | { a; b, c }  | expected ';' or '}' after a block item                    |
      | { a;; b }    | empty statement in block: remove the extra ';'            |

  Scenario: Braced if branches with an empty block parse
    Given the expression source "if ready {} else { go() }"
    When the expression is parsed
    Then the result is an if expression whose then branch is an empty block

  Scenario: A struct literal before a body is still rejected in an if condition
    Given the expression source "if x == Foo{} { a }"
    When the expression is parsed
    Then parsing fails with "struct literal syntax is not allowed in this context"
```

Message constants, added to `src/parser/error_messages.rs` and re-exported
through `src/test_util/mod.rs` for integration tests:

- `BLOCK_OR_MAP_SEPARATOR_ERROR`:
  `expected ':' for a map literal, or ';' or '}' for a block`.
- `BLOCK_ITEM_SEPARATOR_ERROR`: `expected ';' or '}' after a block item`.
- `EMPTY_BLOCK_ITEM_ERROR`: `empty statement in block: remove the extra ';'`.
- `UNCLOSED_BLOCK_ERROR`: `expected '}' to close the block or map literal`.

The existing struct-literal message is unchanged. None of the new messages
contains a `D-…:` prefix, so `DiagnosticCode::from_message` cannot mistake them
for codes.

Unit tables in `src/parser/tests/block_expressions.rs` (new; `rstest`, with
`googletest` matchers for shape checks and `pretty_assertions` for whole-AST
equality):

- Accepted blocks: `{ x }`, `{ 1 }`, `{ f() }`, `{ (x, y) }`, `{ 1 + 2 }`,
  `{ a and b }` (the existing `parses_brace_groups` cases, now `Block`);
  `{ a; b }`; `{ a; b; }`; `{ a; }`; `{}`; `{ { a } }`; `{ {a}; b }`;
  `{ {a: 1} }`; `{ (a; b) }`; `{ (x: T) }`; `{ a; x: T }`; `{ return }`;
  `{ break }`; `{ continue }`; `{ x = 1; x }`; `{ a = b; c }`; `{ a => b; c }`;
  `{ match (x) { A -> 1 }; y }`.
- Tails (each must give two items): `{ if c { a } else { b }; d }`,
  `{ if c { a }; d }`, `{ if a { x } else if b { y }; z }`,
  `{ for (x in xs) { f(x) }; y }`, `{ for (x in xs) f(x); g() }`,
  `{ |x| x; y }`, `{ return x; y }`, `{ a = if c { x } else { y }; z }`.
- Delimiters inside a block still allow `Seq` (unchanged behaviour):
  `{ (a; b); c }`, `{ f(a; b) }`, `{ [a; b] }`, `{ match (x) { A -> a; b } }`.
- Map literals unchanged: `{a: 1}`, `{a and b: 1}`, `{a: 1,}`,
  `{{x: 1}: {y: 2}}`, `{ x: T }`, `{ a + b: T }`, `{ x = y: T }`, and
  `{a: x; y}` (a map whose value is `Seq(x, y)`); `{{}: 1}` now has an
  empty-block key.
- Rejected with exact message and span: `{ a, b }`, `{ a; b, c }`,
  `{ a; b c }`, `{ ; }`, `{ a;; b }`, `{ a`, `{ a;`, and `{ var x = 1; x }`
  (existing "unexpected token: K_VAR", `BG-D10`).

Struct-literal guard table (`BG-D6`), added to `tests/expression_compound.rs`.
Accepted: `if ready {} else { go() }`, `if ready {}`, and
`for (x in xs if ok) {}`. Rejected with the struct-literal message:
`if x == Foo{} { a }`, `for (x in xs if y == Foo{}) {}` (the `for` guard also
activates the guard), and `if c { x: 1 } else { d }` (the kept `{ ident :`
heuristic; a documented limitation).

Existing tests whose expectations change, and only these:

- `src/parser/tests/collections.rs`: `parses_brace_groups` (now `Block`);
  the `{}` cases at lines 59 and 242 (the latter,
  `preserves_raw_map_literal_nodes::empty_map`, is a 2.6.3 stage-boundary case
  and becomes an empty-block case with a comment pointing to `BG-D4`);
  `{{}: 1}`. The `map_comma_without_colon` case matches the substring
  `"expected ':'"`, which the new message still contains, so it does not change.
- `src/parser/tests/expression/fixtures/valid/collections.rs` lines 56 and
  57: `"{}"` now expects the empty block.
- `tests/expression_prefix.rs`: `{1}`, `{}`, `{ x }`, `{ return }`.
- `tests/expression_to_sexpr.rs` line 89: `{}` renders `(block)`, not
  `(map)`.
- `src/parser/tests/control_flow.rs`: `{ break }`, `{ continue }`,
  `{ return }`.
- `src/parser/tests/expression/fixtures/valid/control_flow.rs` and
  `tests/expression_compound.rs`: braced branches become `Block`.
- `src/parser/ast/rule/tests.rs`:
  `for_loop_with_sequence_body_flattens_all_statements` keeps its three-term
  result through the new block flattening.

`src/parser/tests/rules/aggregations.rs` matches a parenthesized
`Aggregate((user), …)` key, not a braced body; it must not change, and a change
there is a regression. `src/parser/tests/collections.rs` line 212 builds
`MapLit(vec![])` directly and stays unchanged.

Snapshot matrix: `src/parser/tests/block_expression_snapshots.rs` renders one
table, a line per input for every accepted and rejected input in the tables
above, each line being the source, then either `to_sexpr()` or the sorted list
of `message @ start..end`, and checks it with a single inline
`insta::assert_snapshot!(rendered, @"…")`. Rendering contract: `{}` renders
`(block)`; `{ a }` renders `(block (value a))`; `{ a; }` renders
`(block (stmts a))`; `{ a; b }` renders `(block (stmts a) (value b))`.

Public-API integration test, `tests/block_expressions.rs`, through
`ddlint::parse`, `ddlint::sema::build`, and the linter `Runner` (the
command-line binary is still a placeholder):

```ddlog
input relation Items(x: u32)
input relation Checked(x: u32)
input relation Witness(x: u32)
output relation Out(x: u32)
Out(x) :- Items(x), for (y in Items(y)) { Checked(y); Checked(x) }.
```

It asserts no parse errors; that rule-body classification yields the iterable
and both `Checked` terms in order; that the `unused_relation` rule does not
report `Checked` (referenced only inside the braced body) but does report
`Witness` (a deliberate, non-vacuous witness); and that the diagnostics equal
those for the parenthesized `( Checked(y); Checked(x) )` variant. Further cases
in `tests/top_level_for_desugaring.rs` assert that
`for (x in Items(x)) { Out(x) }.` and `for (x in Items(x)) { Out(x); }.` at top
level report the existing unsupported-head diagnostic (`BG-D8`).

Red-Green-Refactor evidence to record in `Artefacts and notes`:

- Red: the focused commands fail with `Group` where `Block` is expected,
  `MapLit([])` where an empty block is expected, an `IfElse` swallowing `Seq`
  for tail cases, and wrong consumer results from the stub arms.
- Green: the same commands pass after `block.rs` and the consumer arms land.
- Refactor: the same commands, then all gates, pass.

Quality criteria:

- Tests: `make test` passes, including every suite above.
- Verification: `V-L1` to `V-L5` and `V-I1` to `V-I6` discharged as in
  `Verification plan`, with class-coverage, seeded-fault, and mutation evidence
  recorded. No Verus or Kani command applies unless the approver asks for
  EP-M3a, in which case `make verus` must also pass.
- Lint and formatting: `make check-fmt`, `make lint`, `make markdownlint`,
  and `make nixie` pass.

## Idempotence and recovery

Every step is a file edit followed by a gate run; rerunning a gate is safe.
Seeded faults are applied in the working tree, run once, recorded, and reverted
with `git restore`; they are never committed. If a milestone fails partway,
return to the last green commit (`git reset --hard <sha>` only after confirming
no uncommitted work is needed) and repeat the milestone.

## Artefacts and notes

Record here, during implementation: the red transcripts, the class-coverage
counts, the seeded-fault transcripts (one per fault in `Verification plan`), the
`cargo mutants` summary and any justified survivors, the approved snapshot
text, and the gate log paths.

## Interfaces and dependencies

In `src/parser/ast/expr/block.rs` (new, re-exported from `ddlint::parser::ast`):

```rust
/// Brace-delimited block expression `{ s1; …; sn; v }`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct BlockExpr {
    /// Items followed by `;`, in source order.
    pub statements: Vec<Expr>,
    /// Final item not followed by `;`; `None` means the block yields `()`.
    pub value: Option<Box<Expr>>,
}

impl BlockExpr {
    /// Build a block from its items in source order.
    ///
    /// Returns `None` for an empty block that ends in `;`, which has no
    /// concrete syntax.
    #[must_use]
    pub fn from_items(items: Vec<Expr>, ends_with_semicolon: bool) -> Option<Self> { … }

    /// All items in source order: statements, then the value if present.
    pub fn items(&self) -> impl Iterator<Item = &Expr> { … }

    /// Consume the block, yielding its items in source order.
    pub fn into_items(self) -> impl Iterator<Item = Expr> { … }

    /// Whether the source block ended in `;` (true only when non-empty).
    #[must_use]
    pub fn ends_with_semicolon(&self) -> bool { … }
}
```

There is deliberately no `value()` accessor; read the field with
`block.value.as_deref()`.

In `src/parser/ast/expr.rs`, add `Block(BlockExpr)` to `Expr` with the doc
comment "Brace-delimited block expression; see `BlockExpr`.", and change the
`Group` doc comment to "Parenthesized expression `( e )`.".

In `src/parser/ast/precedence.rs`:

```rust
/// Minimum binding power for block items: one above `;`, so items stop at it.
pub(crate) const BLOCK_ITEM_MIN_BP: u8 = BP_SEQ + 1;
```

In `src/parser/expression/block.rs` (new, private to the expression parser):

```rust
/// Which item the follow token comes after.
pub(super) enum ItemPosition { Opening, First, Later, AfterSeparator }

/// Outcome of classifying the token after `{`, an item, or a `;`.
pub(super) enum BlockFollow {
    Map, Separator, End, EmptyItem, MapOrBlockSeparatorError, ItemSeparatorError, Unclosed,
}

pub(super) fn classify_block_follow(position: ItemPosition, kind: Option<SyntaxKind>) -> BlockFollow;
```

`ItemPosition::Opening` covers the token directly after `{` (`}` gives `End`,
`;` gives `EmptyItem`, anything else starts the first item). `AfterSeparator`
covers the token after a `;` (`}` gives `End`, `;` gives `EmptyItem`, anything
else starts a later item).

In the Pratt parser state (beside the struct-literal state in
`src/parser/expression/pratt.rs`): a `semicolon_terminates: bool` field and the
scoped helpers `with_semicolon_terminating` and
`with_semicolon_terminating_cleared`, each saving and restoring the previous
value.

In `src/test_util/expressions.rs` (feature `test-support`):

```rust
/// Build an `Expr::Block` from statements and an optional value.
pub fn block(statements: Vec<Expr>, value: Option<Expr>) -> Expr { … }
```

Development dependencies (EP-M2), all caret requirements:
`rstest-bdd = "0.6.0"`, `rstest-bdd-macros = "0.6.0"`, `googletest = "0.14.3"`,
`pretty_assertions = "1.4.1"`. Combine `googletest` with `rstest` by placing
`#[gtest]` before `#[rstest]`.

## Related documentation and skills

Read before implementing:

- `docs/differential-datalog-parser-syntax-spec-updated.md` (status policy,
  sections 3.3, 4, 5.3, 5.10, 9, 10, 11.6, 12, 13, 14).
- `docs/parser-implementation-notes.md` (Pratt parser, struct-literal guard,
  feature-specific diagnostic messages).
- `docs/parser-conformance-register.md` (items 8, 14, 15, stable diagnostic
  surface, maintenance rules).
- `docs/haskell-parser-analysis.md` (upstream expression grammar).
- `docs/building-an-error-recovering-parser-with-chumsky.md` (recovery at
  `}` and `;` boundaries, returning `Option` to keep parsing).
- `docs/rust-parser-testing-comprehensive-guide.md` (sections 3.2 snapshots,
  3.3 error-recovery testing, 3.4 Pratt parser tables, 5.2 fuzzing).
- `docs/rust-testing-with-rstest-fixtures.md` (sections IV.A `#[case]`
  tables, V.A fixture composition).
- `docs/ddlint-design.md` (sections 2, 2.4, and 6).
- `docs/complexity-antipatterns-and-refactoring-strategies.md` (section 3
  "bumpy road", section 5.A command/query separation).
- `docs/documentation-style-guide.md` (ADR template).
- `docs/developers-guide.md` (boundary rules, parser test helpers, the BDD
  step fallibility rule).
- `docs/execplans/2-6-7-finalize-legacy-token-compatibility-policy.md`
  (precedent for diagnostics routing and typed fixtures).

Skills: `execplans` (this document); `rust-router`, then `rust-unit-testing`
(rstest, googletest, pretty_assertions, insta), `proptest` (class-first
generators, seeded faults), and `rust-types-and-apis` (`BlockExpr`);
`rust-verification` and `verus` (if the prover decision is revisited);
`arch-decision-records` (ADR-004); `hexagonal-architecture` (keeping the parser
a pure core with no new ports); `codegraph-mcp` (finding `Expr` consumers
before editing); `mapsplice` (roadmap insertion); `changelog`;
`commit-message`; and `en-gb-oxendict-style` (documentation).

## Revision note

2026-10-18: initial draft, written after Wyvern reconnaissance of the parser,
documentation, tooling, and upstream DDlog grammar.

2026-10-23: revised after a four-member expert review. What changed: added
`BG-D11` (scoped `;` terminator) after the review showed control-flow tails
would swallow block separators; corrected the claim that the specification does
not mention blocks and planned the section 5.10 reconciliation; scheduled the
residual deltas as register items 16 and 17 and roadmap items 2.6.9 and 2.6.10;
narrowed `BG-D6`; specified each consumer's block semantics and restated `V-I4`
over public semantic observations; replaced the parser-reusing oracle with an
independent generator oracle and class-first strategy; added `V-I6`; fixed the
`googletest` attribute order, the snapshot design, and the BDD step design;
removed the `value()` accessor and added `from_items`, `into_items`, and
`ends_with_semicolon`; completed the list of changed tests; moved the
changelog, users' guide, and roadmap updates into EP-M3; stopped committing red
steps; required every gate at every milestone; and marked the prover,
empty-braces, and roadmap decisions as requiring user approval. Effect on
remaining work: EP-M3 grows (the `;` state and its tests), and EP-M1 adds
roadmap and register entries; the milestone sequence is otherwise unchanged.
