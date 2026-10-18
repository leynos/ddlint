# Codify brace-delimited block expressions (roadmap 2.6.8)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT

This plan must be approved before implementation begins. Silence is not
approval.

## Purpose / big picture

Roadmap item 2.6.8 asks for a decision on the brace-group extension `{ expr }`:
either codify it or remove it with migration diagnostics. Today the expression
parser accepts `{ expr }` and silently stores it as `Expr::Group`, the same
node as `( expr )`. The syntax specification does not mention it, and section
13 of that specification forbids silently accepting constructs that it does not
define. The conformance register (item 15) records the gap as `scheduled`.

This plan decides to codify. Curly braces around expressions are not a ddlint
invention: upstream Differential Datalog (DDlog) parses `{ e1; e2; … }` as a
core expression term (`braces eseq` in the upstream Haskell parser), uses it
for every braced `if`, `else`, `for`, closure, and match-arm body, and
documents it as "a new block (scope)". Removing it would reject valid DDlog
programs and would break ddlint's own braced `if` and `for` parsing. What is
wrong today is the shape, not the existence, of the extension: it is
unspecified, it conflates braces with parentheses, it cannot represent a
trailing semicolon, and it treats `{}` as an empty map where DDlog treats it as
an empty block.

After this change a ddlint user can write `{ x }`, `{ a; b }`, `{ a; }`, and
`{}` anywhere an expression is allowed and get a typed `Expr::Block` node whose
statements and value are explicit. Map literals `{k: v, …}` keep working.
Malformed braces such as `{ a, b }` or `{ a;; b }` produce deterministic,
documented diagnostics. The specification, the conformance register, the users'
guide, and an Architectural Decision Record (ADR) all state the same contract.
Success is observable by running `make test` and seeing the new
block-expression unit, property, behavioural, snapshot, and end-to-end suites
pass, and by reading register item 15 with status `implemented`.

## Recommended decision

The decision has six parts, each with a stable identifier used in the rest of
this plan.

`BG-D1` (codify): `{ … }` is a normative block expression. ddlint keeps
accepting it and the specification defines it. Rejecting it with migration
diagnostics was considered and rejected; see `Decision log`.

`BG-D2` (distinct node): a block is `Expr::Block(BlockExpr)`, never
`Expr::Group`. `Expr::Group` is reserved for parentheses. `BlockExpr` has two
fields, `statements: Vec<Expr>` for the items terminated by `;`, and
`value: Option<Box<Expr>>` for the final item when it is not followed by `;`.
This mirrors upstream semantics, where the value of a block is its final
expression, or the unit value `()` when the block is empty or ends in `;`.

`BG-D3` (separator, not operator): inside braces `;` is a separator handled by
the block parser, not the `BinaryOp::Seq` infix operator. Items are parsed at a
binding power just above `;`, so no block item is ever a bare `Seq` node. A
trailing `;` is accepted. An empty item (`{ ; }`, `{ a;; b }`) is rejected with
a deterministic diagnostic. The infix `Seq` operator itself is unchanged
outside braces; see `BG-D9`.

`BG-D4` (empty braces): `{}` is the empty block, whose value is unit. It is no
longer an empty map literal. A map literal requires at least one `key: value`
entry. This matches upstream DDlog, which has no empty-map literal and treats
`{}` as `()`. This is the one user-visible behaviour change in the plan and
needs explicit approval; see `Risks` and `Decision log`.

`BG-D5` (disambiguation): after `{`, ddlint parses the first item with `:` as a
terminator, exactly as today. A following `:` selects a map literal; `;` or `}`
selects a block; `,` produces the diagnostic
`expected ':' for a map literal, or ';' or '}' for a block`. Consequently
`{ x: T }` stays a map literal; a block whose value is a type ascription must
be written `{ (x: T) }`. The specification documents this as a deliberate
divergence from upstream.

`BG-D6` (struct-literal guard): in contexts where struct literals are disabled
(the condition of an `if`), `ident {}` is no longer reported as forbidden
struct-literal syntax, because `{}` is now a valid empty block, so
`if ready {} else { go() }` parses. The `ident { ident :` heuristic is kept.

The following are explicitly out of scope and are recorded rather than changed.
`BG-D7`: no new `DiagnosticCode`; messages become constants in
`src/parser/error_messages.rs`, and stable codes for expression diagnostics
wait for roadmap item 2.8.1. `BG-D8`: top-level `for` lowering keeps rejecting
braced bodies through its existing diagnostics. `BG-D9`: the `Seq` operator
outside braces (`( a; b )` and unbraced `for` bodies) is a separate extension,
recorded as new register item 16 with status `decision needed`. `BG-D10`: `var`
declarations inside blocks remain unsupported and keep their existing
"unexpected token" diagnostic.

## Constraints

- Do not change the meaning of any existing `DiagnosticCode` (`D-REL-001` to
  `D-REL-008`) or `DiagnosticCategory`; the conformance register freezes them.
- Do not change parenthesized forms: `( e )` must still yield `Expr::Group`,
  `()` must still yield `Expr::Tuple(vec![])`, and `(a, b)` must still yield
  `Expr::Tuple`.
- Do not change non-empty map literal parsing or its colon-terminating key
  mode (`{a and b: 1}` must still be a one-entry map).
- Do not change the binding power or associativity of any infix operator,
  including `BinaryOp::Seq`, outside braces.
- Do not change top-level `for` lowering semantics, the reserved-token policy
  in `src/parser/reserved_tokens.rs`, or the relation, index, or transformer
  scanners.
- Keep the parser free of input/output, logging side effects, and global state;
  the expression parser is the pure core that the linter and semantic layers
  consume. No new port or adapter is introduced.
- Production code must not panic and must not use `expect`, `unwrap`,
  `unreachable!`, or indexing; Clippy denies them.
- No Rust source file may exceed 400 lines; every module, including test
  modules, needs a `//!` comment (Whitaker enforces both).
- Use only the shared default Cargo cache; never bypass `RUSTC_WRAPPER`
  compile admission.
- Documentation follows `docs/documentation-style-guide.md` and en-GB Oxford
  spelling; `typos.toml` is generated and must not be hand-edited.

## Tolerances (exception triggers)

- Scope: stop and escalate if implementation needs more than 40 changed files
  or more than 1,400 net added lines excluding documentation and test data.
- Interface: the only planned public API change is the new `Expr::Block`
  variant, the new `BlockExpr` type, and the `{}` reinterpretation. Stop and
  escalate if any other public signature in `ddlint::parser` or `ddlint::sema`
  must change.
- Dependencies: the plan adds exactly four development dependencies,
  `rstest-bdd`, `rstest-bdd-macros`, `googletest`, and `pretty_assertions`.
  Stop and escalate before adding any other dependency, any runtime dependency,
  or a `build.rs`.
- Compatibility: if `rstest-bdd` 0.6 cannot coexist with `rstest` 0.27, stop
  and present the options (pin an older `rstest-bdd`, upgrade `rstest`, or
  express behaviour scenarios as `rstest` integration tests) rather than
  choosing silently.
- Blast radius: if reinterpreting `{}` changes the expected result of more
  than 12 existing tests outside `src/parser/tests/collections.rs` and
  `tests/expression_prefix.rs`, stop and report, because that would suggest
  `{}` as an empty map is load-bearing somewhere unexpected.
- Grammar: if any existing passing input other than those enumerated in
  `Validation and acceptance` changes from accepted to rejected, stop and
  escalate.
- Iterations: if a gate still fails after three fix attempts, stop and
  escalate with the log path.
- Time: if any milestone takes more than one working day of elapsed effort,
  stop and re-plan.

## Risks

- Risk: reinterpreting `{}` from empty map to empty block (`BG-D4`) silently
  changes the meaning of code that relied on ddlint's previous behaviour.
  Severity: medium. Likelihood: low, because upstream DDlog has no `{}` map
  literal and ddlint has no released version (no Git tags, version 0.1.0).
  Mitigation: the plan treats this as a decision requiring approval, records it
  in `CHANGELOG.md` under breaking changes and in the users' guide, and keeps
  an alternative open in `Decision log` (retain `{}` as an empty map and only
  codify non-empty blocks).

- Risk: the struct-literal guard change (`BG-D6`) lets an input that used to
  produce a "struct literal syntax is not allowed" error parse cleanly.
  Severity: low. Likelihood: medium. Mitigation: the only removed trigger is
  `ident {}` in a no-struct-literal context; a dedicated table pins both the
  newly accepted `if c {} else { d }` and the still-rejected `if c { x: 1 } …`
  heuristic.

- Risk: `rstest-bdd` 0.6 may require a different `rstest` major version or
  need a `build.rs` to track feature-file changes. Severity: medium.
  Likelihood: medium. Mitigation: milestone EP-M2 adds the harness alone, with
  one scenario over existing behaviour, before any parser change; the
  `Tolerances` section names the escalation.

- Risk: changing `parse_brace_or_map_literal` grows
  `src/parser/expression/data_structures.rs` (310 lines) past 400 lines and
  creates a "bumpy road" function. Severity: low. Likelihood: high. Mitigation:
  move brace handling into a new module `src/parser/expression/block.rs` with
  small single-purpose helpers.

- Risk: downstream consumers that match on `Expr::Group` (semantic resolution,
  variable collection, rule-body classification, the S-expression renderer)
  silently ignore `Expr::Block`, losing relation references or variables inside
  braces. Severity: high. Likelihood: medium, because some matches use a
  wildcard arm. Mitigation: the plan enumerates every consumer, and the
  property and end-to-end tests assert that semantic results for `{ e }` equal
  those for `( e )`.

- Risk: the repository's `make test` has previously hit Rayon `EAGAIN` thread
  exhaustion on this host. Severity: low. Likelihood: medium. Mitigation: rerun
  with `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1` and record the rerun; do not
  treat it as a code failure.

## Progress

- [x] (2026-10-18T01:00Z) Branch renamed to
  `2-6-8-decide-brace-group-extension-policy`.
- [x] (2026-10-18T01:30Z) Reconnaissance of parser code, specification,
  documentation, prior ExecPlans, test tooling, and upstream DDlog grammar.
- [x] (2026-10-18T02:00Z) Draft ExecPlan written.
- [ ] Expert review of the draft and revision.
- [ ] Draft plan committed, pushed, and opened as a draft pull request.
- [ ] Plan approved by the user.
- [ ] EP-M1: decision recorded (ADR-004, design document, register).
- [ ] EP-M2: test harness dependencies added with one green scenario.
- [ ] EP-M3: block expression implemented test-first with specification and
  register aligned.
- [ ] EP-M4: guides, changelog, and roadmap closed out.

## Surprises & discoveries

- Observation: upstream DDlog treats braces as a core block expression, not
  an extension. Evidence: `term = elhs <|> parens … <|> braces eseq <|> term'`
  and the `eseq` definition in upstream
  `src/Language/DifferentialDatalog/Parse.hs`; the upstream tutorial's "Control
  flow" section states that `{ var x: u32 = y + 1; x }` and
  `{ var x: u32 = y + 1; x; }` differ, the latter yielding `()`. Impact: the
  roadmap's "remove" option would reject valid DDlog; the decision is to codify.

- Observation: ddlint's braced `if`, `else`, and `for` bodies, closure bodies,
  and match-arm bodies are all parsed through the brace-group path. Evidence:
  `src/parser/expression/control_flow.rs` parses those bodies with
  `parse_expr(0)`, which dispatches `{` to `parse_brace_or_map_literal` in
  `src/parser/expression/prefix.rs`. Impact: removal is not viable without
  rewriting control flow.

- Observation: `{}` parses as `Expr::MapLit(vec![])` today, and
  `if c {} else { d }` reports "struct literal syntax is not allowed in this
  context". Evidence: `src/parser/expression/data_structures.rs`
  (`parse_brace_or_map_literal` and `parse_ident_expression`). Impact: drives
  `BG-D4` and `BG-D6`.

- Observation: the repository has no Verus, Kani, `rstest-bdd`,
  `googletest`, or `pretty_assertions` setup, no `.feature` files, and no
  `build.rs`. Evidence: `Cargo.toml` dev-dependencies and a repository-wide
  search. Impact: EP-M2 adds the behavioural harness; the `Verification plan`
  explains why no prover lane is added.

- Observation: `ddlint`'s command-line binary is a placeholder that exits with
  status 1. Evidence: `src/main.rs`. Impact: "end-to-end" here means an
  integration test through the public `ddlint::parse` entry point and the linter
  `Runner`, as earlier roadmap items did.

## Decision log

- Decision: codify `{ … }` as a block expression (`BG-D1`) rather than remove
  it. Rationale: upstream DDlog defines it as a core term, ddlint's own
  control-flow bodies depend on it, and removal would need migration
  diagnostics for every braced `if` and `for` body in real DDlog programs. The
  specification's compatibility policy is satisfied by specifying the form, not
  by rejecting it. Date/Author: 2026-10-18, planning agent; requires user
  approval.

- Decision: give blocks their own node, `Expr::Block(BlockExpr)` (`BG-D2`),
  instead of reusing `Expr::Group`. Rationale: braces and parentheses differ
  upstream (a block opens a scope, a block may contain a sequence and a trailing
  `;`, parentheses build tuples), a linter must be able to tell them apart,
  and roadmap 2.7 is about to freeze the `Expr` shape in the `ddlog-syntax`
  crate split (ADR-001). The `statements` plus optional `value` representation
  makes the upstream rule "a block's value is its last expression unless it
  ends in `;`" structural, so no invalid combination (such as a
  trailing-semicolon flag on an empty block) can be represented. Alternatives
  considered: keep `Expr::Group` (rejected: loses fidelity and cannot represent
  `{ a; }`); `Block(Box<Expr>)` wrapping a `Seq` chain (rejected: duplicates the
  `Seq` operator's nesting, cannot represent `{}` or `{ a; }` without
  inventing unit nodes); a flat `items` list plus `has_trailing_semicolon` flag
  (rejected: admits the unrepresentable state "empty block with trailing
  semicolon"). Date/Author: 2026-10-18, planning agent; requires user approval.

- Decision: `{}` is an empty block (`BG-D4`). Rationale: upstream parity and
  the fact that real DDlog code writes `if (c) {} else { … }`; ddlint is
  unreleased. Alternative kept open for the approver: retain `{}` as an empty
  map, codify only non-empty blocks, and document `{}` as a ddlint-specific
  divergence. Choosing the alternative removes `BG-D6`, keeps
  `tests/expression_prefix.rs` unchanged for `{}`, and leaves upstream programs
  that use `{}` as empty bodies parsed incorrectly. Date/Author: 2026-10-18,
  planning agent; requires user approval.

- Decision: do not add a Verus or Kani lane for this item. Rationale: see
  `Verification plan`; the obligations are correspondence properties of the
  handwritten token-stream parser, and the one pure kernel is a finite
  classifier that exhaustive enumeration discharges completely. Date/Author:
  2026-10-18, planning agent.

- Decision: record an ADR (`docs/adr-004-block-expression-policy.md`).
  Rationale: the decision fixes a public AST variant and a normative grammar
  production just before the ADR-001 crate split, which meets the "substantive"
  bar in `AGENTS.md`; ADR-002 is the precedent for AST-shape decisions.
  Date/Author: 2026-10-18, planning agent.

## Outcomes & retrospective

Not started. Complete this section at each milestone boundary and at
completion, comparing the result against `Purpose / big picture`.

## Context and orientation

ddlint is a Rust linter for DDlog. Its parser lives under `src/parser/`. A
tokenizer (`src/tokenizer.rs`, using `logos`) produces `SyntaxKind` tokens
(`src/language.rs`). Span scanners under `src/parser/span_scanners/` find
top-level declarations, and a concrete syntax tree (CST, a lossless tree that
keeps every token) is built with `rowan`. Expressions are not stored as typed
CST nodes; instead an abstract syntax tree (AST, a typed tree that drops
punctuation) is built on demand by a handwritten Pratt parser (a
precedence-climbing parser driven by a binding-power table).

The files that matter for this item are:

- `src/parser/expression/pratt.rs`:
  `parse_expression(src) -> Result<Expr, Vec<Simple<SyntaxKind>>>`, the public
  expression entry point, plus `parse_expr(min_bp)`, `parse_infix`, and
  `parse_map_key_expr`, which parses with `:` acting as a terminator.
- `src/parser/expression/prefix.rs`: dispatches `T_LBRACE` to
  `parse_brace_or_map_literal`.
- `src/parser/expression/data_structures.rs` (310 lines):
  `parse_brace_or_map_literal`, `parse_map_entries`,
  `parse_brace_group_continuation`, `parse_ident_expression` (the
  struct-literal guard), and `parse_parenthesized_expr`.
- `src/parser/expression/control_flow.rs`: `if`, `match`, and `for`
  expression parsing; `parse_if_condition` activates the struct-literal guard.
- `src/parser/ast/precedence.rs`: binding powers; `BP_SEQ` is the lowest, and
  `;` is a left-associative infix `BinaryOp::Seq`.
- `src/parser/ast/expr.rs` (320 lines): the public `Expr` enum (24 variants,
  not `#[non_exhaustive]`), including `Group(Box<Expr>)`.
- `src/parser/ast/expr/sexpr.rs`: `Expr::to_sexpr()`, which renders
  `Group` as `(group …)`; tests use it for compact expectations.
- Consumers of `Expr::Group`: `src/sema/resolve.rs` (lines 72, 107, 146),
  `src/sema/variables.rs` (lines 66, 109),
  `src/parser/ast/rule/classification.rs`
  (`classify_for_body_with_aggregation_tracking`), and
  `src/parser/top_level_for/lowering.rs` (`collect_lowered_terms`, which
  currently treats a braced body as an unsupported head).
- `src/parser/error_messages.rs`: home of contract-bearing diagnostic message
  constants.
- `src/test_util/expressions.rs`: `test-support` feature helpers such as
  `var`, `call`, and `map_lit`.

Existing tests that pin the current behaviour include
`src/parser/tests/collections.rs` (`parses_brace_groups`,
`reports_collection_errors`), `tests/expression_prefix.rs` (`{1}`, `{}`,
`{ x }`, `{ return }`), `src/parser/tests/control_flow.rs`,
`src/parser/tests/expression/fixtures/valid/control_flow.rs`,
`tests/expression_compound.rs`, `src/parser/tests/rules/aggregations.rs`, and
`src/parser/ast/rule/tests.rs`
(`for_loop_with_sequence_body_flattens_all_statements`).

Upstream DDlog defines, in `src/Language/DifferentialDatalog/Parse.hs`:

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

Terms used in this plan: a "block" is a brace-delimited expression
`{ s1; …; sn; v }`; its "statements" are the items followed by `;`; its "value"
is the optional final item not followed by `;`; "unit" is the empty tuple `()`.

## Conformance basis

Upstream artefacts and identifiers:

- `docs/roadmap.md` item 2.6.8 (at commit `d1c2571`), referred to here as
  `RM-2.6.8`.
- `docs/parser-conformance-register.md` item 15 (`REG-15`) and its
  maintenance rules; this plan also creates item 16 (`REG-16`).
- `docs/differential-datalog-parser-syntax-spec-updated.md`: section 3.3
  (collections, `SPEC-3.3`), section 4 (precedence, `SPEC-4`), section 5.10
  (statements, `SPEC-5.10`), section 9 (diagnostics, `SPEC-9`), section 10 (AST
  crosswalk, `SPEC-10`), section 13 compatibility policy (`SPEC-13-COMPAT`:
  "Implementations MUST NOT silently accept constructs that are not part of
  this specification"), and section 14 change log.
- `docs/ddlint-design.md` section 2 (parsing pipeline contracts) and section
  2.4 (split parser crate architecture).
- `docs/adr-001-parser-crate-split.md` (the `Expr` surface moves into
  `ddlog-syntax`), `docs/adr-002-relation-role-kind-modelling.md` (precedent).
- Governing standards: `AGENTS.md`, `docs/documentation-style-guide.md`,
  `docs/developers-guide.md`.

No Terms of Reference document exists for ddlint; none is invented.

Trace links:

```plaintext
SPEC-13-COMPAT -> RM-2.6.8 -> REG-15 -> BG-D1 -> EP-M1 -> docs/adr-004-block-expression-policy.md
SPEC-13-COMPAT -> BG-D1..BG-D5 -> EP-M3 -> SPEC-3.3/SPEC-4/SPEC-9/SPEC-10 text + REG-15 implemented
BG-D2 -> EP-M3 -> tests::block_expressions::* + block_expression_proptest::*
BG-D3 -> EP-M3 -> V-B2, V-B3 -> tests/features/block_expressions.feature
BG-D4 -> EP-M3 -> V-D1 -> tests::expression_prefix::parses_prefix_forms ({} case)
BG-D5 -> EP-M3 -> V-D1 -> block::tests::classifies_every_follow_token
BG-D6 -> EP-M3 -> tests::expression_compound (if c {} else {d})
BG-D8 -> EP-M3 -> tests/top_level_for_desugaring.rs braced-body case
BG-D9 -> EP-M1 -> REG-16 (decision needed)
RM-2.6.8 -> EP-M4 -> docs/roadmap.md 2.6.8 marked done
```

## Verification plan

### Abstract model

The specification of a block is independent of ddlint's token stream. An
abstract block is a finite sequence of expressions `items = [e1, …, en]`
(`n ≥ 0`) together with a flag `t` ("ends in `;`"), subject to the
well-formedness condition `t ⇒ n ≥ 1` (a lone `;` is not a block). Its value is
`en` when `n ≥ 1` and `t` is false, and unit otherwise. Its concrete syntax is
`{` followed by `e1; e2; …; en`, followed by `;` when `t` holds, followed by
`}`.

The representation is `BlockExpr { statements, value }`. The representation
relation `R` maps an abstract block to a `BlockExpr`:

- if `t` is false and `n ≥ 1`: `statements = [e1, …, e(n-1)]` and
  `value = Some(en)`;
- otherwise: `statements = [e1, …, en]` and `value = None`.

### Invariants

- `V-I1` (no bare sequence items): no element of `statements`, and not
  `value`, is an `Expr::Binary { op: BinaryOp::Seq, .. }` at its root. A
  parenthesized sequence `{ (a; b) }` is allowed because its root is
  `Expr::Group`.
- `V-I2` (order preservation): the left-to-right source order of block items
  equals the order of `statements` followed by `value`.
- `V-I3` (brace/parenthesis separation): the parser never produces
  `Expr::Group` for a brace-delimited form and never produces `Expr::Block` for
  a parenthesized form.
- `V-I4` (semantic transparency): for any expression `e`, the semantic
  helpers (`sema` relation and variable collection, rule-body classification)
  return the same results for `{ e }` as for `( e )` and for `e`.
- `V-I5` (totality without panic): for any token sequence that begins with
  `{`, `parse_expression` returns `Ok` or `Err` without panicking, and every
  reported error span lies within the input.

### Lemmas (proof sketch)

These are hypotheses about the proof structure; implementation may refine or
split them.

- `V-L1` (representation is a bijection): `R` is a bijection between
  well-formed abstract blocks and `BlockExpr` values. Sketch: by cases on
  `value`. If `value = Some(v)`, the unique preimage is
  `(statements ++ [v], false)`, which is well-formed because its length is at
  least one. If `value = None` and `statements` is empty, the unique preimage is
  `([], false)`. If `value = None` and `statements` is non-empty, the unique
  preimage is `(statements, true)`, which is well-formed. Injectivity follows
  because the three cases have disjoint images and each case's map is injective
  on lists. Consequence: `BlockExpr` has no junk states, so no runtime
  validation of it is needed.
- `V-L2` (value semantics is preserved): `BlockExpr::value()` returns the
  abstract value under `R`. Sketch: immediate from the case split in `V-L1`.
- `V-L3` (parse correspondence): for item sources `s1, …, sn`, each a valid
  expression containing no top-level `;`, `,`, or `:`, and a flag `t` with
  `t ⇒ n ≥ 1`, parsing the concrete syntax of `([s1, …, sn], t)` yields
  `Expr::Block(R([parse(s1), …, parse(sn)], t))`. Sketch: induction on `n`,
  using that items are parsed at binding power `BP_SEQ + 1`, so each item's
  parse stops exactly at the next `;` or `}` (this is where `V-I1` comes from),
  and that the separator loop consumes exactly one `;` per item.
- `V-L4` (disambiguation is total and deterministic): after `{` and a first
  item, the next token class determines exactly one outcome: `:` selects map,
  `;` or `}` selects block, `,` selects the separator diagnostic, end of input
  selects the unclosed-brace diagnostic, and any other token selects the
  unexpected-token diagnostic. `{` followed directly by `}` selects the empty
  block. Sketch: case analysis over a finite set.
- `V-L5` (transparency): the semantic helpers treat `Expr::Block` as the
  sequential composition of its items and `Expr::Group` as its inner
  expression; for a single-value block this reduces to the inner expression.
  Sketch: structural induction over `Expr`, one case per consumer.

### Axioms

- `V-A1`: `logos` tokenizes `{`, `}`, `;`, `,`, and `:` as `T_LBRACE`,
  `T_RBRACE`, `T_SEMI`, `T_COMMA`, and `T_COLON` respectively, and existing
  tokenizer tests already cover this.
- `V-A2`: `chumsky::error::Simple::custom` stores the span and message given
  to it unchanged.
- `V-A3`: `proptest` explores generated inputs as documented and shrinks
  failures deterministically from its seed file.
- `V-A4`: upstream DDlog semantics are as stated in its tutorial's "Control
  flow" section and `Parse.hs`; ddlint does not execute DDlog, so this axiom
  only fixes the intended meaning of `value`.

### Obligations and methods

`V-L1` and `V-L2`:

- Method: type-level construction plus parameterized unit tests.
- Rationale: the bijection is enforced by the shape of `BlockExpr` (no flag
  field exists to contradict `value`); the remaining executable content is
  `BlockExpr::value()`, a two-arm match.
- Domain: the three cases (empty, value present, trailing `;`).
- Artefact: `src/parser/ast/expr/block.rs` unit tests.
- Evidence: `cargo test --all-features block_value` passes.
- Non-vacuity: one witness per case; seeded fault "return the last statement
  when `value` is `None`" must fail the trailing-`;` case.

`V-L3`, `V-I1`, `V-I2`, `V-I3`:

- Method: `proptest` property tests over generated item sources, plus named
  `rstest` cases for the boundaries.
- Rationale: the obligation ranges over unbounded item counts and nested
  expressions; sampling with shrinking against the real parser gives evidence
  about the executable code, which a proof over a model would not.
- Domain: `n` from 0 to 6, `t` drawn independently (with `n = 0` forcing
  `t = false`), item sources from a bounded recursive generator of identifiers,
  integer literals, `+`, `and`, calls, tuples, parenthesized sequences, and
  nested blocks up to depth 3.
- Artefact: `src/parser/tests/block_expression_proptest.rs`.
- Evidence: `cargo test --all-features block_expression_proptest` passes with
  256 cases per property; red stage observed before the parser change because
  braces still produce `Expr::Group`.
- Non-vacuity: the generator is built with `prop_oneof!` so every class
  (`n = 0`, `n = 1` with and without `t`, `n ≥ 2`, nested depth ≥ 2,
  parenthesized sequence item) is a distinct weighted branch, never a filter; a
  companion `rstest` table gives one deterministic witness per class. Seeded
  faults that must each fail a property: reversing `statements`; dropping the
  final `;` flag (returning `value = Some(last)` for `{ a; }`); parsing items
  at binding power 0 (violates `V-I1`); returning `Expr::Group` for `{ e }`
  (violates `V-I3`).

`V-L4`:

- Method: exhaustive enumeration.
- Rationale: the follow-token domain is finite. The classifier is extracted as
  a pure function `classify_brace_follow(Option<SyntaxKind>) -> BraceFollow`,
  and a test iterates every `SyntaxKind` value (via `FromPrimitive` over the
  `u16` range) plus end of input, asserting the expected class. This is a
  complete check of the kernel, not a sample.
- Artefact: unit test `classifies_every_follow_token` in
  `src/parser/expression/block.rs`, plus an `rstest` table of whole-input cases
  in `src/parser/tests/block_expressions.rs` that exercises each class through
  the real parser.
- Evidence: `cargo test --all-features classifies_every_follow_token`.
- Non-vacuity: the test asserts that each `BraceFollow` variant is produced by
  at least one `SyntaxKind`; seeded fault "map `T_COMMA` to block" must fail.

`V-L5` and `V-I4`:

- Method: `proptest` metamorphic property plus named regression tests.
- Rationale: the property relates three inputs (`e`, `( e )`, `{ e }`) across
  several consumers; a metamorphic property catches a consumer that drops
  `Expr::Block` through a wildcard arm.
- Domain: rule bodies of the form `R(x) :- S(x), <wrapped e>.` where `e` is
  drawn from atoms, conditions, and assignments.
- Artefact: `tests/block_expression_semantics.rs`.
- Evidence: semantic relation references and variable sets are equal for all
  three wrappings.
- Non-vacuity: seeded fault "remove the `Expr::Block` arm from
  `src/sema/resolve.rs`" must fail.

`V-I5`:

- Method: `proptest` panic-freedom property over token-alphabet strings.
- Domain: strings of length 1 to 16 over `{`, `}`, `;`, `,`, `:`, `a`, `1`,
  `+`, `(`, `)`, starting with `{`.
- Artefact: `src/parser/tests/block_expression_proptest.rs`.
- Non-vacuity: the alphabet guarantees unbalanced, doubled-separator, and
  mixed map/block inputs; the property also asserts that at least the
  well-formed cases `{}` and `{a}` round-trip, so a parser that always errors
  fails.

### Where Verus and Kani are and are not warranted

No Verus proof is planned. The lemmas above are about the correspondence
between a grammar and ddlint's handwritten, `Vec`-backed token-stream parser,
which calls into `chumsky` error types and `rowan`-adjacent helpers. Verifying
that code in Verus would require re-implementing the parser in Verus's
supported subset, so a proof would establish a property of a model rather than
of the shipped code. The only pure kernels are `V-L1`/`V-L2`, which the type
definition enforces structurally, and `V-L4`, whose domain is finite and is
discharged by exhaustive enumeration. Neither is contractual business logic.
Introducing a Verus toolchain lane for these would add infrastructure without
adding assurance. Kani is likewise not warranted: there is no `unsafe` code,
arithmetic, or bounded state machine here. If later work (for example
scope-aware `var` handling inside blocks) introduces a scope-resolution
invariant over unbounded nesting, that is the point at which a Verus model
should be reconsidered; the ADR records this.

Residual gap: property tests sample `V-L3`; they do not prove it for every
input. The bounded generator, explicit class witnesses, and seeded-fault
evidence are the mitigation.

## Plan of work

### Stage A: understand and propose (complete when the plan is approved)

No code changes. This plan is the output.

### Stage B: decision record (EP-M1)

Write `docs/adr-004-block-expression-policy.md` using the template in
`docs/documentation-style-guide.md` ("Architectural decision records" section).
Status `Accepted` (the plan approval is the acceptance), with context
(`SPEC-13-COMPAT`, upstream `braces eseq`, register item 15), the decision
`BG-D1` to `BG-D10`, the options considered (remove with migration diagnostics;
keep `Expr::Group`; `Block(Box<Expr>)`; flat list with flag; `{}` as empty
map), consequences, and the Verus note. Add the ADR to `docs/contents.md` (and
add the missing ADR-003 entry while there, as it is a one-line index fix).
Reference ADR-004 from `docs/ddlint-design.md` section 2 next to the 2.6.7
paragraph.

In `docs/parser-conformance-register.md`, rewrite item 15's "Spec/target
behaviour" to state the decision and leave its status `scheduled` until EP-M3
lands. Add item 16, "Sequence operator outside braces", with status
`decision needed`, describing `( a; b )` and unbraced `for` bodies, and no
roadmap item.

### Stage C: behavioural harness (EP-M2)

Add to `[dev-dependencies]` in `Cargo.toml`: `rstest-bdd = "0.6.0"`,
`rstest-bdd-macros = "0.6.0"`, `googletest = "0.14.3"`, and
`pretty_assertions = "1.4.1"`. Create `tests/features/` and
`tests/block_expressions_bdd.rs` with a module comment, a `ScenarioState`
holding `source: Slot<String>` and `outcome: Slot<Result<Expr, Vec<String>>>`,
and step definitions. Bind one scenario, "A map literal parses as a map", which
passes against current behaviour. Confirm whether `#[scenario]` rebuilds on
feature-file changes without a `build.rs`; if it does not, record the finding
and escalate rather than adding `build.rs`.

### Stage D: red tests, then implementation (EP-M3)

Red: add, without production changes, the tests listed in
`Validation and acceptance` (unit tables, the classifier test with a stub that
returns a placeholder, property tests, BDD scenarios, insta snapshot matrix,
semantic metamorphic test, top-level `for` braced-body pin). Because
`Expr::Block` does not exist yet, the red step first adds the type and variant
with `todo!()`-free stubs: `BlockExpr` with its fields and accessors, the
`Expr::Block` variant, a `(block …)` S-expression arm, and consumer arms that
compile. The parser still returns `Expr::Group`, so the new tests fail for the
intended reason ("expected Block, found Group" or "expected block, found map for
`{}`"). Record the failing output.

Green: create `src/parser/expression/block.rs` containing
`classify_brace_follow`, `BraceFollow`, `parse_block_or_map_literal` (replacing
`parse_brace_or_map_literal`), `parse_block_items`, and `finish_block`. Items
are parsed with `parse_expr(BP_SEQ + 1)` inside
`with_struct_literals_suspended`. The first item is parsed with
`parse_map_key_expr(BP_SEQ + 1)` so that `:` still selects a map; when the
follow token is not `:`, continue the first item with
`parse_infix(first, BP_SEQ + 1)` so ascription resumes, as
`parse_brace_group_continuation` does today. Move `parse_map_entries` into the
same module. Update `prefix.rs` dispatch. Remove the `T_RBRACE` trigger from
the struct-literal guard in `parse_ident_expression` (`BG-D6`). Add message
constants to `src/parser/error_messages.rs`: `BLOCK_OR_MAP_SEPARATOR_ERROR`
(`expected ':' for a map literal, or ';' or '}' for a block`) and
`EMPTY_BLOCK_ITEM_ERROR` (`empty statement in block: remove the extra ';'`).
Update consumers: `src/sema/resolve.rs` and `src/sema/variables.rs` traverse
`statements` then `value`; `classify_for_body_with_aggregation_tracking`
flattens `statements` then `value` in order; `collect_lowered_terms` in
`src/parser/top_level_for/lowering.rs` keeps returning the block as an
unsupported head (no behaviour change, `BG-D8`). Add a `block` helper to
`src/test_util/expressions.rs`. Update existing tests whose expectations change
(listed in `Validation and acceptance`).

Then align the specification and register in the same milestone, because the
register's maintenance rules require it: add section 3.4 "Block expressions"
with the grammar below, a note under section 3.3 that map literals need at
least one entry, a sentence under section 4 that `;` inside braces is a
separator, the two diagnostics under section 9, `EBlock` under section 10's
`Expr` crosswalk, and a change-log bullet in section 14. Set register item 15 to
`implemented`.

```ebnf
BlockExpr  ::= '{' BlockBody? '}'
BlockBody  ::= Expr (';' Expr)* ';'?
```

Disambiguation text for section 3.4: "After `{`, the first item is parsed with
`:` as a terminator. A following `:` makes the form a map literal (section 3.3);
`;` or `}` makes it a block; `,` is an error. Therefore `{ x: T }` is a map
literal, and a block whose value is an ascription is written `{ (x: T) }`. `{}`
is the empty block, whose value is `()`."

Refactor: keep each new function under the repository's complexity
expectations, confirm no file exceeds 400 lines, and rerun the focused tests
and all gates.

### Stage E: documentation and close-out (EP-M4)

Update `docs/parser-implementation-notes.md` (the struct-literal guard
paragraph currently calls brace groups "an implementation extension"; replace
with a pointer to section 3.4 and ADR-004, and add `block.rs` to the file
index), `docs/developers-guide.md` (parser module structure entry for
`src/parser/expression/block.rs`, the rule that blocks and groups are distinct
and consumers must handle both, and the new BDD harness location and
conventions), `docs/users-guide.md` (a "Block expressions" section with
examples and the `{}` and `{ (x: T) }` notes), `CHANGELOG.md` (breaking change:
`{}` is an empty block; added: block expressions), and mark `docs/roadmap.md`
item 2.6.8 done. Set this plan's status to `COMPLETE` after reconciling
`Outcomes & retrospective`.

## Milestones and plateaus

### EP-M1: decision recorded

- Outcome: ADR-004 exists and is indexed; the design document references it;
  register item 15 states the decision (still `scheduled`); register item 16
  exists.
- Requirements: `RM-2.6.8` (decision half), `REG-15`, `BG-D9`.
- Acceptance evidence: `make markdownlint`, `make nixie`, and
  `make check-fmt` pass; ADR-004 renders with the required sections.
- Conformance check:
  - Decision text matches `BG-D1` to `BG-D10` exactly.
  - No code changed.
  - Upstream assumption (`braces eseq`) still cited with source.
  - No public interface or dependency change.
  - No persisted-format change.
  - Trace links in this plan updated with the ADR path.
- Recovery: documentation-only; revert the commit to retry.
- Remaining gaps: the code still behaves as before.
- Compatibility decision: none.

### EP-M2: behavioural harness

- Outcome: `rstest-bdd`, `rstest-bdd-macros`, `googletest`, and
  `pretty_assertions` are development dependencies; one BDD scenario runs green
  under `make test`.
- Requirements: test-strategy requirement for `rstest-bdd`, `googletest`,
  and `pretty_assertions`.
- Acceptance evidence: `make test` output includes the
  `a_map_literal_parses_as_a_map` scenario passing; `make lint` passes.
- Conformance check:
  - Only the four named dependencies added.
  - No `build.rs` added.
  - No production code changed.
  - Trace links current.
- Recovery: revert the `Cargo.toml`, `Cargo.lock`, and test-file commit.
- Remaining gaps: no block behaviour yet.
- Compatibility decision: none.

### EP-M3: block expression implemented

- Outcome: `Expr::Block(BlockExpr)` is produced for every brace-delimited
  non-map form; `{}` is an empty block; diagnostics are constants; consumers
  handle blocks; specification sections 3.3, 3.4, 4, 9, 10, and 14 and register
  item 15 (`implemented`) describe the behaviour.
- Requirements: `BG-D1` to `BG-D6`, `BG-D8`, `SPEC-13-COMPAT`, `REG-15`.
- Acceptance evidence: every test in `Validation and acceptance` passes;
  seeded-fault evidence recorded in `Artefacts and notes`.
- Conformance check:
  - All of `V-L1` to `V-L5` and `V-I1` to `V-I5` have passing artefacts.
  - Parenthesized forms, non-empty maps, reserved tokens, and top-level `for`
    behave as before.
  - Only the planned public interface change occurred.
  - No dependency change beyond EP-M2.
  - Register and specification updated in the same milestone.
- Recovery: each step is its own commit (red, green, refactor, spec); revert
  to the last green commit to retry.
- Remaining gaps: guides, changelog, roadmap.
- Compatibility decision: none. `Expr` is a pre-1.0, unreleased API (version
  0.1.0, no tags), so callers are updated in place; no alias for the removed
  brace-to-`Group` behaviour is added.

### EP-M4: documentation closed out

- Outcome: users' guide, developers' guide, implementation notes, changelog,
  and roadmap agree with the specification; plan marked `COMPLETE`.
- Requirements: `RM-2.6.8` done.
- Acceptance evidence: all gates pass; `docs/roadmap.md` shows
  `- [x] 2.6.8.`.
- Conformance check:
  - Every upstream artefact in `Conformance basis` reconciled.
  - No unrecorded deviation.
- Recovery: documentation-only.
- Remaining gaps: register item 16 (sequence operator outside braces) and
  `var` declarations in blocks remain open by design.
- Compatibility decision: none.

## Concrete steps

All commands run from the repository root,
`/home/leynos/.lody/repos/github---leynos---ddlint/worktrees/6df52f2c-b43f-48ab-82f8-16a28331c74b`
or any checkout of the branch. Gates run sequentially, never in parallel, and
each is captured with `tee`:

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
cargo test --all-features --lib block_expressions 2>&1 | tee /tmp/red-block-ddlint-${BRANCH}.out
cargo test --all-features --lib block_expression_proptest
cargo test --all-features --lib classifies_every_follow_token
cargo test --all-features --test block_expressions_bdd
cargo test --all-features --test block_expression_semantics
cargo test --all-features --test expression_prefix
```

Expected red output (abridged) before the parser change:

```plaintext
---- parser::tests::block_expressions::parses_blocks::case_1_single_value stdout ----
assertion `left == right` failed
  left: Group(Variable("x"))
 right: Block(BlockExpr { statements: [], value: Some(Variable("x")) })
```

Expected green output:

```plaintext
test result: ok. N passed; 0 failed; 0 ignored
```

Commit after each green step using the repository's commit conventions
(imperative subject of 50 characters or fewer, wrapped body).

## Validation and acceptance

Behavioural specification, `tests/features/block_expressions.feature`:

```gherkin
Feature: Block expressions
  Brace-delimited expressions are blocks whose value is their final item
  unless the block is empty or ends with a semicolon.

  Scenario: A map literal parses as a map
    Given the expression source "{a: 1, b: 2}"
    When the expression is parsed
    Then the result is a map literal with 2 entries

  Scenario: A single expression in braces is a block value
    Given the expression source "{ x + 1 }"
    When the expression is parsed
    Then the result is a block with 0 statements and a value

  Scenario: Semicolons separate block statements in order
    Given the expression source "{ a; b; c }"
    When the expression is parsed
    Then the result is a block with statements "a, b" and value "c"

  Scenario: A trailing semicolon makes the block value unit
    Given the expression source "{ a; }"
    When the expression is parsed
    Then the result is a block with statements "a" and no value

  Scenario: Empty braces are an empty block
    Given the expression source "{}"
    When the expression is parsed
    Then the result is a block with 0 statements and no value

  Scenario: Braces and parentheses stay distinct
    Given the expression source "( x )"
    When the expression is parsed
    Then the result is a parenthesized group

  Scenario: A comma without a colon is rejected
    Given the expression source "{ a, b }"
    When the expression is parsed
    Then parsing fails with "expected ':' for a map literal, or ';' or '}' for a block"

  Scenario: An empty statement is rejected
    Given the expression source "{ a;; b }"
    When the expression is parsed
    Then parsing fails with "empty statement in block: remove the extra ';'"

  Scenario: Braced if branches with an empty block parse
    Given the expression source "if ready {} else { go() }"
    When the expression is parsed
    Then the result is an if expression whose then branch is an empty block
```

Unit tables in `src/parser/tests/block_expressions.rs` (new, `rstest` with
`googletest` matchers and `pretty_assertions`):

- Accepted: `{ x }`, `{ 1 }`, `{ f() }`, `{ (x, y) }`, `{ 1 + 2 }`,
  `{ a and b }` (the existing `parses_brace_groups` cases, now expecting
  `Block`); `{ a; b }`; `{ a; b; }`; `{ a; }`; `{}`; `{ { a } }`; `{ (a; b) }`;
  `{ (x: T) }`; `{ return }`; `{ break }`; `{ continue }`; `{ x = 1; x }`;
  `{ if c { a } else { b } }`.
- Map literals unchanged: `{a: 1}`, `{a and b: 1}`, `{a: 1,}`,
  `{{x: 1}: {y: 2}}`; `{{}: 1}` now has an empty-block key.
- Rejected with exact messages: `{ a, b }` (separator), `{ ; }` and
  `{ a;; b }` (empty statement), `{ a` (unclosed: existing `expected T_RBRACE`
  message), `{ var x = 1; x }` (existing unexpected-token message, `BG-D10`).

Existing tests whose expectations change, and only these:

- `src/parser/tests/collections.rs`: `parses_brace_groups` (now `Block`),
  the `{}` empty-map case (now empty block), `{{}: 1}`, and the
  `map_comma_without_colon` message.
- `tests/expression_prefix.rs`: `{1}`, `{}`, `{ x }`, `{ return }`.
- `src/parser/tests/control_flow.rs`: `{ break }`, `{ continue }`,
  `{ return }`.
- `src/parser/tests/expression/fixtures/valid/control_flow.rs` and
  `tests/expression_compound.rs`: braced branches become `Block`.
- `src/parser/tests/rules/aggregations.rs`: the `Expr::Group(inner)` match on
  a braced body.
- `src/parser/ast/rule/tests.rs`:
  `for_loop_with_sequence_body_flattens_all_statements` keeps its three-term
  result through the new block flattening.

Snapshot matrix: `src/parser/tests/block_expression_snapshots.rs` uses
`insta::assert_snapshot!` with inline snapshots over a table of 16 inputs (each
accepted and rejected form above), rendering either `to_sexpr()` or the sorted
error messages, so the S-expression format for `(block …)` and every diagnostic
is reviewed in one place. Rendering contract: `{ a; b }` renders as
`(block (stmts a) b)`, `{ a; }` as `(block (stmts a))`, and `{}` as `(block)`;
the exact text is fixed by the first approved snapshot.

End-to-end: `tests/block_expressions.rs` parses a whole program through
`ddlint::parse`:

```plaintext
input relation Items(x: u32)
output relation Out(x: u32)
Out(x) :- Items(x), for (y in Items(y)) { Check(y); Check(x) }.
```

It asserts no parse errors, that rule-body classification yields the iterable,
both `Check` terms in order, and that the linter `Runner` reports the same
diagnostics as for the parenthesized `( Check(y); Check(x) )` variant. A second
case asserts `for (x in Items(x)) { Out(x) }.` at top level still reports the
existing unsupported-head diagnostic (`BG-D8`).

Red-Green-Refactor evidence to record in `Artefacts and notes`:

- Red: the focused commands above fail with `Group` where `Block` is
  expected, and with `MapLit([])` where an empty block is expected.
- Green: the same commands pass after `block.rs` lands.
- Refactor: the same commands, then all six gates, pass.

Quality criteria:

- Tests: `make test` passes, including every new suite above.
- Verification: `V-L1` to `V-L5` and `V-I1` to `V-I5` discharged as in
  `Verification plan`, with seeded-fault evidence recorded; no Verus or Kani
  command applies to this item.
- Lint and formatting: `make check-fmt`, `make lint`, `make markdownlint`,
  and `make nixie` pass.

## Idempotence and recovery

Every step is a file edit followed by a gate run; rerunning a gate is safe.
Seeded faults for non-vacuity evidence are applied in the working tree, run
once, recorded, and reverted with `git restore`; they are never committed. If a
milestone fails partway, reset to the last green commit on the branch
(`git reset --hard <sha>` only after confirming no uncommitted work is needed)
and repeat the milestone.

## Artefacts and notes

Record here, during implementation: the red transcripts, the seeded-fault
transcripts (one per fault in `Verification plan`), the approved snapshot
texts, and the gate log paths.

## Interfaces and dependencies

In `src/parser/ast/expr/block.rs` (new, re-exported from `ddlint::parser::ast`):

```rust
/// Brace-delimited block expression `{ s1; …; sn; v }`.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    /// Items followed by `;`, in source order.
    pub statements: Vec<Expr>,
    /// Final item not followed by `;`; `None` means the block yields `()`.
    pub value: Option<Box<Expr>>,
}

impl BlockExpr {
    /// The expression the block evaluates to, or `None` for unit.
    #[must_use]
    pub fn value(&self) -> Option<&Expr> { self.value.as_deref() }

    /// Items in source order: statements followed by the value, if any.
    pub fn items(&self) -> impl Iterator<Item = &Expr> { … }
}
```

In `src/parser/ast/expr.rs`, add `Block(BlockExpr)` to `Expr` with the doc
comment "Brace-delimited block expression; see `BlockExpr`.", and change the
`Group` doc comment to "Parenthesized expression `( e )`.".

In `src/parser/expression/block.rs` (new, private to the expression parser):

```rust
/// Classification of the token after the first item in braces.
pub(super) enum BraceFollow { Map, BlockSeparator, BlockEnd, MissingColon, Unclosed, Unexpected }

pub(super) fn classify_brace_follow(kind: Option<SyntaxKind>) -> BraceFollow;
```

Development dependencies (EP-M2): `rstest-bdd = "0.6.0"`,
`rstest-bdd-macros = "0.6.0"`, `googletest = "0.14.3"`,
`pretty_assertions = "1.4.1"`, all caret requirements. Combine `googletest` with
`rstest` by placing `#[rstest]` before `#[gtest]` on each test.

## Related documentation and skills

Read before implementing:

- `docs/differential-datalog-parser-syntax-spec-updated.md` (sections 3.3, 4,
  5.10, 9, 10, 13, 14).
- `docs/parser-implementation-notes.md` (Pratt parser, struct-literal guard,
  feature-specific diagnostic messages).
- `docs/parser-conformance-register.md` (items 14, 15, maintenance rules).
- `docs/building-an-error-recovering-parser-with-chumsky.md` (recovery at
  `}` and `;` boundaries, returning `Option` to keep parsing).
- `docs/rust-parser-testing-comprehensive-guide.md` (sections 3.2 snapshots,
  3.3 error-recovery testing, 3.4 Pratt parser tables, 5.2 fuzzing).
- `docs/rust-testing-with-rstest-fixtures.md` (sections IV.A `#[case]`
  tables, V.A fixture composition).
- `docs/ddlint-design.md` (section 2 parsing pipeline, section 2.4 crate
  split).
- `docs/complexity-antipatterns-and-refactoring-strategies.md` (section 3
  "bumpy road", section 5.A command/query separation).
- `docs/documentation-style-guide.md` (ADR template).
- `docs/execplans/2-6-7-finalize-legacy-token-compatibility-policy.md`
  (precedent for diagnostics routing and fixture typing).

Skills: `execplans` (this document), `rust-router` then `rust-unit-testing`
(rstest, googletest, pretty_assertions, insta), `proptest` (generators and
seeded faults), `rust-types-and-apis` (`BlockExpr` shape),
`arch-decision-records` (ADR-004), `hexagonal-architecture` (keeping the parser
a pure core with no new ports), `codegraph-mcp` (finding `Expr::Group`
consumers before editing), `rust-verification` (if the Verus decision is
revisited), and `en-gb-oxendict-style` (documentation).

## Revision note

2026-10-18: initial draft, written after Wyvern reconnaissance of the parser,
documentation, tooling, and upstream DDlog grammar. Awaiting expert review and
user approval.
