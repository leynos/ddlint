# Align `;` sequencing and rule-level `for` with upstream (roadmap 2.6.10)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (awaiting expert review and user approval)

Branch: `2-6-10-decide-semicolon-sequencing`

Roadmap item: `2.6.10` in `docs/roadmap.md`.

Implementation must not begin until the user explicitly approves this plan.
Decisions marked "requires user approval" in `Decision log` are approved with
the plan unless the approver says otherwise.

## Purpose / big picture

ddlint is a linter for Differential Datalog (DDlog). Its parser follows a
normative syntax specification,
`docs/differential-datalog-parser-syntax-spec-updated.md`, which in turn
follows the upstream DDlog compiler (`vmware-archive/differential-datalog`).
Roadmap item 2.6.10 asks how three ddlint constructs should align with
upstream. Reading the upstream parser at commit
`ca5ded823dc2c01bc33aac44e526c12d2fed03a6` (the commit ADR-0006 pinned) shows
four divergences, not three:

1. **`;` outside braces.** ddlint parses `;` as an infix operator,
   `BinaryOp::Seq`, in every expression position, so `(a; b)`, `f(a; b)`, and
   the rule-body literal `A(x); B(x)` all parse. Upstream has no `;` operator:
   the `etable` entry is commented out (`Parse.hs` line 885), and `;` exists
   only as the item separator of a brace block (`eseq`, lines 600-611).
2. **Control-flow tails.** ddlint parses an `if` condition, both `if`
   branches, and a `for` body as full expressions, so in
   `if (c) { a } else { b } ++ x` the `++ x` attaches to the `else` branch.
   Upstream parses each of them as a *term*: a primary expression without unary
   operators, calls, field access, or infix operators (`eite` and `efor`, lines
   799-813). The `++ x` therefore applies to the whole `if`.
3. **Rule-body `for`.** ddlint classifies `R(x) :- for (y in Items(y)) Out(y).`
   as a relational loop, `RuleBodyTerm::ForLoop`, and
   `Rule::flattened_body_terms()` turns it into the join `Items(y), Out(y)`.
   Upstream has no such construct: a `for` in a rule body is an ordinary
   expression clause over a collection (a filter condition, `rhsCondition`,
   lines 429-437) that declares nothing for later clauses.
4. **Top-level `for`.** ddlint accepts `for (a in A(a) if g) Head(a).` with a
   pattern before `in`, an arbitrary iterable, a terminating `.`, and an
   atom-only body. Upstream parses a *statement* (`parseForStatement` and
   `statement`, lines 364-397): the header is a relation atom with an optional
   `v in` binder (`atom`, lines 480-499), there is no terminating `.`, and the
   body is one of seven statement forms (`for`, `if`, `match`, `skip`, a
   `{ s; s }` block, `expr in s`, and a head atom). `Statement.hs`
   (`convertStatement`, lines 129-169) lowers a statement into zero or more
   rules. Upstream's grammar accepts `for (a in A(a)) …`; its validator then
   rejects it because `a` is declared twice in one atom (`Validate.hs` line
   402).

ddlint is unreleased, so no compatibility with earlier ddlint behaviour is
owed. This plan aligns all four with upstream. A small worked example shows the
observable difference. Today:

```plaintext
parse_expression("(a; b)")                    -> Ok(Group(Binary(Seq, a, b)))
parse_expression("if (c) { a } else { b } ++ x") -> IfElse(c, {a}, Binary(++, {b}, x))
parse("R(x) :- for (y in Items(y)) Out(y).")  -> body_terms: [ForLoop(..)]
parse("for (a in A(a)) B(a).")                -> semantic_rules: [B(a) :- A(a)]
```

After this plan:

```plaintext
parse_expression("(a; b)")
  -> Err("`;` separates items only inside a block; wrap the sequence in `{ … }`")
parse_expression("if (c) { a } else { b } ++ x") -> Binary(++, IfElse(c, {a}, {b}), x)
parse("R(x) :- for (y in ys) { f(y) }.")      -> body_terms: [Expression(ForLoop(..))]
parse("for (A(a)) B(a)")                      -> semantic_rules: [B(a) :- A(a)]
parse("for (r in A(a) if a > 0) { if (ok(r)) B(a) else skip; C(a) }")
  -> semantic_rules: [B(a) :- r in A(a), a > 0, ok(r);
                      C(a) :- r in A(a), a > 0]
```

Success is observable by running `make test` and seeing the new sequencing,
control-flow, rule-body, and top-level statement suites pass. Register item 16
then reads `implemented`, register item 8 drops its "known divergence"
paragraph, and roadmap item 2.6.10 is ticked.

## Constraints

- Implementation begins only after the user approves this plan.
- Do not change the meaning of any `DiagnosticCode` (`D-REL-001` to
  `D-REL-008`) or `DiagnosticCategory`; the conformance register freezes them.
  New messages are constants in `src/parser/error_messages.rs` (or the owning
  module) under existing categories; stable codes wait for roadmap 2.8.1.
- Do not change block expressions (ADR-0006): `{}` is the empty block, `{ e }`
  is a block whose value is `e`, an expression block accepts a trailing `;`, and
  `Expr::Group` is reserved for parentheses.
- Do not change operator binding powers or associativity other than deleting
  `;`. The upstream operator table differs from ddlint's in other ways; those
  differences are recorded (`SF-D11`), not changed here.
- Do not change struct-literal or struct-pattern syntax (roadmap 2.6.11),
  `var`, closures, interpolation, or type grammar (2.6.12), function bodies
  (2.6.13), `x @ pat` or rule-body `v in R(…)` binders (2.6.14), or the
  lexical-scope model beyond what `SF-D3` requires (3.3.5).
- Keep the parser a pure core: no input/output, logging, metrics, or global
  state; no new port or adapter. Lowering stays a pure function from a parsed
  statement to semantic rules.
- Keep the concrete syntax tree (CST) lossless: every source token still
  appears in the green tree.
- New production code must not panic (`expect`, `unwrap`, `unreachable!`,
  `todo!`, unchecked slice indexing).
- No Rust source file over 400 lines. Every module, including test modules,
  has a `//!` comment. Every `assert!`, `assert_eq!`, and `assert_ne!` has a
  diagnostic message.
- Use the shared Cargo cache and never bypass `RUSTC_WRAPPER` compile
  admission.
- Every commit passes `make check-fmt`, `make lint`, and `make test` (plus
  `make markdownlint` and `make nixie` when Markdown changes). Red tests are
  observed and recorded, never committed alone.
- Do not edit completed execplans (for example
  `docs/execplans/2-6-8-decide-brace-group-extension-policy.md` and
  `docs/execplans/2-5-4-top-level-for-desugaring.md`) or the text of completed
  roadmap items.
- Documentation follows `docs/documentation-style-guide.md`, en-GB Oxford
  spelling, and no first-person prose outside README and feature files.
  `typos.toml` is generated; exceptions go in `typos.local.toml`.
- No migration guidance or backwards-compatibility framing: ddlint is
  pre-release. Diagnostics describe upstream syntax, not earlier ddlint
  behaviour.

## Tolerances (exception triggers)

Base: `git merge-base HEAD origin/main` (at drafting, `7adbd8bc`). Count with:

```sh
git diff --numstat "$(git merge-base HEAD origin/main)" -- . ':!docs/execplans/'
```

Classify whole files by path. Code is `src/`, `tests/`, `cli/`, and `crates/`
(production source, tests, fixtures, and generators). Configuration is
`Cargo.toml`, `Cargo.lock`, `Makefile`, `.github/`, and `typos.local.toml`.
Documentation is `docs/` (excluding `docs/execplans/`) and other `*.md` files.
Limits are net added lines (additions minus deletions) unless stated. This
ExecPlan is excluded from every count. Each category is enforced on its own;
spare capacity in one category does not offset another.

- Code: stop and escalate above 55 files or 2,400 net added lines.
- Configuration: stop on any change except narrow `typos.local.toml`
  exceptions.
- Documentation: stop above 12 files or 1,000 net added lines.
- Interface: the authorized public API changes are listed under
  `Interfaces and dependencies`. Stop on any other public signature change.
- Dependencies: none. Stop before adding any.
- Grammar: stop if any input changes its parse other than through `SF-D1` to
  `SF-D7`.
- Iterations: stop after three failed fix attempts on one gate.
- Time: stop if a milestone runs eight working hours past its first
  `Progress` timestamp.

The budget comes from the survey in `Context and orientation`. Comparable plans
repeatedly under-estimated: roadmap 3.3.1 budgeted 12 files and 800 lines and
shipped 24 files and 2,595 insertions; 2.6.7 grew from 14 to 54 files; 2.6.8
budgeted 30 files and 1,600 net lines and shipped 44 files and 3,120 net lines,
an overage concentrated in rewritten test fixtures. This plan budgets for
fixture churn up front (about 160 fixture lines use forms that change) and
estimates 1,600 net code lines, with 50 % contingency.

## Risks

- Risk: term tails (`SF-D2`) break many fixtures, because about 77 fixture
  lines write an unparenthesized `if` condition and about 32 write an unbraced
  `for` body. Severity: medium. Likelihood: certain. Mitigation: EP-M3 rewrites
  them in the same commit; a fixture sweep command in `Concrete steps` lists
  every candidate before and after.
- Risk: term tails make `for (x in xs) f(x)` parse silently as
  `(for (x in xs) f)(x)`, a well-formed but useless expression. Severity:
  medium. Likelihood: medium. Mitigation: this is upstream's parse; the users'
  guide states it with the braced form, and a table row pins it. A future
  type-aware lint can flag calls on unit values; it is recorded as a follow-up
  candidate, not scheduled.
- Risk: the top-level statement parser mis-computes a statement's extent now
  that there is no terminating `.`, swallowing the next declaration or leaving
  stray tokens. Severity: high. Likelihood: medium. Mitigation: EP-M0 probes
  the extent API first; extent tests cover every statement form followed by
  every declaration kind; recovery skips to the next line-initial token at
  depth 0.
- Risk: removing `RuleBodyTerm::ForLoop` changes what `unused-relation` and
  `unused-variable` report, because a relation named in a rule-body `for`
  iterable is no longer a relation read. Severity: medium. Likelihood: certain.
  Mitigation: this is the upstream meaning; EP-M4 updates the lint fixtures
  deliberately and adds a regression test naming the new outcome.
- Risk: aggregation checks inside rule-body `for` bodies regress when the
  `ForLoop` term disappears. Severity: medium. Likelihood: medium. Mitigation:
  `SF-D3` keeps aggregation counting inside `for` expressions, and the existing
  `rejects_multiple_aggregations_inside_for_loop` and
  `rejects_aggregation_inside_and_outside_for_loop` tests stay green.
- Risk: lowering mismatches upstream on rare forms (`match` statements,
  `expr in s`, else-less `if`). Severity: medium. Likelihood: low. Mitigation:
  an `rstest` table transcribes each `convertStatement` equation; a property
  test compares lowering with an independent path-enumeration oracle.
- Risk: `make test` hits Rayon `EAGAIN` on the shared host. Severity: low.
  Likelihood: medium. Mitigation: rerun once with
  `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1`.

## Progress

- [x] (2026-11-23T17:40Z) Branch renamed to
  `2-6-10-decide-semicolon-sequencing`; Lody session titled.
- [x] (2026-11-23T18:05Z) Reconnaissance by three Wyvern agents (`;`,
  top-level `for`, rule-body `for`) and an upstream-grammar researcher;
  upstream sources read at `ca5ded82`.
- [x] (2026-11-23T18:40Z) First draft written.
- [ ] Expert design review applied.
- [ ] Plan approved by the user.
- [ ] `EP-M0` extent probe (prototype, not committed).
- [ ] `EP-M1` decision records, specification targets, register, roadmap.
- [ ] `EP-M2` `;` only inside braces.
- [ ] `EP-M3` `if` and `for` tails are terms.
- [ ] `EP-M4` rule-body `for` is an expression clause.
- [ ] `EP-M5` top-level `for` statements and lowering.
- [ ] `EP-M6` documentation close-out, register, roadmap, gates.

## Surprises & discoveries

- Observation: upstream has no terminating `.` after a top-level `for`
  statement. Evidence: `decl` adds `parseForStatement` outside `withPosMany`,
  with no `dot` (`Parse.hs` line 277); the upstream tutorial writes
  `for (person in Person) TargetAudience(person)` with no `.`
  (`doc/tutorial/tutorial.md`, "A more imperative syntax"). Impact: every
  top-level `for` fixture changes, and statement extent must come from the
  grammar rather than from a `.` search (`SF-D6`, EP-M0).
- Observation: upstream's `term` excludes calls. Evidence: `postApply` is a
  postfix entry in `etable` (`Parse.hs` line 859), and `term` lists only
  primaries (lines 569-592). Impact: `if (c) f(x) else g(x)` is a parse error
  upstream, and `for (x in xs) f(x)` parses as a call on the loop (`SF-D2`).
- Observation: a rule-body `for` is a filter condition upstream and fails
  type inference, because a filter must be `bool` and a `for` has type `()`
  (`TypeInference.hs` lines 579 and 853). Impact: ddlint's relational rule-body
  loop has no upstream counterpart (`SF-D3`).
- Observation: statement blocks reject a trailing `;` (`semiSep`), while
  expression blocks accept one (`eseq`). Impact: the two block forms keep
  different separator rules (`SF-D6`).
- Observation: a `match` statement lowers each arm to a boolean `match`
  filter (`explodeMatchCases`, `Statement.hs` lines 129-132), so an arm's
  pattern variables are not visible in the arm's rules. Impact: lowering
  reproduces this; the users' guide states it.
- Observation: the upstream operator table differs from ddlint's beyond `;`:
  `++` is its own row below the shifts, the bitwise operators bind more loosely
  than the comparisons, `==` and `!=` are left-associative, and `=>` is
  left-associative (`Parse.hs` lines 859-885). Impact: out of scope; `SF-D11`
  proposes a register item and roadmap entry.
- Observation: upstream extracts a `group_by` call anywhere inside a rule
  clause (`exprFoldM extractGroupBy`, `Parse.hs` lines 429-448), while ddlint
  classifies only top-level calls and calls inside rule-body `for` bodies.
  Impact: out of scope; `SF-D11` proposes a register item.
- Observation: documentation drift. The specification's `ForStmt` EBNF puts
  the guard after `)` while section 6.5 and the code put it inside; section
  5.13 spells `if … then … else`, and upstream has no `then`; section 6.5 says
  rule-body loops are `Expr::ForLoop` when they are `RuleBodyTerm::ForLoop`;
  `docs/users-guide.md` says `Expr::Group` carries sequences outside braces;
  `docs/haskell-parser-analysis.md` describes `parseForStatement` as
  `for (pat in expr)`, the misreading behind ddlint's header; and
  `tests/rule_behaviour/for_loops.rs` calls `;` right-associative when it is
  left-associative. Impact: EP-M1 and EP-M6 correct each.
- Observation: sema declares no pattern bindings for an `Expr::ForLoop`
  nested inside another expression (`src/sema/variables.rs`,
  `walk_for_loop_uses`). Impact: `SF-D3` routes every `for` expression through
  one loop-scope path, closing this gap.

## Decision log

- SF-D1. Decision: `;` appears only as a block-item separator. Delete
  `BinaryOp::Seq`, its precedence entry, and its renderings. A `;` reached by
  the infix loop outside a block item reports `SEMICOLON_OUTSIDE_BLOCK_ERROR`
  (`` `;` separates items only inside a block; wrap the sequence in `{ … }` ``)
  at the `;` span. The existing scoped flag, renamed `in_block_item`, decides
  between ending the item and reporting the error; its set and clear points
  from ADR-0006 `BG-D11` are unchanged. This also covers the rule-body literal
  `A(x); B(x)`, `return x; y`, and `x ;`. Rationale: faithful to `eseq` and the
  commented-out `etable` row. The strongest alternative, keeping `Seq` as a
  documented ddlint extension, is rejected because the specification's
  compatibility policy forbids silent extensions and nothing depends on it.
  Date/Author: 2026-11-23, planning agent; requires user approval.
- SF-D2. Decision: the `if` condition, the `then` and `else` branches, and
  the `for` body parse as terms through a new `Pratt::parse_term`, matching
  upstream `term`: literals, variables, qualified names, constructors and
  struct literals, parenthesized groups and tuples, blocks, vector and map
  literals, `match`, `if`, `for`, `break`, `continue`, `return`, and closures.
  Unary operators, postfix forms, and infix operators are not part of a term;
  after the control-flow expression they apply to the whole expression. `match`
  arms, closure bodies, and `return` values stay full expressions, as upstream.
  Two targeted diagnostics replace generic token errors: an operator where a
  term is expected after `if` reports `IF_TERM_EXPECTED_ERROR`, and an `else`
  that does not directly follow the `then` term reports `DANGLING_ELSE_ERROR`
  (message texts under `Interfaces and dependencies`). The specification's
  `ExprIf` becomes `'if' Term Term ('else' Term)?`. Rationale: the upstream
  comment at `Parse.hs` lines 799-805 states the intent, and register item 16
  assigns it here. Alternative: schedule term tails separately to cut fixture
  churn; rejected because `SF-D1` and `SF-D2` both decide where a control-flow
  tail ends and share the same fixtures. Date/Author: 2026-11-23, planning
  agent; requires user approval.
- SF-D3. Decision: a rule-body `for` is an ordinary expression clause.
  Delete `RuleBodyTerm::ForLoop`, `RuleForLoop`, and
  `Rule::flattened_body_terms()`; a `for` literal classifies as
  `RuleBodyTerm::Expression(Expr::ForLoop { .. })`, and `var v = for (…) …` as
  an assignment. Aggregation counting still descends into `for` bodies, so the
  existing "multiple aggregations" errors stand. sema opens a loop scope for
  every `Expr::ForLoop`, wherever it appears: the iterable and guard are uses
  in the enclosing scope, and the pattern declares into the body only. A
  relation name inside a `for` iterable is an expression, not a relation read.
  Rationale: upstream has no relational rule-body loop; Appendix A rule 10 of
  the 2.6.8 plan and specification section 8.1 already state that a rule-body
  `for` declares nothing for later clauses. Alternative: keep relational
  flattening as a ddlint extension; rejected for the same policy reason as
  `SF-D1`. Date/Author: 2026-11-23, planning agent; requires user approval.
- SF-D4. Decision: rule-level `=` stays the assignment split. With term
  bodies, `for (x in xs) y = f(x)` assigns to the loop expression, which
  upstream also rejects; ddlint reports `ASSIGNMENT_TARGET_ERROR`
  (`` the left-hand side of `=` must be a pattern; wrap a loop body in `{ … }` ``)
  instead of a raw pattern-parse error. Rationale: upstream `=` is the
  lowest-precedence non-associative operator, so a depth-0 `=` in a clause is
  always the assignment. Date/Author: 2026-11-23, planning agent.
- SF-D5. Decision: the top-level header is
  `for ( (LcName 'in')? HeaderAtom ('if' Expr)? )`. `HeaderAtom` is a relation
  atom in any form ddlint accepts in a rule body (`R(args)`, `R[e]`, `R'(…)`,
  `R-<n>(…)`, qualified names) or a bare relation name `R`, which upstream
  allows here and which matches every record. Named-argument atoms
  (`R(.f = e)`) wait for 2.6.11. `for (a in A(a))` parses, because upstream's
  grammar accepts it; the double declaration is a scope error assigned to
  roadmap 4.1.5. A pattern that is not a single lower-case name before `in`
  reports `TOP_LEVEL_FOR_BINDER_ERROR`, and a header that is not an atom reports
  `TOP_LEVEL_FOR_ATOM_ERROR`. Rationale: `parseForStatement` calls
  `atom False`, whose only binder is `varIdent <* "in"`. Date/Author:
  2026-11-23, planning agent; requires user approval.
- SF-D6. Decision: the body is an upstream statement, with no terminating
  `.`.
  `Statement ::= ForStmt | 'skip' | 'if' '(' Expr ')' Statement
  ('else' Statement)? | 'match' '(' Expr ')' '{' (Pattern '->' Statement)
  (',' …)* ','? '}' | Expr 'in' Statement | '{' (Statement (';'
  Statement)*)? '}' | HeadAtom ('@' Expr)?`.
  A statement block rejects a trailing `;`
  (`STATEMENT_BLOCK_TRAILING_SEPARATOR_ERROR`) and allows `{}`. A `.` directly
  after a statement reports `TOP_LEVEL_FOR_TERMINATOR_ERROR`
  (`` a top-level `for` statement takes no terminating `.` ``) and is consumed
  for recovery. The statement parser determines the extent; the rules scanner
  skips the reported extent instead of searching for a `.`, which also removes
  the quadratic longest-prefix search in `parse_top_level_for_statement`.
  Recovery from a malformed statement skips to the next line-initial token at
  delimiter depth 0. Rationale: faithful to `statement` (`Parse.hs` lines
  370-397). Date/Author: 2026-11-23, planning agent; requires user approval.
- SF-D7. Decision: lowering follows `convertStatement` exactly and yields
  rules in source order of their head atoms. `SemanticRule` replaces
  `patterns()` and `body() -> &[Expr]` with ordered `SemanticBodyTerm`s:
  `Literal(SemanticLiteral { binder, atom })` for a header atom,
  `Condition(Expr)` for a guard, an `if` condition (with `not c` for `else`), a
  `match` arm filter, or an `expr in` statement whose expression is not an
  assignment, and `Assignment(RuleAssignment)` for `pat = e in …`. Heads carry
  an optional `@` location. `skip` and `{}` yield no rules. Rationale: the
  typed terms make the binder and the assignment visible to sema without
  re-parsing, and reuse the existing `RuleAssignment` type. Alternative: keep
  `Vec<Expr>` and add an `Expr` binder node; rejected because 2.6.14 owns the
  general `v in R(…)` expression form. Date/Author: 2026-11-23, planning agent.
- SF-D8. Decision: no new `DiagnosticCode`; top-level statement messages use
  `DiagnosticCategory::TopLevelFor` and expression messages keep their existing
  categories. `UNSUPPORTED_TOP_LEVEL_FOR_BODY` and
  `UNSUPPORTED_TOP_LEVEL_FOR_HEAD` are deleted. Rationale: ADR-0006 `BG-D7`
  precedent; stable codes wait for 2.8.1. Date/Author: 2026-11-23, planning
  agent.
- SF-D9. Decision: no Verus proof in this item. The lowering is the only
  contractual logic introduced; its lemmas are recorded in
  `docs/ddlint-design.md` and discharged by an `rstest` equation table and a
  property test against an independent path-enumeration oracle. Rationale: see
  `Formal verification decision`. Date/Author: 2026-11-23, planning agent;
  requires user approval.
- SF-D10. Decision: record the decisions in a new ADR,
  `docs/adrs/adr-0007-statement-sequencing-and-ftl-statements.md`, referenced
  from `docs/ddlint-design.md`. The 2.6.8 plan informally reserved ADR-0007 for
  the lexical-scope model (3.3.5), but no live document records that
  reservation, so 3.3.5 takes the next free number. Date/Author: 2026-11-23,
  planning agent; requires user approval.
- SF-D11. Decision: record two divergences found during planning as new
  register items and roadmap entries rather than fixing them here: register
  item 23 and roadmap 2.6.16, "Align the operator table with upstream `etable`
  (`++` row, bitwise below comparisons, associativity of `==`, `!=`, `=>`)";
  register item 24 and roadmap 2.6.17, "Decide whether `group_by` extraction
  descends into nested expressions as upstream `extractGroupBy` does".
  Date/Author: 2026-11-23, planning agent; requires user approval.
- SF-D12. Decision: no new CST node kind for top-level `for` statements;
  their tokens stay bare children of `N_DATALOG_PROGRAM`, as today. Rationale:
  scope control; a node kind belongs with the crate split (2.7). Date/Author:
  2026-11-23, planning agent.
- SF-D13. Decision: the end-to-end boundary is the public library pipeline
  (`parse()` to `sema::build()` to the lint `Runner`), because the `ddlint`
  binary is still a placeholder (`cli/tests/placeholder_cli.rs`). Date/Author:
  2026-11-23, planning agent.

## Outcomes & retrospective

Not started.

## Context and orientation

Inspected base: `7adbd8bc` ("codify brace blocks and adopt upstream map
literals (2.6.8)"). Line numbers below refer to that revision; relocate symbols
with `rg -n '<symbol>' src`.

### Expression parsing and `;`

The expression parser is a hand-written Pratt parser in
`src/parser/expression/`. `Pratt::parse_expr` (`pratt/mod.rs`) runs a prefix
production (`prefix.rs`, dispatching `if`, `match`, and `for` to
`control_flow.rs`), then postfix forms (`pratt/postfix.rs`), then
`Pratt::parse_infix` (`infix.rs`). Binding powers live in
`src/parser/ast/precedence.rs`; `;` is `BinaryOp::Seq` with `BP_SEQ = 0`
(left-associative), and `BLOCK_ITEM_MIN_BP = BP_SEQ + 1`. `BinaryOp` is public
(`src/parser/ast/expr/mod.rs`, re-exported from `src/parser/ast/mod.rs`); its
symbol is `";"` and its s-expression is `(; lhs rhs)`.

Blocks (`src/parser/expression/block.rs`, `src/parser/ast/expr/block.rs`) never
build `Seq`. While a block item is parsed, the flag
`Pratt::semicolon_terminates` makes `parse_infix` stop at `;`. It is set by
`with_block_item_scope` and cleared by `with_nested_delimiter_scope` and
`with_guard_expression_scope`; `if` conditions and branches, `for` bodies,
`return` values, and pipe-closure bodies inherit it. Outside blocks the flag is
false and `;` builds `Seq` in parentheses, call arguments, collection items,
struct fields, `match` arms, iterables, guards, and rule-body literals. The
rule-body scanner (`src/parser/span_scanners/rules.rs`, `rule_statement`)
splits literals on `,` only, so `A(x); B(x)` is one literal.

Tests asserting `Seq` (13 cases): `src/parser/ast/expr/tests.rs` (symbol row),
`src/parser/tests/control_flow.rs` (`"return; x"`),
`src/parser/tests/operator_precedence.rs` (`"a = b; c"`, `"a => b; c"`,
`"(a => b); c"`, `"a => (b; c)"`), `src/parser/tests/block_expressions/mod.rs`
(`parenthesized_sequence`, `nested_delimiters_still_allow_sequence`),
`tests/rule_behaviour/for_loops.rs` (two cases), plus error-side cases in
`src/parser/tests/expression/characterization.rs` and
`src/parser/tests/expression/fixtures/errors.rs` (`"x ;"`), and generators in
`src/parser/tests/block_expressions/props.rs` and `strategy.rs` that emit
`(a; b)`.

### Control-flow tails

`control_flow.rs`: `if` parses its condition with `parse_if_condition` under
struct-literal activation and each branch with `parse_if_clause`
(`parse_expr(0)`); a missing `else` yields `Expr::Tuple(vec![])`. `for` parses
`( pattern in iterable [if guard] )` and then a body with `parse_expr(0)` and
struct literals suspended. `match` arms use `parse_expr(0)` inside a nested
delimiter scope. AST shapes:
`Expr::IfElse { condition, then_branch, else_branch }` and
`Expr::ForLoop { pattern, iterable, guard, body }`.

### Rule bodies

`Rule::body_terms()` (`src/parser/ast/rule/mod.rs`) classifies each literal
through `classification::parse_rule_body_term`, which first tries
`split_assignment` (a depth-0 `=` found by `find_top_level_eq_span` in
`src/parser/delimiter.rs`) and then `parse_expression`. `classify_expression`
turns `Expr::ForLoop` into `RuleBodyTerm::ForLoop(RuleForLoop)`, descending
into bodies through `classify_for_body_with_aggregation_tracking` (which unwraps
`Seq`, nested loops, groups, and blocks). `Rule::flattened_body_terms` expands
`ForLoop` terms into iterable, guard, and body terms, dropping the pattern.
Consumers: `src/sema/traverse.rs` (`collect_for_loop_term`, a child
`ScopeKind::ForLoop` scope, `UseOrigin::ForIterable`),
`src/parser/cst_builder/mod.rs`, `src/parser/mod.rs`, and the lint
`unused_relation` through sema. Tests: `src/parser/ast/rule/tests.rs` (18
references), `tests/rule_behaviour/for_loops.rs` (14),
`tests/rule_behaviour/body_bounds.rs`, `src/sema/tests/mod.rs`
(`for_loop_bindings_do_not_escape_loop_scope`), `src/sema/tests/name_span.rs`,
`tests/unused_relation_rule.rs`, and `tests/unused_variable_rule.rs`.

### Top-level `for`

There is no CST node. `collect_rule_spans` sends a line-initial `for` to
`skip_top_level_for_statement` (`src/parser/span_scanners/rules.rs`), which
skips to a terminating `.`. Separately, `collect_desugared_top_level_for_rules`
(`src/parser/top_level_for/mod.rs`, called from `src/parser/mod.rs` after
`top_level_for_exclusions`) tries every later terminating `.` and keeps the
longest prefix that parses as `Expr::ForLoop`, then `lower_top_level_for`
(`lowering.rs`) emits one `SemanticRule` whose body is iterable, guard, nested
iterable, nested guard, and whose head is the terminal body. `if`, `match`,
`break`, `continue`, `return`, and `Seq` bodies report
`UNSUPPORTED_TOP_LEVEL_FOR_BODY`; a non-atom terminal reports
`UNSUPPORTED_TOP_LEVEL_FOR_HEAD`; both use `DiagnosticCategory::TopLevelFor`.
`SemanticRule` (`src/parser/ast/semantic_rule.rs`) is public with `origin()`,
`source_span()`, `patterns()`, `head()`, and `body()`;
`Parsed::semantic_rules()` is in `src/parser/cst_builder/mod.rs`; sema consumes
it in `src/sema/builder.rs` (`collect_semantic_rules`). Tests: ten in
`src/parser/tests/rules/top_level_for.rs`, ten in
`tests/top_level_for_desugaring.rs`, and uses in `src/sema/tests/`,
`tests/semantic_scope_resolution.rs`, `tests/block_expressions.rs`,
`tests/parser_observability.rs`, and `src/parser/tests/reserved_tokens/mod.rs`.
About 52 fixture lines start a string with `for (`.

### Documentation touched

`docs/differential-datalog-parser-syntax-spec-updated.md` sections 3.4, 4, 5.8,
5.10, 5.13, 6.5, 8.1, 10, and 12; `docs/parser-conformance-register.md` items 8
and 16 (and new 23 and 24); `docs/roadmap.md` 2.6.10 (and new 2.6.16 and
2.6.17); `docs/parser-implementation-notes.md` (the `;` flag and `### for`
sections); `docs/ddlint-design.md`; `docs/users-guide.md`;
`docs/developers-guide.md`; `docs/haskell-parser-analysis.md`; and the new ADR.

### Tooling

Tests use `rstest` 0.x, `rstest-bdd` and `rstest-bdd-macros` 0.6.0 (feature
files under `tests/features/`, for example `block_expressions.feature` driven by
`tests/block_expressions_bdd.rs`), `proptest` 1.6, `insta` 1.47.2,
`googletest` 0.14.4, and `pretty_assertions` 1.4.1, all already
dev-dependencies. `make verus` runs the pinned `prover-tools` Verus lane
(currently the smoke proof `verus/smoke/main.rs`); it is outside `make test` and
`make lint`.

### Budget survey

Production modules affected: `top_level_for/` (462 lines), `control_flow.rs`
(323), `infix.rs` (58), `pratt/mod.rs` (305), `precedence.rs`,
`ast/expr/mod.rs`, `ast/rule/mod.rs` (399, at the file limit) and
`classification.rs` (341), `semantic_rule.rs` (74), `sema/builder.rs` (359),
`sema/traverse.rs` (225), `sema/variables.rs` (329), and
`span_scanners/rules.rs` (379). Estimated net code: EP-M2 +100, EP-M3 +150
(fixture rewrites are mostly churn), EP-M4 -150, EP-M5 +1,300 (statement AST
and parser about 450, lowering 150, `SemanticRule` terms 100, sema 100, tests
and generators 700, minus about 300 deleted), EP-M6 0. Total about 1,400 to
1,600 net. Historical overruns (see `Tolerances`) justify the 50 % contingency.
Documentation estimate: ADR 220, specification 250, register 80, roadmap 20,
notes 60, design 80, users' guide 120, developers' guide 30, Haskell analysis
20, about 880 net.

### Relevant skills and documents

Skills: `execplans`, `rust-router` (then `rust-state-machines` for the
statement parser and lowering, `rust-unit-testing` and `proptest` for tests,
`rust-verification` and `verus` for the proof decision),
`hexagonal-architecture` (the parser stays a pure core), `codegraph-mcp`
(unavailable during drafting; use `rg` instead), `firecrawl-mcp` for upstream
sources, `en-gb-oxendict-style`. Documents:
`docs/differential-datalog-parser-syntax-spec-updated.md`,
`docs/parser-implementation-notes.md`,
`docs/building-an-error-recovering-parser-with-chumsky.md`,
`docs/rust-parser-testing-comprehensive-guide.md`,
`docs/rust-testing-with-rstest-fixtures.md`, `docs/ddlint-design.md`,
`docs/complexity-antipatterns-and-refactoring-strategies.md`,
`docs/adrs/adr-0006-block-expressions-and-collection-syntax.md`, and
`docs/haskell-parser-analysis.md`.

## Conformance basis

Upstream artefacts:

- Syntax specification
  `docs/differential-datalog-parser-syntax-spec-updated.md` at `7adbd8bc`,
  including the source-of-truth policy (`SPEC-POLICY`, "Status and
  source-of-truth policy") and section 13's compatibility policy.
- `docs/parser-conformance-register.md` items 8 and 16 at `7adbd8bc`.
- `docs/roadmap.md` item 2.6.10 at `7adbd8bc`.
- ADR-0006 (`docs/adrs/adr-0006-block-expressions-and-collection-syntax.md`),
  decisions `BG-D3`, `BG-D8`, `BG-D9`, and `BG-D11`.
- `docs/execplans/2-6-8-decide-brace-group-extension-policy.md` Appendix A,
  rule 10 (historical input, not modified).
- Upstream DDlog at `ca5ded823dc2c01bc33aac44e526c12d2fed03a6`: `Parse.hs`,
  `Statement.hs`, `Syntax.hs`, `TypeInference.hs`, `Validate.hs`, and
  `doc/tutorial/tutorial.md`. Where the language reference disagrees with the
  compiler (it lists `expr ";" expr` and a `for (expr in atom …)` header), the
  compiler is followed.

No Terms of Reference document exists for this item.

Requirements:

- `REQ-SEQ`: `;` appears only as a block-item separator (`eseq`).
- `REQ-TERM`: the `if` condition and branches and the `for` body are terms;
  trailing operators apply to the whole control-flow expression.
- `REQ-RBFOR`: a rule-body `for` is an expression clause that declares
  nothing for later clauses.
- `REQ-FTL-HEADER`: a top-level header is an atom with an optional `v in`
  binder and an optional guard.
- `REQ-FTL-STMT`: the top-level body is an upstream statement with no
  terminating `.`.
- `REQ-FTL-LOWER`: lowering matches `convertStatement`.
- `REQ-DOC`: specification, register, roadmap, ADR, and guides agree with the
  implementation.

Gaps (baseline to target):

- `GAP-SEQ-INFIX`: `BinaryOp::Seq` outside braces.
- `GAP-TAIL-EXPR`: control-flow tails parse as full expressions.
- `GAP-RBFOR-REL`: rule-body `for` flattens into relational joins.
- `GAP-RBFOR-EQ`: `for (x in xs) y = f(x)` reports a raw pattern error.
- `GAP-FTL-HEADER`: pattern-plus-expression header.
- `GAP-FTL-BODY`: atom-only bodies and a mandatory `.`.
- `GAP-SEMA-FOR`: nested `for` expressions declare no pattern bindings.
- `GAP-DOC-DRIFT`: the drift listed in `Surprises & discoveries`.

Trace links:

```plaintext
SPEC-POLICY -> REQ-SEQ -> GAP-SEQ-INFIX -> SF-D1 -> EP-M2 -> tests::sequencing::*, V-SEQ-PROP
SPEC-POLICY -> REQ-TERM -> GAP-TAIL-EXPR -> SF-D2 -> EP-M3 -> tests::control_flow_terms::*
REG-16 -> REQ-RBFOR -> GAP-RBFOR-REL, GAP-SEMA-FOR -> SF-D3 -> EP-M4 -> rule_behaviour::for_loops::*
REG-16 -> REQ-RBFOR -> GAP-RBFOR-EQ -> SF-D4 -> EP-M4 -> body_terms::assignment_target_*
REG-8, REG-16 -> REQ-FTL-HEADER -> GAP-FTL-HEADER -> SF-D5 -> EP-M5 -> top_level_for::header_*
REG-8, REG-16 -> REQ-FTL-STMT -> GAP-FTL-BODY -> SF-D6 -> EP-M5 -> top_level_for::extent_*
REG-8 -> REQ-FTL-LOWER -> SF-D7 -> EP-M5 -> top_level_for::lowering_*, V-FTL-PROP
SPEC-POLICY -> REQ-DOC -> GAP-DOC-DRIFT -> SF-D10, SF-D11 -> EP-M1, EP-M6 -> markdown gates
```

## Verification plan

### Abstract model

Statements form a finite tree:

```plaintext
S ::= For(binder?, atom, guard?, S)
    | If(c, S, S?)
    | Match(e, [(p1, S1), …, (pn, Sn)])     n ≥ 1
    | Assign(e, S)
    | Block([S1, …, Sk])                     k ≥ 0
    | Skip
    | Insert(head, location?)
```

A rule is `(head, location?, [t1, …, tm])` where each term is
`Lit(binder?, atom)`, `Cond(e)`, or `Asg(pat, e)`. `lower : S -> [Rule]` is the
executable lowering. The specification is declarative and independent of
`lower`: a *path* from the root to an `Insert` leaf records one step per
ancestor, and each step contributes terms:

```plaintext
contrib(For(b, a, g, _))       = [Lit(b, a)] ++ [Cond(g) | g present]
contrib(If(c, _, _), then)     = [Cond(c)]
contrib(If(c, _, _), else)     = [Cond(not c)]
contrib(Match(e, arms), i)     = [Cond(match e { p1 -> i == 1, …, pn -> i == n })]
contrib(Assign(e, _))          = [Asg(p, v)] if e is `p = v`, else [Cond(e)]
contrib(Block(_), j)           = []
spec(S) = [ (h, loc, concat(contrib(step) for step in path)) | (path, Insert(h, loc)) ∈ leaves(S) ]
```

`leaves(S)` lists `Insert` leaves in source order: for `If`, then-leaves before
else-leaves; for `Match` and `Block`, children in order.

Malformed input never reaches `lower`: the statement parser either returns a
well-formed tree or diagnostics, and a statement with diagnostics yields no
rules.

### Invariants and lemmas

- `INV-SEQ` (`REQ-SEQ`): no parse result contains a sequencing node, and
  every `;` outside a block item is reported at its own span. The first half
  holds by construction once `BinaryOp::Seq` is deleted; the second is
  observable.
- `INV-TERM` (`REQ-TERM`): for every binary operator `⊕` and every
  control-flow head `H` in {`if (c) {a} else {b}`, `if (c) {a}`,
  `for (x in xs) {a}`}, `H ⊕ y` parses as `Binary(⊕, H, y)`.
- `INV-RBFOR` (`REQ-RBFOR`): no rule-body term exposes a `for` as anything
  but an expression or an assignment value, and no name declared by a `for`
  pattern is visible outside that loop's body.
- `INV-EXTENT` (`REQ-FTL-STMT`): a well-formed top-level statement's extent
  ends at its last token; the following declaration parses exactly as it would
  without the statement before it.
- `L-LOWER` (`REQ-FTL-LOWER`): `lower(S) = spec(S)` for every well-formed `S`.
- `L-COUNT`: `|lower(S)|` equals the number of `Insert` leaves of `S`; in
  particular `Skip` and `Block([])` yield none and an else-less `If` yields
  only its then-leaves.
- `L-PREFIX`: for `For(b, a, g, S')`, every rule of `lower` begins with
  `Lit(b, a)` and then `Cond(g)` if present, and the remainders, in order, are
  exactly `lower(S')`.
- `L-EXCL`: for `If(c, S1, S2)`, every rule from `S1` carries `Cond(c)` and
  no rule from `S2` does; every rule from `S2` carries `Cond(not c)`. For a
  `Match`, the filter of arm `i` is true exactly for arm `i`.

`L-PREFIX`, `L-COUNT`, and `L-EXCL` are corollaries of `L-LOWER` stated for
readers and for focused tests; the main obligation is `L-LOWER`, which follows
by structural induction using two collection facts: prepending a term
distributes over concatenation, and `leaves` of a `Block` is the concatenation
of its children's leaves.

### Axioms (trusted, not verified here)

- `AXIOM-UPSTREAM`: the upstream sources at `ca5ded82` define the target
  grammar and lowering. Boundary: the specification text and the `rstest`
  tables transcribe `Parse.hs` and `Statement.hs`.
- `AXIOM-TOKENIZER`: `tokenize_without_trivia` yields the same tokens for a
  statement whether or not it is followed by other declarations. Boundary:
  `INV-EXTENT` tests.
- `AXIOM-PROPTEST`: `proptest` generates and shrinks as documented.

### Methods per obligation

- Obligation: `INV-SEQ`. Method: deletion plus a property test (`V-SEQ-PROP`)
  and an `rstest` table (`V-SEQ-TABLE`). Rationale: the insertion points are an
  open space (any non-block position), but each case is cheap. Domain: the
  existing block-expression generator
  (`src/parser/tests/block_expressions/strategy.rs`), extended to choose a
  non-block insertion point (parenthesized group, call argument, vector item,
  struct field, `match` arm, iterable, guard, `return` value) or a block-item
  boundary. Artefacts: `src/parser/tests/sequencing/mod.rs` (table) and
  `src/parser/tests/sequencing/props.rs` (property). Evidence: before EP-M2 the
  property fails with a parse that contains `(; …)`; after, a `;` at a
  non-block point yields exactly one `SEMICOLON_OUTSIDE_BLOCK_ERROR` whose span
  equals the inserted `;`, and a `;` at a block boundary parses. Oracle: the
  generator's own record of where it inserted `;`. Non-vacuity: classify and
  count each insertion class (require every class at least 20 times in 256
  cases); seeded fault: clear `in_block_item` inside blocks, which must make
  the block-boundary class fail.
- Obligation: `INV-TERM`. Method: `rstest` table over every `BinaryOp`
  variant crossed with the three heads (finite, about 75 rows) in
  `src/parser/tests/control_flow_terms.rs`, plus named rows for the diagnostics
  (`if x > 1 { a }`, `if (c) f(x) else g(x)`) and the silent postfix case
  (`for (x in xs) f(x)` as `Apply(ForLoop, [x])`). Rationale: the operator set
  is finite and enumerable. Non-vacuity: the table is built from an exhaustive
  `match` over `BinaryOp`, so adding an operator without a row fails to
  compile; seeded fault: parse the `else` branch with `parse_expr(0)` again,
  which must fail every `else` row.
- Obligation: `INV-RBFOR`. Method: named tests and sema tests. Artefacts:
  `tests/rule_behaviour/for_loops.rs` (rewritten), `src/sema/tests/mod.rs`
  (`for_expression_bindings_stay_in_loop_body`, including a `for` inside an
  `if` branch), `tests/unused_relation_rule.rs` (a relation named only in a
  `for` iterable is reported unused). Evidence: the nested-`for` sema test
  fails before EP-M4 (the body use is unresolved) and passes after.
  Non-vacuity: each test asserts a positive resolution inside the body and a
  negative one after the clause.
- Obligation: `INV-EXTENT`. Method: `rstest` table over every statement form
  followed by each declaration kind (relation, rule, function, index, import,
  another `for`), asserting the following item's span and kind, plus recovery
  rows (malformed header, stray `.`, unterminated block). Artefact:
  `src/parser/tests/rules/top_level_for_extent.rs`. Non-vacuity: each row
  asserts the following declaration's exact span; seeded fault: end the extent
  one token early, which must fail every row.
- Obligation: `L-LOWER` (with `L-COUNT`, `L-PREFIX`, `L-EXCL`). Method: an
  `rstest` table transcribing each `convertStatement` equation (`V-FTL-TABLE`)
  and a property test (`V-FTL-PROP`) comparing `Parsed::semantic_rules()` with
  an independent oracle. Rationale: an open input space of trees with a small,
  crisp declarative specification. Domain: generated statement trees of depth
  at most 4 and block width at most 3 over a fixed atom vocabulary, rendered to
  source. Oracle: an explicit-stack depth-first path enumerator in the test
  module that computes `spec(S)` from the generator's tree, sharing no code with
  `lower`. Artefacts: `src/parser/tests/rules/top_level_for.rs` (table) and
  `src/parser/tests/rules/top_level_for_props.rs` plus
  `top_level_for_strategy.rs`. Evidence: before EP-M5 the property cannot parse
  the rendered statements; after, rules match the oracle exactly, including
  order. Non-vacuity: count generated classes (binder present and absent, guard,
  `if` with and without `else`, `match` with two or more arms, `pat = e in`,
  non-assignment `e in`, empty block, multi-statement block, `skip`, nested
  `for`, insert with location) and require each at least 10 times in 256 cases;
  seeded faults: drop the guard, emit `c` instead of `not c` for `else`,
  reverse block order, and omit `Lit` for nested `for`; each must be rejected
  with a shrunk counterexample. Regressions persist in `proptest-regressions/`
  files committed with the test.

### Formal verification decision

Verus is not warranted in this item (`SF-D9`). The decisions `SF-D1` to `SF-D6`
are grammar choices whose contracts are finite tables or cheap generated
inputs; proofs would restate the grammar. The lowering is contractual, but it
is a seven-case structural recursion whose cases are one-line equations. A
Verus proof would need a declarative specification and a proof-to-production
bridge; the bridge (a marked kernel region compared by an eraser) is scheduled
as infrastructure in roadmap 3.3.6, and building it here would duplicate that
work while leaving the same production text unproved until the bridge exists.
The property test against an independent path-enumeration oracle exercises
every equation and their compositions up to depth 4, which covers every
interaction between two nested forms; the lowering has no arithmetic, ordering
ties, or state whose bugs need deeper trees. Stronger assurance would add
unbounded depth only. If the user prefers a proof, the recommended route is to
add the lowering kernel to roadmap 3.3.6's bridged proof set, with `L-LOWER` as
its theorem and the abstract model above as its specification. Kani is not
used: the inputs are recursive trees, where bounded model checking needs
bespoke generators and gives less than the property test at this size.

### Suggested BDD features

Two features, each driven by an `rstest-bdd` runner in the same style as
`tests/block_expressions_bdd.rs`.

`tests/features/sequencing_and_control_flow.feature` (runner
`tests/sequencing_and_control_flow_bdd.rs`; `REQ-SEQ`, `REQ-TERM`, `REQ-RBFOR`;
EP-M2 to EP-M4):

```gherkin
Feature: Sequencing and control-flow tails follow upstream DDlog
  Scenario: A semicolon sequences items inside a block
    Given the expression "{ a; b }"
    When the expression is parsed
    Then the result is a block with statement "a" and value "b"

  Scenario: A semicolon outside a block is rejected
    Given the expression "(a; b)"
    When the expression is parsed
    Then parsing fails with "`;` separates items only inside a block; wrap the sequence in `{ … }`"

  Scenario: An operator after an if expression applies to the whole if
    Given the expression "if (c) { a } else { b } ++ x"
    When the expression is parsed
    Then the s-expression is "(++ (if c (block a) (block b)) x)"

  Scenario: An unparenthesized comparison is not an if condition
    Given the expression "if x > 1 { a }"
    When the expression is parsed
    Then parsing fails with "an `if` condition and branches are single terms; parenthesize the condition and brace the branches"

  Scenario: A rule-body for loop is an expression clause
    Given the program "R(x) :- S(x), for (y in ys(x)) { f(y) }."
    When the rule body terms are classified
    Then the second term is an expression containing a for loop
```

`tests/features/top_level_for_statements.feature` (runner
`tests/top_level_for_statements_bdd.rs`; `REQ-FTL-*`; EP-M5):

```gherkin
Feature: Top-level for statements lower into rules
  Scenario: A for statement over an atom becomes one rule
    Given the program "for (A(a)) B(a)"
    When the program is parsed
    Then the semantic rules are "B(a) :- A(a)"

  Scenario: A binder, guard, and if-else statement lower into two rules
    Given the program "for (r in A(a) if a > 0) if (ok(r)) B(a) else C(a)"
    When the program is parsed
    Then the semantic rules are "B(a) :- r in A(a), a > 0, ok(r)" and "C(a) :- r in A(a), a > 0, not ok(r)"

  Scenario: A statement block lowers each statement in order
    Given the program "for (A(a)) { B(a); skip; C(a) }"
    When the program is parsed
    Then the semantic rules are "B(a) :- A(a)" and "C(a) :- A(a)"

  Scenario: A terminating dot is rejected
    Given the program "for (A(a)) B(a)."
    When the program is parsed
    Then parsing fails with "a top-level `for` statement takes no terminating `.`"

  Scenario: A tuple pattern is not a binder
    Given the program "for ((k, v) in Pairs(k, v)) Out(k)"
    When the program is parsed
    Then parsing fails with "a top-level `for` binds a single lower-case variable before `in`"
```

## Plan of work

### `EP-M0` extent probe (prototype)

Question: can a `Pratt` parser driven over the token stream from a line-initial
`for` to end of file parse one statement and report the index of the first
unconsumed token, so the rules scanner can skip exactly that extent? Scratch
artefact: a `#[cfg(test)]` function in a throwaway branch that builds
`Pratt::new(tokens[start..].iter().cloned(), src)`, parses
`for ( atom ) Insert` by hand, and reads the token stream's next span. Expected
observation: the next span is the first token of the following declaration for
`for (A(a)) B(a)\nR(x) :- A(x).`. Go: implement EP-M5 with a `StatementParser`
that wraps `Pratt` and exposes the consumed end. No-go: fall back to a
delimiter-aware token scanner that finds the statement end from the grammar's
closing tokens, then parse the sliced range; record the choice in
`Decision log`. The probe is deleted.

### `EP-M1` decision records, specification targets, register, roadmap

Assigned: `REQ-DOC`, `GAP-DOC-DRIFT`, `SF-D10`, `SF-D11`. Write
`docs/adrs/adr-0007-statement-sequencing-and-ftl-statements.md` recording
`SF-D1` to `SF-D8` with the upstream evidence. In the specification, rewrite
section 3.4's `;` note and section 4's note to state `;` is only a block
separator (target, with an "implementation status: scheduled" line), section
5.10 to the upstream statement grammar of `SF-D6` with the header of `SF-D5`,
section 5.13 to `ExprIf ::= 'if' Term Term ('else' Term)?` with a `Term`
definition and the postfix consequence, section 5.8's `RhsTerm` to drop the
relational loop, section 6.5 to the `convertStatement` lowering with worked
examples, and section 12 with the new failing examples. Mark these sections as
targets until EP-M6. Update register items 8 and 16 to `scheduled` with the
decided behaviour, add items 23 and 24, add roadmap entries 2.6.16 and 2.6.17,
reference the ADR from `docs/ddlint-design.md`, and correct
`docs/haskell-parser-analysis.md`'s `parseForStatement` description. Run
`make fmt`, `make markdownlint`, `make nixie`, and `make check-fmt`.

### `EP-M2` `;` only inside braces

Assigned: `REQ-SEQ`, `GAP-SEQ-INFIX`, `SF-D1`. Red: add
`src/parser/tests/sequencing/mod.rs` with an `rstest` table of rejected inputs
(`"(a; b)"`, `"f(a; b)"`, `"[a; b]"`, `"S { x: a; b }"`,
`"match (x) { _ -> a; b }"`, `"for (x in a; b) { c }"`, `"return x; y"`,
`"x ;"`, `"a; b"`) and accepted inputs (`"{ a; b }"`, `"{ (a, b); c }"`,
`"{ if (c) { a } else { b }; d }"`), asserting the message and span with
`googletest` matchers; and the property `V-SEQ-PROP`. Run
`cargo test --lib sequencing` and observe failures (the rejected inputs parse).
Green: delete `BinaryOp::Seq` and its precedence row, symbol, and s-expression
arm; rename `semicolon_terminates` to `in_block_item`; make `parse_infix` report
`SEMICOLON_OUTSIDE_BLOCK_ERROR` and stop when it meets `;` outside a block
item; remove the `Seq` arms in `classification.rs` and
`top_level_for/lowering.rs`. Rewrite the 13 `Seq` assertions to the new outcome
(rejection outside blocks, block form inside), fix the `for_loops.rs` comments,
and replace `(a; b)` in the block generators with `{ a; b }`. Refactor: confirm
`BLOCK_ITEM_MIN_BP` is still needed (it protects operator right-hand sides;
keep it if any test fails without it). Add the sequencing scenarios of the
first BDD feature.

### `EP-M3` `if` and `for` tails are terms

Assigned: `REQ-TERM`, `GAP-TAIL-EXPR`, `SF-D2`. Red: add
`src/parser/tests/control_flow_terms.rs` with the `INV-TERM` table and the
diagnostic rows; run and observe that `else`-branch rows fail. Green: add
`Pratt::parse_term` in `src/parser/expression/prefix.rs` (prefix productions
only, rejecting unary operators with `IF_TERM_EXPECTED_ERROR` when called from
`if`, and a generic "expected a term" message otherwise); call it for the `if`
condition (keeping struct-literal activation), both branches, and the `for`
body (keeping struct literals suspended); report `DANGLING_ELSE_ERROR` from
`parse_with_context` when a drained unexpected token is `else`. Rewrite
fixtures: parenthesize `if` conditions and brace non-term branches and `for`
bodies across `src/` and `tests/` (sweep command in `Concrete steps`). Add the
control-flow scenarios of the first BDD feature.

### `EP-M4` rule-body `for` is an expression clause

Assigned: `REQ-RBFOR`, `GAP-RBFOR-REL`, `GAP-RBFOR-EQ`, `GAP-SEMA-FOR`, `SF-D3`,
`SF-D4`. Red: rewrite `tests/rule_behaviour/for_loops.rs` to the new
classification, add `for_expression_bindings_stay_in_loop_body` (including a
`for` inside an `if` branch), the unused-relation regression, and an
`assignment_target_error` row in `src/parser/tests/rules/body_terms.rs`; run
and observe failures. Green: remove `RuleBodyTerm::ForLoop`, `RuleForLoop`, and
`Rule::flattened_body_terms` and every caller (`src/parser/ast/mod.rs`,
`src/parser/cst_builder/mod.rs`, `src/parser/mod.rs`,
`src/parser/ast/rule/tests.rs`, `tests/rule_behaviour/body_bounds.rs`); keep
aggregation counting inside `for` bodies through a helper in
`classification.rs`; map a pattern-parse failure whose left-hand side begins
with `for` or `if` to `ASSIGNMENT_TARGET_ERROR`. In sema, move the loop-scope
logic from `traverse.rs::collect_for_loop_term` into the expression walker in
`variables.rs`, so every `Expr::ForLoop` opens a `ScopeKind::ForLoop` scope;
delete the relational `UseOrigin::ForIterable` path if nothing else uses it.
Update `unused_relation` and `unused_variable` fixtures and add the rule-body
scenario of the first BDD feature.

### `EP-M5` top-level `for` statements and lowering

Assigned: `REQ-FTL-HEADER`, `REQ-FTL-STMT`, `REQ-FTL-LOWER`, `GAP-FTL-HEADER`,
`GAP-FTL-BODY`, `SF-D5` to `SF-D8`, `SF-D12`. Red: rewrite the top-level tables
(`src/parser/tests/rules/top_level_for.rs`,
`tests/top_level_for_desugaring.rs`) to upstream syntax and expected rules, add
the extent table, the lowering equation table, the property test, and the
second BDD feature; run and observe failures. Green, in order:

1. Add `src/parser/top_level_for/statement.rs` with the private statement
   tree (`Statement`, `HeaderAtom`, `InsertHead`) mirroring the abstract model.
2. Add `src/parser/top_level_for/parse.rs`, a `StatementParser` over the
   token stream (per EP-M0) that parses `SF-D5` and `SF-D6` and returns the
   tree, the consumed extent, and diagnostics. Reuse `Pratt` for expressions
   and atoms, the shared pattern parser for `match` patterns and assignment
   left-hand sides, and the rule-head parsing helpers in
   `src/parser/ast/rule_head/` for head atoms and `@` locations.
3. Replace `lowering.rs` with the `convertStatement` lowering into
   `SemanticRule` values with `SemanticBodyTerm` bodies (`SF-D7`).
4. Change `src/parser/ast/semantic_rule.rs` to the new public types (see
   `Interfaces and dependencies`) and update `src/test_util/assertions.rs`.
5. Replace the dot search in `top_level_for/mod.rs` and
   `skip_top_level_for_statement` in `rules.rs` with one statement scan whose
   extents both the rules scanner and the lowering pass use; delete the
   duplicate line-start helper in `rules_support.rs` if it becomes unused.
6. Update `src/sema/builder.rs::collect_semantic_rules`: a literal records a
   relation use and declares its binder and atom pattern names; a condition is
   walked for uses; an assignment declares its pattern names for later terms.
7. Rewrite the remaining top-level fixtures in `src/sema/tests/`,
   `tests/semantic_scope_resolution.rs`, `tests/block_expressions.rs`,
   `tests/parser_observability.rs`, and `src/parser/tests/reserved_tokens/`.

Add the end-to-end test `tests/top_level_for_pipeline.rs`: a program whose only
read of relation `A` is a top-level header produces no `unused-relation`
diagnostic through the lint `Runner`, while an unread relation does.

### `EP-M6` documentation close-out

Assigned: `REQ-DOC`. Flip the specification target notes to implemented; set
register items 8 and 16 to `implemented`; update
`docs/parser-implementation-notes.md` (the `;` flag, `parse_term`, the
statement parser, and lowering), `docs/ddlint-design.md` (the lowering contract
and lemmas `L-LOWER`, `L-COUNT`, `L-PREFIX`, `L-EXCL`, and the
`SemanticBodyTerm` model), `docs/users-guide.md` (sequencing, term tails with
the `for (x in xs) f(x)` consequence, rule-body `for`, top-level statements,
and the `match` statement scoping note; correct the `Expr::Group` claim), and
`docs/developers-guide.md` (where statement parsing lives and the rule that new
top-level statement forms extend `StatementParser` and the lowering equations
together). Tick roadmap 2.6.10 with a "Done:" summary. Search for stale claims
with the command below, then run every gate.

```sh
rg -n 'Seq|sequencing|flattened_body_terms|RuleForLoop|patterns\(\)' \
  docs src tests --glob '!docs/execplans/**'
```

## Milestones and plateaus

### EP-M1

- Identifier and outcome: `EP-M1`, decisions recorded; specification states
  targets; register and roadmap scheduled.
- Requirements and gaps: `REQ-DOC`; `GAP-DOC-DRIFT` (specification parts).
- Acceptance evidence: `make markdownlint`, `make nixie`, `make check-fmt`
  pass; register item 16 reads `scheduled`.
- Conformance check:
  - Every `SF-D` decision appears in the ADR.
  - Specification sections carry target markers, so code and text do not
    silently disagree.
  - No code changed.
- Recovery: revert the documentation commit.
- Remaining gaps: all code gaps.
- Compatibility decision: none.

### EP-M2

- Identifier and outcome: `EP-M2`, `;` is only a block separator.
- Requirements and gaps: `REQ-SEQ`, `GAP-SEQ-INFIX`.
- Acceptance evidence: `V-SEQ-TABLE`, `V-SEQ-PROP`, the first feature's
  sequencing scenarios; `make check-fmt`, `make lint`, `make test`.
- Conformance check:
  - `BinaryOp::Seq` no longer exists (`rg -n 'Seq\b' src` finds no variant).
  - Blocks unchanged (`tests/block_expressions*.rs` green without edits
    beyond `Seq` removals).
  - Only the authorized `BinaryOp` change to public API.
  - Trace links current.
- Recovery: revert the milestone commit.
- Remaining gaps: tails, rule-body `for`, top-level `for`.
- Compatibility decision: none (pre-release API).

### EP-M3

- Identifier and outcome: `EP-M3`, `if` and `for` tails are terms.
- Requirements and gaps: `REQ-TERM`, `GAP-TAIL-EXPR`.
- Acceptance evidence: `INV-TERM` table and diagnostics; feature scenarios;
  gates.
- Conformance check:
  - `match` arms, closures, and `return` unchanged.
  - No other binding power changed.
  - Trace links current.
- Recovery: revert the milestone commit.
- Remaining gaps: rule-body and top-level `for`.
- Compatibility decision: none.

### EP-M4

- Identifier and outcome: `EP-M4`, rule-body `for` is an expression clause.
- Requirements and gaps: `REQ-RBFOR`, `GAP-RBFOR-REL`, `GAP-RBFOR-EQ`,
  `GAP-SEMA-FOR`.
- Acceptance evidence: rewritten `for_loops.rs`, sema loop-scope tests,
  unused-relation regression, aggregation tests unchanged; gates.
- Conformance check:
  - Authorized removals only (`RuleBodyTerm::ForLoop`, `RuleForLoop`,
    `flattened_body_terms`).
  - Aggregation guarantees of 2.6.2 kept.
  - Trace links current.
- Recovery: revert the milestone commit.
- Remaining gaps: top-level `for`.
- Compatibility decision: none.

### EP-M5

- Identifier and outcome: `EP-M5`, upstream top-level statements parse and
  lower.
- Requirements and gaps: `REQ-FTL-*`, `GAP-FTL-HEADER`, `GAP-FTL-BODY`.
- Acceptance evidence: extent table, lowering table, `V-FTL-PROP` with class
  counts, second feature, pipeline test; gates.
- Conformance check:
  - Lowering equations match `Statement.hs` row by row.
  - CST still lossless (`tests/span_validation.rs` green).
  - Only the authorized `SemanticRule` API change.
  - Trace links current.
- Recovery: revert the milestone commits; EP-M4 remains a coherent plateau.
- Remaining gaps: documentation close-out.
- Compatibility decision: none.

### EP-M6

- Identifier and outcome: `EP-M6`, documentation and roadmap closed.
- Requirements and gaps: `REQ-DOC`, `GAP-DOC-DRIFT`.
- Acceptance evidence: all gates; register items 8 and 16 `implemented`;
  roadmap 2.6.10 ticked.
- Conformance check: stale-claim search returns only historical execplans.
- Recovery: revert the documentation commit.
- Remaining gaps: register items 23 and 24 (scheduled).
- Compatibility decision: none.

## Concrete steps

Working directory: the repository root. Every gate pipes through `tee` with
`set -o pipefail`:

```sh
set -o pipefail
BRANCH=$(git branch --show-current)
make fmt 2>&1 | tee "/tmp/fmt-ddlint-$BRANCH.out"
make check-fmt 2>&1 | tee "/tmp/check-fmt-ddlint-$BRANCH.out"
make lint 2>&1 | tee "/tmp/lint-ddlint-$BRANCH.out"
make test 2>&1 | tee "/tmp/test-ddlint-$BRANCH.out"
make markdownlint 2>&1 | tee "/tmp/markdownlint-ddlint-$BRANCH.out"
make nixie 2>&1 | tee "/tmp/nixie-ddlint-$BRANCH.out"
```

Focused commands, red then green:

```plaintext
EP-M2 red:   cargo test --lib sequencing        -> rejected inputs parse; property finds "(; a b)"
EP-M2 green: cargo test --lib sequencing        -> all rows pass; class counts printed
EP-M3 red:   cargo test --lib control_flow_terms -> else-branch rows show Binary(++, {b}, x)
EP-M3 green: cargo test --lib control_flow_terms -> all rows pass
EP-M4 red:   cargo test --test rule_behaviour for_loops -> expected Expression(ForLoop), found ForLoop
EP-M4 green: same                                -> pass
EP-M5 red:   cargo test --lib top_level_for      -> upstream statements report errors
EP-M5 green: cargo test --lib top_level_for; cargo test --test top_level_for_statements_bdd -> pass
```

Fixture sweeps (run before and after EP-M3 and EP-M5; after, only intentional
rows remain):

```sh
rg -n '"[^"]*\bif [^(]' src tests
rg -n '"[^"]*for \([^)]*\) [^{]' src tests
rg -n '"for \(' src tests
```

## Validation and acceptance

Quality criteria:

- Tests: `make test` passes; the new tables, properties, features, and
  pipeline test pass; each named red test failed before its milestone.
- Verification: `INV-SEQ`, `INV-TERM`, `INV-RBFOR`, `INV-EXTENT`, and
  `L-LOWER` are discharged by the methods above, with class counts and seeded
  faults recorded in `Artefacts and notes`.
- Lint and format: `make check-fmt` and `make lint` pass with no new
  `#[expect]` except narrowly reasoned ones.
- Documentation: `make markdownlint` and `make nixie` pass.

Quality method: the gate sequence in `Concrete steps`, run sequentially, with
logs under `/tmp`.

## Idempotence and recovery

Every step is a source edit followed by gates; rerunning is safe. Each
milestone ends in a commit, so `git revert` of that commit restores the
previous plateau. Seeded faults are applied in the working tree only, observed,
and reverted with `git checkout -- <file>`; any `proptest-regressions` entries
they produce are deleted.

## Artefacts and notes

Reconnaissance evidence (planning time, upstream `ca5ded82`):

```haskell
-- Parse.hs 600-611: `;` only in eseq
eseq = do ... Just e -> do extra_semi <- optionMaybe semi ...
-- Parse.hs 885: the operator row is commented out
         --,[sbinary ";" ESeq AssocRight]
-- Parse.hs 364-368: the top-level header is an atom
parseForStatement = withPos $ ForStatement nopos <$ reserved "for"
    <*> (symbol "(" *> atom False) <*> (optionMaybe (reserved "if" *> expr))
    <*> (symbol ")" *> statement)
-- Parse.hs 806-813: if and for tails are terms
eite = do reserved "if"; cond <- term; th <- term; ... (reserved "else" *> term)
efor = eFor <$ ... <*> (loop_pattern <* reserved "in") <*> (expr <* symbol ")") <*> term
```

Implementation evidence will be added here by milestone.

## Interfaces and dependencies

No new dependencies. Authorized public API changes (pre-release; every caller
changes in the same milestone):

- `BinaryOp::Seq` is removed (EP-M2).
- `RuleBodyTerm::ForLoop`, `RuleForLoop`, and `Rule::flattened_body_terms`
  are removed (EP-M4).
- `SemanticRule` becomes (EP-M5):

```rust
/// One body term of a lowered rule, in evaluation order.
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticBodyTerm {
    /// A positive relation atom with an optional `v in` binder.
    Literal(SemanticLiteral),
    /// A filter condition: a guard, an `if` condition or its negation, a
    /// `match` arm filter, or a non-assignment `expr in` statement.
    Condition(Expr),
    /// A `pattern = expr in` statement.
    Assignment(RuleAssignment),
}

/// A relation atom read by a lowered rule.
#[derive(Debug, Clone, PartialEq)]
pub struct SemanticLiteral { /* binder: Option<String>, atom: Expr */ }

impl SemanticLiteral {
    pub fn binder(&self) -> Option<&str>;
    pub const fn atom(&self) -> &Expr;
}

impl SemanticRule {
    pub const fn origin(&self) -> SemanticRuleOrigin;
    pub fn source_span(&self) -> Span;
    pub const fn head(&self) -> &Expr;
    pub fn location(&self) -> Option<&Expr>;
    pub fn body(&self) -> &[SemanticBodyTerm];
}
```

New diagnostic message constants (`src/parser/error_messages.rs`):

```rust
pub const SEMICOLON_OUTSIDE_BLOCK_ERROR: &str =
    "`;` separates items only inside a block; wrap the sequence in `{ … }`";
pub const IF_TERM_EXPECTED_ERROR: &str = concat!(
    "an `if` condition and branches are single terms; ",
    "parenthesize the condition and brace the branches",
);
pub const DANGLING_ELSE_ERROR: &str =
    "`else` must directly follow the `if` branch; wrap the branch in `{ … }`";
pub const ASSIGNMENT_TARGET_ERROR: &str =
    "the left-hand side of `=` must be a pattern; wrap a loop body in `{ … }`";
pub const TOP_LEVEL_FOR_BINDER_ERROR: &str =
    "a top-level `for` binds a single lower-case variable before `in`";
pub const TOP_LEVEL_FOR_ATOM_ERROR: &str =
    "a top-level `for` iterates a relation atom: write `for (R(…))` or `for (v in R(…))`";
pub const TOP_LEVEL_FOR_TERMINATOR_ERROR: &str =
    "a top-level `for` statement takes no terminating `.`";
pub const STATEMENT_BLOCK_TRAILING_SEPARATOR_ERROR: &str =
    "statements in a block are separated by `;` with no trailing `;`";
```

`SemanticRuleSpec` changes to match; `patterns()` is removed. Reuse policy:
`SemanticBodyTerm` is owned by `src/parser/ast/semantic_rule.rs` and built only
by `src/parser/top_level_for/lowering.rs`; sema reads it and must not re-parse
source. `StatementParser` is `pub(crate)` in `src/parser/top_level_for/` and is
the only parser of top-level statements; the rules scanner consumes its extents
rather than scanning for statement ends itself. `Pratt::parse_term` is
`pub(super)` within `src/parser/expression/` and is the only definition of an
upstream term.

## Revision note

- 2026-11-23: initial draft after Wyvern reconnaissance and upstream source
  reading. Awaiting expert review.
