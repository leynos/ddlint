# Align `;` sequencing and rule-level `for` with upstream (roadmap 2.6.10)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (expert review applied; awaiting user approval)

Branch: `2-6-10-decide-semicolon-sequencing`

Roadmap item: `2.6.10` in `docs/roadmap.md`.

Implementation must not begin until the user explicitly approves this plan.
Decisions marked "requires user approval" in `Decision log` are approved with
the plan unless the approver says otherwise.

## Purpose / big picture

ddlint is a linter for Differential Datalog (DDlog). Its parser follows a
normative syntax specification,
`docs/differential-datalog-parser-syntax-spec-updated.md`, which in turn
follows the upstream DDlog compiler. Throughout this plan, "upstream" means
`vmware-archive/differential-datalog` at commit
`ca5ded823dc2c01bc33aac44e526c12d2fed03a6`, the commit ADR-0006 pinned. Its
files are readable under the raw-content URL below, chiefly
`src/Language/DifferentialDatalog/Parse.hs`, `Statement.hs`, `Validate.hs`, and
`TypeInference.hs`, and `doc/tutorial/tutorial.md`. Line numbers below refer to
those files.

```plaintext
https://raw.githubusercontent.com/vmware-archive/differential-datalog/ca5ded823dc2c01bc33aac44e526c12d2fed03a6/<path>
```

Roadmap item 2.6.10 asks how `;` sequencing and rule-level `for` should align
with upstream. Reading the upstream parser shows five divergences:

1. **`;` outside braces.** ddlint parses `;` as an infix operator,
   `BinaryOp::Seq`, in every expression position, so `(a; b)`, `f(a; b)`, and
   the rule-body literal `A(x); B(x)` all parse. Upstream has no `;` operator:
   the `etable` row is commented out (`Parse.hs` line 885), and `;` exists only
   as the item separator of a brace block (`eseq`, lines 600-611).
2. **Control-flow tails.** ddlint parses an `if` condition, both `if`
   branches, and a `for` body as full expressions, so in
   `if (c) { a } else { b } ++ x` the `++ x` attaches to the `else` branch.
   Upstream parses each of them as a *term*, a primary expression without unary
   operators, calls, field access, or infix operators (`eite` and `efor`, lines
   799-813), so the `++ x` applies to the whole `if`.
3. **Guards on expression `for`.** ddlint accepts `for (x in xs if g) e` as an
   expression. Upstream's expression `for` has no guard (`efor`, line 813);
   only the top-level statement takes one (line 367).
4. **Rule-body `for`.** ddlint classifies `R(x) :- for (y in Items(y)) Out(y).`
   as a relational loop, `RuleBodyTerm::ForLoop`, and
   `Rule::flattened_body_terms()` turns it into the join `Items(y), Out(y)`.
   Upstream has no such construct: a `for` in a rule body is an ordinary
   expression clause over a collection (`rhsCondition`, lines 429-437) that
   declares nothing for later clauses and that type inference rejects as a
   filter, because a `for` has type `()` (`TypeInference.hs` lines 579 and 853).
5. **Top-level `for`.** ddlint accepts `for (a in A(a) if g) Head(a).` with a
   pattern before `in`, an arbitrary iterable, a terminating `.`, and an
   atom-only body. Upstream parses a *statement* (`parseForStatement` and
   `statement`, lines 364-397): the header is a relation atom with an optional
   `v in` binder (`atom`, lines 480-499), there is no terminating `.`, and the
   body is one of seven statement forms. `convertStatement` (`Statement.hs`
   lines 139-169) lowers a statement into zero or more rules. Upstream's
   grammar accepts `for (a in A(a)) …`; its validator then rejects it because
   `a` is declared twice in one atom (`Validate.hs` lines 401-402).

ddlint is unreleased, so no compatibility with earlier ddlint behaviour is
owed. This plan aligns all five with upstream. A worked example shows the
observable difference. Today:

```plaintext
parse_expression("(a; b)")                       -> Ok, (group (; a b))
parse_expression("if (c) { a } else { b } ++ x") -> (if (group c) (block (value a)) (++ (block (value b)) x))
parse("R(x) :- for (y in Items(y)) Out(y).")     -> body_terms: [ForLoop(..)]
parse("for (a in A(a)) B(a).")                   -> semantic_rules: [B(a) :- A(a)]
```

After this plan:

```plaintext
parse_expression("(a; b)")
  -> error: `;` separates items only inside a block; wrap the sequence in `{ … }`
parse_expression("if (c) { a } else { b } ++ x")
  -> (++ (if (group c) (block (value a)) (block (value b))) x)
parse("R(x) :- S(ys), for (y in ys) { f(y) }.")
  -> body_terms: [Expression(S(ys)), Expression(ForLoop(..))]
parse("for (A(a)) B(a)")
  -> semantic_rules:
       B(a) :- A(a)
parse("for (r in A(a) if a > 0) { if (ok(r)) B(a) else skip; C(a) }")
  -> semantic_rules:
       B(a) :- r in A(a), a > 0, ok(r)
       C(a) :- r in A(a), a > 0
```

The shipped example `examples/imperative_syntax.dl` is already written in
upstream statement syntax and parses with lowered rules after this plan.

Success is observable by running `make test` and seeing the new sequencing,
control-flow, rule-body, and top-level statement suites pass, with register
item 16 reading `implemented`, register item 8 dropping its "known divergence"
paragraph, and roadmap item 2.6.10 ticked.

## Constraints

- Implementation begins only after the user approves this plan.
- Do not change the meaning of any `DiagnosticCode` (`D-REL-001` to
  `D-REL-008`) or `DiagnosticCategory`; the conformance register freezes them.
  New messages are constants in `src/parser/error_messages.rs` under existing
  categories; stable codes wait for roadmap 2.8.1.
- Do not change block expressions (ADR-0006): `{}` is the empty block, `{ e }`
  is a block whose value is `e`, an expression block accepts a trailing `;`, and
  `Expr::Group` is reserved for parentheses.
- Do not change operator binding powers or associativity other than deleting
  `;`. The other upstream operator-table differences are recorded (`SF-D11`),
  not changed.
- Do not change struct-literal or struct-pattern syntax or the struct-literal
  guard in `if` conditions (roadmap 2.6.11); `var` expressions, closures,
  interpolation, or the type grammar (2.6.12); function bodies (2.6.13);
  `x @ pat`, rule-body `v in R(…)` binders, or bit-slice syntax (2.6.14); or
  the lexical-scope model beyond what `SF-D3` and `SF-D7` require (3.3.5). Atom
  arguments in lowered rules stay uses, as in explicit rules; implicit
  declaration by positive atoms is 3.3.5's.
- Keep the parser a pure core: no input/output, logging, metrics, or global
  state; no new port or adapter. Lowering stays a pure function from a parsed
  statement to semantic rules, written as plain structural recursion with no
  depth-dependent state.
- Keep the concrete syntax tree (CST) lossless: every source token still
  appears in the green tree.
- New production code must not panic (`expect`, `unwrap`, `unreachable!`,
  `todo!`, unchecked slice indexing), including on deeply nested input.
- No Rust source file over 400 lines. Every module, including test modules,
  has a `//!` comment. Every `assert!`, `assert_eq!`, and `assert_ne!` has a
  diagnostic message.
- Fixtures are written once: every fixture a milestone touches is rewritten
  directly into its final upstream form (braced loop bodies, parenthesized
  non-term conditions, statement syntax), which parses the same way before and
  after the term-tail milestone.
- Each milestone updates, in the same commit, the specification section,
  register item, and users' guide paragraph for the behaviour it changes, as
  the register's maintenance rule requires.
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

Base: `git merge-base HEAD origin/main` for the stack's first pull request, and
the parent branch for each later one (at drafting, `7adbd8bc`). Count with:

```sh
git diff --numstat "$(git merge-base HEAD origin/main)" -- . ':!docs/execplans/'
```

Classify whole files by path. Code is `src/`, `tests/`, `cli/`, `crates/`, and
`examples/` (production source, tests, fixtures, generators, and example
programs). Configuration is `Cargo.toml`, `Cargo.lock`, `Makefile`, `.github/`,
and `typos.local.toml`. Documentation is `docs/` (excluding `docs/execplans/`)
and other `*.md` files. Limits are net added lines (additions minus deletions)
over the whole stack. This ExecPlan is excluded from every count. Each category
is enforced on its own; spare capacity in one category does not offset another.

- Code: stop and escalate above 75 files or 3,000 net added lines across the
  stack, or above 35 files in any one pull request.
- Configuration: stop on any change except narrow `typos.local.toml`
  exceptions.
- Documentation: stop above 15 files or 1,300 net added lines.
- Interface: the authorized public API changes are listed under
  `Interfaces and dependencies`. Stop on any other public signature change.
- Dependencies: none. Stop before adding any.
- Grammar: stop if any input changes its parse other than through the
  decisions `SF-D1` to `SF-D7`, `SF-D14`, and `SF-D16`.
- Iterations: stop after three failed fix attempts on one gate.
- Time: stop if a milestone runs eight working hours past its first
  `Progress` timestamp.

The budget comes from the survey in `Context and orientation` and the review
counts. Comparable plans repeatedly under-estimated: roadmap 3.3.1 budgeted 12
files and 800 lines and shipped 24 files and 2,595 insertions; 2.6.7 grew from
14 to 54 files; 2.6.8 budgeted 30 files and 1,600 net lines and shipped 44
files and 3,120 net lines, an overage concentrated in rewritten fixtures. The
review counted about 98 unbraced `for` lines across 27 files and about 50
existing files plus 15 new ones touched, so this plan budgets 75 files and
2,000 estimated net lines with 50 % contingency, and splits delivery into
stacked pull requests (`SF-D15`) so no single review exceeds 35 code files.

## Risks

- Risk: a top-level statement's extent is computed wrongly now that there is
  no terminating `.`, swallowing the next declaration, dropping text after the
  statement, or leaving stray tokens. Severity: high. Likelihood: medium.
  Mitigation: the statement parser runs inside the rules scanner, which owns
  extents (`SF-D6`); statement ends are line-start boundaries; head atoms use
  the upstream atom grammar with no postfix or infix tail; `EP-M0` probes the
  hazards; `INV-EXTENT` and `INV-COVER` cover them.
- Risk: lowering one statement into several rules creates duplicate or false
  `unused-variable` reports. Severity: high. Likelihood: high without a
  mitigation. Mitigation: a statement scope declares each binder once, rule
  scopes are its children, and atom arguments stay uses (`SF-D7`); pipeline
  tests pin the outcome for multi-rule statements.
- Risk: term tails break many fixtures. Severity: medium. Likelihood:
  certain. Mitigation: term tails come last (`EP-M6`), after every fixture
  touched by earlier milestones is already in final form; failing tests, not
  regular expressions, list the remaining fixtures.
- Risk: `for (x in xs) f(x)` would parse silently as a call on the loop.
  Severity: medium. Likelihood: medium. Mitigation: `SF-D16` rejects a postfix
  form on a `for` at parse time, since upstream type inference always rejects
  it.
- Risk: removing `RuleBodyTerm::ForLoop` changes `unused-relation` outcomes,
  because a relation named in a rule-body `for` iterable is no longer a
  relation read. Severity: medium. Likelihood: certain. Mitigation: this is the
  upstream meaning; `EP-M3` changes the fixtures deliberately with a named
  regression test, and the users' guide says so.
- Risk: aggregation checks inside rule-body `for` bodies regress. Severity:
  medium. Likelihood: medium. Mitigation: `SF-D3` keeps aggregation counting
  inside `for` expressions; the existing aggregation tests stay green.
- Risk: deeply nested statements overflow the stack. Severity: medium.
  Likelihood: low. Mitigation: `INV-DEPTH` caps statement nesting with a
  diagnostic, and `parse_term` counts towards `MAX_EXPR_DEPTH`.
- Risk: the stack of pull requests stalls part-way. Severity: medium.
  Likelihood: low. Mitigation: each pull request ends on a coherent plateau
  with its own specification, register, and guide updates.
- Risk: `make test` hits Rayon `EAGAIN` on the shared host. Severity: low.
  Likelihood: medium. Mitigation: rerun once with
  `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1`.

## Progress

- [x] (2026-11-23T17:40Z) Branch renamed to
  `2-6-10-decide-semicolon-sequencing`; Lody session titled.
- [x] (2026-11-23T18:05Z) Reconnaissance by three Wyvern agents (`;`,
  top-level `for`, rule-body `for`) and an upstream-grammar researcher.
- [x] (2026-11-23T18:40Z) First draft written and committed.
- [x] (2026-11-23T19:50Z) Expert design review (upstream fidelity, structure
  and contracts, failure modes and scaling, verification, alternatives and
  maintainability) received; verdict "proceed with conditions".
- [x] (2026-11-23T20:30Z) Review findings applied (see `Revision note`).
- [ ] Plan approved by the user.
- [ ] `EP-M0` extent and backtracking probe (prototype, not committed).
- [ ] `EP-M1` decision records, specification targets, register, roadmap.
- [ ] `EP-M2` `;` only inside braces.
- [ ] `EP-M3` rule-body `for` is an expression clause.
- [ ] `EP-M4` top-level statements: header, `for`, insert, block, `skip`,
  extent, CST node, new `SemanticRule` API, statement scope; expression `for`
  loses its guard.
- [ ] `EP-M5` top-level statements: `if`, `match`, `expr in`, equation table,
  properties.
- [ ] `EP-M6` `if` and `for` tails are terms.
- [ ] `EP-M7` close-out: design lemmas, stale-claim sweep, roadmap tick.

## Surprises & discoveries

- Observation: upstream has no terminating `.` after a top-level `for`
  statement. Evidence: `decl` adds `parseForStatement` outside `withPosMany`,
  with no `dot` (`Parse.hs` line 277); the tutorial writes
  `for (person in Person) TargetAudience(person)`. Impact: statement extent
  must come from the grammar (`SF-D6`).
- Observation: upstream's `term` excludes calls, unary operators, and
  negative literals. Evidence: `postApply` is a postfix entry in `etable` (line
  859), the prefix operators are separate rows (lines 860-863), and `term`
  lists only primaries (lines 569-592). Impact: `if (c) f(x) else g(x)` and
  `if (c) -1 else 1` are parse errors upstream, while a bare term condition
  such as `if c { a }` is legal (`SF-D2`).
- Observation: upstream's expression `for` has no guard (`efor`, line 813).
  Impact: `SF-D14`.
- Observation: a rule-body `for` is a filter condition upstream and fails
  type inference. Impact: ddlint's relational rule-body loop has no upstream
  counterpart (`SF-D3`).
- Observation: statement blocks reject a trailing `;` (`semiSep`, line 381),
  while expression blocks accept one (`eseq`). Impact: the two block forms keep
  different separator rules (`SF-D6`).
- Observation: `statement` is an ordered choice whose alternatives commit at
  different points: `for`, `skip`, `if`, and `match` commit on their keyword;
  `try (expr <* "in")` is tried before insert and block, so `x in B(x)` and
  `{ e } in s` are assignment statements. Impact: `SF-D6` transcribes the order
  and commit points.
- Observation: a `match` statement lowers each arm to a boolean `match`
  filter (`explodeMatchCases`, `Statement.hs` lines 129-132), so an arm's
  pattern variables are not visible in the arm's rules, and upstream validation
  rejects a non-exhaustive `match` statement. Impact: lowering reproduces the
  filter; the users' guide states both consequences.
- Observation: statement guards and `expr in` statements are plain
  expressions upstream (lines 367 and 391), so `group_by` is never extracted
  inside a top-level `for`. Impact: lowering yields no aggregation terms
  (`SF-D7`).
- Observation: an insert statement's location is a full expression
  (`rulelhs`, line 411), so `B(a) @ l` followed by a line starting
  `&C(x) :- D(x).` reads `l & C(x)` upstream. Impact: ddlint reproduces it; an
  `INV-EXTENT` row pins the behaviour.
- Observation: ddlint parses no bracket-form atom `R[e]` anywhere today; a
  `[` postfix is a bit slice that requires a comma (`pratt/postfix.rs`).
  Impact: the header does not accept `R[e]` either; it follows the bit-slice
  decision in 2.6.14.
- Observation: the upstream tutorial's main join,
  `for (person in Person(.region = region.id))`, uses a named argument. Impact:
  it parses only after 2.6.11; a row pins the deferred diagnostic.
- Observation: the upstream operator table differs from ddlint's beyond `;`
  (list in `SF-D11`). Impact: register item 23 and roadmap 2.6.16.
- Observation: upstream extracts a `group_by` call anywhere inside a rule
  clause (`exprFoldM extractGroupBy`, lines 429-448); ddlint classifies only
  top-level calls and calls in rule-body `for` bodies. Impact: register item 24
  and roadmap 2.6.17.
- Observation: documentation drift. The specification's `ForStmt` EBNF puts
  the guard after `)`; section 5.13 spells `if … then … else`; section 6.5 says
  rule-body loops are `Expr::ForLoop`; section 9.1 and register item 9 cite
  `flattened_body_terms`; `docs/users-guide.md` says `Expr::Group` carries
  sequences outside braces; `docs/haskell-parser-analysis.md` describes
  `parseForStatement` as `for (pat in expr)`, the misreading behind ddlint's
  header; `docs/developers-guide.md` describes the flattening helper
  `assert_flattened_expression_terms`; and `tests/rule_behaviour/for_loops.rs`
  calls `;` right-associative when it is left-associative. Impact: corrected in
  the milestone that changes each behaviour.
- Observation: sema declares no pattern bindings for an `Expr::ForLoop`
  nested inside another expression (`src/sema/variables.rs`,
  `walk_for_loop_uses`). Impact: `SF-D3` routes every `for` expression through
  one loop-scope path.
- Observation: the line-start helpers treat only a newline or a preceding `.`
  as a boundary (`src/parser/span_scanners/rules_support.rs` and
  `src/parser/top_level_for/mod.rs`). Impact: without a terminating `.`, a
  declaration on the same line after a statement would be lost unless a
  statement's end is also a boundary (`SF-D6`).

## Decision log

- SF-D1. Decision: `;` appears only as a block-item separator. Delete
  `BinaryOp::Seq` (and its `SYMBOLS` entry, precedence row, and s-expression
  arm). `parse_infix` meeting `;` outside a block item reports
  `SEMICOLON_OUTSIDE_BLOCK_ERROR` at the `;` span, consumes the `;`, parses and
  discards the following item at the same minimum binding power, and returns
  the left operand, so the input yields exactly one diagnostic per stray `;`
  and no follow-on "expected `)`" error. The scoped flag is renamed
  `in_block_item`; its set and clear points from ADR-0006 `BG-D11` are
  unchanged. This covers the rule-body literal `A(x); B(x)`, `return x; y`, and
  `x ;`. Rationale: faithful to `eseq` and the commented-out `etable` row.
  Alternative: keep `Seq` as a documented ddlint extension; rejected because
  the specification's compatibility policy forbids silent extensions and
  nothing depends on it. Date/Author: 2026-11-23, planning agent; requires user
  approval.
- SF-D2. Decision: the `if` condition, the `then` and `else` branches, and
  the `for` body parse as terms through a new `Pratt::parse_term`, matching
  upstream `term`: literals (including interned strings), variables, qualified
  names, `_`, constructors and struct literals, parenthesized groups and
  tuples, blocks, vector and map literals, `match`, `if`, `for`, `break`,
  `continue`, `return`, and closures. Unary operators (so negative literals),
  postfix forms, and infix operators are not part of a term; after the
  control-flow expression they apply to the whole expression. A bare term
  condition such as `if c { a }` stays legal. `match` arms, closure bodies, and
  `return` values stay full expressions. `parse_term` counts towards
  `MAX_EXPR_DEPTH`. ddlint keeps its struct-literal activation and suspension
  inside terms, a ddlint policy owned by 2.6.11. Upstream's `elhs` rule (an
  lvalue such as `acc.f` followed by `=` is a term) is not reproduced; it
  changes only which error an ill-formed assignment reports, and register item
  16 records it as a bounded divergence. An operator where a term is expected
  after `if` reports `IF_TERM_EXPECTED_ERROR`; an `else` that does not directly
  follow the `then` term reports `DANGLING_ELSE_ERROR`, through one shared
  unexpected-token helper used both by the top-level drain and by delimiter
  `expect` calls. The specification's `ExprIf` becomes
  `'if' Term Term ('else' Term)?`. Rationale: upstream's comment at `Parse.hs`
  lines 799-805 states the intent; register item 16 assigns it here.
  Alternative: schedule term tails separately; rejected because the write-once
  fixture rule makes the incremental cost small once they run last.
  Date/Author: 2026-11-23, planning agent; requires user approval.
- SF-D3. Decision: a rule-body `for` is an ordinary expression clause.
  Delete `RuleBodyTerm::ForLoop`, `RuleForLoop`, and
  `Rule::flattened_body_terms()`; a `for` literal classifies as
  `RuleBodyTerm::Expression(Expr::ForLoop { .. })` and `var v = for (…) …` as
  an assignment. Aggregation counting still descends into `for` bodies, so the
  existing multiple-aggregation errors stand. sema opens a `ScopeKind::ForLoop`
  scope for every `Expr::ForLoop` wherever it appears: the iterable is a use in
  the enclosing scope, and the pattern declares into the body only. A relation
  name inside a `for` iterable is an expression, not a relation read;
  `UseOrigin::ForIterable` and `UseOrigin::ForGuard` are removed. Rationale:
  upstream has no relational rule-body loop; specification section 8.1 already
  says a rule-body `for` declares nothing for later clauses. Alternative: keep
  relational flattening as a ddlint extension; rejected for the same policy
  reason as `SF-D1`. Date/Author: 2026-11-23, planning agent; requires user
  approval.
- SF-D4. Decision: rule-level `=` stays the assignment split. With term
  bodies, `for (x in xs) y = f(x)` assigns to a loop expression, which
  upstream's `checkLExpr` rejects (`Validate.hs` lines 793-794); ddlint reports
  `ASSIGNMENT_TARGET_ERROR` instead of a raw pattern-parse error. Rationale:
  upstream `=` is the lowest-precedence non-associative operator, so a depth-0
  `=` in a clause is always the assignment. Date/Author: 2026-11-23, planning
  agent.
- SF-D5. Decision: the top-level header is
  `for ( (LcName 'in')? HeaderAtom ('if' Expr)? )`, where `HeaderAtom` is
  upstream's `atom False`: an optional `&`, a relation name (plain or
  qualified), an optional delay `-<n>`, an optional diff mark `'`, and either a
  parenthesized argument list or nothing (a bare `R` matches every record).
  Arguments are expressions parsed by `Pratt` inside the parentheses. The
  bracket form `R[e]` follows the bit-slice decision in 2.6.14, and named
  arguments (`R(.f = e)`) follow 2.6.11; both report `TOP_LEVEL_FOR_ATOM_ERROR`
  until then, pinned by rows. `not` before the atom is a parse error.
  `for (a in A(a))` parses, because upstream's grammar accepts it; the double
  declaration is a scope error assigned to roadmap 4.1.5, which covers
  rule-body binders too. A binder that is not a single lower-case name reports
  `TOP_LEVEL_FOR_BINDER_ERROR`. An attribute before a top-level `for` is
  rejected, as upstream does (`Parse.hs` lines 282-284). Rationale:
  `parseForStatement` calls `atom False`, whose only binder is
  `varIdent <* "in"`. Date/Author: 2026-11-23, planning agent; requires user
  approval.
- SF-D6. Decision: the body is an upstream statement, with no terminating
  `.`, parsed by ordered choice with upstream's commit points:
  1. `for` (commits on the keyword): a nested `ForStmt`.
  2. `skip` (commits): no rules.
  3. `if` (commits): `'(' Expr ')' Statement ('else' Statement)?`; a missing
     parenthesis reports `STATEMENT_IF_PAREN_ERROR`.
  4. `match` (commits): `'(' Expr ')' '{' Arm (',' Arm)* ','? '}'` with at
     least one arm, `Arm ::= Pattern '->' Statement`; a comma-less arm list is
     an error, as in the compiler (the tutorial's comma-less example does not
     compile).
  5. Assignment (backtracking): an expression, or `Pattern '=' Expr`, followed
     by `in`, then a statement. It is recognized at token level: a depth-0 `in`
     before the next statement boundary, with an optional depth-0 `=` before
     it splitting pattern from expression (so `var` patterns work before
     2.6.12). If no such `in` exists, the parser rewinds.
  6. Insert: a `HeaderAtom`-style head atom (`&`, name, delay, diff mark,
     arguments) with an optional `'@' Expr` location; head atoms accept and
     reject exactly what rule heads do (a `&` head lowers through the shared
     `ref_new` helper).
  7. Block: `'{' (Statement (';' Statement)*)? '}'`; a trailing `;` reports
     `STATEMENT_BLOCK_TRAILING_SEPARATOR_ERROR`; `{}` yields no rules.

  A `.` directly after a statement reports `TOP_LEVEL_FOR_TERMINATOR_ERROR` and
  is consumed for recovery. Statement nesting is capped at
  `MAX_STATEMENT_DEPTH` (256, matching `MAX_EXPR_DEPTH`) with
  `STATEMENT_DEPTH_ERROR`. The statement parser is called once, from the
  `K_FOR` arm of `collect_rule_spans` in `src/parser/span_scanners/rules.rs`,
  which already knows whether a `for` sits inside a rule body; it returns the
  extent (the end of the last consumed token), the tree, and diagnostics. The
  end of a statement is a line-start boundary, so a declaration on the same
  line after it is scanned normally. The parse never crosses the start of the
  next span claimed by an earlier scanner. Recovery from a malformed statement
  resynchronizes at the first column-0 token that can start a declaration,
  ignoring delimiter depth, and never past the next claimed span. This replaces
  `skip_top_level_for_statement`, `collect_desugared_top_level_for_rules`,
  `top_level_for_exclusions`, and the quadratic longest-prefix search.
  Rationale: faithful to `statement` (`Parse.hs` lines 370-397), with a single
  owner for extents. Date/Author: 2026-11-23, planning agent; requires user
  approval.
- SF-D7. Decision: lowering follows `convertStatement` and yields rules in the
  source order of their insert statements. `SemanticRule` replaces `patterns()`
  and `body() -> &[Expr]` with ordered, spanned `SemanticBodyTerm`s:
  `Literal(SemanticLiteral { binder, atom, span })` for a header atom;
  `Condition { expr, span }` for a guard, an `if` condition (with
  `Unary(Not, c)` for `else`, no added group), a `match` arm filter, or an
  `expr in` statement without a depth-0 `=`; and
  `Assignment { assignment: RuleAssignment, span }` for `pat = e in …`. The
  `Asg`/`Cond` split is ddlint's representation, matching its own
  `split_assignment`; erasing `Assignment(p, v)` to `Condition(p = v)` gives
  upstream's `RHSCondition` exactly. Each rule carries `statement_index` (which
  top-level statement produced it), `source_span` (the whole statement), `head`,
  `head_span`, and an optional `@` location. Lowering produces no aggregation
  terms, and the rule-body classifier never runs on statement terms, because
  upstream never extracts `group_by` in statements. `SemanticRule::new` and
  `SemanticRuleSpec` become `pub(crate)`; only lowering builds them. sema opens
  one statement scope per top-level statement (child of the program scope) that
  declares each binder once with `SymbolOrigin::ForStatementBinder`; each
  lowered rule's scope is a child of it, heads declare as today, atom arguments
  and conditions are uses, and an assignment declares its pattern names for
  later terms of the same rule. Rationale: typed, spanned terms let sema avoid
  re-parsing and pin diagnostics precisely; one statement scope prevents
  duplicate binder warnings. Alternative: keep `Vec<Expr>` and add an `Expr`
  binder node; rejected because 2.6.14 owns the general `v in R(…)` expression
  form. 2.6.14 and 2.9.1 should merge `SemanticBodyTerm` with `RuleBodyTerm`
  when the binder form lands, so two term enums do not harden into the sema
  model. Date/Author: 2026-11-23, planning agent.
- SF-D8. Decision: no new `DiagnosticCode`; statement messages use
  `DiagnosticCategory::TopLevelFor` inside the existing top-level attempt, so
  observer event order is unchanged, and expression messages keep their
  existing categories. `UNSUPPORTED_TOP_LEVEL_FOR_BODY`,
  `UNSUPPORTED_TOP_LEVEL_FOR_HEAD`, and `UNTERMINATED_TOP_LEVEL_FOR` are
  deleted. Rationale: ADR-0006 `BG-D7` precedent; stable codes wait for 2.8.1.
  Date/Author: 2026-11-23, planning agent.
- SF-D9. Decision: no Verus proof in this item, and `make verus` is not an
  acceptance command here; it keeps running the smoke proof. The lowering's
  lemmas are recorded in `docs/ddlint-design.md` as a contract checked by tests
  and unproved, and as a deferred Verus candidate if the lowering kernel is
  later reshaped as an index arena suitable for the bridge planned in roadmap
  3.3.6. Rationale: see `Formal verification decision`. Date/Author:
  2026-11-23, planning agent; requires user approval.
- SF-D10. Decision: record the decisions in
  `docs/adrs/adr-0007-semicolon-sequencing-and-for-statements.md`, referenced
  from `docs/ddlint-design.md` and indexed in `docs/contents.md`, and amend
  ADR-0006 with an "Amended by ADR-0007" banner and an `## Amendments` entry
  (`BG-D3` named `BinaryOp::Seq`, and `BG-D11`'s propagation through `if` and
  `for` tails becomes moot). The 2.6.8 plan informally reserved ADR-0007 for
  the lexical-scope model (3.3.5), but no live document records that
  reservation, so 3.3.5 takes the next free number. Date/Author: 2026-11-23,
  planning agent; requires user approval.
- SF-D11. Decision: record two divergences found during planning as new
  register items and roadmap entries rather than fixing them here. Register
  item 23 and roadmap 2.6.16, status `decision needed`: "Decide whether to
  align the operator table with upstream `etable`": upstream makes `<`, `<=`,
  `>`, `>=` non-associative; puts `=` lowest and non-associative (ddlint puts
  it above `=>` and right-associative); makes `=>` left-associative (ddlint:
  right); has `:` and `as` as tightest postfix operators (ddlint: infix at the
  type level); places `++` in its own row below the shifts; places the bitwise
  operators below the comparisons; and gives each prefix operator its own row
  (`&`, then `-`, `~`, `not`). Register item 24 and roadmap 2.6.17, status
  `decision needed`: "Decide whether `group_by` extraction descends into nested
  expressions as upstream `extractGroupBy` does". Both sit in section 2.6, so
  they gate 2.7 under its existing preamble. Date/Author: 2026-11-23, planning
  agent; requires user approval.
- SF-D12. Decision: add a CST node kind `N_FOR_STATEMENT` wrapping each
  top-level statement's extent, conditional on `EP-M0` showing the CST builder
  accepts one more span list without restructuring. Rationale: the extent is
  computed anyway; the node gives lints, sema, and autofix a handle and
  replaces bare root-level tokens. If the probe shows a restructure is needed,
  tokens stay bare children of `N_DATALOG_PROGRAM` and the decision is
  recorded. Date/Author: 2026-11-23, planning agent; requires user approval.
- SF-D13. Decision: the end-to-end boundary is the public library pipeline
  (`parse()` to `sema::build()` to the lint `Runner`), because the `ddlint`
  binary is still a placeholder (`cli/tests/placeholder_cli.rs`). The shipped
  `examples/imperative_syntax.dl` is the end-to-end fixture. Date/Author:
  2026-11-23, planning agent.
- SF-D14. Decision: an expression `for` has no guard. Remove the `guard`
  field from `Expr::ForLoop`; `for (x in xs if g) e` in an expression reports
  `FOR_GUARD_ERROR`. The top-level statement keeps its guard (`SF-D5`).
  Rationale: upstream `efor` (line 813) has none, and rule-body `for` becomes
  an expression here. Date/Author: 2026-11-23, planning agent; requires user
  approval.
- SF-D15. Decision: deliver the approved plan as a stack of pull requests,
  each a coherent plateau: (A) `EP-M1`; (B) `EP-M2`; (C) `EP-M3`; (D) `EP-M4`
  and `EP-M5`; (E) `EP-M6` and `EP-M7`. Roadmap 2.6.10 is ticked only by (E);
  no roadmap sub-items are added. Rationale: comparable items ran two to three
  times over budget; smaller reviews and per-plateau rollback outweigh four
  extra review rounds. Alternative: one pull request, as 2.6.8 did; rejected on
  reviewability. Date/Author: 2026-11-23, planning agent; requires user
  approval.
- SF-D16. Decision: a postfix form (call, method call, field access, tuple
  index, index, or bit slice) applied to a `for` expression reports
  `FOR_POSTFIX_ERROR` at parse time. Rationale: a `for` has type `()`, so
  upstream type inference rejects every such program; reporting it at the parse
  stops `for (x in xs) f(x)` silently meaning `(for (x in xs) f)(x)`. This
  narrows what ddlint accepts to what upstream accepts, so it is not an
  extension. Date/Author: 2026-11-23, planning agent; requires user approval.

## Outcomes & retrospective

Not started.

## Context and orientation

Inspected base: `7adbd8bc` ("codify brace blocks and adopt upstream map
literals (2.6.8)"). Line numbers below refer to that revision; relocate symbols
with `rg -n '<symbol>' src`.

### Expression parsing and `;`

The expression parser is a handwritten Pratt parser in `src/parser/expression/`.
`Pratt` (`pratt/mod.rs`, `pub(super)`) runs a prefix production (`prefix.rs`,
dispatching `if`, `match`, and `for` to `control_flow.rs`), then postfix forms
(`pratt/postfix.rs`: calls, method calls, field and tuple access, bit slices,
`AtomDiff`, `AtomDelay`), then `Pratt::parse_infix` (`infix.rs`). Qualified
names resolve in prefix while their argument lists are postfix. Binding powers
live in `src/parser/ast/precedence.rs`; `;` is `BinaryOp::Seq` with
`BP_SEQ = 0` (left-associative), and `BLOCK_ITEM_MIN_BP = BP_SEQ + 1`.
`BinaryOp` is public (`src/parser/ast/expr/mod.rs`); `BinaryOp::SYMBOLS` is
indexed by discriminant and length-checked by a `const` assertion, so deleting
`Seq` must delete its entry. `parse_with_context` (`pratt/mod.rs`) drains
unexpected trailing tokens into errors, consulting `rejection_for`.
`MAX_EXPR_DEPTH = 256` bounds expression recursion. `TokenStream`
(`token_stream.rs`) wraps a `Clone` iterator and offers `peek_span`.

Blocks (`src/parser/expression/block.rs`, `src/parser/ast/expr/block.rs`) never
build `Seq`. While a block item is parsed, the flag
`Pratt::semicolon_terminates` makes `parse_infix` stop at `;`. It is set by
`with_block_item_scope` and cleared by `with_nested_delimiter_scope` and
`with_guard_expression_scope`; `if` conditions and branches, `for` bodies,
`return` values, and pipe-closure bodies inherit it. Outside blocks the flag is
false and `;` builds `Seq` in parentheses, call arguments, collection items,
struct fields, `match` arms, iterables, guards, and rule-body literals. No code
outside `Pratt` that relies on `;` (the relation-suffix, primary-key, typedef,
and function span scanners) reaches `parse_infix`.

Tests asserting `Seq` (13 cases): `src/parser/ast/expr/tests.rs`,
`src/parser/tests/control_flow.rs` (`"return; x"`),
`src/parser/tests/operator_precedence.rs` (four cases),
`src/parser/tests/block_expressions/mod.rs` (`parenthesized_sequence`,
`nested_delimiters_still_allow_sequence`), `tests/rule_behaviour/for_loops.rs`
(two), plus `"x ;"` in `src/parser/tests/expression/characterization.rs` and
`src/parser/tests/expression/fixtures/errors.rs`, and generators in
`src/parser/tests/block_expressions/props.rs` and `strategy.rs` that emit
`(a; b)` and a `for (x in xs) f(x)` tail.

### Control-flow tails

`control_flow.rs`: `if` parses its condition with `parse_if_condition` under
struct-literal activation and each branch with `parse_if_clause`
(`parse_expr(0)`); a missing `else` yields `Expr::Tuple(vec![])`. `for` parses
`( pattern in iterable [if guard] )` and then a body with `parse_expr(0)` and
struct literals suspended. `match` arms use `parse_expr(0)` inside a nested
delimiter scope. AST shapes:
`Expr::IfElse { condition, then_branch, else_branch }` and
`Expr::ForLoop { pattern, iterable, guard, body }`. S-expressions: `(if c t e)`,
`(group e)` for parentheses, and `(block (stmts …) (value …))` for blocks
(`src/parser/ast/expr/sexpr.rs`).

### Rule bodies

`collect_rule_spans` (`src/parser/span_scanners/rules.rs`) finds rule spans; its
`rule_statement` sub-grammar (with `rules_assign.rs`) is a permissive extent
finder for statement-style `for`, `if`, and `skip` inside rule bodies, tested in
`src/parser/span_scanners/tests/rule_statement_tests.rs`; it splits literals on
`,` only. `Rule::body_terms()` (`src/parser/ast/rule/mod.rs`, 399 lines)
classifies each literal through `classification::parse_rule_body_term`, which
first tries `split_assignment` (a depth-0 `=` from `find_top_level_eq_span` in
`src/parser/delimiter.rs`) and then `parse_expression`. `classify_expression`
turns `Expr::ForLoop` into `RuleBodyTerm::ForLoop(RuleForLoop)`, descending
into bodies through `classify_for_body_with_aggregation_tracking`.
`Rule::flattened_body_terms` expands those terms. Consumers:
`src/sema/traverse.rs` (`collect_for_loop_term`), `src/sema/model.rs`
(`UseOrigin::ForIterable`, `UseOrigin::ForGuard`, public),
`src/parser/cst_builder/mod.rs`, `src/parser/mod.rs`, and the `unused_relation`
lint through sema. Tests: `src/parser/ast/rule/tests.rs`,
`tests/rule_behaviour/for_loops.rs`, `tests/rule_behaviour/body_bounds.rs`,
`src/sema/tests/mod.rs`, `src/sema/tests/name_span.rs`,
`tests/unused_relation_rule.rs` (line 79 relies on `ForGuard` marking a
relation read), and `tests/unused_variable_rule.rs`.

### Top-level `for`

There is no CST node. `collect_rule_spans` sends a line-initial `for` to
`skip_top_level_for_statement`, which skips to a terminating `.`. Separately,
`collect_desugared_top_level_for_rules` (`src/parser/top_level_for/mod.rs`,
called from `src/parser/mod.rs` after `top_level_for_exclusions`) tries every
later terminating `.` and keeps the longest prefix that parses as
`Expr::ForLoop`, an O(F·N²) search, then `lower_top_level_for` (`lowering.rs`)
emits one `SemanticRule`. `UNSUPPORTED_TOP_LEVEL_FOR_BODY`,
`UNSUPPORTED_TOP_LEVEL_FOR_HEAD`, and `UNTERMINATED_TOP_LEVEL_FOR` use
`DiagnosticCategory::TopLevelFor`. `SemanticRule`
(`src/parser/ast/semantic_rule.rs`) is public with `origin()`, `source_span()`,
`patterns()`, `head()`, and `body()`; `Parsed::semantic_rules()` is in
`src/parser/cst_builder/mod.rs`; sema consumes it in `src/sema/builder.rs`
(`collect_semantic_rules`, 359 lines), declaring head bindings and `for`
patterns per rule. Rule-head helpers in `src/parser/ast/rule_head/` work on
text slices whose end is known (`find_rule_head_end`); only `lower_by_ref_head`
is reusable. The match pattern parser is `parse_pattern_tokens`
(`src/parser/pattern/mod.rs`). Tests: `src/parser/tests/rules/top_level_for.rs`,
`tests/top_level_for_desugaring.rs`, `src/sema/tests/`,
`tests/semantic_scope_resolution.rs`, `tests/block_expressions.rs`,
`tests/parser_observability.rs` (lines 118-134 assert observer event order), and
`src/parser/tests/reserved_tokens/mod.rs`.

### Documentation touched

`docs/differential-datalog-parser-syntax-spec-updated.md` sections 3.4, 4, 5.8,
5.10, 5.13, 6.5, 8.1, 9.1, 10, and 12; `docs/parser-conformance-register.md`
items 8, 9, and 16 (and new 23 and 24); `docs/roadmap.md` 2.6.10 (and new
2.6.16 and 2.6.17); `docs/parser-implementation-notes.md`;
`docs/ddlint-design.md`; `docs/users-guide.md`; `docs/developers-guide.md`;
`docs/haskell-parser-analysis.md`; `docs/contents.md`; ADR-0006; and the new
ADR-0007.

### Tooling

Tests use `rstest`, `rstest-bdd` and `rstest-bdd-macros` 0.6.0 (feature files
under `tests/features/`, for example `block_expressions.feature` driven by
`tests/block_expressions_bdd.rs`, which already has a
`Given the expression source {source:string}` step and tags scenarios),
`proptest` 1.6 (deterministic class-coverage samples via
`TestRunner::deterministic()`, as in
`src/parser/tests/block_expressions/props.rs`), `insta` 1.47.2 (pattern:
`src/parser/tests/block_expression_snapshots.rs`), `googletest` 0.14.4 (pattern:
`src/parser/ast/relation/primary_key/tests.rs`), and `pretty_assertions`
1.4.1, all existing dev-dependencies. `make verus` runs the pinned
`prover-tools` Verus lane on `verus/smoke/main.rs`, outside `make test` and
`make lint`.

### Budget survey

Production modules affected: `top_level_for/` (462 lines), `control_flow.rs`
(323), `infix.rs` (58), `pratt/mod.rs` (305), `precedence.rs`,
`ast/expr/mod.rs`, `ast/rule/mod.rs` (399, at the limit; shrinks when
`RuleForLoop` goes) and `classification.rs` (341), `semantic_rule.rs` (74),
`sema/builder.rs` (359), `sema/traverse.rs` (225), `sema/variables.rs` (329),
`sema/model.rs` (321), `span_scanners/rules.rs` (379), and `cst_builder/`. New
modules keep every file under 400 lines:
`src/parser/expression/statement/{mod,header,dispatch,block}.rs`,
`src/sema/semantic_rules.rs`, and `src/sema/loop_scope.rs`. Estimated net code:
`EP-M2` +150, `EP-M3` -50, `EP-M4` +900, `EP-M5` +600, `EP-M6` +250, `EP-M7` 0,
about 1,850 to 2,000 net. Documentation estimate: ADR-0007 240, ADR-0006
amendment 20, specification 280, register 90, roadmap 25, notes 70, design 100,
users' guide 150, developers' guide 50, Haskell analysis 20, contents 3, about
1,050 net.

### Relevant skills and documents

Skills: `execplans`; `rust-router`, then `rust-state-machines` for the
statement parser and lowering, `rust-unit-testing` and `proptest` for tests,
`rust-errors` for diagnostic constants, and `rust-verification` and `verus` for
the proof decision; `hexagonal-architecture` (the parser stays a pure core);
`arch-decision-records` for ADR-0007; `mapsplice` for inserting roadmap 2.6.16
and 2.6.17; `addressing-whitaker-findings` for `make lint`; `commit-message`;
`codegraph-mcp` (unavailable during drafting; use `rg`); `firecrawl-mcp` for
upstream sources; and `en-gb-oxendict-style`. Documents:
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
  including the source-of-truth policy (`SPEC-POLICY`) and section 13's
  compatibility policy.
- `docs/parser-conformance-register.md` items 8 and 16 at `7adbd8bc`
  (`REG-8`, `REG-16`).
- `docs/roadmap.md` item 2.6.10 at `7adbd8bc`.
- ADR-0006, decisions `BG-D3`, `BG-D8`, `BG-D9`, and `BG-D11`.
- `docs/execplans/2-6-8-decide-brace-group-extension-policy.md` Appendix A,
  rule 10 (historical input, not modified).
- Upstream DDlog at the pinned commit (see `Purpose / big picture`). Where the
  language reference disagrees with the compiler (it lists `expr ";" expr`, a
  `for (expr in atom …)` header, and comma-less `match` statements), the
  compiler is followed.

No Terms of Reference document exists for this item.

Requirements:

- `REQ-SEQ`: `;` appears only as a block-item separator (`eseq`).
- `REQ-TERM`: the `if` condition and branches and the `for` body are terms;
  trailing operators apply to the whole control-flow expression.
- `REQ-FOR-EXPR`: an expression `for` has no guard and is never the target
  of a postfix form.
- `REQ-RBFOR`: a rule-body `for` is an expression clause that declares
  nothing for later clauses.
- `REQ-FTL-HEADER`: a top-level header is an atom with an optional `v in`
  binder and an optional guard.
- `REQ-FTL-STMT`: the top-level body is an upstream statement with no
  terminating `.`.
- `REQ-FTL-LOWER`: lowering matches `convertStatement`.
- `REQ-DOC`: specification, register, roadmap, ADRs, and guides agree with
  the implementation.

Gaps (baseline to target):

- `GAP-SEQ-INFIX`: `BinaryOp::Seq` outside braces.
- `GAP-TAIL-EXPR`: control-flow tails parse as full expressions.
- `GAP-FOR-GUARD`: expression `for` accepts a guard.
- `GAP-RBFOR-REL`: rule-body `for` flattens into relational joins.
- `GAP-RBFOR-EQ`: `for (x in xs) y = f(x)` reports a raw pattern error.
- `GAP-FTL-HEADER`: pattern-plus-expression header.
- `GAP-FTL-BODY`: atom-only bodies and a mandatory `.`.
- `GAP-SEMA-FOR`: nested `for` expressions declare no pattern bindings.
- `GAP-DOC-DRIFT`: the drift listed in `Surprises & discoveries`.

Trace links:

```plaintext
SPEC-POLICY -> REQ-SEQ -> GAP-SEQ-INFIX -> SF-D1 -> EP-M2 -> V-SEQ-TABLE, V-SEQ-PROP
REG-16 -> REQ-RBFOR -> GAP-RBFOR-REL, GAP-SEMA-FOR -> SF-D3 -> EP-M3 -> V-RBFOR
REG-16 -> REQ-RBFOR -> GAP-RBFOR-EQ -> SF-D4 -> EP-M3 -> V-RBFOR
REG-16 -> REQ-FOR-EXPR -> GAP-FOR-GUARD -> SF-D14 -> EP-M4 -> V-RBFOR
REG-8, REG-16 -> REQ-FTL-HEADER -> GAP-FTL-HEADER -> SF-D5 -> EP-M4 -> V-FTL-EXTENT, V-FTL-TABLE
REG-8, REG-16 -> REQ-FTL-STMT -> GAP-FTL-BODY -> SF-D6, SF-D12 -> EP-M4, EP-M5 -> V-FTL-EXTENT, V-FTL-COVER
REG-8 -> REQ-FTL-LOWER -> SF-D7 -> EP-M4, EP-M5 -> V-FTL-TABLE, V-FTL-LOWER, V-FTL-PARSE, V-PIPELINE
SPEC-POLICY -> REQ-TERM -> GAP-TAIL-EXPR -> SF-D2 -> EP-M6 -> V-TERM
REG-16 -> REQ-FOR-EXPR -> SF-D16 -> EP-M6 -> V-TERM
SPEC-POLICY -> REQ-DOC -> GAP-DOC-DRIFT -> SF-D10, SF-D11 -> EP-M1..EP-M7 -> markdown gates
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
`Lit(binder?, atom)`, `Cond(e)`, or `Asg(p, v)`. `lower : S -> [Rule]` is the
executable lowering. The specification is declarative and independent of
`lower`: a path from the root to an `Insert` leaf records one step per
ancestor, and each step contributes terms:

```plaintext
contrib(For(b, a, g, _))     = [Lit(b, a)] ++ [Cond(g) | g present]
contrib(If(c, _, _), then)   = [Cond(c)]
contrib(If(c, _, _), else)   = [Cond(Unary(Not, c))]
contrib(Match(e, arms), i)   = [Cond(Match(e, [(p1, i = 1), …, (pn, i = n)]))]
contrib(Assign(e, _))        = [Asg(p, v)] if e has a depth-0 `=` splitting p from v, else [Cond(e)]
contrib(Block(_), j)         = []
spec(S) = [ (h, loc, concat(contrib(step) for step in path)) | (path, Insert(h, loc)) ∈ leaves(S) ]
```

In the `Match` row, `i = j` denotes the Boolean literal `true` when the arm
index equals `j` and `false` otherwise. `leaves(S)` lists `Insert` leaves in
source order: for `If`, then-leaves before else-leaves; for `Match` and
`Block`, children in order. Erasing every `Asg(p, v)` to `Cond(p = v)` maps
`spec(S)` onto upstream's `convertStatement` output term for term.

Malformed input never reaches `lower`: the statement parser returns either a
well-formed tree or diagnostics, and a statement with diagnostics yields no
rules.

### Invariants and lemmas

- `INV-SEQ` (`REQ-SEQ`): no parse result contains a sequencing node, and
  every `;` outside a block item yields exactly one
  `SEMICOLON_OUTSIDE_BLOCK_ERROR` at its own span, in source order, with no
  other diagnostic. The first half holds by construction once `BinaryOp::Seq`
  is deleted.
- `INV-TERM` (`REQ-TERM`): for every binary operator `⊕` and every
  control-flow head `H` in {`if (c) {a} else {b}`, `if (c) {a}`,
  `if c {a} else {b}`}, `H ⊕ y` parses as `Binary(⊕, H, y)`, and `y ⊕ H` parses
  as `Binary(⊕, y, H)`; for `Ascribe` and `Cast`, `H : T` and `H as T` apply to
  the whole `H`.
- `INV-FOR-EXPR` (`REQ-FOR-EXPR`): an expression `for` with a guard, or with
  a postfix form applied to it, yields exactly one diagnostic.
- `INV-RBFOR` (`REQ-RBFOR`): no rule-body term exposes a `for` as anything but
  an expression or an assignment value, and no name declared by a `for` pattern
  is visible outside that loop's body.
- `INV-EXTENT` (`REQ-FTL-STMT`): a well-formed top-level statement's extent
  ends at its last token, and the next declaration, on the same or a later
  line, parses exactly as it would without the statement before it.
- `INV-COVER` (`REQ-FTL-STMT`): every non-trivia token of a program lies in a
  declaration span, a statement extent, or a diagnostic span; nothing is
  silently dropped.
- `INV-DEPTH`: statement nesting beyond `MAX_STATEMENT_DEPTH` yields
  `STATEMENT_DEPTH_ERROR` without stack overflow; at the limit it parses.
- `L-LOWER` (`REQ-FTL-LOWER`): `lower(S) = spec(S)` for every well-formed
  `S`.
- `L-COUNT`: `|lower(S)|` equals the number of `Insert` leaves of `S`; `Skip`
  and `Block([])` yield none, and an else-less `If` yields only its then-leaves.
- `L-PREFIX`: for `For(b, a, g, S')`, every rule of `lower` begins with
  `Lit(b, a)` and then `Cond(g)` if present, and the remainders, in order, are
  exactly `lower(S')`.
- `L-EXCL`: for `If(c, S1, S2)`, every rule from `S1` carries `Cond(c)` and
  every rule from `S2` carries `Cond(Unary(Not, c))`. For `Match`, the filter
  added for arm `i` has the syntactic form of `explodeMatchCases`: arm `i` maps
  to `true` and every other arm to `false`, so it holds when arm `i` is the
  first matching arm.

`L-PREFIX`, `L-COUNT`, and `L-EXCL` are corollaries of `L-LOWER`, stated for
readers and focused tests. `L-LOWER` follows by structural induction using two
collection facts: prepending terms distributes over concatenation, and `leaves`
of a `Block` is the concatenation of its children's leaves. These are checked
by tests and remain unproved (`SF-D9`).

### Axioms (trusted, not verified here)

- `AXIOM-UPSTREAM`: the upstream sources at the pinned commit define the
  target grammar and lowering. Boundary: the specification text and the
  `rstest` tables transcribe `Parse.hs` and `Statement.hs` row by row.
- `AXIOM-TOKENIZER`: `tokenize_without_trivia` yields the same tokens for a
  statement whether or not other declarations follow it. Boundary: the
  `INV-EXTENT` table.
- `AXIOM-PROPTEST`: `proptest` generates, shrinks, and replays regression
  files as documented.

### Methods per obligation

- `V-SEQ-TABLE` and `V-SEQ-PROP` (`INV-SEQ`). Method: an `rstest` table plus
  a property. Domain: the block-expression generator
  (`src/parser/tests/block_expressions/strategy.rs`) extended to insert one or
  two `;` at recorded points, each classified by its nearest enclosing
  delimiter: parenthesized group, call argument, vector item, struct field,
  `match` arm, iterable, closure body outside a block, `if` or `for` tail
  outside a block, top level, or block boundary. A rule-body literal
  `A(x); B(x)` is covered through `parse()` in the table. Artefacts:
  `src/parser/tests/sequencing/mod.rs`, `src/parser/tests/sequencing/props.rs`.
  Evidence: before `EP-M2` the rejected rows parse and the property finds
  `(; …)`; after, each non-block `;` yields one diagnostic at its span in
  source order and nothing else (asserted with `googletest` `expect_that!`),
  and the block-boundary class parses to the generator's expected tree (reuse
  `expected_item`). Oracle: the generator's record of insertion points.
  Non-vacuity: a `generator_covers_every_class`-style deterministic sample of
  2,048 draws with at least 20 per class; 2,048 property cases. Seeded faults:
  set `in_block_item` true outside blocks (the non-block classes must fail);
  stop clearing it in `with_nested_delimiter_scope` (the paren, call, and
  vector classes must fail).
- `V-TERM` (`INV-TERM`, `INV-FOR-EXPR`). Method: a loop over
  `const ALL_BINARY_OPS: [BinaryOp; N]` crossed with the heads and both operand
  positions, using `expect_that!` so every failing row is reported, plus a test
  that an exhaustive `match` in `op_index` maps `ALL_BINARY_OPS` one-to-one onto
  `0..N` (adding an operator without updating the array fails). Named rows:
  `Ascribe` and `Cast`; postfix field, index, `: T`, and `as T` after an `if`;
  positive term conditions (`if c {a} else {b}`, `if true {a}`); negatives
  (`if x > 1 { a }`, `if (c) f(x) else g(x)`, `if (c) -1 else 1`);
  `for (x in xs if g) e`; and `for (x in xs) f(x)`. Artefact:
  `src/parser/tests/control_flow_terms.rs`. Seeded fault: parse the `else`
  branch with `parse_expr(0)` again, which must fail every `else` row.
- `V-RBFOR` (`INV-RBFOR`, `SF-D4`). Method: named tests. Artefacts:
  `tests/rule_behaviour/for_loops.rs` (rewritten), `src/sema/tests/mod.rs`
  (`for_expression_bindings_stay_in_loop_body`, including a `for` inside an
  `if` branch, which fails before `EP-M3`), `tests/unused_relation_rule.rs` (a
  relation named only in a `for` iterable is reported unused), and
  `src/parser/tests/rules/body_terms.rs` (`ASSIGNMENT_TARGET_ERROR` row).
  Non-vacuity: each sema test asserts a positive resolution inside the body and
  a negative one after the clause.
- `V-FTL-EXTENT` (`INV-EXTENT`, `INV-DEPTH`). Method: an `rstest` table over
  every statement form followed by each declaration kind (relation, rule,
  `&`-headed rule, function, extern, typedef, index, import, apply,
  transformer, another `for`), on the same line and on the next line, asserting
  the following item's span and kind; recovery rows (malformed header, stray
  `.` at end of file and before a rule, unclosed `(` and `{`, end of file
  inside the header, body, and after `if (c)`, an attribute before `for`, an
  indented inner statement after a bad header) asserting the exact diagnostic
  count; the `@ l` then `&C(x)` hazard row; and depth rows at the limit and one
  above. Artefact: `src/parser/tests/rules/top_level_for_extent.rs`. Seeded
  fault: end the extent one token early, which must fail every row.
- `V-FTL-TABLE` (`L-LOWER` cases). Method: an `rstest` table transcribing
  each `convertStatement` equation and the tutorial's forms
  (`var is_audience: bool = f(p) in …`; `Some{var x} = e in …` once 2.6.11
  lands, pinned meanwhile as its deferred diagnostic; `match` with a trailing
  comma; comma-less `match` rejected; `x in B(x)` as an assignment;
  `{ e } in s` as an assignment; an `&B(a)` head; a `B(a) @ l` head; a
  `B-<1>(a)` head rejected as rule heads reject it), asserting full
  `SemanticRule` values with `pretty_assertions::assert_eq`, including
  `statement_index`, `source_span`, `head_span`, and term spans. Artefact:
  `src/parser/tests/rules/top_level_for.rs`.
- `V-FTL-LOWER` (`L-LOWER`). Method: a property on crate-private statement
  trees built directly by the generator and lowered without parsing. Oracle: an
  explicit-stack depth-first path enumerator in the test module computing
  `spec(S)`, sharing no code with `lower`; expected `Expr`s come from a fixed
  vocabulary built with `test_util` builders. Domain: depth at most 4, block
  width at most 3, `match` arms at most 3. Comparison: rendered rule text with
  `prop_assert_eq!` so shrunk counterexamples are readable. Artefacts:
  `src/parser/tests/rules/top_level_for_props.rs` and
  `top_level_for_strategy.rs`. Non-vacuity: a deterministic 2,048-draw sample
  counts each single form and each of the 35 parent-to-child pairs (five parent
  forms by seven child forms), requiring at least 10 each; 2,048 property
  cases. Depth 3 exercises every pair; 4 is margin, valid because the lowering
  has no depth-dependent state (a constraint). Seeded faults, each of which
  must be rejected with a shrunk counterexample: drop the guard; emit `c`
  instead of `not c` for `else`; emit else-rules before then-rules; reverse
  block order; omit `Lit` for a nested `for`; append terms instead of
  prepending; classify `Asg` as `Cond`; drop the `@` location; and build the
  `match` filter as `match e { pi -> true, _ -> false }`, which loses
  first-match semantics.
- `V-FTL-PARSE` (statement parser). Method: a property that renders a
  generated tree to source and checks the statement parser returns the same
  tree with no diagnostics and an extent equal to the source length. Artefact:
  `src/parser/tests/rules/top_level_for_props.rs`.
- `V-FTL-COVER` (`INV-COVER`). Method: a property over generated programs
  mixing statements and other declarations, checking that the union of
  declaration spans, statement extents, and diagnostic spans covers every
  non-trivia token. Artefact: `src/parser/tests/rules/top_level_for_props.rs`.
- `V-PIPELINE` (`REQ-FTL-LOWER`, end to end). Method: named tests through
  `parse()`, `sema::build()`, and the lint `Runner`. Artefact:
  `tests/top_level_for_pipeline.rs`: a relation read only in a header is not
  reported unused, while an unread relation is;
  `for (r in A(a, b)) { B(a); C(b) }` yields one `unused-variable` report for
  `r` and none for `a` or `b`; `examples/imperative_syntax.dl` parses with no
  errors and lowers to `Main(x.f1, y.f2) :- x in R1, y in R2`.
- `V-SNAP`. Method: `insta` snapshots paired with the semantic tables above,
  never replacing them. Artefacts: `src/parser/tests/sequencing_snapshots.rs`
  (`source => sexpr | message @ span` for stray `;`, term tails, dangling
  `else`, `for` guard and postfix, and assignment-target diagnostics) and
  `src/parser/tests/rules/top_level_for_snapshots.rs` (program to rendered
  rules, one per equation plus compositions, and multi-declaration recovery
  rows with full errors and spans). Rendering uses a test-only
  `test_util::render_semantic_rule` (`Head @ loc :- t1, t2`, binders as
  `r in A(a)`, compound conditions as `not (c)`, assignments as `p = v`),
  shared with the BDD `Then` steps. Snapshot churn in
  `src/parser/tests/expression/characterization.rs` is reviewed, not accepted
  blindly.

### Formal verification decision

Verus is not warranted in this item (`SF-D9`). The grammar decisions are finite
tables or cheap generated inputs; proofs would restate the grammar. The
lowering is contractual, and a Verus proof of `L-LOWER` is sketchable: a spec
datatype `Stmt` over abstract atom and condition identifiers, a `spec fn lower`
built bottom-up, a `spec fn paths(S, prefix)` built top-down, and the theorem
`lower(S).map(prepend(prefix)) == paths(S, prefix)` by structural induction
with a distribution lemma over concatenation. Such a proof would only show that
two renderings of the same recursion agree over a mirror of the code. It could
not reach the shipped lowering: hosting `verus!` in production needs `vstd`, a
new dependency this plan forbids, and the production kernel works over `Expr`,
`String`, and `Box`, which Verus handles poorly; the proof-to-production bridge
is infrastructure scheduled for roadmap 3.3.6. The real risks (transcription
from upstream, statement extent, and concrete `Expr` construction such as
whether `not c` gains a group) sit outside any such proof and inside
`V-FTL-TABLE`, `V-FTL-LOWER`, `V-FTL-PARSE`, and `V-FTL-EXTENT`, which run
against production code. Kani is not used: inputs are recursive trees, where
bounded model checking needs bespoke generators and gives less than the
property tests. The lemmas are recorded in `docs/ddlint-design.md` as tested
and unproved, with the route to a proof if the kernel is later reshaped as an
index arena. The pull request answers any proof-coverage review question with
this scope statement.

### BDD features

Two features, each driven by an `rstest-bdd` runner in the style of
`tests/block_expressions_bdd.rs`, reusing its
`Given the expression source {source:string}` step, tagging scenarios with
decision identifiers, and using data tables for variable-length expectations
and docstrings for multi-line programs. Long message cells are abbreviated in
this plan with `…`; the feature files carry the full constants.

`tests/features/sequencing_and_control_flow.feature` (runner
`tests/sequencing_and_control_flow_bdd.rs`; `REQ-SEQ`, `REQ-TERM`,
`REQ-FOR-EXPR`, `REQ-RBFOR`; scenarios land with `EP-M2`, `EP-M3`, and `EP-M6`):

```gherkin
Feature: Sequencing and control-flow tails follow upstream DDlog

  @SF-D1
  Scenario: A semicolon sequences items inside a block
    Given the expression source "{ a; b }"
    When the expression is parsed
    Then the s-expression is "(block (stmts a) (value b))"

  @SF-D1
  Scenario Outline: A semicolon outside a block is rejected once
    Given the expression source "<source>"
    When the expression is parsed
    Then parsing fails with exactly one diagnostic "<message>"

    Examples:
      | source      | message                                                               |
      | (a; b)      | `;` separates items only inside a block; wrap the sequence in `{ … }` |
      | f(a; b)     | `;` separates items only inside a block; wrap the sequence in `{ … }` |
      | return x; y | `;` separates items only inside a block; wrap the sequence in `{ … }` |

  @SF-D2
  Scenario: An operator after an if expression applies to the whole if
    Given the expression source "if (c) { a } else { b } ++ x"
    When the expression is parsed
    Then the s-expression is "(++ (if (group c) (block (value a)) (block (value b))) x)"

  @SF-D2 @SF-D14 @SF-D16
  Scenario Outline: Control-flow tails are single terms
    Given the expression source "<source>"
    When the expression is parsed
    Then parsing fails with exactly one diagnostic "<message>"

    Examples:
      | source                   | message                                                                 |
      | if x > 1 { a }           | an `if` condition and branches are single terms; …                      |
      | if (c) f(x) else g(x)    | `else` must directly follow the `if` branch; wrap the branch in `{ … }` |
      | for (x in xs) f(x)       | a `for` loop has type `()`; brace the body: `for (x in xs) { f(x) }`    |
      | for (x in xs if g) { a } | an expression `for` takes no `if` guard; test the condition in the body |

  @SF-D3
  Scenario: A rule-body for loop is an expression clause
    Given the program "R(x) :- S(x), for (y in ys(x)) { f(y) }."
    When the rule body terms are classified
    Then term 1 is an expression containing a for loop

  @SF-D4
  Scenario: A loop is not an assignment target
    Given the program "R(x) :- S(xs), for (x in xs) y = f(x)."
    When the rule body terms are classified
    Then classification fails with "the left-hand side of `=` must be a pattern; …"
```

`tests/features/top_level_for_statements.feature` (runner
`tests/top_level_for_statements_bdd.rs`; `REQ-FTL-*`; scenarios land with
`EP-M4` and `EP-M5`):

```gherkin
Feature: Top-level for statements lower into rules

  @SF-D5 @SF-D7
  Scenario: A for statement over an atom becomes one rule
    Given the program "for (A(a)) B(a)"
    When the program is parsed
    Then the semantic rules are
      | rule         |
      | B(a) :- A(a) |

  @SF-D6 @SF-D7
  Scenario: A binder, guard, and if-else statement lower into two rules
    Given the program "for (r in A(a) if a > 0) if (ok(r)) B(a) else C(a)"
    When the program is parsed
    Then the semantic rules are
      | rule                                  |
      | B(a) :- r in A(a), a > 0, ok(r)       |
      | C(a) :- r in A(a), a > 0, not (ok(r)) |

  @SF-D6 @SF-D7
  Scenario: A statement block lowers each statement in order and skip yields nothing
    Given the program "for (A(a)) { B(a); skip; C(a) }"
    When the program is parsed
    Then the semantic rules are
      | rule         |
      | B(a) :- A(a) |
      | C(a) :- A(a) |

  @SF-D7
  Scenario: A match statement filters each arm
    Given the program "for (A(a)) match (a) { 0 -> B(a), _ -> C(a) }"
    When the program is parsed
    Then the semantic rules are
      | rule                                              |
      | B(a) :- A(a), match (a) { 0 -> true, _ -> false } |
      | C(a) :- A(a), match (a) { 0 -> false, _ -> true } |

  @SF-D7
  Scenario: An assignment statement binds a pattern
    Given the program "for (A(a)) var b = f(a) in B(b)"
    When the program is parsed
    Then the semantic rules are
      | rule                       |
      | B(b) :- A(a), var b = f(a) |

  @SF-D6
  Scenario: A declaration after a statement parses normally
    Given the program
      """
      for (A(a)) B(a)
      R(x) :- A(x).
      """
    When the program is parsed
    Then there are no diagnostics
    And the program has 1 rule and 1 semantic rule

  @SF-D5 @SF-D6
  Scenario Outline: Malformed statements are rejected once
    Given the program "<program>"
    When the program is parsed
    Then parsing fails with exactly one diagnostic "<message>"

    Examples:
      | program                            | message                                                          |
      | for (A(a)) B(a).                   | a top-level `for` statement takes no terminating `.`             |
      | for ((k, v) in Pairs(k, v)) Out(k) | a top-level `for` binds a single lower-case variable before `in` |
      | for (x in xs) Out(x)               | a top-level `for` iterates a relation atom: …                    |
      | for (A(a)) if a > 0 B(a)           | an `if` statement condition is parenthesized: `if (cond) …`      |
      | for (A(a)) { B(a); }               | statements in a block are separated by `;` with no trailing `;`  |
```

## Plan of work

### `EP-M0` extent and backtracking probe (prototype)

Questions: (1) can the statement parser, driven over a token slice from a
line-initial `for`, parse one statement and report the end of its last consumed
token; (2) can it checkpoint and rewind `TokenStream` (whose iterator is
`Clone`) to try the assignment form before the insert form, truncating
speculative errors; (3) does the CST builder accept one more span list for
`N_FOR_STATEMENT` without restructuring. Scratch artefact: a `#[cfg(test)]`
function on a throwaway branch. Inputs: `for (A(a)) B(a)\nR(x) :- A(x).`,
`for (A(a)) B(a).\nR(x) :- A(x).`, `for (A(a)) B(a)\n&R(x) :- A(x).`,
`for (A(a)) B(a) R(x) :- A(x).`, and `for (A(a)) var b = f(a) in B(b)`.
Expected observations: the extent ends at `)` of `B(a)` in each; the stray `.`
is the next token; the assignment form is chosen only for the last input. Go:
implement `EP-M4` as planned. No-go on (2): split assignments purely at token
level (depth-0 `=` and `in`) with no rewind. No-go on (3): drop
`N_FOR_STATEMENT` per `SF-D12`. Record results in `Decision log`; delete the
probe.

### `EP-M1` decision records, specification targets, register, roadmap

Assigned: `REQ-DOC`, `SF-D10`, `SF-D11`. Write ADR-0007 recording `SF-D1` to
`SF-D8` and `SF-D12` to `SF-D16` with upstream evidence; amend ADR-0006; index
ADR-0007 in `docs/contents.md`; reference it from `docs/ddlint-design.md`. In
the specification, add target notes (implementation status: scheduled) to
sections 3.4, 4, 5.8, 5.10, 5.13, and 6.5 stating the decided behaviour. Update
register items 8 and 16 to `scheduled`, add items 23 and 24 as
`decision needed`, add roadmap 2.6.16 and 2.6.17 with `mapsplice`, add 2.6.10
to item 8's roadmap line, and correct `docs/haskell-parser-analysis.md`'s
description of `parseForStatement`. Run `make fmt`, `make markdownlint`,
`make nixie`, and `make check-fmt`.

### `EP-M2` `;` only inside braces

Assigned: `REQ-SEQ`, `GAP-SEQ-INFIX`, `SF-D1`. Red: add
`src/parser/tests/sequencing/mod.rs` with rejected inputs (`"(a; b)"`,
`"f(a; b)"`, `"[a; b]"`, `"S { x: a; b }"`, `"match (x) { _ -> a; b }"`,
`"return x; y"`, `"x ;"`, `"a; b"`, `"(a; b; c)"`) asserting one diagnostic
each with its span, and accepted inputs (`"{ a; b }"`, `"{ (a, b); c }"`,
`"{ if (c) { a } else { b }; d }"`), plus the rule-body literal
`R(x) :- A(x); B(x).` through `parse()`; and `V-SEQ-PROP`. Run
`cargo test --lib sequencing` and observe failures. Green: delete
`BinaryOp::Seq` and its `SYMBOLS` entry, precedence row, and s-expression arm;
rename `semicolon_terminates` to `in_block_item`; implement the
report-consume-discard path in `parse_infix`; add the shared unexpected-token
helper used by the drain and by `expect`; remove the `Seq` arms in
`classification.rs` and `top_level_for/lowering.rs`. Rewrite the 13 `Seq`
assertions, fix the `for_loops.rs` associativity comments, and replace `(a; b)`
items in the block generators with `{ a; b }`. Refactor: keep
`BLOCK_ITEM_MIN_BP` if any test fails without it. Update specification sections
3.4 and 4, register item 16's `;` paragraph, the users' guide (including the
`Expr::Group` correction), and ADR-0007's status line; add the `@SF-D1`
scenarios.

### `EP-M3` rule-body `for` is an expression clause

Assigned: `REQ-RBFOR`, `GAP-RBFOR-REL`, `GAP-RBFOR-EQ`, `GAP-SEMA-FOR`, `SF-D3`,
`SF-D4`. Red: rewrite `tests/rule_behaviour/for_loops.rs` to final-form
fixtures (braced bodies, no guards) and the new classification; add
`for_expression_bindings_stay_in_loop_body`, the unused-relation regression,
and the `ASSIGNMENT_TARGET_ERROR` row; run and observe failures. Green: remove
`RuleBodyTerm::ForLoop`, `RuleForLoop`, `Rule::flattened_body_terms`, and every
caller (`src/parser/ast/mod.rs`, `src/parser/cst_builder/mod.rs`,
`src/parser/mod.rs`, `src/parser/ast/rule/tests.rs`,
`tests/rule_behaviour/body_bounds.rs`); keep aggregation counting inside `for`
bodies through a helper in `classification.rs`; map a pattern-parse failure
whose left-hand side begins with `for` or `if` to `ASSIGNMENT_TARGET_ERROR`. In
sema, move the loop-scope logic from `traverse.rs::collect_for_loop_term` into
`src/sema/loop_scope.rs`, called by the expression walker in `variables.rs` for
every `Expr::ForLoop`; remove `UseOrigin::ForIterable` and
`UseOrigin::ForGuard`. The rule-statement scanner stays a permissive extent
finder and `validate_expression` owns diagnostics; add rows for
term-tail-followed literals to `rule_statement_tests.rs`. The guard field of
`Expr::ForLoop` stays until `EP-M4`, because the old top-level lowering still
parses headers through the expression `for` and reads its guard. Update the
`unused_relation` and `unused_variable` fixtures, specification sections 5.8,
6.5 (rule-body paragraph), 8.1, 9.1, and 10, register items 9 and 16, the
users' guide, and the developers' guide (`Rule-behaviour setup ownership`); add
the `@SF-D3` and `@SF-D4` scenarios.

### `EP-M4` top-level statements: core forms, extent, API, scope

Assigned: `REQ-FTL-HEADER`, `REQ-FTL-STMT`, part of `REQ-FTL-LOWER`,
`GAP-FTL-HEADER`, `GAP-FTL-BODY`, `GAP-FOR-GUARD`, `SF-D5` to `SF-D8`, `SF-D12`,
`SF-D14`. Red: rewrite the top-level tables
(`src/parser/tests/rules/top_level_for.rs`,
`tests/top_level_for_desugaring.rs`) to upstream syntax; add the extent table,
the `@SF-D5` and core `@SF-D6` scenarios, and
`tests/top_level_for_pipeline.rs`, and the guard rejection row of the `@SF-D14`
outline; run and observe failures. Green, in order:

1. Add `src/parser/expression/statement/` with one `pub(crate)` entry point,
   `parse_top_level_statement`, returning
   `TopLevelStatement { span, tree, diagnostics }`; the private tree mirrors
   the abstract model. Split it into `header.rs` (binder and `HeaderAtom`),
   `dispatch.rs` (ordered choice), and `block.rs` (statement blocks). This
   milestone supports `for`, `skip`, insert, and blocks; `if`, `match`, and
   assignment statements report `STATEMENT_FORM_PENDING_ERROR`, which `EP-M5`
   deletes. This is a partial grammar, not compatibility machinery: no caller
   depends on it.
2. Call it from the `K_FOR` arm of `collect_rule_spans`; record the end of a
   statement as a line-start boundary; delete `skip_top_level_for_statement`,
   `collect_desugared_top_level_for_rules`, `top_level_for_exclusions`,
   `ExclusionCursor`, the longest-prefix search, and the duplicate line-start
   helper if unused. Return the statements through `parse_tokens` as a
   `pub(crate)` collection, not through the public `ParsedSpans`, and emit
   their diagnostics inside the existing top-level attempt.
3. Add `N_FOR_STATEMENT` (per `EP-M0`) to `SyntaxKind` and the CST builder.
4. Replace `src/parser/top_level_for/lowering.rs` with the `convertStatement`
   lowering for the supported forms, and change
   `src/parser/ast/semantic_rule.rs` to the new types; make `lower_by_ref_head`
   `pub(crate)` for `&` heads.
5. Add `src/sema/semantic_rules.rs` with the statement scope
   (`ScopeKind::ForStatement`, `ScopeOrigin::TopLevelForStatement`,
   `SymbolOrigin::ForStatementBinder`), moving `collect_semantic_rules` out of
   `builder.rs`.
6. Add `test_util::render_semantic_rule` and update
   `src/test_util/assertions.rs`.
7. Remove the `guard` field of `Expr::ForLoop` (and its `sexpr.rs`
   rendering), now that no caller reads it, and report `FOR_GUARD_ERROR` in
   `control_flow.rs`.
8. Rewrite the remaining top-level fixtures in `src/sema/tests/`,
   `tests/semantic_scope_resolution.rs`, `tests/block_expressions.rs`,
   `tests/parser_observability.rs`, and
   `src/parser/tests/reserved_tokens/mod.rs` to final form.

Update specification sections 5.10 and 6.5, register item 8, the users' guide,
and `docs/parser-implementation-notes.md`.

### `EP-M5` top-level statements: `if`, `match`, `expr in`, properties

Assigned: the rest of `REQ-FTL-LOWER` and `REQ-FTL-STMT`. Red: add the remaining
`V-FTL-TABLE` rows, `V-FTL-LOWER`, `V-FTL-PARSE`, `V-FTL-COVER`, `V-SNAP` for
statements, and the remaining `@SF-D6` and `@SF-D7` scenarios; run and observe
failures. Green: add the `if`, `match`, and assignment forms to `dispatch.rs`
(with the checkpoint-and-rewind or token-level split chosen by `EP-M0`), their
lowering equations, and `STATEMENT_IF_PAREN_ERROR`; delete
`STATEMENT_FORM_PENDING_ERROR`. Run the seeded faults of `V-FTL-LOWER` and
record the rejections. Update the users' guide with the `match` scoping and
exhaustiveness notes, and register item 8.

### `EP-M6` `if` and `for` tails are terms

Assigned: `REQ-TERM`, the postfix half of `REQ-FOR-EXPR`, `GAP-TAIL-EXPR`,
`SF-D2`, `SF-D16`. Red: add `src/parser/tests/control_flow_terms.rs` (`V-TERM`)
and the `@SF-D2` scenarios; run and observe that `else`-branch rows fail.
Green: add `Pratt::parse_term` in `src/parser/expression/prefix.rs` (prefix
productions only; counts towards `MAX_EXPR_DEPTH`); call it for the `if`
condition (keeping struct-literal activation), both branches, and the `for`
body (keeping struct literals suspended); report `IF_TERM_EXPECTED_ERROR`,
`DANGLING_ELSE_ERROR` (through the shared helper), and `FOR_POSTFIX_ERROR` (in
`pratt/postfix.rs` when the receiver is `Expr::ForLoop`). Rewrite the fixtures
that the red run lists, parenthesizing non-term conditions only. Update the
`for` tail item in the block generators. Update specification sections 4 and
5.13, register item 16, ADR-0006's amendment if needed, and the users' guide.

### `EP-M7` close-out

Assigned: `REQ-DOC`. Record `L-LOWER`, `L-COUNT`, `L-PREFIX`, and `L-EXCL`, the
`SemanticBodyTerm` contract, and the deferred-proof note in
`docs/ddlint-design.md`; document in `docs/developers-guide.md` where statement
parsing lives and that a new statement form extends the parser, the lowering
equation table, and `V-FTL-LOWER` together; set register item 16 to
`implemented` and remove register item 8's divergence paragraph; tick roadmap
2.6.10 with a "Done:" summary that names the validator, not the grammar, as
what rejects `for (a in A(a))` upstream (owned by 4.1.5). Sweep for stale
claims, then run every gate:

```sh
rg -n 'Seq\b|flattened_body_terms|RuleForLoop|assert_flattened_expression_terms|patterns\(\)|ForIterable|ForGuard' \
  docs src tests --glob '!docs/execplans/**'
```

## Milestones and plateaus

Each milestone below records its conformance check at its boundary: assigned
requirements satisfied; decisions followed; upstream assumptions still valid;
only the authorized interface changes; no trust-boundary or persisted-format
change; trace links current. Recovery is `git revert` of the milestone's
commits, or closing the milestone's stacked pull request. No milestone carries
compatibility machinery: every caller changes with its interface.

- `EP-M1`: decisions recorded and specification targets marked. Evidence:
  Markdown gates. Remaining: all code gaps. Pull request A.
- `EP-M2`: `;` is only a block separator. Evidence: `V-SEQ-TABLE`,
  `V-SEQ-PROP`, `@SF-D1` scenarios, gates. Check: `rg -n 'Seq\b' src` finds no
  variant; block suites green with only `Seq` edits. Remaining: rule-body and
  top-level `for`, tails. Pull request B.
- `EP-M3`: rule-body `for` is an expression clause.
  Evidence: `V-RBFOR`, aggregation tests unchanged, `@SF-D3` and `@SF-D4`
  scenarios, gates. Remaining: top-level statements, tails. Pull request C.
- `EP-M4`: upstream top-level statements parse for `for`, insert, block, and
  `skip`, with the new API, CST node, and statement scope; expression `for` has
  no guard; other forms report a pending diagnostic. Evidence: `V-FTL-EXTENT`,
  core `V-FTL-TABLE` rows, `V-PIPELINE`, `tests/span_validation.rs` green,
  gates. Remaining: three statement forms. Pull request D.
- `EP-M5`: every statement form parses and lowers. Evidence: full
  `V-FTL-TABLE`, `V-FTL-LOWER` with pair counts and seeded faults,
  `V-FTL-PARSE`, `V-FTL-COVER`, statement snapshots, gates. Remaining: tails.
  Pull request D.
- `EP-M6`: `if` and `for` tails are terms; postfix on `for` rejected.
  Evidence: `V-TERM`, expression snapshots, `@SF-D2` scenarios, gates.
  Remaining: close-out. Pull request E.
- `EP-M7`: documentation closed and roadmap ticked. Evidence: all gates;
  stale-claim sweep returns only historical execplans. Pull request E.

## Concrete steps

Working directory: the repository root. Every gate pipes through `tee` with
`set -o pipefail`, and gates run sequentially:

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
EP-M2 red:   cargo test --lib sequencing            -> rejected inputs parse; property finds "(; a b)"
EP-M2 green: cargo test --lib sequencing            -> pass; class counts reported
EP-M3 red:   cargo test --test rule_behaviour       -> expected Expression(ForLoop), found ForLoop
EP-M3 green: cargo test --test rule_behaviour; cargo test --lib sema -> pass
EP-M4 red:   cargo test --lib top_level_for         -> upstream statements report errors
EP-M4 green: cargo test --lib top_level_for; cargo test --test top_level_for_pipeline -> pass
EP-M5 red:   cargo test --lib top_level_for_props   -> if/match/assign forms report pending diagnostic
EP-M5 green: same, plus cargo test --test top_level_for_statements_bdd -> pass
EP-M6 red:   cargo test --lib control_flow_terms    -> else-branch rows show (++ (block (value b)) x)
EP-M6 green: cargo test --lib control_flow_terms    -> pass
```

The fixtures to rewrite in `EP-M3`, `EP-M4`, and `EP-M6` are the failures
reported by the red runs, not regular-expression sweeps.

## Validation and acceptance

Quality criteria:

- Tests: `make test` passes; the new tables, properties, snapshots, features,
  and pipeline test pass; each named red test failed before its milestone for
  the stated reason.
- Verification: `INV-SEQ`, `INV-TERM`, `INV-FOR-EXPR`, `INV-RBFOR`,
  `INV-EXTENT`, `INV-COVER`, `INV-DEPTH`, and `L-LOWER` are checked by the
  methods above, with class and pair counts and seeded-fault rejections
  recorded in `Artefacts and notes`. No obligation is claimed as proved.
- Lint and format: `make check-fmt` and `make lint` pass with no new
  `#[expect]` except narrowly reasoned ones.
- Documentation: `make markdownlint` and `make nixie` pass.

Quality method: the gate sequence in `Concrete steps`, with logs under `/tmp`.

## Idempotence and recovery

Every step is a source edit followed by gates; rerunning is safe. Each
milestone ends in a commit, so `git revert` restores the previous plateau.
Seeded faults are applied in the working tree only, observed, and reverted with
`git checkout -- <file>`; any `proptest-regressions` entries they produce are
deleted. Regression files from genuine failures are committed with their fix.

## Artefacts and notes

Reconnaissance evidence (planning time, upstream pinned commit):

```haskell
-- Parse.hs 600-611: `;` only in eseq
eseq = do ... Just e -> do extra_semi <- optionMaybe semi ...
-- Parse.hs 885: the operator row is commented out
         --,[sbinary ";" ESeq AssocRight]
-- Parse.hs 364-368: the top-level header is an atom
parseForStatement = withPos $ ForStatement nopos <$ reserved "for"
    <*> (symbol "(" *> atom False) <*> (optionMaybe (reserved "if" *> expr))
    <*> (symbol ")" *> statement)
-- Parse.hs 806-813: if and for tails are terms; efor has no guard
eite = do reserved "if"; cond <- term; th <- term; ... (reserved "else" *> term)
efor = eFor <$ ... <*> (loop_pattern <* reserved "in") <*> (expr <* symbol ")") <*> term
-- Statement.hs 129-132: match arm filters
explodeMatchCases l = map (map (\(a,b) -> (l !! a, if a == b then eTrue else eFalse))) pairs
```

Implementation evidence will be added here by milestone.

## Interfaces and dependencies

No new dependencies. Authorized public API changes (pre-release; every caller
changes in the same milestone):

- `BinaryOp::Seq` is removed (`EP-M2`).
- `RuleBodyTerm::ForLoop`, `RuleForLoop`, `Rule::flattened_body_terms`,
  `UseOrigin::ForIterable`, and `UseOrigin::ForGuard` are removed (`EP-M3`).
- The `guard` field of `Expr::ForLoop` is removed (`EP-M4`).
- `SyntaxKind::N_FOR_STATEMENT` is added, subject to `SF-D12` (`EP-M4`).
- `ScopeKind::ForStatement`, `ScopeOrigin::TopLevelForStatement { index }`,
  and `SymbolOrigin::ForStatementBinder` are added (`EP-M4`).
- `SemanticRule` becomes (`EP-M4`):

```rust
/// One body term of a lowered rule, in evaluation order: the concatenation of
/// the terms contributed by each statement on the path to the rule's head.
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticBodyTerm {
    /// A positive relation atom with an optional `v in` binder.
    Literal(SemanticLiteral),
    /// A guard, an `if` condition or its negation, a `match` arm filter, or an
    /// `expr in` statement without a top-level `=`.
    Condition { expr: Expr, span: Span },
    /// A `pattern = expr in` statement.
    Assignment { assignment: RuleAssignment, span: Span },
}

/// A relation atom read by a lowered rule.
#[derive(Debug, Clone, PartialEq)]
pub struct SemanticLiteral { /* binder: Option<String>, atom: Expr, span: Span */ }

impl SemanticLiteral {
    pub fn binder(&self) -> Option<&str>;
    pub const fn atom(&self) -> &Expr;
    pub fn span(&self) -> Span;
}

impl SemanticRule {
    pub const fn origin(&self) -> SemanticRuleOrigin;
    /// Index of the top-level statement that produced this rule.
    pub const fn statement_index(&self) -> usize;
    /// Span of the whole top-level statement.
    pub fn source_span(&self) -> Span;
    pub const fn head(&self) -> &Expr;
    pub fn head_span(&self) -> Span;
    pub fn location(&self) -> Option<&Expr>;
    pub fn body(&self) -> &[SemanticBodyTerm];
}
```

`SemanticRule::new` and `SemanticRuleSpec` become `pub(crate)`, with fields
`origin`, `statement_index`, `source_span`, `head`, `head_span`, `location`, and
`body`; `patterns()` is removed.

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
pub const FOR_POSTFIX_ERROR: &str =
    "a `for` loop has type `()`; brace the body: `for (x in xs) { f(x) }`";
pub const FOR_GUARD_ERROR: &str =
    "an expression `for` takes no `if` guard; test the condition in the body";
pub const ASSIGNMENT_TARGET_ERROR: &str =
    "the left-hand side of `=` must be a pattern; wrap a loop body in `{ … }`";
pub const TOP_LEVEL_FOR_BINDER_ERROR: &str =
    "a top-level `for` binds a single lower-case variable before `in`";
pub const TOP_LEVEL_FOR_ATOM_ERROR: &str =
    "a top-level `for` iterates a relation atom: write `for (R(…))` or `for (v in R(…))`";
pub const TOP_LEVEL_FOR_TERMINATOR_ERROR: &str =
    "a top-level `for` statement takes no terminating `.`";
pub const STATEMENT_IF_PAREN_ERROR: &str =
    "an `if` statement condition is parenthesized: `if (cond) …`";
pub const STATEMENT_BLOCK_TRAILING_SEPARATOR_ERROR: &str =
    "statements in a block are separated by `;` with no trailing `;`";
pub const STATEMENT_DEPTH_ERROR: &str = "top-level `for` statements are nested too deeply";
```

Reuse policy: `parse_top_level_statement` in `src/parser/expression/statement/`
is the only parser of top-level statements, called only by the rules scanner;
the rules scanner consumes its extents rather than scanning for statement ends
itself. `SemanticBodyTerm` is owned by `src/parser/ast/semantic_rule.rs` and
built only by `src/parser/top_level_for/lowering.rs`; sema reads it and never
re-parses source. `Pratt::parse_term` is `pub(super)` within
`src/parser/expression/` and is the only definition of an upstream term. The
shared unexpected-token helper is the only place `rejection_for`,
`DANGLING_ELSE_ERROR`, and generic unexpected-token messages are chosen.
`test_util::render_semantic_rule` is test support only.

## Revision note

- 2026-11-23: initial draft after Wyvern reconnaissance and upstream source
  reading.
- 2026-11-23: second draft after the five-reviewer expert design review
  (verdict: proceed with conditions). What changed: added `SF-D14` (no guard on
  expression `for`), `SF-D15` (stacked pull requests), and `SF-D16` (postfix on
  `for` rejected); reordered milestones so term tails come last and split
  top-level statements into `EP-M4` and `EP-M5`, avoiding an incoherent
  plateau; moved the statement parser under `src/parser/expression/statement/`,
  called once from the rules scanner, with head atoms parsed by the upstream
  atom grammar, line-start statement ends, column-0 recovery, and a depth cap;
  added `INV-COVER`, `INV-DEPTH`, and `INV-FOR-EXPR`; gave `SemanticRule`
  statement indices, spans, and a statement scope in sema so multi-rule
  statements do not duplicate warnings; made `SF-D1` recovery yield one
  diagnostic; transcribed the statement grammar's ordered choice and commit
  points; completed the term definition; corrected the `SF-D11` operator list
  and the s-expression examples; split the lowering property into lowering,
  parsing, and coverage properties with pair counts and nine seeded faults;
  added `insta` snapshot modules and a test-only rule renderer; rewrote the BDD
  features with tags, outlines, data tables, and unhappy paths; adopted
  `N_FOR_STATEMENT` conditionally (`SF-D12`); listed every public API change;
  raised the budgets to 75 code files, 3,000 net code lines, 15 documentation
  files, and 1,300 documentation lines; required per-milestone register and
  guide updates; and reworded `SF-D9` so tests check, and do not prove, the
  lowering lemmas. Effect: seven milestones in five stacked pull requests;
  implementation still awaits approval.
