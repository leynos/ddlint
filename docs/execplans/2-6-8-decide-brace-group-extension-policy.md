# Codify brace blocks and adopt upstream map literals (roadmap 2.6.8)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: COMPLETE

This plan is approved for implementation. Decisions marked "requires user
approval" in `Decision log` are approved together with the plan unless the
approver says otherwise.

## Purpose / big picture

Roadmap item 2.6.8 asks for a decision on the brace-group extension `{ expr }`.
The user has directed three outcomes:

1. Codify braces as upstream Differential Datalog (DDlog) block expressions.
   Upstream parses `{ e1; e2; … }` as a core expression term (`braces eseq`; see
   `docs/haskell-parser-analysis.md`, "Expression grammar") and uses it for
   every braced `if`, `else`, `for`, closure, match-arm, and function body.
2. Drop brace-delimited map literals `{k: v, …}`, a ddlint extension, and stay
   faithful to upstream: map literals are `[k -> v, …]` and map types are
   `Map<K, V>`.
3. Make the linter model upstream DDlog's lexical scope semantics correctly.

No compatibility with earlier ddlint behaviour is owed: ddlint is unreleased.

Two expert reviews showed that outcome 3 is a programme rather than a parser
decision. Correct scoping needs parser support ddlint lacks (`var` expressions,
`function` closures, interpolation segments, function bodies, a type grammar
for ascriptions, and per-identifier source spans), followed by a rework of the
semantic layer and a verified resolution kernel. This plan therefore delivers
outcomes 1 and 2 in full, records the upstream scoping rules normatively in the
specification, and schedules outcome 3 as dependent roadmap items, each with
its own ExecPlan and stacked pull request. The analysis, abstract model, and
proof sketch for outcome 3 are kept in `Appendix A` so the follow-on plans
start from reviewed material.

After this plan:

- `{ x }`, `{ a; b }`, `{ a; }`, `{}`, and `{ if c { a } else { b }; d }`
  parse as `Expr::Block` with explicit statements and value.
- `[k1 -> v1, k2 -> v2]` parses as `Expr::MapLit`; braces never form a map, so
  `{a: 1, b: 2}` is an ordinary malformed block.
- The specification defines block expressions, bracket map literals, and the
  upstream variable-scoping rules (the last as a target scheduled for roadmap
  3.3.5).
- The roadmap and conformance register carry every remaining divergence from
  upstream that this work found.

Success is observable by running `make test` and seeing the block, map,
behavioural, snapshot, and public-API suites pass, and by reading register
items 10 and 15 as `implemented` and roadmap item 2.6.8 ticked.

## Recommended decision

`BG-D1` (codify): `{ … }` is a normative block expression.

`BG-D2` (distinct node): a block is `Expr::Block(BlockExpr)` with public fields
`statements: Vec<Expr>` (items followed by `;`) and `value: Option<Box<Expr>>`
(the final item when not followed by `;`; `None` means unit, `()`).
`Expr::Group` is reserved for parentheses. Every `(statements, value)` pair is
a valid block, so public fields expose no invariant.

`BG-D3` (separator): inside braces `;` is a separator handled by the block
parser, not the `BinaryOp::Seq` operator. A trailing `;` is accepted; an empty
item (`{ ; }`, `{ a;; b }`) is rejected.

`BG-D4` (empty braces, approved 2026-11-17): `{}` is the empty block, whose
value is unit.

`BG-D5` (braces always open a block): there are no brace map literals. Items
after `{` are ordinary expressions, so `{ x: T }` is a block whose value is the
ascription `x: T`. The colon-terminating key mode is deleted. Because
ascription types are still parsed as expressions (`parse_type` is
`parse_expr(u8::MAX)`), inputs such as `{a: 1}` and `{a and b: 1}` parse as
blocks containing nonsensical ascriptions; this is a pre-existing gap, recorded
in register item 18 and scheduled with the type grammar (`BG-D16`).

`BG-D6` (struct-literal guard): in an `if` condition or a `for` guard, an
identifier followed by `{` is not a struct literal. Today the guard also
reports an error when the identifier is followed by `{}` or `{ ident :`. After
this change `ident {}` is reported only when another `{` follows
(`if x == Foo{} { a }`), so `if ready {} else { go() }` parses. The `{ ident :`
heuristic is kept while ddlint's struct syntax is unchanged (`BG-D15`); it
wrongly rejects the valid `if ready { x: T }`, which is recorded as a known
false positive in register item 17.

`BG-D11` (`;` terminates tails): while block items are parsed, a scoped parser
state makes the infix loop stop at `;`. Items are parsed at `BLOCK_ITEM_MIN_BP`
(one above `;`), which already protects operator right-hand sides. The state
matters for prefix constructs whose tails call `parse_expr(0)`: the `if`
condition and both branches, the `for` body, the `return` value, and the
pipe-closure body. It is kept through those tails and cleared, then restored,
at every nested delimiter: parentheses, call arguments, bit slices, vector and
map literals, nested blocks, struct fields, the `match` scrutinee and arms, and
the `for` iterable. Without it `{ if c { a } else { b }; d }` would parse as one
`if` whose `else` branch is `Seq({ b }, d)`.

`BG-D12` (upstream map literals): `[k -> v, …]` is a map literal producing
`Expr::MapLit`, with at least one entry and an optional trailing comma. `->` is
neither an infix nor a postfix operator, so the first element's parse stops at
it, which reproduces upstream's `[ expr ->` lookahead without backtracking. A
literal mixing elements and entries is rejected. Upstream vector literals also
need at least one element, so `[]` is rejected with `EMPTY_VECTOR_LITERAL_ERROR`
(`empty vector literals are not DDlog syntax; use vec_empty() instead`);
upstream's standard library provides `vec_empty()` and `map_empty()` (approved
2026-11-17).

`BG-D14` (types): the specification's `MapType ::= '{' Type ':' Type '}'` and
`VecType ::= '[' Type ']'` are replaced by upstream's library types `Map<K, V>`
and `Vec<T>`. ddlint stores type text, so this changes only the specification.

Recorded and scheduled rather than changed here (each needs a register item and
roadmap entry under the specification's source-of-truth policy):

- `BG-D7`: no new `DiagnosticCode`. Messages are constants in
  `src/parser/error_messages.rs`; stable codes wait for roadmap item 2.8.1.
- `BG-D9`: the `Seq` operator outside braces (`( a; b )`, unbraced `for`
  bodies, call arguments, vector items, struct fields, match-arm bodies) is not
  upstream; upstream has `;` only inside braces. Together with ddlint's
  rule-level `for` forms (`BG-D8`: upstream's top-level `for` header is an atom
  with an optional `v in` binder, `for (a in A(a))` is rejected upstream, and
  upstream's rule-body `for` is an expression loop over a collection) and the
  remaining tail divergence (`{ if c {a} else {b} ++ x }`, where upstream
  branches are terms), this becomes roadmap 2.6.10 and register item 16.
- `BG-D15`: upstream struct-literal and struct-pattern syntax (`Foo{.x = e}`,
  positional `Foo{e}`) versus ddlint's `Foo { x: e }`. Roadmap 2.6.11, register
  item 17.
- `BG-D16`: the parser forms correct scoping needs: `var x` as an
  expression, `function(…)` closures with typed and `mut` parameters and
  optional return types, `${…}` interpolation segments (on a new expression
  node, not on `StringLiteral`), a token-level type grammar for ascriptions and
  closure parameters, and source-absolute spans on identifier-bearing nodes.
  Roadmap 2.6.12, register item 18.
- `BG-D17`: function bodies (`{ … }` and `= expr`), the function scanner
  fixes they need, and a vendored upstream conformance corpus as a decision
  gate. Roadmap 2.6.13, register item 19.
- `BG-D18`: further upstream expression forms not in the specification:
  `x @ pat` bindings, `v in R(…)` atom binders, the `?` operator, `e[h:l]`
  slices (ddlint uses `e[h, l]`), and adjacent string-literal concatenation.
  Roadmap 2.6.14, register item 20.
- `BG-D19`: upstream type syntax (`typedef`, `bit<N>`, `signed<N>`,
  `bigint`, `double`, `float`) versus the 2.6.7 policy of `type`, sized
  integers, and `f32`/`f64`. Roadmap 2.6.15, register item 22.
- `LS-D0`: the upstream lexical-scope model in `sema` (roadmap 3.3.5,
  ADR-0007, register item 21) and its Verus-proved resolution kernel with a
  `make verus` proof lane (roadmap 3.3.6), following `Appendix A`.
- Lint consequences: roadmap 4.1.3 (`shadowed-variable`) is re-scoped to
  nested-scope shadowing, and new 4.1.5 covers upstream scope errors (unknown
  variables, same-scope redeclaration where the compiler rejects it, a variable
  declared and used in one atom).

## Constraints

- Do not change the meaning of any `DiagnosticCode` (`D-REL-001` to
  `D-REL-008`) or `DiagnosticCategory`; the register freezes them.
- Do not change parenthesized forms: `( e )` yields `Expr::Group`, `()`
  yields `Expr::Tuple(vec![])`, and `(a, b)` yields `Expr::Tuple`.
- Do not change any parse outside braces and bracket literals, and do not
  change the binding power or associativity of any operator; these are scope
  controls, because every other divergence has its own scheduled item.
- Do not change top-level `for` lowering, the reserved-token policy, or the
  relation, index, function, and transformer scanners in this item.
- Do not edit completed historical documents: `docs/execplans/3-3-1-…`,
  `docs/execplans/4-1-2-…`, and the text of completed roadmap items.
- Keep the parser a pure core: no input/output, logging, metrics, or global
  state; no new port or adapter.
- New production code must not panic (`expect`, `unwrap`, `unreachable!`,
  `todo!`, slice indexing).
- No Rust source file over 400 lines; every module, including test modules,
  has a `//!` comment; every `assert!`, `assert_eq!`, and `assert_ne!` has a
  diagnostic message.
- Use the shared Cargo cache; never bypass `RUSTC_WRAPPER` compile admission.
- Every commit passes the gates for its change type; red tests are recorded,
  never committed alone.
- Documentation follows `docs/documentation-style-guide.md`, en-GB Oxford
  spelling, and no first-person prose outside README and feature files;
  `typos.toml` is generated.

## Tolerances (exception triggers)

Base: `git merge-base HEAD origin/main`. Count with
`git diff --numstat <base> -- . ':!docs/execplans/'`, classified by path: code
(`src/`, `tests/`), configuration (`Cargo.*`, `Makefile`, `.github/`,
`typos.local.toml`), and documentation (`docs/`, `*.md`). Limits are net added
lines unless stated.

- Code: stop and escalate above 30 files or 1,600 net lines.
- Configuration: stop on any change except `typos.local.toml` exceptions.
- Documentation: stop above 15 files or 1,200 net lines.
- History behind these numbers: 3.3.1 budgeted 12 files and 800 lines and
  shipped 24 files and 2,595 insertions; 2.6.7 grew from 14 to 54 files. This
  item's code is narrower than either, but rewrites about 50 existing test
  cases.
- Interface: the only public API changes are `Expr::Block`, `BlockExpr` and
  its methods, the `test_util::block` and `test_util::map_entry` helper
  updates, and the parse of `{}`, braces, and `[k -> v]`. Stop on any other
  public signature change.
- Dependencies: none. Stop before adding any.
- Grammar: stop if any input outside braces and bracket literals changes its
  parse.
- Iterations: stop after three failed fix attempts on one gate.
- Time: stop if a milestone runs eight working hours past its first
  `Progress` timestamp.

## Risks

- Risk: `BG-D11` misses a tail or a delimiter. Severity: high. Likelihood:
  medium. Mitigation: one scoped helper pair; tables and generators cover each
  tail and delimiter named in `BG-D11`; seeded faults remove propagation for
  one tail at a time.
- Risk: removing brace maps breaks about 50 existing test cases across
  `src/parser/tests/collections.rs`, `tests/expression_to_sexpr.rs`,
  `tests/collection_literal_boundary.rs`, `tests/expression_prefix.rs`, and
  `src/parser/tests/expression/fixtures/valid/collections.rs`. Severity: low.
  Likelihood: certain. Mitigation: EP-M2 adds bracket maps and rewrites those
  cases before EP-M3 removes brace maps, so no test is written twice.
- Risk: lax ascription parsing (`BG-D5`) lets malformed brace input such as
  `{a: 1}` pass silently. Severity: medium. Likelihood: certain. Mitigation:
  documented in the specification and register item 18; fixed by roadmap
  2.6.12's type grammar.
- Risk: the follow-on programme stalls and outcome 3 is not delivered.
  Severity: high. Likelihood: low. Mitigation: each follow-on item is on the
  roadmap with `Requires` links and the reviewed design in `Appendix A`;
  roadmap 2.6.8 is ticked only for outcomes 1 and 2.
- Risk: `make test` hits Rayon `EAGAIN`. Severity: low. Likelihood: medium.
  Mitigation: rerun once with `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1`.

## Progress

- [x] (2026-10-18T01:00Z) Branch renamed; first reconnaissance.
- [x] (2026-10-18T02:00Z) First draft committed.
- [x] (2026-10-18T03:30Z) First expert review completed.
- [x] (2026-10-23T09:30Z) Second draft pushed; draft pull request #408.
- [x] (2026-11-17T10:00Z) User decisions recorded: empty braces as empty
  block, scheduling of residual deltas, and no Verus for the block
  representation lemma accepted; brace maps to be dropped; upstream lexical
  scope to be modelled; no compatibility with earlier ddlint behaviour.
- [x] (2026-11-17T11:00Z) Rebased onto `origin/main` (`1e63592`); gates pass
  on the rebased branch.
- [x] (2026-11-17T13:00Z) Third draft (single programme) committed.
- [x] (2026-11-17T18:00Z) Second expert review (five reviewers) completed;
  Stage A code facts established.
- [x] (2026-11-17T20:00Z) Fourth draft: scope narrowed to outcomes 1 and 2,
  outcome 3 scheduled with `Appendix A`.
- [x] (2026-11-17T21:00Z) User approved the split, bounded fidelity,
  identifier spans, and the new roadmap and register items; directed that `[]`
  follow upstream (rejected).
- [x] (2026-11-17T21:30Z) Roadmap items 2.6.8 (reworded), 2.6.10 to 2.6.15,
  3.3.5, 3.3.6, 4.1.3 (re-scoped), and 4.1.5, register items 16 to 22, and
  specification section 8.1 created ahead of EP-M1 at the user's request.
- [x] (2026-11-17T22:00Z) User confirmed that rule-level `for` alignment is
  deferred to roadmap 2.6.10, being out of scope for this item.
- [x] (2026-11-17) Plan approved by the user for implementation.
- [x] (2026-11-17) EP-M1: ADR-0006 written and indexed; design section 2
  summarizes it and section 3.2.1 names roadmap 3.3.5 and specification section
  8.1 as the planned change; register items 8, 10 and 15 rewritten.
  Documentation gates pass (`check-fmt`, `markdownlint` with `spelling`,
  `nixie`). Roadmap entries and register items 16 to 22 were completed earlier
  on 2026-11-17.
- [x] (2026-11-17) EP-M2: bracket map literals implemented in
  `parse_vector_literal`; `[]` and mixed element/entry literals rejected with
  the new message constants; `V-M1`, `V-M2`, `V-M4` tables written and green
  (`60` collection tests); `V-B7` characterization snapshots recorded over
  every non-brace fixture; brace-map test cases rewritten to bracket form in
  the unit and integration suites. Specification sections 3.3, 5.2, 6.4, and
  11.6 and register item 10 updated. Gates pass.
- [x] (2026-11-18) EP-M3: block expressions replace brace parsing. Braces now
  build `Expr::Block(BlockExpr)`; brace maps, brace groups, and the
  colon-terminating map-key mode are gone; the `;` state, `BG-D11` delimiter
  clearing, the narrowed struct guard, and the interim `sema` arms are in
  place. Block unit, proptest, behavioural, snapshot, and end-to-end suites are
  green; the rule span scanner now scans an assignment whose right-hand side is
  a control-flow expression, so the acceptance program parses. Ten seeded
  faults (the eight listed faults plus the two `V-B1` non-vacuity faults) were
  each observed red and reverted; the generator covers all 16 classes with
  counts of at least `12`; the mutation run has no missed mutant. Evidence is in
  `Artefacts and notes`. Alignment documentation is complete.
- [x] (2026-11-18) EP-M4: close-out complete. Developers' guide documents both
  block modules, the `items()`/`into_items()` traversal rule, `sema`'s
  dedicated block arms, `BLOCK_ITEM_MIN_BP`, and `test_util::block`; roadmap
  2.6.8 is marked done with its `Done:` note; the full gate set is green; and
  no unrecorded deviation remains.
- [x] (2026-11-18) Third CodeRabbit CLI review triage. The rule span scanner
  now treats a braced left-hand side as one unit, so `1 + { y = obj.field; y }`
  scans as one rule with two body terms. The block classifier reports an
  unclosed block when an enclosing `)` or `]` arrives before `}`, exercised by
  four new table rows. The s-expression renderer derives both groups from
  `items()`, and the block property skips the unrepresentable `{ ; }` draw. The
  end-to-end program gained a statement-bearing block, and its tests use a
  fallible `rstest` fixture, assert a semicolon statement and a final value
  independently, identify the `y` use by source text, and pin its resolution to
  the assignment binding that shadows the head under interim rule 7. The BDD
  scenario now asserts the `else` branch as a block whose value is `go()`.
  Rustdoc was added to the diagnostic aliases and the `map_lit` doctest keeps
  its parse diagnostics. Documentation corrections: users' guide distinguishes
  standalone blocks from struct literals and describes `{k: v}` as an
  ascription block; implementation notes describe the narrowed lookahead and the
  `for` guard's active struct-literal guard. The `is_res` definition names
  `i.name = u.name`. The struct-syntax deferral remains register item 17 and
  roadmap 2.6.11.
- [x] (2026-11-18) Fourth CodeRabbit CLI review triage. A brace statement that
  is followed by an operator no longer ends the rule body early: `{ x } + 2` is
  one term, so `R(x) :- { x } + 2, Other(x).` scans as one rule with two body
  terms, proved red before the fix and green after. `return` inside a rule body
  is now diagnosed at the keyword
  (`return is not valid in a rule body; use a function or closure body`) via an
  `in_rule_body` Pratt context that closure bodies clear, so a closure nested
  in a rule body keeps `return` legal. A map entry cut short by end of input
  reports `expected '->' in map entry`, while a comma or `]` still reports the
  mixed-literal diagnostic. Span extraction in the rule-scanner tests now fails
  loudly on an out-of-range span instead of slicing. The users' guide documents
  both new diagnostics; the characterization, snapshot, property, BDD, and
  end-to-end suites gained the corresponding assertions. Focused suites are
  green: `918` library tests and the affected integration binaries
  (`block_expressions` 5, `block_expressions_bdd` 15,
  `collection_literal_boundary` 4, `expression_to_sexpr` 44).
- [x] (2026-11-18) Hosted-gate follow-ups after the fourth review. CI's
  `lint-whitaker` rejected the differential sweep's nested loop and match with
  `bumpy_road_function`, although the same gate passed locally, so the sweep
  was restructured into counting helpers; that refactor is commit `7f1b3c9`,
  and CI run 44351195440 is green on it. CodeScene then reported introduced
  code duplication in `src/parser/expression/block.rs`, where the reference
  oracle used three same-shaped helpers. The oracle is now a token-by-position
  expectation table, so the sweep still checks every corpus token in every loop
  position, with the accepting and rejecting counts pinned at 16 and 17,
  without duplicating structure or exceeding the Clippy cognitive-complexity
  limit. `cs delta origin/main` reports no findings, and all six gates are
  green on the result (commit `2bfdde4`).

## Surprises & discoveries

- Observation: upstream treats braces as a core block expression, and a
  single-item `eseq` returns the item itself, so `{ e }` and `( e )` are both
  just `e` upstream. Evidence: `braces eseq` and `eseq` in `Parse.hs`;
  `eTuple [a] = a` in `Syntax.hs`. Impact: `BG-D1`; transparency of
  single-value blocks in `Appendix A`.
- Observation: control-flow tails restart at binding power 0 and would
  swallow block separators. Evidence: `parse_if_clause`, the `for` body, the
  `return` value, and pipe-closure bodies call `parse_expr(0)`. Impact:
  `BG-D11`.
- Observation: upstream map literals are `[k -> v, …]`, non-empty; upstream
  vector literals are also non-empty; upstream has no brace map form and no
  brace or bracket type syntax. Evidence: `emap_literal` and `evec_literal` in
  `Parse.hs`. Impact: `BG-D12`, `BG-D14`.
- Observation: ascription types are parsed as expressions, so
  `x: Vec<u32>` misparses and `{a: 1}` parses. Evidence: `parse_type` in
  `src/parser/expression/pratt/mod.rs`. Impact: `BG-D5` limitation; `BG-D16`.
- Observation (Stage A): `var t = amt.group_by(item).sum()` is classified as
  `RuleBodyTerm::Assignment`, not `RuleAggregation`; only a clause that is
  exactly `group_by(p, k)` becomes an aggregation. Upstream's
  `rhsCondition`/`extractGroupBy` splits any clause containing `group_by` into
  a group-by clause binding a hidden `__group` and a following clause. Evidence:
  `parse_rule_body_term` in `src/parser/ast/rule/classification.rs`;
  `Parse.hs` lines 423–448. Impact: `Appendix A` places the barrier from
  `sema`, and `RuleAggregation` does not need a binding field.
- Observation (Stage A): in a rule-body `for (x in xs) y = f(x)`, the `=` is
  at depth zero, so `split_assignment` splits the clause there and pattern
  parsing fails; braced bodies are unaffected. Impact: recorded under roadmap
  2.6.10.
- Observation (Stage A): with `Out(x) :- In(z), var x = 1, Sink(x).`, today's
  `sema` reports the head `x` unused, because heads declare and the later uses
  resolve to the assignment. Impact: confirms the rule-model inversion in
  `Appendix A`.
- Observation: ddlint's normative specification deliberately departs from
  vmware upstream in places (`type` and sized integers instead of `typedef` and
  `bit<N>`, decided by 2.6.7; struct syntax; slice syntax). Impact: fidelity is
  bounded to braces, maps, and scoping here; other divergences are scheduled,
  and a vendored upstream corpus must be normalized to the specification's
  dialect (roadmap 2.6.13).
- Observation: `main` provides `rstest-bdd`, `googletest`,
  `pretty_assertions`, `fixture-lints`, and the behavioural template
  `tests/relation_primary_key_bdd.rs`; ADRs live in `docs/adrs/adr-NNNN-…` with
  0004 (twice) and 0005 taken; roadmap 2.6.9 is taken. Impact: ADR numbers 0006
  and 0007; new roadmap items start at 2.6.10.
- Observation (EP-M3): the rule span scanner could not scan an assignment whose
  right-hand side is a control-flow expression. `rule_statement`'s token loop
  excludes `K_IF`, `K_FOR`, and `K_SKIP`, so
  `var y = if x > 1 { x } else { 0 }` stopped at `if` and the rule body failed
  to scan while `parse_expression` accepted the same text. Evidence: the
  acceptance program reported `Unexpected ... found: Some(K_IF)` at the `if`,
  whereas `Out(y) :- if x > 1 { x } else { 0 }.` scanned; the exclusion is
  pre-existing and `src/parser/span_scanners/rules.rs` was otherwise untouched
  by this plan. Impact: an `assign_stmt` alternative now scans a left-hand side
  followed by `= <statement>`, so the acceptance program parses; two scanner
  cases pin it and the malformed-keyword cases still report their errors.
- Observation (EP-M3): a bare uppercase constructor pattern such as
  `match (x) { A -> 1 }` is rejected by the pattern parser with
  `expected '{' to start a struct pattern`, so the planned tail case
  `{ match (x) { A -> 1 }; y }` uses `_ -> 1` instead. Impact: acceptance text
  amended below; constructor-pattern syntax remains roadmap 2.6.11 work.
- Observation (EP-M3): `sema` records variable uses with the enclosing term's
  span, so two uses inside one assignment share a span. The end-to-end test
  therefore asserts the use count and term text rather than per-identifier
  spans, and checks resolution through a head binding (`y`) because free
  relation-argument variables such as `x` are unresolved by design until
  roadmap 3.3.5.

## Decision log

- Decision: codify braces as block expressions (`BG-D1`, `BG-D2`).
  Rationale: upstream core term; ddlint control flow depends on it; a linter
  must distinguish braces from parentheses. Date/Author: 2026-10-18, planning
  agent.
- Decision: `{}` is an empty block (`BG-D4`). Approved by the user,
  2026-11-17.
- Decision: no Verus proof for the `BlockExpr` representation lemma.
  Approved by the user, 2026-11-17.
- Decision: drop brace maps and adopt `[k -> v]` (`BG-D5`, `BG-D12`,
  `BG-D14`). Directed by the user, 2026-11-17.
- Decision: no migration diagnostics, migration guidance, or breaking-change
  framing. Directed by the user, 2026-11-17 ("NO backwards compatibility");
  this overrides the general `AGENTS.md` bullet on migration guidance for this
  unreleased project. The brace-map migration diagnostic proposed in the third
  draft is withdrawn.
- Decision: split the work. This plan delivers outcomes 1 and 2; outcome 3
  is scheduled as roadmap 2.6.12, 2.6.13, 3.3.5, and 3.3.6 with `Appendix A` as
  their input. Rationale: two independent reviewers found the combined
  programme (about 100 files) too large for one item and pull request, found
  that it placed semantic and proof work ahead of the 2.7 crate split, and
  found further prerequisites (identifier spans, a type grammar, scanner
  fixes). Date/Author: 2026-11-17, planning agent. Approved by the user on
  2026-11-17.
- Decision: bound upstream fidelity to braces, maps, and scoping; record and
  schedule every other divergence without reopening 2.6.7, each with a register
  entry and a roadmap item (2.6.11, 2.6.14, and 2.6.15 for type syntax).
  Date/Author: 2026-11-17, planning agent. Approved by the user on 2026-11-17.
- Decision: defer rule-level `for` alignment to roadmap 2.6.10. This item
  changes neither top-level `for` lowering nor rule-body `for` classification;
  braced `for` bodies simply parse as blocks, and the interim `sema` handling
  keeps today's flattening. Directed by the user on 2026-11-17 ("If it is in
  scope for this task, keep it aligned with upstream. Otherwise, defer."); it
  is not in scope.
- Decision: add source-absolute spans to identifier-bearing AST nodes in
  roadmap 2.6.12, because scope tests and unused-variable diagnostics must
  identify individual occurrences. Date/Author: 2026-11-17, planning agent.
  Approved by the user on 2026-11-17.
- Decision: add roadmap items 2.6.10 to 2.6.15, 3.3.5, 3.3.6, and 4.1.5,
  register items 16 to 22, and re-scope 4.1.3. Approved by the user on
  2026-11-17 and created the same day, ahead of the rest of EP-M1, at the
  user's request; specification section 8.1 was added with them so every
  reference resolves. `mapsplice` could not parse the roadmap (the un-nested
  `2.6.6.1` task), so the entries were added by exact-anchor edits.
- Decision: write the upstream scoping rules into the specification now, as
  a new section 8.1 marked as a scheduled target (register item 21), so the
  normative contract exists before the implementation items begin. Date/Author:
  2026-11-17, planning agent.
- Decision: two ADRs. ADR-0006 (this plan) records block expressions and
  collection syntax. ADR-0007 (roadmap 3.3.5) will record the scope model and
  state that it replaces design section 3.2.1's "head bindings are visible from
  the start of a rule scope". Rationale: different owners after the ADR-0001
  split and different lifecycles. Date/Author: 2026-11-17, planning agent.
- Decision: reject `[]`, as upstream does, naming `vec_empty()` in the
  diagnostic. Directed by the user on 2026-11-17 ("Adhere to upstream").
- Decision: messages are constants, not diagnostic codes (`BG-D7`).
  Rationale: register stable-surface rules; 2.6.7 precedent. Date/Author:
  2026-10-23, planning agent.
- Decision: Kani is not used. `classify_block_follow` has a finite domain and
  is enumerated exhaustively; nothing in this item has `unsafe` code,
  arithmetic, or a bounded state machine. Date/Author: 2026-11-17, planning
  agent.

## Outcomes & retrospective

### EP-M1 outcome

Complete. `docs/adrs/adr-0006-block-expressions-and-collection-syntax.md`
records decisions `BG-D1` to `BG-D6`, `BG-D11`, `BG-D12` and `BG-D14` with the
upstream evidence, the representation comparison, the rejected options, the
scheduled non-goals, and the consequences for the ADR-001 split. Design section
2 states the block and collection contract and links the record; section 3.2.1
names roadmap 3.3.5 and specification section 8.1 as the planned scope
alignment. Register item 15 now states the block-expression target, item 10
withdraws brace maps and `[]` in favour of `[k -> v]`, and item 8 records the
upstream `for` differences with a pointer to item 16. Documentation gates
passed: `make check-fmt`, `make markdownlint` (which runs `make spelling`), and
`make nixie`.

### EP-M2 outcome

Complete. `parse_vector_literal` parses the first element and branches on
`T_ARROW` into bracket map entries, so `[k -> v]` produces `Expr::MapLit`
without backtracking; a mixed literal reports `MIXED_COLLECTION_ENTRY_ERROR` at
the first offending item, and `[]` reports `EMPTY_VECTOR_LITERAL_ERROR`. `V-M1`,
`V-M2`, and `V-M4` are covered by the rewritten
`src/parser/tests/collections.rs` (60 passing cases, including exact rejection
spans), and the integration suites `tests/collection_literal_boundary.rs`,
`tests/expression_prefix.rs`, and `tests/expression_to_sexpr.rs` use the
bracket form and pass. The `V-B7` characterization in
`src/parser/tests/expression/characterization.rs` records the rendering and
first diagnostic of every non-brace fixture in two inline snapshots, giving
EP-M3 a regression fence. Brace maps still parse; nothing new depends on them.

### EP-M3 outcome

Complete. `src/parser/expression/block.rs` parses brace items at
`BLOCK_ITEM_MIN_BP` with `classify_block_follow`, and
`src/parser/ast/expr/block.rs` stores `statements` and `value` behind
`Expr::Block`. The `;`-terminating Pratt flag is scoped and cleared at every
nested delimiter. Brace groups, brace maps, the colon-terminating map-key mode,
and their parser functions are deleted, and the struct-literal guard now fires
only on `{ {` or `{ ident :`.

Evidence: `src/parser/tests/block_expressions.rs` (block, tail, delimiter,
rejection, and S-expression tables), its `props` and `strategy` modules,
`src/parser/tests/block_expression_snapshots.rs` (one accepted inline snapshot),
`tests/block_expressions_bdd.rs` (15 passing scenarios), and
`tests/block_expressions.rs` (5 passing end-to-end tests through `parse`,
`sema::build`, and `Runner`). The semantic model records the block branch and
the bracket map key of the acceptance program, and `Runner` reports `Witness`
but not `Checked`. The full parser library suite passed 790 tests after the
change, and the focused expression, collection, span, and rule suites were
green.

Two discoveries changed the planned test text and one changed production code:
the `match` tail uses `_ -> 1` because bare constructor patterns are not
supported, and `rule_statement` gained an `assign_stmt` alternative so the
acceptance program's `var y = if …` scans. Both are recorded under
`Surprises & discoveries`.

Verification evidence: all ten seeded faults (the eight listed faults and the
two `V-B1` non-vacuity faults) were applied, rejected by the named test, and
reverted; `generator_covers_every_class` counts 16 generator classes over 256
deterministic draws, the smallest count being `12`; and the mutation run over
both block modules has no missed mutant after the two survivors were killed by
new tests. The `V-B6` method was amended from an every-`SyntaxKind` sweep to a
semantically exhaustive nine-token sweep checked against an independent
reference table, because the classifier distinguishes only `}` and `;`. Details
and log paths are in `Artefacts and notes`.

### EP-M4 outcome

Complete. The developers' guide documents `BlockExpr`, both `block.rs` modules,
the `items()`/`into_items()` traversal rule, `sema`'s dedicated `Expr::Block`
and `Expr::Group` arms, ownership of `BLOCK_ITEM_MIN_BP`, and the
`test_util::block` helper. Roadmap 2.6.8 is marked done with its `Done:` note,
and this plan is `COMPLETE`.

The first full gate run exposed quality findings that the earlier focused runs
could not, and all were fixed at source rather than suppressed:

- Clippy rejected the nested scope wrappers. They became three named helpers,
  `with_block_item_scope`, `with_nested_delimiter_scope`, and
  `with_guard_expression_scope`, each combining the `;` state with the
  struct-literal state it needs. `rule_statement` was split by extracting
  `assignment_statement`, and the follow-token table was merged into
  `(AfterItem | AfterSeparator, Some(T_RBRACE)) => Close`.
- Clippy also required `mod.rs` layout for new modules, `map_or_else` instead
  of `match` on `Option`, no shadowed bindings, and no bare `let _` on a
  `#[must_use]` result.
- Whitaker reported module sizes over 400 lines and two bumpy-road functions.
  `assignment_statement` moved to `span_scanners/rules_assign.rs`, the rule
  statement tests moved to `span_scanners/tests/rule_statement_tests.rs`,
  `relation_name` gained `callee_variable_name` and `block_relation_name`
  helpers, and `walk_variable_uses_composite` split into the positional and
  container walkers. Whitaker also required replacing `unwrap_or_else`-with-
  panic closures in test helpers where the lint flags them.

The final sequential gate run is green: `make check-fmt` (2s), `make typecheck`
(48s), `make lint` (182s; interrogate 184/184, clippy, rustdoc, clippy-expect
policy, Whitaker, and spelling all clean), `make test` (237s; 53 suites, 1552
tests, 0 failures, doctests included), `make markdownlint` (73 files, 0
issues), and `make nixie` (all diagrams valid). Logs are named
`/tmp/<gate>-ddlint-2-6-8-decide-brace-group-extension-policy.out`.

## Context and orientation

ddlint is a Rust linter for DDlog. A tokenizer (`src/tokenizer.rs`) produces
`SyntaxKind` tokens (`src/language.rs`); span scanners
(`src/parser/span_scanners/`) find declarations; a lossless concrete syntax
tree (CST) is built with `rowan`. Expressions are parsed on demand into an
abstract syntax tree (AST) by a handwritten Pratt parser, a precedence-climbing
parser driven by binding powers.

Files that matter:

- `src/parser/expression/pratt/mod.rs`: `parse_expression(src)`,
  `parse_expr(min_bp)`, `parse_type`, and the scoped struct-literal and
  `;`-terminating states.
- `src/parser/expression/block.rs`: block item parsing,
  `classify_block_follow`, and the block diagnostics.
- `src/parser/expression/infix.rs`: the infix loop; the colon-terminating
  mode is gone.
- `src/parser/expression/prefix.rs`: prefix dispatch for `T_LBRACE` and
  `T_LBRACKET`.
- `src/parser/expression/data_structures.rs`: `parse_vector_literal`,
  `parse_parenthesized_expr`, `parse_closure_literal`, `parse_ident_expression`
  (struct-literal guard), `parse_struct_fields`; the brace map and brace group
  parsers are gone.
- `src/parser/expression/control_flow.rs`: `if`, `match`, `return`, `for`.
- `src/parser/expression/pratt/postfix.rs`: call arguments and bit slices.
- `src/parser/ast/expr/mod.rs`: the public `Expr` enum, including
  `Expr::Block(BlockExpr)`; `MapLit` documents `[k -> v, …]`.
- `src/parser/ast/expr/block.rs`: `BlockExpr` and its item-order API.
- `src/parser/ast/expr/sexpr.rs`: S-expression rendering for tests.
- `src/parser/ast/precedence.rs`: binding powers; private `BP_SEQ = 0` and
  `BLOCK_ITEM_MIN_BP = BP_SEQ + 1`.
- `src/sema/resolve.rs`, `src/sema/variables.rs`: consumers with dedicated
  `Expr::Block` and `Expr::Group` arms (`relation_name`, use collection).
- `src/parser/ast/rule/classification.rs`:
  `classify_for_body_with_aggregation_tracking` flattens `BlockExpr`.
- `src/parser/top_level_for/lowering.rs`: `collect_lowered_terms`.
- `src/parser/error_messages.rs`: crate-private message constants, selected
  ones re-exported through `src/test_util/mod.rs`.
- `src/test_util/expressions.rs`: `var`, `call`, `block`, `map_lit`,
  `map_entry`.

Terms: a "block" is `{ s1; …; sn; v }`; "statements" are items followed by `;`;
the "value" is the optional final item; "unit" is `()`. A "tail" is the part of
a prefix construct parsed after its header. A "delimiter" is a bracketed
construct that opens a nested expression context. A "seeded fault" is a
temporary deliberate bug that proves a test can fail; "non-vacuity" is evidence
that a check can fail.

## Conformance basis

- `docs/roadmap.md` item 2.6.8 (`RM-2.6.8`); new items 2.6.10 to 2.6.15,
  3.3.5, 3.3.6, and 4.1.5; re-scoped 4.1.3.
- `docs/parser-conformance-register.md` items 8, 10, and 15 (`REG-15`); new
  items 16 to 22; maintenance rules; stable diagnostic surface.
- `docs/differential-datalog-parser-syntax-spec-updated.md`: source-of-truth
  policy (`SPEC-POLICY`), sections 3.3, 4, 5.2, 5.3, 5.10, 6.4, 9, 10, 11.6,
  12, 13's compatibility-policy bullet (`SPEC-13-COMPAT`), and 14; new sections
  3.4 (blocks) and 8.1 (variable scoping, target).
- `docs/haskell-parser-analysis.md`, "Expression grammar" (`UP-ESEQ`), and
  the upstream sources cited in `Appendix A` (`UP-SCOPE`), from
  `vmware-archive/differential-datalog` at `master`; EP-M1 records the commit
  hash.
- `docs/ddlint-design.md` sections 2 and 3.2.1.
- `docs/adrs/adr-0001-parser-crate-split.md` (`ADR-0001`; expression AST
  ownership is decided at roadmap 2.7.1, and new node variants then need joint
  approval).
- Governing standards: `AGENTS.md`, `docs/documentation-style-guide.md`,
  `docs/developers-guide.md`, `docs/repository-layout.md`.

No Terms of Reference document exists for ddlint; none is invented.

Trace links:

```plaintext
SPEC-13-COMPAT + UP-ESEQ -> RM-2.6.8 -> REG-15 -> BG-D1..BG-D6, BG-D11 -> EP-M3 -> V-B1..V-B8
user direction (maps) -> BG-D5, BG-D12, BG-D14 -> EP-M2, EP-M3 -> V-M1, V-M2, V-M4 -> REG-10
user direction (scope) + UP-SCOPE -> Appendix A -> EP-M1 -> spec 8.1 + REG-21 + RM-3.3.5, RM-3.3.6
SPEC-POLICY -> BG-D8, BG-D9, BG-D15..BG-D19 -> EP-M1 -> REG-16..REG-20, REG-22, RM-2.6.10..RM-2.6.15
LS-D0 lint consequences -> EP-M1 -> RM-4.1.3 (re-scoped), RM-4.1.5
all -> EP-M4 -> developers' guide, contents, RM-2.6.8 done
```

## Verification plan

### Abstract model

An abstract block is items `[e1, …, en]` (`n ≥ 0`) with a flag `t` ("ends in
`;`"), well-formed when `t ⇒ n ≥ 1`. Its value is `en` when `n ≥ 1 ∧ ¬t`, else
unit. The representation relation `R` maps it to
`BlockExpr { statements, value }`: when `n ≥ 1 ∧ ¬t`,
`statements = [e1 … e(n-1)]` and `value = Some(en)`; otherwise
`statements = items` and `value = None`. Executable forms:
`BlockExpr::from_items(items, t)` (returning `None` exactly for `([], true)`),
`items()`, and `ends_with_semicolon()`.

### Obligations

- `V-B1` (representation bijection): `R` is a bijection between well-formed
  abstract blocks and `BlockExpr` values. Method: named `rstest` cases with
  explicit `BlockExpr` expectations (`block_renders_as_sexpr` and the
  `parses_blocks` table) and a `proptest` round trip over lengths 0 to 4. The
  round trip's oracle calls `from_items` too, so it checks the parser against
  the representation rather than the representation itself; the named cases and
  the seeded faults carry that part of the obligation. Non-vacuity: seeded
  faults "accept `([], true)`" and "move the last statement into `value` when
  `t` holds" must fail. Verus: not used (approved); the property follows from
  the type's shape and a three-case split that the round trip covers completely.
- `V-B2` (no swallowed separator): inside a block item every `BinaryOp::Seq`
  lies beneath a delimiter node within that item.
- `V-B3` (order): the source order of items equals `items()` order.
- `V-B4` (braces versus parentheses): braces never produce `Group`;
  parentheses never produce `Block`.
- `V-B5` (parse correspondence): for item sources drawn from the
  generator's grammar (identifiers, literals, `+`, `and`, calls, tuples,
  parenthesized sequences, nested blocks, `[k -> v]` maps, and the tails
  `if`/`else`, `else if`, `for`, pipe closures, `return`), parsing
  `{ s1; …; sn[;] }` yields `Expr::Block(from_items([ê1 … ên], t))`, where `êi`
  is built independently by the generator. Sketch: induction on `n`; each item
  stops at the next `;` or `}` because the `;`-terminates state is set,
  inherited by every tail, and cleared and restored at every delimiter.
- `V-B6` (follow classification is total):
  `classify_block_follow(position, kind)` over positions {opening, after item,
  after separator} and every `SyntaxKind` plus end of input yields exactly one
  outcome.
- `V-B7` (no change outside braces and bracket literals): every existing
  expression fixture without `{` or `->` inside brackets renders identically
  before and after.
- `V-B8` (panic freedom): any token sequence beginning with `{` parses to
  `Ok` or `Err` without panicking, with error spans `start <= end <= src.len()`.
- `V-M1` (bracket maps): `[k1 -> v1, …, kn -> vn]` (`n ≥ 1`, optional
  trailing comma) yields `Expr::MapLit` with entries in order; `[a, b]` yields
  `VecLit`; `[]` is rejected with `EMPTY_VECTOR_LITERAL_ERROR`.
- `V-M2` (no mixing): a bracket literal containing both an element and an
  entry is rejected with `MIXED_COLLECTION_ENTRY_ERROR` at the first offending
  item.
- `V-M4` (lowering boundary preserved): `MapLit` from `[k -> v]` is
  preserved raw by `parse()`, as register item 10 requires.

### Methods and non-vacuity

- `V-B2` to `V-B5`: class-first `proptest` generators emitting
  `(source, expected Expr)` pairs. Each class is built, never filtered; item
  sources are a fixed list of forms that each parse as exactly one item, and
  `a` versus `b` makes order observable. Classes: empty block; one value; one
  statement with trailing `;`; two or more items with and without trailing `;`;
  nesting depth two or more; first item a nested block or map; the `if`, `for`,
  `return`, and closure tails; tuple, parenthesized-sequence, vector, call, and
  binary items. A `generator_covers_every_class` test counts these 16 classes
  over 256 draws from `TestRunner::deterministic()` and asserts each count is
  at least 5; the observed counts are recorded in `Artefacts and notes`. Named
  `rstest` tables give one witness per class, including the `match`,
  assignment, and `else if` forms the generator does not draw.
- `V-B6`: amended during implementation. The classifier distinguishes only
  the close brace, the separator, and an enclosing delimiter among concrete
  tokens, so the test sweeps an eleven-token corpus (end of input, `{`, `}`,
  `;`, `,`, `:`, an identifier, a number, `+`, `)`, and `]`) over the three
  positions, which covers every semantic class rather than restating the
  `match` for every `SyntaxKind`. It compares the result with an independently
  shaped reference decision table and counts accepted and rejected outcomes
  (`16` and `17`). The whole-input table remains the stronger evidence,
  exercising every outcome through the real parser with exact message and span.
- `V-B7`: an inline `insta` characterization snapshot of all existing
  expression fixtures, taken in EP-M2 before any block change and paired with
  semantic assertions on representative rows.
- `V-B8`: two `proptest` properties of at least 2,048 cases over
  space-separated token sequences: a structural fuzz over
  `{ } ( ) [ ] ; , : -> a b 1 + if else for in return | Foo`, and a
  differential property against a small reference recognizer over
  `{ } ; , : a 1 +` that predicts accept or reject and, when accepted, the root
  form. Each asserts that `Ok` and `Err` both occur at least 100 times.
- `V-M1`, `V-M2`, `V-M4`: `rstest` tables, including `[(a, b) -> c]`,
  `[f(x) -> y]`, `[a -> |x| x, b -> 1]` (the closure tail must stop at `,`),
  nested maps with trailing commas, and `[[k -> v] -> 1]`.
- Seeded faults, each recorded with the test that rejects it: reverse
  statements; drop the trailing-`;` flag; parse items without the
  `;`-terminates state; omit propagation into `for` bodies only; omit clearing
  inside call arguments (must break `{ f(a; b) }`); return `Group` for `{ e }`;
  set the state at top level (must break `V-B7`); accept a mixed bracket
  literal.
- `cargo mutants --file src/parser/expression/block.rs --file src/parser/ast/expr/block.rs`
  after green; survivors are killed by a new test or justified in
  `Decision log`.

### Axioms

- `A1`: `logos` tokenizes `{`, `}`, `;`, `,`, `:`, and `->` as their
  `SyntaxKind`s; existing tokenizer tests cover this.
- `A2`: `chumsky::error::Simple::custom` stores span and message unchanged.
- `A3`: `proptest` explores and replays as documented.

### Where formal verification is and is not warranted

No proof is planned in this item: its obligations concern the correspondence
between a grammar and a handwritten parser, which tests exercise directly, and
its only pure kernels are a three-case representation (approved without Verus)
and a finite classifier (enumerated exhaustively). Verus is warranted for the
scope-resolution kernel in roadmap 3.3.6; `Appendix A` states its model and
lemma sketch.

## Plan of work

### Stage A: understand and propose (complete)

Code facts were established by reading (`Surprises & discoveries`, Stage A
entries). No code changes before approval.

### EP-M1: decisions, specification targets, register, and roadmap

Documentation only.

- Write `docs/adrs/adr-0006-block-expressions-and-collection-syntax.md` from
  the style guide's ADR template: status `Accepted` with the approval date and
  a one-line summary; context (`SPEC-13-COMPAT`, `UP-ESEQ`, upstream commit
  hash, register items 10 and 15); decisions `BG-D1` to `BG-D6`, `BG-D11`,
  `BG-D12`, `BG-D14`; architectural rationale (no flag field in `BlockExpr`;
  the pure classifier; the scoped `;` state); rejected options (remove braces;
  reuse `Expr::Group`; `Block(Box<Expr>)` over `Seq`; a flat list with a flag;
  keeping brace maps); non-goals (`BG-D7` to `BG-D9`, `BG-D15` to `BG-D18`,
  `LS-D0`); consequences (ADR-0001 joint approval for later `Expr` changes;
  `sema` exhaustiveness relies on `Expr` not being `#[non_exhaustive]`). Index
  it and this plan in `docs/contents.md`.
- Summarize ADR-0006 in `docs/ddlint-design.md` section 2, after the 2.6.7
  paragraph. Add to section 3.2.1 a short "Planned change" paragraph naming
  roadmap 3.3.5 and specification section 8.1.
- Specification section 8.1, "Variable scoping (target)": done on
  2026-11-17 with the roadmap entries.
- Register: rewrite item 15's target text (status stays `scheduled`); note
  in item 10 that brace maps and `[]` are withdrawn and `[k -> v]` is the
  target; note in item 8 the upstream `for` differences and a pointer to item
  16.
- Roadmap entries and register items 16 to 22: done on 2026-11-17
  (`Appendix B`).

### EP-M2: bracket map literals

Additive. Red: add the `V-M1`, `V-M2`, and `V-M4` tables and the bracket-map
behavioural scenarios; take the `V-B7` characterization snapshot; record the
failures (`[k -> v]` reports `expected T_RBRACKET`). Green: in
`parse_vector_literal`, after the first element, branch on `T_ARROW` into entry
parsing (`parse_map_entries_bracketed`), reject mixed literals, and require at
least one entry. Rewrite every brace-map test case to bracket form, and update
the `MapLit` doc comment and the `test_util::map_lit` example. Brace maps still
parse at the end of this milestone; nothing new depends on them. Update
specification section 3.3, 5.2 (`BG-D14`), 6.4, and 11.6, and register item 10
(status `implemented` for bracket maps, brace maps noted as pending EP-M3).

### EP-M3: block expressions replace brace parsing

Red: add `src/parser/ast/expr/block.rs` (`BlockExpr`, `from_items`, `items`,
`into_items`, `ends_with_semicolon`, `Default`), the `Expr::Block` variant, the
`(block …)` S-expression arm, the `test_util::block` helper, the message
constants, and `sema` arms that compile; then the block tests and scenarios in
`Validation and acceptance`; record the failures. Green, in one commit with red:

- Create `src/parser/expression/block.rs` with `ItemPosition`,
  `BlockFollow`, `classify_block_follow`, `parse_block`, `parse_block_items`,
  and `finish_block`; dispatch `T_LBRACE` to `parse_block`.
- Delete `parse_brace_or_map_literal`, `parse_map_entries` (brace form),
  `parse_brace_group_continuation`, `parse_map_key_expr`,
  `parse_infix_with_colon_mode`, and `parse_expr_for_map_key`.
- Add `pub(crate) const BLOCK_ITEM_MIN_BP: u8 = BP_SEQ + 1;` to
  `precedence.rs`.
- Add the `;`-terminates state beside the struct-literal state, combined with
  it by `with_block_item_scope`, and restore both at every delimiter named in
  `BG-D11` through `with_nested_delimiter_scope`. A `for` guard uses
  `with_guard_expression_scope`.
- Narrow the struct-literal guard (`BG-D6`).
- `sema` interim semantics, replaced by roadmap 3.3.5: `relation_name(Block)`
  is `relation_name(v)` when `statements` is empty and `value` is `Some(v)`,
  else `None`; the use collectors visit `items()` in order; for-body
  classification flattens `into_items()` in order; top-level `for` lowering
  keeps treating a braced body as an unsupported head.
- Update the existing tests listed in `Validation and acceptance`.

Refactor, then the alignment commit: specification section 3.4 (grammar below),
sections 4, 5.10, 9, 10, 11.6, 12, and 14; register items 10 and 15
(`implemented`); `docs/parser-implementation-notes.md` (struct-literal guard
paragraph, an "Expression parser invariants" entry for `BLOCK_ITEM_MIN_BP`, the
`;` state, `classify_block_follow`, and `V-B2` to `V-B4`, and the file index);
the users' guide ("Block expressions and collection literals", describing
current behaviour only); and `CHANGELOG.md` under `## Unreleased`, as plain
`### Changed` and `### Added` entries without migration text.

```ebnf
BlockExpr  ::= '{' BlockBody? '}'
BlockBody  ::= Expr (';' Expr)* ';'?
MapLit     ::= '[' MapEntry (',' MapEntry)* ','? ']'
MapEntry   ::= Expr '->' Expr
```

### EP-M4: close-out

Developers' guide: module structure entries for both `block.rs` files; a
boundary rule that `BlockExpr::items()` and `into_items()` are the only
traversal order and that `sema` handles `Expr::Block` and `Expr::Group` in
dedicated arms; ownership of `BLOCK_ITEM_MIN_BP` and the `;` state; the
`test_util::block` helper. Reconcile every upstream artefact; mark roadmap
2.6.8 done with a "Done:" note after all acceptance passes; set this plan to
`COMPLETE`.

### Abstraction policy compliance

Sweep: no block type, follow-token classifier, or `;`-aware item loop exists;
`parse_comma_separated_expressions` is unsuitable because a trailing `;`
changes the value. Reused: the struct-literal state pattern for the `;` state;
`parse_comma_separated_expressions` for bracket map entries' comma handling
where its shape fits. Re-use policy: `items()` and `into_items()` are the only
traversal order; `classify_block_follow` is private to `block.rs` with one
caller; `BLOCK_ITEM_MIN_BP` is owned by `precedence.rs`. Recorded in the
developers' guide at EP-M4.

## Milestones and plateaus

Every milestone ends with `make check-fmt`, `make typecheck`, `make lint`, and
`make test` passing, plus `make markdownlint` and `make nixie` when
documentation changed; logs go to `/tmp/<gate>-ddlint-<branch>.out`.

Shared conformance checks, one line each, applied at every milestone:

- The milestone's decisions are implemented as written.
- Parses outside braces and bracket literals are unchanged.
- Upstream assumptions cited for the milestone still hold.
- Only planned public interfaces changed; no dependency was added.
- No trust-boundary or persisted-format change.
- Trace links are current.

Compatibility decision for every milestone: none. ddlint is unreleased (no
tags; the `0.1.0` in `Cargo.toml` is a placeholder) and the user requires no
compatibility with earlier ddlint behaviour; callers are updated in place.

### EP-M1

- Outcome: ADR-0006, specification section 8.1 (target), register items 8,
  10, 15 to 22, and roadmap items in place (roadmap, register items 16 to 22,
  and section 8.1 created on 2026-11-17).
- Requirements and gaps: `RM-2.6.8` (decision part), `SPEC-POLICY`, user
  direction on scope (recorded and scheduled).
- Acceptance evidence: gates pass; the ADR has the template's required
  sections; every new roadmap item has a `Requires` line and a register
  reference.
- Milestone-specific check: no code changed.
- Recovery: revert the documentation commits.
- Remaining gaps: all code.

### EP-M2

- Outcome: bracket maps parse; brace-map tests rewritten; the `V-B7`
  snapshot exists.
- Requirements and gaps: `BG-D12`, `BG-D14`, `V-M1`, `V-M2`, `V-M4`.
- Acceptance evidence: map tables, scenarios, and seeded fault "accept a
  mixed literal" recorded.
- Milestone-specific check: no expression without `->` inside brackets
  changes its parse.
- Recovery: revert to the EP-M1 commit.
- Remaining gaps: braces still form maps.

### EP-M3

- Outcome: braces always form blocks; the colon-terminating mode is gone;
  register items 10 and 15 are `implemented`.
- Requirements and gaps: `BG-D1` to `BG-D6`, `BG-D11`, `V-B1` to `V-B8`.
- Acceptance evidence: block suites pass; seeded-fault, class-count, and
  mutation evidence recorded in `Artefacts and notes`; the `V-B7` snapshot
  unchanged.
- Milestone-specific check: `sema` interim semantics match the `Group`
  behaviour they replace.
- Recovery: revert to the last green commit.
- Remaining gaps: developers' guide; outcome 3 (scheduled).

### EP-M4

- Outcome: documentation reconciled; roadmap 2.6.8 done.
- Requirements and gaps: `AGENTS.md` documentation obligations.
- Acceptance evidence: all gates pass.
- Milestone-specific check: no unrecorded deviation.
- Recovery: documentation only.
- Remaining gaps: roadmap 2.6.10 to 2.6.15, 3.3.5, 3.3.6, 4.1.5 by design.

## Concrete steps

All commands run from the repository root, sequentially:

```sh
BRANCH=$(git branch --show-current)
set -o pipefail
make fmt 2>&1 | tee "/tmp/fmt-ddlint-${BRANCH}.out"
make check-fmt 2>&1 | tee "/tmp/check-fmt-ddlint-${BRANCH}.out"
make typecheck 2>&1 | tee "/tmp/typecheck-ddlint-${BRANCH}.out"
make lint 2>&1 | tee "/tmp/lint-ddlint-${BRANCH}.out"
make test 2>&1 | tee "/tmp/test-ddlint-${BRANCH}.out"
make markdownlint 2>&1 | tee "/tmp/markdownlint-ddlint-${BRANCH}.out"
make nixie 2>&1 | tee "/tmp/nixie-ddlint-${BRANCH}.out"
```

`make fmt` may reformat unrelated Markdown; restore unrelated churn before
committing. If `make test` fails with `Resource temporarily unavailable`, rerun
it once as `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1 make test`.

Focused commands use the same flags as `make test` so they share its build
cache:

```sh
export RUSTFLAGS="-D warnings"
cargo test --all-targets --all-features map_literal          # EP-M2
cargo test --all-targets --all-features block_expr           # EP-M3
cargo test --all-targets --all-features block_expression_proptest
cargo test --all-features --test block_expressions_bdd
cargo test --all-features --test block_expressions
```

Expected red output (abridged) at EP-M3:

```plaintext
assertion `left == right` failed: `{ x }` must parse as a block
Diff < left / right > :
<Group(
>Block(
```

Commit after each green step with an imperative subject of 50 characters or
fewer and a body wrapped at 72 columns.

## Validation and acceptance

### Behavioural specification

`tests/features/block_expressions.feature` is bound by
`tests/block_expressions_bdd.rs`, following
`tests/relation_primary_key_bdd.rs`: a world struct with `RefCell` fields,
`#[allow_fixture_expansion_lints]` on fixtures, and steps returning
`StepResult`. "Parsing fails with" means some reported error message equals the
quoted text exactly; item lists use data tables and are compared by parsing
each expected item, not by text.

```gherkin
Feature: Block expressions and collection literals
  Braces delimit blocks whose value is their final item unless the block is
  empty or ends with a semicolon. Map literals use brackets and arrows.

  @BG-D3 @V-B5
  Scenario: Semicolons separate block statements in order
    Given the expression source "{ a; b; c }"
    When the expression is parsed
    Then the result is a block with statements
      | item |
      | a    |
      | b    |
    And the block value is "c"

  @BG-D3 @V-B1
  Scenario: A trailing semicolon makes the block value unit
    Given the expression source "{ a; }"
    When the expression is parsed
    Then the result is a block with statements
      | item |
      | a    |
    And the block has no value

  @BG-D4
  Scenario: Empty braces are an empty block
    Given the expression source "{}"
    When the expression is parsed
    Then the result is a block with no statements
    And the block has no value

  @BG-D11 @V-B2
  Scenario: An if expression inside a block does not swallow the separator
    Given the expression source "{ if c { a } else { b }; d }"
    When the expression is parsed
    Then the result is a block with statements
      | item                  |
      | if c { a } else { b } |
    And the block value is "d"

  @BG-D12 @V-M1 @V-B4
  Scenario Outline: Brackets build vectors and maps; braces build blocks
    Given the expression source "<source>"
    When the expression is parsed
    Then the result is a <form>

    Examples:
      | source           | form                |
      | [a, b]           | vector literal      |
      | [a -> 1, b -> 2] | map literal         |
      | { x }            | block               |
      | ( x )            | parenthesized group |
      | { x: T }         | block               |

  @V-B6 @V-M2
  Scenario Outline: Malformed braces and brackets are rejected
    Given the expression source "<source>"
    When the expression is parsed
    Then parsing fails with "<message>"

    Examples:
      | source       | message                                                   |
      | {a: 1, b: 2} | expected ';' or '}' after a block item                    |
      | [a, b -> c]  | cannot mix vector elements and map entries in one literal |
      | []           | empty vector literals are not DDlog syntax; use vec_empty() instead |
      | { a, b }     | expected ';' or '}' after a block item                    |
      | { a;; b }    | empty statement in block: remove the extra ';'            |

  @BG-D6
  Scenario: Braced if branches with an empty block parse
    Given the expression source "if ready {} else { go() }"
    When the expression is parsed
    Then the result is an if expression whose then branch is an empty block
```

### Unit and property suites

- Blocks (`src/parser/tests/block_expressions.rs`, its proptest module with
  the generator in a submodule, and `src/parser/expression/block.rs` unit
  tests). Accepted: `{ x }`, `{ 1 }`, `{ f() }`, `{ (x, y) }`, `{ 1 + 2 }`,
  `{ a; b }`, `{ a; b; }`, `{ a; }`, `{}`, `{ { a } }`, `{ {a}; b }`,
  `{ (a; b) }`, `{ x: T }`, `{ a; x: T }`, `{ return }`, `{ x = 1; x }`,
  `{ |x| x; y }`, `{ match (x) { _ -> 1 }; y }` (a bare constructor pattern
  such as `A` is not yet supported, see `Surprises & discoveries`),
  `{ [a -> 1] }`. Tails (each gives two items): `{ if c { a } else { b }; d }`,
  `{ if c { a }; d }`, `{ if a { x } else if b { y }; z }`,
  `{ for (x in xs) { f(x) }; y }`, `{ for (x in xs) f(x); g() }`,
  `{ return x; y }`, `{ a = if c { x } else { y }; z }`. Delimiters still allow
  `Seq` (unchanged behaviour, register item 16): `{ (a; b); c }`, `{ f(a; b) }`,
  `{ [a; b] }`, `{ match (x) { A -> a; b } }`. Rejected with exact message and
  span: `{ a, b }`, `{ a; b, c }`, `{ a; b c }`, `{ ; }`, `{ a;; b }`, `{ a`,
  `{ a;`, `{a: 1, b: 2}`, and `{ var x = 1; x }` (existing "unexpected token:
  K_VAR" until roadmap 2.6.12).
- Struct-literal guard (`tests/expression_compound.rs`): accepted
  `if ready {} else { go() }`, `if ready {}`, `for (x in xs if ok) {}`;
  rejected with the struct-literal message `if x == Foo{} { a }`,
  `for (x in xs if y == Foo{}) {}`, and `if c { x: 1 } else { d }` (the kept
  heuristic).
- Maps (`src/parser/tests/collections.rs`, rewritten): `[a -> 1]`,
  `[a -> 1,]`, `[a -> 1, b -> 2]`, `[a -> [b -> 2]]`, `[[k -> v] -> 1]`,
  `[(a, b) -> c]`, `[f(x) -> y]`, `[a -> |x| x, b -> 1]`, `[a, b]`; rejected
  `[]`, `[a, b -> c]`, `[a -> 1, b]`, `[a ->]`, `[-> a]`.

Existing tests whose expectations change, and only these:

- `src/parser/tests/collections.rs`: every brace-map case (rewritten to
  bracket form in EP-M2), `parses_brace_groups` (now `Block`), the `{}` cases
  at lines 59 and 242, `{{}: 1}`, and the `[]` cases (`empty`, `nested_empty`,
  `empty_vec`), which move to the rejected table in EP-M2.
- `src/parser/tests/expression/fixtures/valid/collections.rs`: brace-map
  cases, `"{}"`, and `"[]"` (moved to the rejected fixtures).
- `tests/expression_prefix.rs`: `{1}`, `{}`, `{ x }`, `{ return }`, and
  `[]` (now rejected).
- `tests/expression_to_sexpr.rs`: `map_literals_format` rows (bracket form),
  `{}` rendering `(block)`, and the `[]` row (removed; the `(vec)` rendering of
  a directly built empty `VecLit` stays covered by
  `src/parser/tests/collections.rs` case `vector_empty`).
- `tests/collection_literal_boundary.rs`: three brace-map programs
  (bracket form).
- `src/parser/tests/control_flow.rs`: `{ break }`, `{ continue }`,
  `{ return }`.
- `src/parser/tests/expression/fixtures/valid/control_flow.rs` and
  `tests/expression_compound.rs`: braced branches become `Block`.
- `src/parser/ast/rule/tests.rs`:
  `for_loop_with_sequence_body_flattens_all_statements` keeps its result.

`src/parser/tests/rules/aggregations.rs` matches a parenthesized key and must
not change.

### Snapshot and public-API tests

- `src/parser/tests/block_expression_snapshots.rs`: one inline `insta`
  snapshot rendering every accepted and rejected block and collection input as
  its source followed by `to_sexpr()` or `message @ start..end`, paired with
  semantic assertions on representative rows. Rendering: `{}` is `(block)`,
  `{ a }` is `(block (value a))`, `{ a; }` is `(block (stmts a))`, and
  `[a -> 1]` is `(map (entry a 1))`.
- `tests/block_expressions.rs`: the end-to-end test at the library boundary
  (the command-line binary is a placeholder until roadmap 5.1), through
  `ddlint::parse`, `ddlint::sema::build`, and the linter `Runner`, over the
  program below. It asserts no parse errors; that `unused-relation` reports
  `Witness` and not `Checked`; that the semantic model records a variable use of
  `x` for each braced `if` branch (resolution follows the interim model until
  roadmap 3.3.5); and that `[x -> 1]` is preserved as `MapLit`. Top-level
  `for (x in Items(x)) { Out(x) }.` keeps the existing unsupported-head
  diagnostic.

```ddlog
input relation Items(x: u32)
input relation Checked(x: u32)
input relation Witness(x: u32)
output relation Out(x: u32)
Out(y) :- Items(x), var y = if x > 1 { x } else { 0 }, Checked(y), var m = [x -> 1].
```

### Red-Green-Refactor evidence

Recorded in `Artefacts and notes`: red transcripts per milestone, green
transcripts, seeded-fault transcripts, class counts, the `cargo mutants`
summary, and gate log paths.

Quality criteria: every suite above passes; `V-B1` to `V-B8`, `V-M1`, `V-M2`,
and `V-M4` are discharged by their tests; formatting, type checking, lint,
Markdown, and Mermaid gates pass.

## Idempotence and recovery

Every step is an edit followed by a gate run; reruns are safe. Seeded faults
are applied in the working tree, recorded, and reverted with `git restore`,
never committed. To retry a milestone, return to its last green commit after
confirming no uncommitted work is needed.

## Artefacts and notes

### Implementation transcripts

- EP-M2: the collection tables were written before the bracket-literal change
  and failed against the brace-map implementation; after the change the
  collection suite passed with `60` tests. Log:
  `/tmp/test-ddlint-ep-m2-collections.out`.
- EP-M3 scanner:
  `Out(y) :- Items(x), var y = if x > 1 { x } else { 0 }, …` first reported
  `Unexpected ... found: Some(K_IF)` because the span scanner stopped at `if`.
  Adding the `assign_stmt` alternative made the end-to-end suite green
  (`5 passed`). Log: `/tmp/test-ddlint-2-6-8-block.out`.
- EP-M3 semantics: the first end-to-end semantic assertion failed with
  "use at 129 should resolve" because `sema` records the enclosing term's span
  for each use and leaves free relation-argument variables unresolved. The test
  now asserts use term text plus resolution through the head binding `y`.
- EP-M3 compatibility: `tests/expression_compound.rs` initially expected
  `Expr::Group` for `if` branches written with braces; the suite failed until
  the expectations became `block(...)`, which is the intended behaviour change.
  All `27` cases then passed.
- Every seeded fault below was applied, observed red against a named test, and
  reverted by restoring a copy from `/tmp/ddlint-2-6-8/`; the restored tree was
  green afterwards.

### Generator class coverage

`generator_covers_every_class` draws `256` `(items, ends_with_semicolon)`
samples from `TestRunner::deterministic()` and counts the 16 classes described
under `V-B2` to `V-B5`. Observed counts:

| Class                  | Count |
| ---------------------- | ----- |
| empty block            | 61    |
| single value           | 22    |
| single statement       | 21    |
| multi value            | 79    |
| multi statement        | 73    |
| nested block           | 34    |
| map first              | 13    |
| if tail                | 12    |
| for tail               | 33    |
| return tail            | 41    |
| closure tail           | 31    |
| tuple item             | 33    |
| parenthesized sequence | 34    |
| vector item            | 42    |
| call item              | 38    |
| binary item            | 27    |

Every count is at least `5`, the floor the test asserts.

### Classifier sweep (`V-B6`)

`block_follow_classification_matches_the_reference_table` sweeps the three
positions over nine tokens (end of input, `{`, `}`, `;`, `,`, `:`, an
identifier, a number, and `+`), compares each result with the independently
shaped reference table, and asserts the outcome split
`(16 accepted, 11 rejected)`.

### Seeded faults

| Fault                                                        | Test that rejected it                                                                                  |
| ------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------ |
| accept `([], true)` (`V-B1`)                                 | `block_expr_from_items_rejects_empty_trailing_separator`                                               |
| move the last statement into `value` when `t` holds (`V-B1`) | `parses_blocks::case_07_trailing_semicolon`, `case_08_single_statement`                                |
| reverse statements                                           | `parses_blocks::case_06_statements`, `case_07_trailing_semicolon`, `case_11_nested_statement`          |
| drop the trailing-`;` flag                                   | `parses_blocks::case_07_trailing_semicolon`                                                            |
| parse items without the `;`-terminates state                 | `parses_blocks::case_17_closure_then_value`                                                            |
| omit propagation into `for` bodies only                      | `parse_construct_tails_as_separate_items::case_5_for_with_expression_body`                             |
| omit clearing inside call arguments                          | `nested_delimiters_still_allow_sequence::case_2_call_arguments`                                        |
| return `Group` for `{ e }`                                   | `parses_blocks::case_01_variable`, `case_03_call`, `case_04_tuple`, `case_05_binary`                   |
| set the state at top level                                   | `expression::characterization::non_brace_error_diagnostics_are_characterized` (`V-B7`)                 |
| accept a mixed bracket literal                               | `rejects_empty_and_mixed_literals::case_3_mixed_element_then_entry`, `case_4_mixed_entry_then_element` |

### Mutation testing

Command:
`cargo mutants --file src/parser/expression/block.rs --file
src/parser/ast/expr/block.rs --all-features --cargo-test-arg=--lib`.

The first attempt without `--cargo-test-arg=--lib` was stopped after its
baseline exceeded 30 minutes on a host whose compiler slots were saturated by
other repositories: the unmutated build compiles the integration, behavioural,
and `trybuild` targets, none of which adds block coverage beyond the library
suites that own it. Scoping to `--lib` is therefore the documented method for
this item; the end-to-end and behavioural block suites remain part of the
commit gates.

Run 1 over 22 mutants: `12` caught, `2` missed, `7` unviable, `1` timeout, in
`34m`. Log: `/tmp/mutants-ddlint-2-6-8.out`.

- Missed `BlockExpr::items -> empty`: killed by the new
  `block_items_follow_source_order` table, which asserts both `items()` and
  `into_items()` yield statements then value in source order.
- Missed `&& -> ||` in `ends_with_semicolon`: killed by the new
  `block_ends_with_semicolon_only_for_a_unit_value` table over the empty,
  value-only, statement-only, and both shapes.
- Timeout `ends_with_semicolon -> true`: re-run with
  `--minimum-test-timeout 120`, because the auto-set `20s` limit was reached
  while the host was loaded and no code path loops on this predicate.

Run 2 repeated the filtered survivors: `3` caught, `1` unviable, no misses
(`/tmp/mutants-rerun-ddlint-2-6-8.out`). The timed-out mutant was then run
alone and was caught in `102s` build plus `1s` test, confirming the earlier
timeout was host contention rather than a hang
(`/tmp/mutants-target-ddlint-2-6-8.out`). No missed mutant remains.

## Interfaces and dependencies

`src/parser/ast/expr/block.rs` (re-exported from `ddlint::parser::ast`):

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
    /// Build a block from its items; `None` for an empty block ending in `;`.
    #[must_use]
    pub fn from_items(items: Vec<Expr>, ends_with_semicolon: bool) -> Option<Self> { … }
    /// All items in source order.
    pub fn items(&self) -> impl Iterator<Item = &Expr> { … }
    /// Consume the block, yielding its items in source order.
    pub fn into_items(self) -> impl Iterator<Item = Expr> { … }
    /// Whether the source block ended in `;` (true only when non-empty).
    #[must_use]
    pub fn ends_with_semicolon(&self) -> bool { … }
}
```

`Expr` gains `Block(BlockExpr)`; `Group`'s doc comment becomes "Parenthesized
expression `( e )`."; `MapLit`'s documents `[k -> v, …]`.

`src/parser/ast/precedence.rs` gains
`pub(crate) const BLOCK_ITEM_MIN_BP: u8 = BP_SEQ + 1;`.

`src/parser/expression/block.rs` (private) defines `ItemPosition` (`Opening`,
`AfterItem`, `AfterSeparator`), `BlockFollow`, and
`classify_block_follow(position, Option<SyntaxKind>) -> BlockFollow`.

Message constants in `src/parser/error_messages.rs`, re-exported through
`src/test_util/mod.rs`: `BLOCK_ITEM_SEPARATOR_ERROR`, `EMPTY_BLOCK_ITEM_ERROR`,
`UNCLOSED_BLOCK_ERROR`, `MIXED_COLLECTION_ENTRY_ERROR`, and
`EMPTY_VECTOR_LITERAL_ERROR`.

No dependency is added.

## Related documentation and skills

Read before implementing: the specification sections in `Conformance basis`;
`docs/parser-implementation-notes.md`; `docs/parser-conformance-register.md`;
`docs/haskell-parser-analysis.md`;
`docs/building-an-error-recovering-parser-with-chumsky.md`;
`docs/rust-parser-testing-comprehensive-guide.md` (snapshots, error recovery,
Pratt tables, fuzzing); `docs/rust-testing-with-rstest-fixtures.md`;
`docs/ddlint-design.md` (sections 2 and 3.2.1);
`docs/complexity-antipatterns-and-refactoring-strategies.md`;
`docs/documentation-style-guide.md` (ADR template and numbering); and
`docs/developers-guide.md` (googletest and rstest attribute order, fixture
lints, behavioural steps).

Skills: `execplans`; `rust-router`, then `rust-unit-testing`, `proptest`,
`rust-types-and-apis`, and `rust-state-machines`; `arch-decision-records`;
`hexagonal-architecture`; `codegraph-mcp`; `mapsplice`; `changelog`;
`commit-message`; `pr-creation`; `github-stacks` (for the follow-on stack); and
`en-gb-oxendict-style`.

## Appendix A: upstream lexical scope (input to roadmap 3.3.5 and 3.3.6)

This appendix records reviewed findings for the follow-on items. It is not
implemented by this plan; EP-M1 writes its rules into specification section 8.1
as a target.

### Upstream scoping rules

Sources: `vmware-archive/differential-datalog` at `master` (commit recorded in
ADR-0006): `NS.hs` (`ctxVars'`), `Rule.hs` (`ruleRHSVars'`), `Expr.hs`
(`exprVarDecls`), `ECtx.hs` (`ctxInRuleRHSPositivePattern`), `Parse.hs` (`eseq`,
`rhsCondition`, `extractGroupBy`, `pterm`, `loop_term`, `lhs`,
`parseForStatement`), `Statement.hs` (`convertStatement`), and `Validate.hs`.
Where the language reference and the compiler disagree, the compiler is
followed.

1. Rule clauses are ordered. Clause `i` sees declarations of clauses before
   `i`; the head and its location see every body declaration (after any
   barriers). Heads declare nothing.
2. A positive atom declares every pattern-position occurrence of a name not
   visible before the clause. Pattern positions are the atom value and,
   recursively, struct fields (named and positional), tuple components, typed
   operands, `@` bindings, and `&` operands; parentheses are transparent.
   Occurrences in the same atom do not see each other: in `R(x, x)` with `x`
   fresh both occurrences declare, which upstream rejects ("both declared and
   used"); in `R(x, x + 1)` the second `x` is an unknown variable.
3. A clause wrapped in parentheses, `(R(x))`, is a condition, not an atom.
4. Negated atoms, conditions, and `Inspect` declare nothing. `Inspect` sees
   the implicit variables `ddlog_weight` and `ddlog_timestamp`; primary-key and
   index expressions see their key and index variables.
5. An assignment clause declares only `var` names on its left-hand side
   (through struct, tuple, typed, and `&` positions); a bare name there is a
   use. FlatMap patterns accept only `var` names. Redeclaring a visible name in
   a rule condition, FlatMap pattern, or `@` binding is rejected upstream.
6. Any clause containing `group_by` (method or function form) or legacy
   `Aggregate` is split: first a group-by clause, whose project and key
   expressions see earlier variables and which binds a hidden `__group`; then
   the remainder, which sees only the key's variables, `__group`, and later
   declarations. Names hidden by the barrier may be declared again.
7. Inside expressions, a declaration made by item `k` of a `;` sequence is
   visible to items after `k` of that sequence only, shadowing outer names.
   Declarations propagate out of a sub-expression only through struct fields,
   tuple components, an assignment's left-hand side, `@` bindings, typed
   operands, and `&` operands; any other node seals them. A single-item block
   `{ e }` and a group `( e )` are `e` upstream and so are transparent. Rule
   clauses are stricter: only a top-level assignment's left-hand side declares
   for later clauses.
8. `if` conditions do not declare into branches. Match patterns declare bare
   and `var` names, which shadow outer names in the arm. A `for` loop's pattern
   declares into its body and shadows; the iterable is evaluated outside the
   loop scope. Closure parameters precede captured variables. A function body
   sees only its parameters.
9. Shadowing by `var` is allowed in expressions (the compiler's check for
   `EVarDecl` is commented out in `Validate.hs`), contrary to the language
   reference; it is never allowed in rule clauses.
10. Upstream's top-level `for` header is an atom with an optional `v in`
    binder, lowered by prepending the atom and guard to the body; ddlint's
    `for (a in A(a))` is rejected upstream, and a rule-body `for` upstream is
    an expression loop over a collection that declares nothing for later
    clauses (roadmap 2.6.10).
11. Mutability (writable versus read-only variables) is not modelled; it is
    a non-goal of the scope model.

### Abstract model for the resolution kernel

- A scope tree with root `P`; each scope has a parent, an anchor position in
  its parent's ordering, and an ordered position domain: clause positions for
  rules (with a group-by clause occupying two positions, `2i` and `2i + 1`),
  item positions for sequences, and positions {0 = binders, 1 = body} for
  match-arm, `for`, closure, and function scopes. A `for` iterable and a
  `match` scrutinee are uses in the parent at the anchor.
- A declaration `d = (name, scope, pos, order, intra)`; `intra` marks
  atom-pattern declarations, which are visible to nothing in their own clause.
  Declarations that do not propagate (rule 7) are placed in a sealed scope no
  use can see.
- A barrier `b = (scope, pos, keep)` hides, from positions after `b.pos`,
  declarations of that scope at positions strictly less than `b.pos` whose name
  is not in `keep`.
- A use `u = (name, scope, pos, order)`.
- `ancestor(a, s)` holds when `a` is `s` or reachable from `s` by parent links.
  `proj(a, s, p)` is the anchor of the child of `a` on the path to `s`, or `p`
  when `a = s`.
- `visible(d, u)`: `ancestor(d.scope, u.scope)`,
  `d.pos < proj(d.scope, u.scope, u.pos)`, and no barrier in `d.scope` hides
  `d` from that projected position.
- `is_res(u, r)`: either `r = None` and no same-name declaration is visible
  to `u`; or `r = Some(i)`, `i.name = u.name`, `visible(i, u)`, and every
  visible same-name `j` other than `i` lies in a strict ancestor of `i`'s
  scope, or in the same scope at a smaller position, or at the same position
  with a greater `order` (upstream's first match wins among same-position
  declarations). The ancestor comparison is strict and `i` itself is excluded:
  the inclusive `ancestor` relation would otherwise accept every same-scope
  declaration regardless of its position and order, defeating `K-L0`.

### Kernel lemmas (to be proved in Verus in roadmap 3.3.6)

The trusted specification is declarative (`is_res` defined by quantifiers), not
a second copy of the lookup walk.

- `K-L0` (functionality): for each `u` at most one `r` satisfies
  `is_res(u, r)`.
- `K-L1` (soundness and completeness): under well-formedness (every parent
  index is smaller than its child's; every scope index is in range; every
  `(scope, pos, order)` is unique), `resolve` returns `r` with `is_res(u, r)`.
  Termination follows from the required `decreases` on the scope index.
- Specification-validation lemmas, which say more than the definitions:
  adding a declaration with a fresh name leaves `is_res` unchanged for other
  names; an injective renaming preserves `is_res`; an inner same-name
  declaration hides an outer one.
- Definitional corollaries, stated for readers rather than proved
  separately: declarations never escape their scope; barriers hide what they
  hide.

The bridge from proof to production is a marked kernel region whose text is
compared between the proof file and the production file by a dependency-free
line and brace-depth eraser, with a mutation-detection test of the eraser
itself; Verus-friendly code uses slice `get`, explicit `while` loops, and no
closures, with any lint conflict expected item by item. Proofs reach a commit
only when complete; `make verus` runs on demand and in a path-filtered
workflow, outside `make test` and `make lint`. A differential property test
compares the kernel with a reference resolver built from the definitions
(enumerate, close ancestors, filter, take the maximum), over generated
well-formed arenas whose class coverage (same-scope, ancestor, shadowed, order
tie-break, barrier-hidden, barrier-kept, sibling, absent) is counted. The
bridge test is excluded from mutation runs, because it kills every kernel
mutant by construction.

## Appendix B: roadmap entries (created)

The roadmap entries were created on 2026-11-17: `docs/roadmap.md` items 2.6.8
(reworded), 2.6.10 to 2.6.15, 3.3.5, 3.3.6, 4.1.3 (re-scoped), and 4.1.5, with
register items 16 to 22 in `docs/parser-conformance-register.md` and the
scoping target in section 8.1 of
`docs/differential-datalog-parser-syntax-spec-updated.md`.

## Revision note

2026-10-18: initial draft.

2026-10-23: second draft after the first expert review.

2026-11-17: third draft after user direction (drop brace maps; model upstream
lexical scope), covering the whole programme in nine milestones.

2026-11-17: fourth draft after the second expert review (five reviewers) and
the user's no-compatibility directive. What changed: the brace-map migration
diagnostic and all migration framing are removed; the plan is narrowed to
blocks and upstream map literals, with bracket maps added before braces replace
map parsing; the scope programme is scheduled as roadmap 2.6.10 to 2.6.14,
3.3.5, 3.3.6, and 4.1.5, with the corrected upstream rules, declarative kernel
specification, and bridge design kept in `Appendix A`; `BG-D11` names the exact
tails and delimiters; tolerances gain a counting base and categories; milestone
records are complete. Effect: four milestones.

2026-11-17: fifth revision after user decisions. What changed: the split,
bounded fidelity, identifier spans, and new roadmap and register items are
approved; `[]` is rejected as upstream does (`BG-D12`, `V-M1`, tests); type
syntax gains roadmap 2.6.15 and register item 22 (`BG-D19`); the roadmap,
register, and specification section 8.1 entries were created ahead of EP-M1,
which now covers the ADR, design document, and register items 8, 10, and 15.
Effect: implementation awaits approval of the plan itself.
