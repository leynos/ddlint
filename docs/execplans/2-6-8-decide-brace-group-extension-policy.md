# Codify brace blocks and model upstream lexical scope (roadmap 2.6.8)

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (third revision)

This plan must be approved before implementation begins. Silence is not
approval. Decisions marked "requires user approval" in `Decision log` are
approved together with the plan unless the approver says otherwise.

## Purpose / big picture

Roadmap item 2.6.8 asks for a decision on the brace-group extension `{ expr }`:
codify it or remove it with migration diagnostics. The user has directed three
outcomes for this item:

1. Codify braces as upstream Differential Datalog (DDlog) block expressions.
   Upstream parses `{ e1; e2; … }` as a core expression term (`braces eseq`; see
   `docs/haskell-parser-analysis.md`, "Expression grammar") and uses it for
   every braced `if`, `else`, `for`, closure, match-arm, and function body.
2. Stop supporting brace-delimited map literals `{k: v, …}`, which are a
   ddlint extension, and stay faithful to upstream syntax: map literals are
   `[k -> v, …]` and map types are `Map<K, V>`.
3. Make the linter model the lexical scope semantics of upstream DDlog
   correctly, so that every variable use resolves to the declaration the
   upstream compiler would bind it to.

Today the expression parser stores `{ expr }` as `Expr::Group`, the node used
for `( expr )`, and reads `{}` and `{k: v}` as map literals. The semantic layer
(`src/sema/`) treats rule-head variables as declarations and body occurrences
as uses, which inverts upstream's rule, where body clauses declare variables
and the head uses them. It opens no scope for blocks or match arms, cannot see
`var` declarations inside expressions, never parses function bodies, and does
not look inside interpolated strings.

After this change:

- `{ x }`, `{ a; b }`, `{ a; }`, `{}`, and
  `{ if c { a } else { b }; d }` parse as `Expr::Block` with explicit
  statements and value.
- `[k1 -> v1, k2 -> v2]` parses as `Expr::MapLit`; `{a: 1, b: 2}` is rejected
  with a migration diagnostic that names the replacement.
- `var x = e` parses inside any expression; function bodies, `function(…)`
  closures, and `${…}` interpolations are parsed into the abstract syntax tree
  (AST).
- `ddlint::sema::build` resolves every variable use by upstream's rules:
  positive atoms declare implicitly, assignments declare only through `var`,
  `group_by` hides ungrouped variables, the head sees everything the body
  declared, a declaration in one `;`-separated item is visible only to later
  items of the same sequence, and match, `for`, closure, and function scopes
  nest and shadow as upstream's compiler does.
- A pure scope-resolution kernel carries a Verus proof that resolution is
  sound, complete, and most-specific, and that declarations never escape their
  scope.
- The `unused-variable` lint reports variables that the body declares but
  never uses, instead of head variables.

Success is observable by running `make test` and `make verus` and seeing the
block, map, scope, corpus, behavioural, and proof suites pass; by parsing the
vendored upstream corpus with no errors; and by reading register item 15 with
status `implemented` and roadmap item 2.6.8 ticked.

## Recommended decision

Stable identifiers are used throughout this plan. `BG-` identifiers concern
braces and collections; `LS-` identifiers concern lexical scope.

### Braces and collections

`BG-D1` (codify): `{ … }` is a normative block expression.

`BG-D2` (distinct node): a block is `Expr::Block(BlockExpr)` with
`statements: Vec<Expr>` (items followed by `;`) and `value: Option<Box<Expr>>`
(the final item when not followed by `;`; `None` means unit, `()`).
`Expr::Group` is reserved for parentheses.

`BG-D3` (separator): inside braces `;` is a separator handled by the block
parser, not the `BinaryOp::Seq` operator. A trailing `;` is accepted; an empty
item (`{ ; }`, `{ a;; b }`) is rejected.

`BG-D4` (empty braces, approved): `{}` is the empty block, whose value is unit.

`BG-D5` (braces always open a block): there are no brace map literals. After
`{`, items are ordinary expressions, so `{ x: T }` is a block whose value is
the ascription `x: T`, as upstream reads it. The colon-terminating key mode
(`parse_map_key_expr`, `colon_terminates` in `src/parser/expression/infix.rs`)
is removed.

`BG-D6` (struct-literal guard): in an `if` condition or a `for` guard, an
identifier followed by `{` is not a struct literal. Today the guard also
reports an error when the identifier is followed by `{}` or `{ ident :`. After
this change `ident {}` is reported only when another `{` follows
(`if x == Foo{} { a }`), so `if ready {} else { go() }` parses; the `{ ident :`
heuristic is kept because ddlint's struct-literal syntax is unchanged by this
plan (`BG-D15`).

`BG-D11` (`;` terminates tails): while block items are parsed, a scoped parser
state makes the infix loop stop at `;`. The state is kept through
prefix-construct tails that restart at binding power 0 (`if`/`else` branches,
`for` bodies, closure bodies, `return` values, operator right-hand sides) and
cleared inside every nested delimiter (`( … )`, call arguments, `[ … ]` and bit
slices, nested `{ … }`, struct fields, the `match` scrutinee and arms, the
`for ( … )` header). Without it `{ if c { a } else { b }; d }` would parse as
one `if` whose `else` branch is `Seq({ b }, d)`.

`BG-D12` (upstream map literals): `[k -> v, …]` is a map literal producing
`Expr::MapLit`. It needs at least one entry and accepts a trailing comma. `[]`
remains the empty vector. A literal mixing elements and entries is rejected.
`->` is not an infix operator, so the first element's parse stops at it.

`BG-D13` (brace-map migration diagnostic): inside braces, when the first item
is an ascription and is followed by `,`, or is an ascription whose right-hand
side is a number, string, or Boolean literal, the parser reports
`brace map literals are not DDlog syntax; write [k -> v, …] instead`, skips to
the matching `}`, and continues. `{ x: T }` with a type-shaped right-hand side
is an ordinary block (upstream's reading), and is documented as such.

`BG-D14` (types): the specification's `MapType ::= '{' Type ':' Type '}'` and
`VecType ::= '[' Type ']'` are replaced by upstream's library types `Map<K, V>`
and `Vec<T>`. ddlint stores type text, so this is a specification-only change.

Recorded and scheduled rather than changed here:

- `BG-D7`: no new `DiagnosticCode`; messages are constants in
  `src/parser/error_messages.rs`; stable codes wait for roadmap item 2.8.1.
- `BG-D8` and `BG-D9`: braced top-level `for` bodies and the `Seq` operator
  outside braces (`( a; b )`, unbraced `for` bodies, call arguments, vector
  items, struct fields, match-arm bodies) are scheduled together as new roadmap
  item 2.6.10 and register item 16.
- `BG-D15`: ddlint's struct-literal syntax `Foo { x: 1 }` differs from
  upstream's `Foo{.x = 1}` and positional `Foo{1}`, and its pattern syntax
  differs similarly. Scheduled as new roadmap item 2.6.11 and register item 17.

### Lexical scope

`LS-D1` (normative reference): ddlint's scope model follows upstream's
compiler, which is the behaviour real programs depend on: `ctxVars'` in `NS.hs`,
`ruleRHSVars'` in `Rule.hs`, `exprVarDecls` in `Expr.hs`, and the pattern
grammars in `Parse.hs`. The rules are written into the specification as a new
section 7.1, "Variable scoping", and summarized in ADR
`docs/adrs/adr-0006-block-expressions-and-lexical-scope.md`. Where the upstream
language reference and compiler disagree (the reference forbids shadowing by
`var`; the compiler's check is commented out), the compiler wins, and the
disagreement is recorded.

`LS-D2` (rule clauses): a rule body is an ordered sequence of clauses.
Variables declared by clause `i` are visible to clauses after `i` and to the
head.

- A positive atom declares, implicitly, each variable in a pattern position
  of its arguments that is not already visible. Pattern positions are the
  atom's arguments and, recursively, struct and tuple components, typed
  (`e: T`) operands, and `&` operands. Other positions (`R(x + 1)`) are uses.
- A negated atom (`not R(x)`) and a plain condition declare nothing.
- An assignment clause declares only the names marked `var` on its left-hand
  side; a bare name on the left is a use (upstream reports an unknown variable).
- A `group_by` clause hides every earlier variable except those used in the
  grouping key, and declares the group variable.
- The head, its `@` location, and its arguments are uses resolved against
  every body declaration. Heads no longer declare variables.
- Semantic rules produced by top-level `for` desugaring follow the same
  clause rules, so a loop's iterable atom declares its pattern variables.

`LS-D3` (sequences and blocks): inside expressions, a declaration made by one
item of a `;`-separated sequence is visible to the later items of that sequence
and to nothing else. Declarations propagate out of an expression only through
assignment left-hand sides and, inside them, struct, tuple, typed, and `&`
positions, exactly as upstream's `exprVarDecls` does. They never escape `if`
branches, match arms, loop bodies, closures, or blocks with more than one item
or a trailing `;`. A single-value block `{ e }` and a group `( e )` are
transparent, because upstream represents both as `e`; this quirk is specified
and tested.

`LS-D4` (nested scopes): match arms, `for` loops (rule-level and
expression-level), closures, and functions open scopes. Match and `for`
patterns declare both bare names and `var` names; their bindings shadow outer
variables of the same name inside the arm or body. Closure and function
parameters scope their bodies.

`LS-D5` (`var` expressions): `var x` is an expression, `Expr::VarDecl(name)`,
accepted by the parser wherever an expression is, as upstream's parser does.
Whether a declaration is allowed in that context is a semantic fact recorded by
`sema`.

`LS-D6` (function bodies and closures): `function f(args): T { … }` and
`function f(args): T = e` bodies are parsed into `Expr` through a lazy typed
accessor `Function::body()` with source-absolute error spans, following the
`PrimaryKeyExpression` precedent from roadmap item 2.6.6.1. Body parse errors
are added to `Parsed::errors()`. `function(args) { … }` closures are parsed into
`Expr::Closure`.

`LS-D7` (interpolation): `${…}` segments of interpolated strings are parsed
into expressions stored on the string literal, so uses inside them are recorded.

`LS-D8` (facts, not new diagnostics): `sema` records shadowing, redeclaration,
declarations in disallowed contexts, and unresolved uses as model facts.
Reporting them is lint work: roadmap item 4.1.3 (`shadowed-variable`) already
exists, and new item 4.1.5 covers upstream scope errors (unknown variable,
redeclaration where the compiler forbids it, a variable declared and used in
the same atom).

`LS-D9` (unused variables): `unused-variable` reports body-introduced
declarations (implicit atom declarations, `var` declarations, `for` and match
pattern bindings) with no resolved use. Closure and function parameters stay
ineligible, as closure parameters are today.

`LS-D10` (verified kernel): resolution is computed by a pure, dependency-free
kernel, `src/sema/scope_kernel.rs`, over an arena of scopes and declarations.
Its soundness, completeness, most-specific choice, frame (no-escape), and
termination properties are proved in Verus, and its executable text is tied to
the proved text by a contract test.

## Constraints

- Do not change the meaning of any existing `DiagnosticCode` (`D-REL-001` to
  `D-REL-008`) or `DiagnosticCategory`.
- Do not change parenthesized forms: `( e )` yields `Expr::Group`, `()`
  yields `Expr::Tuple(vec![])`, and `(a, b)` yields `Expr::Tuple`.
- Do not change the binding power or associativity of any infix operator,
  and do not change any parse outside braces except the planned changes to
  `[ … ]` literals, `var`, `function` closures, and interpolated strings.
- Do not change top-level `for` lowering, the reserved-token policy, or the
  relation, index, and transformer scanners.
- Keep the parser and `sema` pure cores: no input/output, logging, metrics,
  or global state. No new port or adapter. The scope kernel has no dependencies
  beyond `core` and `alloc`.
- New production code must not panic (`expect`, `unwrap`, `unreachable!`,
  `todo!`, slice indexing) and must not use silent wildcard arms over `Expr` in
  `sema`: every `Expr` variant is handled explicitly so a new variant is a
  compile error, not a dropped use.
- No Rust source file over 400 lines; every module, including test modules,
  has a `//!` comment. Every `assert!`, `assert_eq!`, and `assert_ne!` carries
  a diagnostic message.
- Use only the shared Cargo cache; never bypass `RUSTC_WRAPPER` compile
  admission. `make verus` stays out of `make test` and `make lint`.
- No `assume`, `admit`, `external_body`, or trusted proof bodies in `verus/`,
  except `external` declarations of the pinned `vstd` library itself.
- Every commit passes the gates for its change type; red tests are observed
  and recorded, never committed alone.
- Documentation follows `docs/documentation-style-guide.md`, en-GB Oxford
  spelling, and no first-person prose outside README and feature files;
  `typos.toml` is generated.

## Tolerances (exception triggers)

- Scope: stop and escalate if any single milestone changes more than 40
  files or adds more than 1,500 net lines of Rust, or if the whole plan exceeds
  120 files or 6,000 net lines of Rust (excluding vendored corpus files and
  `proptest-regressions/`).
- Interface: the planned public API changes are `Expr::Block`, `BlockExpr`,
  `Expr::VarDecl`, the `Closure` parameter type change, interpolation segments
  on `StringLiteral`, `Function::body()`, the `{}`, `{k: v}`, and `[k -> v]`
  behaviour changes, a `group_by` binding on `RuleAggregation` if Stage A
  confirms it is missing, and the `sema` model additions listed in
  `Interfaces and dependencies`. Stop and escalate for any other public
  signature change.
- Dependencies: no new Cargo dependency. The Verus toolchain is installed by
  `rust-prover-tools` from in-tree pins and is not a Cargo dependency. Stop
  before adding anything else.
- Corpus gate (EP-M2): if the vendored upstream corpus has more than 15
  distinct unsupported expression constructs inside function bodies, stop and
  present options before EP-M6.
- Proof gate (EP-M7): if a kernel lemma needs more than two working days of
  proof engineering, or needs an axiom beyond `vstd`, stop and escalate with
  the open verification conditions.
- Behaviour: if the new scope model changes `unused-variable` or
  `unused-relation` results on `examples/` beyond the changes predicted in
  `Validation and acceptance`, stop and report.
- Iterations: if a gate still fails after three fix attempts, stop and
  escalate with the log path.
- Time: if a milestone runs more than eight working hours past its first
  `Progress` timestamp, stop and re-plan.

## Risks

- Risk: function bodies in real programs use expression forms the parser
  does not support yet (`?`, `x @ pat` bindings, `.field = e` named struct
  arguments), so parsing bodies turns silent acceptance into parse errors.
  Severity: high. Likelihood: high. Mitigation: EP-M2 measures this on a
  vendored upstream corpus before any body is parsed; the corpus gate decides
  whether EP-M6 proceeds, narrows, or needs new roadmap items.

- Risk: inverting the rule model (heads use, bodies declare) changes
  `unused-variable` results broadly. Severity: medium. Likelihood: certain.
  Mitigation: this is the requested behaviour; the predicted changes are listed
  in `Validation and acceptance`, the users' guide and changelog explain them
  with migration guidance, and the behaviour tolerance stops the work on
  unpredicted changes.

- Risk: the Verus proof proves a model that drifts from the shipped kernel.
  Severity: high. Likelihood: medium. Mitigation: the kernel's executable
  functions exist once in production form; the proof file contains the same
  functions with added specifications, and a contract test strips specification
  clauses and compares the two token streams; a differential property test
  compares the kernel with an independent reference resolver.

- Risk: `BG-D11` misses a tail or a delimiter. Severity: high. Likelihood:
  medium. Mitigation: one scoped helper pair; tables and generators cover every
  tail and delimiter; seeded faults remove propagation per tail.

- Risk: removing brace maps breaks existing tests and fixtures (about 50
  cases across `src/parser/tests/collections.rs`,
  `tests/expression_to_sexpr.rs`, `tests/collection_literal_boundary.rs`,
  `tests/expression_prefix.rs`, and
  `src/parser/tests/expression/fixtures/valid/collections.rs`). Severity: low.
  Likelihood: certain. Mitigation: each case is rewritten to `[k -> v]` or
  turned into a migration-diagnostic case, enumerated in EP-M4.

- Risk: Verus installation or solver behaviour on this host is unreliable.
  Severity: medium. Likelihood: medium. Mitigation: pinned release and checksum
  through `rust-prover-tools`; the proof gate tolerance.

- Risk: `make test` hits Rayon `EAGAIN` thread exhaustion. Severity: low.
  Likelihood: medium. Mitigation: rerun once with
  `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1` and record it.

## Progress

- [x] (2026-10-18T01:00Z) Branch renamed; reconnaissance of parser, docs,
  tooling, and upstream grammar.
- [x] (2026-10-18T02:00Z) First draft committed.
- [x] (2026-10-18T03:30Z) First expert review completed.
- [x] (2026-10-23T09:30Z) Second draft pushed; draft pull request #408.
- [x] (2026-11-17T10:00Z) User decisions: empty braces as empty block,
  scheduling of residual deltas, and no Verus for the block representation
  lemma accepted; brace maps to be dropped; upstream lexical scope to be
  modelled.
- [x] (2026-11-17T11:00Z) Branch rebased onto `origin/main` (`1e63592`);
  reconnaissance of `sema`, maps, types, function bodies, and upstream scoping
  rules (`NS.hs`, `Rule.hs`, `Expr.hs`, `Validate.hs`).
- [x] (2026-11-17T13:00Z) Third draft written.
- [ ] Second expert review and revision.
- [ ] Plan approved by the user.
- [ ] EP-M1: decisions recorded.
- [ ] EP-M2: upstream corpus vendored and measured (decision gate).
- [ ] EP-M3: block expressions.
- [ ] EP-M4: upstream map literals and brace-map migration.
- [ ] EP-M5: `var` expressions, `function` closures, interpolation.
- [ ] EP-M6: function bodies.
- [ ] EP-M7: verified scope kernel.
- [ ] EP-M8: upstream scope model in `sema`.
- [ ] EP-M9: close-out.

## Surprises & discoveries

- Observation: upstream treats braces as a core block expression.
  Evidence: `braces eseq` in upstream `Parse.hs`, quoted in
  `docs/haskell-parser-analysis.md`. Impact: codify, not remove.

- Observation: control-flow tails restart at binding power 0 and would
  swallow block separators. Evidence: `parse_if_clause`, the `for` body, the
  `return` value, and closure bodies call `parse_expr(0)`. Impact: `BG-D11`.

- Observation: upstream map literals are `[k -> v, …]` and must be
  non-empty; upstream has no `{…}` map form and no brace or bracket type
  syntax. Evidence: `emap_literal` in `Parse.hs`. Impact: `BG-D12` to `BG-D14`.

- Observation: upstream's compiler scopes sequence declarations to the rest
  of the sequence, and declarations escape an expression only through
  assignment left-hand sides. Evidence: `CtxSeq2` in `ctxVars'` (`NS.hs`) and
  `exprVarDecls` (`Expr.hs`), whose fold propagates only through `EStruct`,
  `ETuple`, `ESet` left-hand sides, `EBinding`, `ETyped`, and `ERef`. Impact:
  `LS-D3`.

- Observation: upstream rule scoping is clause-ordered with implicit
  declaration in positive atoms, explicit-only declaration in assignments, and
  variable hiding after `group_by`. Evidence: `ruleRHSVars'` in `Rule.hs`;
  `ctxInRuleRHSPositivePattern` in `ECtx.hs`. Impact: `LS-D2`.

- Observation: in upstream `match` and `for` patterns, both bare names and
  `var` names declare, while an assignment's left-hand side declares only
  through `var`. Evidence: `pterm`, `loop_term`, and `lhs` in `Parse.hs`.
  Impact: `LS-D2` and `LS-D4`.

- Observation: ddlint's `sema` treats heads as declarations and body
  occurrences as uses, never walks head arguments, opens no block or match-arm
  scope, ignores match and expression-level `for` patterns, and records `x = e`
  in expressions as a use only. Evidence: `src/sema/traverse.rs`
  (`collect_rule_heads`, `collect_head_expr`) and `src/sema/variables.rs`
  (`walk_variable_uses`). Impact: `LS-D2` to `LS-D4`; the second draft's claim
  that `{ x = f(y); x }` binds `x` in the enclosing scope was wrong and is
  withdrawn.

- Observation: upstream's language reference says a `var` declaration
  cannot shadow a visible variable, but the compiler's check for `var` is
  commented out (`exprValidate1` for `EVarDecl` in `Validate.hs`); it checks
  redeclaration only for rule conditions, FlatMap patterns, and `@` bindings.
  Impact: `LS-D1` follows the compiler; `LS-D8` records the stricter cases as
  facts for lints.

- Observation: function bodies are never parsed (`Function` has no body
  accessor), `function(…)` closures are unsupported, and interpolated strings
  are stored as flags only. Evidence: `src/parser/ast/function.rs`,
  `src/parser/expression/prefix.rs`, `src/parser/ast/string_literal.rs`. Impact:
  `LS-D6` and `LS-D7`.

- Observation: `main` already provides `rstest-bdd`, `googletest`,
  `pretty_assertions`, the `fixture-lints` crate, and a behavioural suite
  template (`tests/relation_primary_key_bdd.rs`); ADRs moved to
  `docs/adrs/adr-NNNN-…` with numbers 0004 and 0005 taken; roadmap 2.6.9 is
  taken; `AGENTS.md` now requires assertion messages and a plausible wrong
  implementation per changed behaviour. Impact: no harness milestone; ADR
  number 0006; new roadmap items start at 2.6.10.

- Observation: ddlint's examples use ddlint-specific syntax (for example
  `Some{x = …}`), so they cannot serve as an upstream conformance corpus.
  Impact: EP-M2 vendors upstream test programs.

## Decision log

- Decision: codify braces as block expressions (`BG-D1`, `BG-D2`).
  Rationale: upstream core term; ddlint control flow depends on it; a linter
  must distinguish braces from parentheses. Date/Author: 2026-10-18, planning
  agent.

- Decision: `{}` is an empty block (`BG-D4`). Approved by the user on
  2026-11-17.

- Decision: no Verus proof for the `BlockExpr` representation lemma.
  Approved by the user on 2026-11-17. The new kernel proof (`LS-D10`) is a
  separate decision.

- Decision: drop brace map literals and adopt `[k -> v]` (`BG-D5`,
  `BG-D12` to `BG-D14`). Directed by the user on 2026-11-17. The second draft's
  disambiguation rule and roadmap item for bracket maps are withdrawn.

- Decision: model upstream lexical scope (`LS-D1` to `LS-D9`). Directed by
  the user on 2026-11-17. Following the compiler rather than the language
  reference where they differ is a planning decision, because ddlint must
  accept what upstream accepts.

- Decision: schedule braced top-level `for` bodies with the `Seq` operator
  outside braces as roadmap 2.6.10 (register item 16). The scheduling principle
  was approved on 2026-11-17; the number changed because `main` used 2.6.9.

- Decision: schedule upstream struct-literal and struct-pattern syntax as
  roadmap 2.6.11 (register item 17, `BG-D15`) instead of changing it here.
  Rationale: it is not a map form, it affects patterns and rule atoms
  throughout, and changing it would double this item. Date/Author: 2026-11-17,
  planning agent. Requires user approval.

- Decision: add roadmap item 4.1.5 for upstream scope-error lints (`LS-D8`).
  Rationale: the 3.3.1 design keeps unresolved names as facts, not parser
  diagnostics. Date/Author: 2026-11-17, planning agent. Requires user approval.

- Decision: vendor a small set of upstream DDlog test programs as a
  conformance corpus under `tests/fixtures/upstream/` with upstream's MIT
  licence notice. Rationale: function-body parsing needs evidence from real
  upstream code. Date/Author: 2026-11-17, planning agent. Requires user
  approval.

- Decision: prove the scope kernel in Verus (`LS-D10`). Rationale: scope
  resolution is contractual logic every scope-aware lint depends on, it ranges
  over unbounded nesting and ordering, and a small pure kernel exists to prove
  it over, which is the trigger the second draft named. The builder that places
  declarations is tested, not proved, because its correctness is correspondence
  with upstream's rules, for which there is no formal upstream specification to
  prove against. Date/Author: 2026-11-17, planning agent. Requires user
  approval, because it adds Verus tooling (`tools/verus/`,
  `tools/rust-prover-tools/REF`, `make verus`).

- Decision: the ADR is
  `docs/adrs/adr-0006-block-expressions-and-lexical-scope.md`. Rationale: new
  ADR location and numbering from `main`. Date/Author: 2026-11-17, planning
  agent.

- Decision: messages are constants, not diagnostic codes (`BG-D7`).
  Rationale: register stable-surface rules; 2.6.7 precedent. Date/Author:
  2026-10-23, planning agent.

## Outcomes & retrospective

Not started. Complete at each milestone boundary and at completion.

## Context and orientation

ddlint is a Rust linter for DDlog. A tokenizer (`src/tokenizer.rs`) produces
`SyntaxKind` tokens (`src/language.rs`). Span scanners
(`src/parser/span_scanners/`) find declarations, and a lossless concrete syntax
tree (CST) is built with `rowan`. Expressions are parsed on demand into an AST
by a handwritten Pratt parser (a precedence-climbing parser driven by binding
powers). `src/sema/` builds a semantic model of scopes, symbols (declarations),
and use sites from the AST, and lint rules in `src/linter/rules/` query it.

Parser files:

- `src/parser/expression/pratt/mod.rs`: `parse_expression(src)`,
  `parse_expr(min_bp)`, `parse_map_key_expr` (to be removed), `parse_type`, and
  the scoped struct-literal states that `BG-D11` imitates.
- `src/parser/expression/infix.rs`: the infix loop and colon-terminating
  mode (to be removed).
- `src/parser/expression/prefix.rs`: prefix dispatch (`T_LBRACE`,
  `T_LBRACKET`, `T_PIPE`, keywords); `parse_operator_prefix` emits
  `unexpected token: K_VAR` today.
- `src/parser/expression/data_structures.rs`: `parse_brace_or_map_literal`,
  `parse_map_entries`, `parse_brace_group_continuation`, `parse_vector_literal`,
  `parse_closure_literal`, `parse_ident_expression` (struct-literal guard),
  `parse_struct_fields`.
- `src/parser/expression/control_flow.rs`: `if`, `match`, `return`, `for`.
- `src/parser/ast/expr/mod.rs` (314 lines): the public `Expr` enum.
- `src/parser/ast/expr/sexpr.rs`: S-expression rendering for tests.
- `src/parser/ast/precedence.rs`: binding powers; private `BP_SEQ = 0`.
- `src/parser/ast/function.rs`: `Function` (name, parameters, return type;
  no body).
- `src/parser/ast/string_literal.rs`: string literals with an `interpolated`
  flag.
- `src/parser/ast/pattern.rs`: `Pattern` (`Var { declared, name }`, `Tuple`,
  `Struct`, `Typed`, `Literal`, `Wildcard`).
- `src/parser/ast/rule/mod.rs` and `classification.rs`: `RuleBodyTerm`
  (`Expression`, `Assignment`, `Aggregation`, `ForLoop`), `RuleAggregation`
  (`project`, `key`, `source`).
- `src/parser/ast/relation/primary_key/mod.rs`: `PrimaryKeyExpression`, the
  lazy source-absolute parsing precedent.

Semantic files:

- `src/sema/model.rs`: `SemanticModel`, `Scope`, `Symbol` (with
  `source_order`, `visible_from_rule_order`), `UseSite`, `Resolution`
  (`Resolved`, `Unresolved`, `Ignored`), `SymbolOrigin`, `UseOrigin`.
- `src/sema/scope_origin.rs`: `ScopeKind` (`Program`, `Rule`, `ForLoop`,
  `Closure`) and `ScopeOrigin`.
- `src/sema/builder.rs`, `traverse.rs`, `variables.rs`: scope and symbol
  construction and expression walking (several `_ => {}` fallbacks).
- `src/sema/resolve.rs`: `resolve_name` walks the parent chain, scanning each
  scope's symbols latest first, filtered by `symbol_visible`.
- `src/linter/rules/correctness/unused_variable.rs`: reports warning-eligible
  symbols with no resolved use.

Tests to know: `tests/semantic_scope_resolution.rs`,
`tests/unused_variable_rule.rs`, `src/sema/tests/`,
`src/parser/tests/collections.rs`, `tests/expression_*.rs`,
`tests/collection_literal_boundary.rs`, and the behavioural template
`tests/relation_primary_key_bdd.rs` with `tests/features/`.

Upstream sources consulted (MIT-licensed, `vmware-archive/differential-datalog`
at `master`): `Parse.hs` (`eseq`, `emap_literal`, `pattern`, `loop_term`,
`lhs`), `NS.hs` (`ctxVars'`), `Rule.hs` (`ruleRHSVars'`), `Expr.hs`
(`exprVarDecls`), `ECtx.hs`, `Validate.hs`,
`doc/language_reference/language_reference.md`, and `doc/tutorial/tutorial.md`.
EP-M1 records the exact upstream commit hash in the ADR.

Terms: a "block" is `{ s1; …; sn; v }`; "statements" are items followed by `;`;
the "value" is the optional final item; "unit" is `()`. A "tail" is the part of
a prefix construct parsed after its header. A "clause" is one comma-separated
rule-body term. A "pattern position" is an argument position in which upstream
declares variables implicitly. "Shadowing" is a nested or later declaration
hiding an earlier one of the same name. A "seeded fault" is a temporary
deliberate bug that proves a test can fail; "non-vacuity" is evidence that a
check can fail; a "metamorphic property" relates outputs of related inputs.

## Conformance basis

- `docs/roadmap.md` item 2.6.8 (`RM-2.6.8`), new items 2.6.10, 2.6.11, and
  4.1.5; existing items 4.1.2 and 4.1.3.
- `docs/parser-conformance-register.md` items 8, 10, 15 (`REG-15`), and new
  items 16 and 17; maintenance rules; stable diagnostic surface.
- `docs/differential-datalog-parser-syntax-spec-updated.md`: status policy
  (`SPEC-POLICY`), sections 3.3, 4, 5.2, 5.3, 5.10, 5.12 (patterns), 6.4, 9,
  10, 11.6, 12, 13's compatibility-policy bullet (`SPEC-13-COMPAT`), and 14;
  new sections 3.4 (blocks) and 7.1 (variable scoping).
- `docs/haskell-parser-analysis.md`, "Expression grammar" (`UP-ESEQ`).
- Upstream compiler sources listed in `Context and orientation`
  (`UP-SCOPE`).
- `docs/ddlint-design.md` sections 2, 2.4, 3.2.1 (semantic model), 3.3
  (lint catalogue), and 6.
- `docs/adrs/adr-0001-parser-crate-split.md` (`ADR-0001`; the expression AST
  owner is decided at roadmap 2.7.1),
  `docs/adrs/adr-0002-relation-role-kind-modelling.md`, and
  `docs/adrs/adr-0005-fixture-expansion-lint-helper.md`.
- `docs/execplans/3-3-1-symbol-table-and-scope-resolution.md` (prior
  decisions: unresolved names are facts; ordered rule visibility).
- Governing standards: `AGENTS.md`, `docs/documentation-style-guide.md`,
  `docs/developers-guide.md`, `docs/repository-layout.md`.

No Terms of Reference document exists for ddlint; none is invented.

Trace links:

```plaintext
SPEC-13-COMPAT + UP-ESEQ -> RM-2.6.8 -> REG-15 -> BG-D1..BG-D6, BG-D11 -> EP-M3 -> V-B*, block suites
user direction (maps) -> BG-D5, BG-D12..BG-D14 -> EP-M4 -> V-M* -> map suites + register item 10
user direction (scope) + UP-SCOPE -> LS-D1..LS-D9 -> EP-M5, EP-M6, EP-M8 -> V-S* -> scope suites
LS-D10 -> EP-M7 -> K-L1..K-L6 -> verus/sema_scope_kernel.rs + make verus + kernel bridge tests
LS-D6 -> EP-M2 (corpus gate) -> EP-M6 -> tests/upstream_corpus.rs
SPEC-POLICY -> BG-D8/BG-D9, BG-D15, LS-D8 -> EP-M1 -> REG-16, REG-17, RM-2.6.10, RM-2.6.11, RM-4.1.5
all -> EP-M9 -> users' guide, developers' guide, design 3.2.1, CHANGELOG, RM-2.6.8 done
```

## Verification plan

The plan has three families of obligations: block parsing (`V-B`), map literals
(`V-M`), and scope semantics (`V-S` for model correspondence, `K-L` for the
proved kernel). The relationship between proof and tests is stated in "Proof
and test responsibilities".

### Block parsing

Abstract model: an abstract block is items `[e1, …, en]` (`n ≥ 0`) with a flag
`t` ("ends in `;`"), well-formed when `t ⇒ n ≥ 1`; its value is `en` when
`n ≥ 1 ∧ ¬t`, else unit. The representation relation `R` maps it to
`BlockExpr { statements, value }`: `value = Some(en)` and
`statements = [e1 … e(n-1)]` when `n ≥ 1 ∧ ¬t`, else `value = None` and
`statements = items`. Executable forms: `BlockExpr::from_items(items, t)`
(`None` exactly for `([], true)`), `items()`, and `ends_with_semicolon()`.

Invariants and lemmas:

- `V-B1` (representation bijection): `R` is a bijection between well-formed
  abstract blocks and `BlockExpr` values. Method: three-row `rstest` table plus
  a `proptest` round trip over lengths 0 to 8 with distinct items. Non-vacuity:
  seeded faults "accept `([], true)`" and "move the last statement into `value`
  when `t` holds". No Verus (approved).
- `V-B2` (no swallowed separator): inside a block item every `BinaryOp::Seq`
  lies beneath a delimiter node within that item.
- `V-B3` (order): source order of items equals `items()` order.
- `V-B4` (braces versus parentheses): braces never produce `Group`;
  parentheses never produce `Block`.
- `V-B5` (parse correspondence): for item sources drawn from the generator's
  grammar (identifiers, literals, `+`, `and`, calls, tuples, parenthesized
  sequences, nested blocks, `[k -> v]` maps, `var` declarations, and the tails
  `if`/`else`, `else if`, `for`, closures, `return`), parsing
  `{ s1; …; sn[;] }` yields `Expr::Block(from_items([ê1 … ên], t))`, where `êi`
  is built independently by the generator. Sketch: induction on `n`; each item
  stops at the next `;` or `}` because the `;`-terminates state is set,
  inherited by every tail, and cleared and restored by every delimiter.
- `V-B6` (follow classification is total):
  `classify_block_follow(position, kind)` over positions {opening, after item,
  after separator} and every `SyntaxKind` plus end of input yields exactly one
  outcome.
- `V-B7` (no change outside braces): every existing expression fixture
  without `{`, `var`, `function`, interpolation, or `[ … -> … ]` renders
  identically before and after.
- `V-B8` (panic freedom): any token sequence beginning with `{` parses to
  `Ok` or `Err` without panicking, with error spans `start <= end <= src.len()`.

Methods: `V-B2` to `V-B5` by class-first `proptest` generators that emit
`(source, expected Expr)` pairs (classes built, never filtered; items use
distinct names; a `generator_covers_every_class` test counts classes over 256
deterministic draws and asserts each count is at least 5), plus named `rstest`
tables; `V-B6` by enumerating every `SyntaxKind` from `0` to `N_ERROR`
(asserting `from_u16` covers the range), with the whole-input table as the
stronger evidence; `V-B7` by an inline `insta` characterization snapshot taken
before EP-M3 and paired with semantic assertions on representative rows; `V-B8`
by two `proptest` properties of at least 2,048 cases over space-separated token
alphabets (a structural fuzz including `if`, `else`, `for`, `return`, `|`,
`var`, and `Foo`; and a differential property against a small reference
recognizer for `{ } ; , : a 1 +`), each asserting that `Ok` and `Err` occur at
least 100 times. Seeded faults: reverse statements; drop the trailing-`;` flag;
parse items without the `;`-terminates state; omit propagation into `for`
bodies only; return `Group` for `{ e }`; set the state at top level (must break
`V-B7`).

### Map literals

- `V-M1` (bracket maps): `[k1 -> v1, …, kn -> vn]` (`n ≥ 1`, optional
  trailing comma) yields `Expr::MapLit` with entries in order; `[]` yields
  `VecLit([])`; `[a, b]` yields `VecLit`.
- `V-M2` (no mixing): a bracket literal containing both a plain element and
  an entry is rejected with `MIXED_COLLECTION_ENTRY_ERROR` at the first
  offending item.
- `V-M3` (migration trigger is exact): the brace-map diagnostic fires if and
  only if the first block item is an ascription and either a `,` follows it or
  its right-hand side is a number, string, or Boolean literal; parsing then
  resumes after the matching `}` with exactly one diagnostic.
- `V-M4` (lowering boundary preserved): `MapLit` from `[k -> v]` is
  preserved raw by `parse()`, as register item 10 requires for map literals
  today.

Methods: `rstest` tables for `V-M1`, `V-M2`, and `V-M4`; for `V-M3`, a
truth-table test over the three trigger conditions crossed with follow tokens,
plus a `proptest` property generating brace inputs from item shapes (ascription
with type-shaped or literal right-hand side, non-ascription) and asserting the
diagnostic appears exactly when predicted. Seeded faults: trigger on any
ascription (breaks `{ x: T }`); omit the literal right-hand side case; omit
recovery (must produce cascading errors).

### Scope semantics: abstract model

The model is independent of ddlint's data structures.

- Names `N`. A scope tree with root `P` (program). Each scope `s` has a
  parent `parent(s)` (none for `P`), an anchor position `anchor(s)` in its
  parent's ordering, and an ordered position domain (clause indices for rules,
  item indices for sequences, a single position for other scopes).
- A declaration `d = (name, scope, pos, order)`; `order` breaks ties among
  declarations at the same position (source order).
- A barrier `b = (scope, pos, keep)` hides, for positions after `pos` in
  `scope`, every declaration of that scope at positions up to `pos` whose name
  is not in `keep` (`group_by`).
- A use `u = (name, scope, pos)`.
- Projection: for scope `s` and an ancestor `a`, `proj(a, s, p)` is the
  anchor in `a` of the child of `a` on the path to `s`, or `p` when `a = s`.
- Visibility: `visible(d, u)` holds when `d.scope` is `u.scope` or an
  ancestor `a` of it, `d.pos < proj(a, u.scope, u.pos)` (strict: a clause or
  item never sees its own declarations), and no barrier in `a` with
  `d.pos <= b.pos < proj(…)` hides `d`.
- Resolution `res(u)`: among visible declarations named `u.name`, the one
  whose scope is deepest; within that scope, the greatest `(pos, order)`.
  `None` when there is none.

Upstream rules map onto this model as follows: rule clauses are positions of a
rule scope; heads use the position after the last clause; a sequence is a scope
whose positions are its items; match arms, `for` bodies, closures, and
functions are child scopes whose pattern or parameter declarations sit at a
position before the body; single-value blocks and groups create no scope.

### Scope kernel lemmas (proved in Verus)

The kernel computes `res(u)` over an arena: scopes with parent indices and
anchors, declarations with scope index, position, order, and an interned name
identifier (`u32`), and barriers. Well-formedness `WF`: every parent index is
smaller than its child's index (so the root is `0` and the parent chain is
acyclic), and every declaration's and barrier's scope index is in range.

- `K-L1` (termination): under `WF`, the ancestor walk terminates; decreasing
  measure: the current scope index.
- `K-L2` (soundness): `resolve(u) = Some(i)` implies `decl[i].name = u.name`
  and `visible(decl[i], u)`.
- `K-L3` (completeness): if any declaration named `u.name` is visible to
  `u`, `resolve(u) != None`.
- `K-L4` (most specific): `resolve(u) = Some(i)` implies that no visible
  declaration of the same name is in a deeper scope, or in the same scope at a
  greater `(pos, order)`.
- `K-L5` (frame, no escape): a declaration in scope `s` is never returned
  for a use whose scope is not `s` or a descendant of `s`. This follows from
  `K-L2` and the ancestor definition, and is stated separately because it is
  the property lints rely on.
- `K-L6` (barrier): after a barrier `(s, b, keep)`, a declaration of `s` at a
  position up to `b` with a name outside `keep` is never returned for a use of
  `s` at a later position.

Proof sketch, to be refined when implementation exposes the verification
conditions: define `visible` and `res` as `spec fn`s over `Seq`-based arena
views; prove `K-L1` from `WF`; prove the per-scope scan correct (`scan(s, u)`
returns the greatest visible declaration of the name in `s`) by induction over
the declaration list; prove the walk returns the first ancestor with a
non-empty scan, which gives `K-L2` to `K-L4`; derive `K-L5` and `K-L6` as
corollaries. No lemma may assume its conclusion; `WF` is the only premise about
the arena, and its satisfiability is shown by a concrete witness arena.

Non-vacuity for the proofs: a witness arena with nested scopes, shadowing, a
same-scope redeclaration, and a barrier, on which the spec `res` is shown to
return specific declarations; and recorded negative controls (temporarily
reverse the ancestor walk; drop the barrier check) that Verus must reject.

Bridge to production: `src/sema/scope_kernel.rs` contains the kernel's
executable functions in a subset that Verus accepts (no traits beyond `Copy` and
`Clone`, no iterators in proved loops, plain `Vec` and indices).
`verus/sema_scope_kernel.rs` contains the same functions inside `verus!` with
`requires`, `ensures`, `invariant`, `decreases`, and proof blocks added. The
test `tests/scope_kernel_bridge.rs` strips those clauses and blocks from the
proof file and compares the resulting token stream with the production file's
functions, failing on any difference. A differential `proptest` compares
`scope_kernel::resolve` with an independent reference resolver written in the
test directly from the abstract model's definitions, over generated arenas of
up to 12 scopes, 30 declarations, and 3 barriers.

### Scope semantics: model correspondence (tested)

These obligations concern whether `sema` places declarations, uses, scopes, and
barriers where upstream's rules say. There is no formal upstream specification
to prove against, so they are discharged by tests whose expected values are
derived from the cited upstream functions.

- `V-S1` (clause order): a declaration made by clause `i` is visible to
  clauses after `i` and to the head, not to clause `i` itself or earlier.
- `V-S2` (implicit declaration): in a positive atom, the first occurrence in
  a pattern position of a name not already visible is a declaration; other
  occurrences and non-pattern positions are uses; negated atoms and conditions
  declare nothing.
- `V-S3` (explicit declaration): an assignment declares exactly its `var`
  names; bare names on the left are uses.
- `V-S4` (`group_by` barrier): after a `group_by` clause only the grouping
  key's variables and the group variable resolve.
- `V-S5` (heads use): head arguments and locations are uses; heads declare
  nothing.
- `V-S6` (sequences): in `{ s1; …; sn }`, a declaration in `sk` resolves
  only for uses in `s(k+1) … sn`; nothing escapes the block unless it is a
  single-value block or a group (transparency quirk).
- `V-S7` (nested scopes): match-arm and `for` pattern bindings, closure
  parameters, and function parameters resolve only inside their arm, body, or
  function, shadowing outer names.
- `V-S8` (interpolation): a `${x}` segment records a use of `x`.
- `V-S9` (explicit dispatch): every `Expr` variant is handled by name in
  `sema`; a new variant without handling fails to compile.

Methods:

- One named `rstest` case per upstream rule above, each citing the upstream
  function it is derived from, asserting the resolved declaration (by name
  span) for every use.
- Metamorphic `proptest` properties over generated rule bodies and blocks:
  consistent renaming of a variable leaves the resolution graph unchanged;
  inserting an unrelated clause or item that declares a fresh name leaves other
  resolutions unchanged; wrapping a single value in `{ … }` or `( … )` leaves
  resolutions unchanged; wrapping a multi-item sequence in a block hides its
  declarations from later items.
- Behavioural scenarios in `tests/features/lexical_scope.feature`.
- `V-S9` by the compiler: `sema` matches over `Expr` have no wildcard arm,
  so adding a variant is a compile error; a `trybuild` test is not needed.
- Seeded faults, each of which must fail a named test: heads declare again;
  implicit declaration also in negated atoms; bare assignment names declare;
  barrier ignored; sequence declarations escape a block; match patterns
  ignored; interpolation segments skipped.

### Proof and test responsibilities

The Verus proofs establish that, given an arena, resolution returns exactly the
most specific visible declaration and never leaks across scopes. The tests
establish that `sema` builds the arena upstream's rules require (`V-S1` to
`V-S9`), that the shipped kernel text is the proved text (bridge test), and
that the whole pipeline, from source text through lint diagnostics, behaves as
users observe it (behavioural and public-API tests). Neither replaces the
other: a correct kernel over a wrongly built arena gives wrong answers, and
tests alone cannot cover every arena shape.

### Axioms

- `A1`: `logos` tokenizes `{`, `}`, `;`, `,`, `:`, `->`, `var`, and
  `function` as their `SyntaxKind`s.
- `A2`: `chumsky::error::Simple::custom` stores span and message unchanged.
- `A3`: `proptest` explores and replays as documented.
- `A4`: upstream semantics are those of the cited
  `vmware-archive/differential-datalog` sources at the recorded commit; ddlint
  does not run the upstream compiler, so differential testing against it is not
  possible.
- `A5`: Verus and its `vstd` library at the pinned release are sound for the
  fragment used, and `rust-prover-tools` runs the pinned binary faithfully.

## Plan of work

### Stage A: understand and propose (until approval)

No code changes. Before EP-M3, confirm three code facts by reading or a scratch
test: how `var c = e.group_by(k).agg()` is classified (does `RuleAggregation`
carry the group variable?); whether `split_assignment` splits
`for (…) var y = e` at the wrong `=`; and whether head variables used only via
a later `var` binding are reported today. Record results in
`Surprises & discoveries` and adjust interfaces within tolerance.

### EP-M1: decisions recorded (documentation only)

Write `docs/adrs/adr-0006-block-expressions-and-lexical-scope.md` from the
style guide's ADR template: context, decisions `BG-D1` to `BG-D6`, `BG-D11` to
`BG-D14`, `LS-D1` to `LS-D10`, the upstream commit hash, the architectural
rationale (proof obligations shaped `BlockExpr` without a flag, the pure
classifier, the scoped `;` state, and the arena kernel), rejected options,
non-goals (`BG-D7` to `BG-D9`, `BG-D15`, `LS-D8` diagnostics), and consequences
(ADR-0001 joint approval for later `Expr` changes). Index it in
`docs/contents.md` with this plan. Summarize it in `docs/ddlint-design.md`
sections 2 and 3.2.1. Update register item 15's target text (status stays
`scheduled`), add the brace-map withdrawal to item 10's target text, add items
16 and 17, and add a note to item 8. Use the `mapsplice` skill to add roadmap
items 2.6.10, 2.6.11, and 4.1.5.

### EP-M2: upstream corpus (prototype and decision gate)

Vendor six to ten upstream test programs that exercise functions, blocks,
`var`, match, closures, maps, and rules into `tests/fixtures/upstream/`, with
`LICENSE-UPSTREAM` (MIT) and a `README.md` recording the source commit and file
list. Add `tests/upstream_corpus.rs`, which parses each file and asserts its
declarations parse, and a measurement test (marked as such in its name) that
extracts every function body span and parses it with `parse_expression`,
recording the distinct unsupported constructs in `Artefacts and notes`. Apply
the corpus gate tolerance. Keep the fixtures for EP-M6 and EP-M8 acceptance.

### EP-M3: block expressions

Red (recorded, not committed): add `src/parser/ast/expr/block.rs` with
`BlockExpr`, `from_items`, `items`, `into_items`, `ends_with_semicolon`, and
`Default`; the `Expr::Block` variant; the `(block …)` S-expression arm; the
`test_util::block` helper; message constants; stub `sema` arms; then the tests
listed in `Validation and acceptance`. Green: create
`src/parser/expression/block.rs` (`BlockFollow`, `ItemPosition`,
`classify_block_follow`, `parse_block`, `parse_block_items`, `finish_block`);
add `pub(crate) const BLOCK_ITEM_MIN_BP` to `precedence.rs`; add the
`;`-terminates state with `with_semicolon_terminating` and
`with_semicolon_terminating_cleared`, applied at every delimiter; narrow the
struct-literal guard; give `sema` interim block handling equal to today's
`Group` handling (the scope semantics arrive in EP-M8). Brace maps still parse
at this milestone, so `parse_block` keeps today's map branch unchanged until
EP-M4. Update the specification (new section 3.4; sections 4, 5.10, 9, 10,
11.6, 12, and 14), register item 15 (`implemented`), and the implementation
notes in the same milestone. Commit red and green together, then refactor.

### EP-M4: upstream map literals and brace-map migration

Red: rewrite every brace-map test case to `[k -> v]` or to a migration case;
add the `V-M1` to `V-M4` tests and the brace-map behavioural scenarios. Green:
in `parse_vector_literal`, after the first element, branch on `T_ARROW` into
map-entry parsing; reject mixed literals; remove the map branch from
`parse_block`, `parse_map_key_expr`, `parse_map_entries`, and the
colon-terminating mode; add the `BG-D13` trigger and recovery in `block.rs`;
update the `MapLit` doc comment and the `test_util::map_lit` example. Update
specification sections 3.3, 5.2 (`BG-D14`), 6.4, and 11.6, register item 10,
`CHANGELOG.md` (breaking change with migration guidance), and the users' guide
in the same milestone.

### EP-M5: `var` expressions, `function` closures, interpolation

Red, then green: a `K_VAR` prefix arm producing `Expr::VarDecl(name)` (a
lowercase identifier is required, as in `parse_var_pattern`); a `K_FUNCTION`
prefix arm parsing `function(params): T? { … }` into `Expr::Closure` with
`params: Vec<ClosureParam>` (name and optional type text), updating `|x| e`
parsing and every `Closure` consumer; interpolation segments, where the
string-literal parser locates `${…}` segments (handling nesting and escapes as
`contains_unescaped_interpolation` does), parses each with `parse_expression`,
and stores `interpolations: Vec<Expr>` on the `StringLiteral`, with errors
mapped to source-absolute spans. `sema` gets interim handling: uses are
recorded; declarations arrive in EP-M8.

### EP-M6: function bodies

Add `Function::body()` returning
`Option<Result<FunctionBody, Vec<Simple<SyntaxKind>>>>`, where `FunctionBody`
holds the body `Expr` (a `Block` for braced bodies) and its span, parsed lazily
with source-absolute spans like `PrimaryKeyExpression::parse`. Extend the
function scanner to accept bodies of the form `= expr` terminated by `;` or a
following declaration. Fold body errors into `Parsed::errors()` at the parse
boundary, deduplicated by span and message as reserved-token completion does.
Acceptance includes every vendored corpus function body parsing without errors,
or the constructs EP-M2 scheduled being the only failures, asserted as expected
failures with a register reference.

### EP-M7: verified scope kernel

Add `tools/verus/VERSION`, `tools/verus/SHA256SUMS`, `tools/verus/README.md`,
and `tools/rust-prover-tools/REF`, pinned at implementation time to the latest
non-rolling Verus release (`0.2026.11.02.8bf68ee` at planning time) and the
latest `rust-prover-tools` commit. Add `make install-verus` and `make verus`
targets delegating through `$(PROVER_TOOLS)` as the `verus` skill's on-ramp
describes, outside `make test`, `make lint`, and `make all`, and add the
Makefile contract tests the on-ramp lists under `tests/workflow_contracts/`.
Write the spec model and lemma statements first (`verus/sema_scope_kernel.rs`),
then the executable kernel (`src/sema/scope_kernel.rs`) and its proofs, then
the bridge test and the differential property. `make verus` must pass with no
`assume`, `admit`, or `external_body`.

### EP-M8: upstream scope model in `sema`

Red: write the `V-S1` to `V-S9` tests, the lexical-scope feature file, the
metamorphic properties, and the predicted `unused-variable` changes; record the
failures. Green: replace head-binding declaration with head uses; add
`ScopeKind::{Sequence, MatchArm, Function}`; add symbol origins (`AtomPattern`,
`VarDeclaration`, `MatchPattern`, `FunctionParameter`) and remove `RuleHead` as
a declaration origin; add barriers for `group_by`; declare implicit atom
bindings, `var` declarations, sequence declarations, match and `for` patterns,
and function parameters; record interpolation uses; record shadowing and
redeclaration facts; route resolution through `scope_kernel`; replace every
`_ => {}` arm over `Expr` with explicit handling. Update `unused-variable`
eligibility (`LS-D9`). Update specification section 7.1, design section 3.2.1,
and the users' guide.

### EP-M9: close-out

Update the developers' guide (module structure, boundary rules, the kernel
bridge and `make verus`, a proof inventory saying what each proof covers and
does not, helper re-use policy, corpus maintenance), the repository layout
(`verus/`, `tools/verus/`, `tests/fixtures/upstream/`, `tests/features/`), and
design section 6 (proof lane); reconcile every upstream artefact; mark roadmap
2.6.8 done after all acceptance passes; set this plan to `COMPLETE`.

### Abstraction policy compliance

Sweep result: no existing block type, follow-token classifier, scope kernel,
interpolation parser, or function-body accessor exists;
`parse_comma_separated_expressions` is unsuitable for blocks because a trailing
`;` changes the value. Re-use policy: `BlockExpr::items()` and `into_items()`
are the only traversal order; `classify_block_follow` is private to `block.rs`;
`scope_kernel` is called only by `src/sema/resolve.rs` and the bridge tests,
and must stay dependency-free and Verus-compatible; `Function::body()` and the
`StringLiteral` interpolations are the only parsers of those regions. Recorded
in the developers' guide at EP-M9.

## Milestones and plateaus

Every milestone ends with `make check-fmt`, `make typecheck`, `make lint`, and
`make test` passing, plus `make markdownlint` and `make nixie` when
documentation changed, and `make verus` from EP-M7 onwards. Logs go to
`/tmp/<gate>-ddlint-<branch>.out`. Each milestone's conformance check confirms
that its decisions are implemented as written, parses outside its scope are
unchanged, only planned public interfaces changed, no dependency was added, and
trace links are current. Compatibility decision for every milestone: none,
because ddlint is pre-1.0 and unreleased (version 0.1.0, no tags); callers are
updated in place and no alias preserves old behaviour.

- `EP-M1`, decisions recorded. Evidence: gates pass; the ADR has the
  required sections. Recovery: revert. Gaps: no code changes.
- `EP-M2`, corpus vendored and measured. Evidence:
  `tests/upstream_corpus.rs` passes; the unsupported-construct list is
  recorded; the corpus gate is decided. Recovery: delete the fixtures. Gaps:
  bodies not yet parsed.
- `EP-M3`, block expressions. Evidence: `V-B1` to `V-B8` artefacts pass;
  seeded-fault evidence recorded; register item 15 `implemented`. Recovery:
  revert to the last green commit. Gaps: brace maps still parse; scope
  semantics interim.
- `EP-M4`, upstream maps. Evidence: `V-M1` to `V-M4` pass; no brace map
  parses. Gaps: none for collections.
- `EP-M5`, `var`, closures, and interpolation parsed. Evidence: parser
  tables and properties pass; the `V-B7` snapshot is unchanged except planned
  rows.
- `EP-M6`, function bodies. Evidence: corpus bodies parse (or scheduled
  exceptions are asserted); `Function::body()` doctests pass.
- `EP-M7`, verified kernel. Evidence: `make verus` passes; the bridge test
  and differential property pass; negative controls recorded.
- `EP-M8`, scope model. Evidence: `V-S1` to `V-S9` pass; predicted lint
  changes observed and no others; behavioural scenarios pass.
- `EP-M9`, close-out. Evidence: all gates; roadmap 2.6.8 done.

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
make verus 2>&1 | tee "/tmp/verus-ddlint-${BRANCH}.out"   # from EP-M7
```

`make fmt` may reformat unrelated Markdown; restore unrelated churn before
committing. If `make test` fails with `Resource temporarily unavailable`, rerun
it once as `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1 make test`.

Focused commands per milestone:

```sh
cargo test --all-features --lib block_expr                 # EP-M3
cargo test --all-features --lib block_expression_proptest
cargo test --all-features --test block_expressions_bdd
cargo test --all-features --lib map_literal                # EP-M4
cargo test --all-features --test upstream_corpus           # EP-M2, EP-M6
cargo test --all-features --test scope_kernel_bridge       # EP-M7
cargo test --all-features --lib scope_kernel
cargo test --all-features --test lexical_scope_bdd         # EP-M8
cargo test --all-features --test semantic_scope_resolution
cargo test --all-features --test unused_variable_rule
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

### Behavioural specifications

`tests/features/block_expressions.feature` is bound by
`tests/block_expressions_bdd.rs`, following
`tests/relation_primary_key_bdd.rs`: a world struct with `RefCell` fields,
`#[allow_fixture_expansion_lints]` on fixtures, and steps returning
`StepResult`. "Parsing fails with" means some reported error message equals the
quoted text exactly; item lists are compared by parsing each expected item, not
by text.

```gherkin
Feature: Block expressions and collection literals
  Braces delimit blocks whose value is their final item unless the block is
  empty or ends with a semicolon. Map literals use brackets and arrows.

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

  Scenario Outline: Brackets build vectors and maps
    Given the expression source "<source>"
    When the expression is parsed
    Then the result is a <form>

    Examples:
      | source           | form                |
      | [a, b]           | vector literal      |
      | []               | vector literal      |
      | [a -> 1, b -> 2] | map literal         |
      | { x }            | block               |
      | ( x )            | parenthesized group |
      | { x: T }         | block               |

  Scenario Outline: Malformed braces and brackets are rejected
    Given the expression source "<source>"
    When the expression is parsed
    Then parsing fails with "<message>"

    Examples:
      | source       | message                                                            |
      | {a: 1, b: 2} | brace map literals are not DDlog syntax; write [k -> v, …] instead |
      | {a: 1}       | brace map literals are not DDlog syntax; write [k -> v, …] instead |
      | [a, b -> c]  | cannot mix vector elements and map entries in one literal          |
      | { a, b }     | expected ';' or '}' after a block item                             |
      | { a;; b }    | empty statement in block: remove the extra ';'                     |

  Scenario: Braced if branches with an empty block parse
    Given the expression source "if ready {} else { go() }"
    When the expression is parsed
    Then the result is an if expression whose then branch is an empty block
```

`tests/features/lexical_scope.feature` is bound by
`tests/lexical_scope_bdd.rs`; steps parse a program, build the semantic model,
and look up the declaration a named use resolves to by its source position.

```gherkin
Feature: Lexical scope follows upstream DDlog
  Variables resolve to the declaration the upstream compiler would bind.

  Scenario: Positive atoms declare and heads use
    Given the DDlog program
      """
      input relation Edge(a: u32, b: u32)
      output relation Out(a: u32)
      Out(x) :- Edge(x, y).
      """
    When the semantic model is built
    Then the use of "x" in the head resolves to the "x" declared in "Edge(x, y)"
    And the variable "y" is declared but never used

  Scenario: An assignment declares only var names
    Given the DDlog program
      """
      input relation In(a: u32)
      output relation Out(a: u32)
      Out(y) :- In(x), var y = x + 1, z = x.
      """
    When the semantic model is built
    Then the use of "y" in the head resolves to the "y" declared by "var y"
    And the use of "z" is unresolved

  Scenario: Negated atoms do not declare
    Given the DDlog program
      """
      input relation A(a: u32)
      input relation B(a: u32)
      output relation Out(a: u32)
      Out(x) :- A(x), not B(w).
      """
    When the semantic model is built
    Then the use of "w" is unresolved

  Scenario: group_by hides ungrouped variables
    Given the DDlog program
      """
      input relation Sale(item: string, amt: u32, who: string)
      output relation Total(item: string, total: u32)
      Total(item, t) :- Sale(item, amt, who), var t = amt.group_by(item).sum(), who == "x".
      """
    When the semantic model is built
    Then the use of "who" after the group_by is unresolved
    And the use of "item" in the head resolves to the "item" declared in "Sale(item, amt, who)"

  Scenario: A sequence declaration is visible only to later items
    Given the DDlog program
      """
      function f(a: u32): u32 { var b = a + 1; b }
      """
    When the semantic model is built
    Then the use of "b" resolves to the "b" declared by "var b"
    And the use of "a" resolves to the parameter "a"

  Scenario: Block declarations do not escape
    Given the DDlog program
      """
      function g(a: u32): u32 { { var b = a; b }; b }
      """
    When the semantic model is built
    Then the second use of "b" is unresolved

  Scenario: Match patterns shadow outer variables inside the arm
    Given the DDlog program
      """
      function h(v: Option<u32>, x: u32): u32 { match (v) { Some{x} -> x, None -> x } }
      """
    When the semantic model is built
    Then the use of "x" in the first arm resolves to the pattern "x"
    And the use of "x" in the second arm resolves to the parameter "x"

  Scenario: Interpolated strings record uses
    Given the DDlog program
      """
      function s(ip: string): string { "${ip}:80" }
      """
    When the semantic model is built
    Then the use of "ip" inside the string resolves to the parameter "ip"
```

The struct pattern `Some{x}` above uses ddlint's current pattern syntax; it
changes with roadmap 2.6.11.

### Unit and property suites

- Blocks (`src/parser/tests/block_expressions.rs`, its proptest module, and
  `src/parser/expression/block.rs` unit tests): accepted rows `{ x }`, `{ 1 }`,
  `{ f() }`, `{ (x, y) }`, `{ 1 + 2 }`, `{ a; b }`, `{ a; b; }`, `{ a; }`, `{}`,
  `{ { a } }`, `{ {a}; b }`, `{ (a; b) }`, `{ x: T }`, `{ a + b: T }`,
  `{ a; x: T }`, `{ return }`, `{ x = 1; x }`, `{ var x = 1; x }`,
  `{ |x| x; y }`, `{ match (x) { A -> 1 }; y }`; tail rows
  `{ if c { a } else { b }; d }`, `{ if c { a }; d }`,
  `{ if a { x } else if b { y }; z }`, `{ for (x in xs) { f(x) }; y }`,
  `{ for (x in xs) f(x); g() }`, `{ return x; y }`,
  `{ a = if c { x } else { y }; z }`; delimiter rows `{ (a; b); c }`,
  `{ f(a; b) }`, `{ [a; b] }`, `{ match (x) { A -> a; b } }`; rejected rows
  `{ a, b }`, `{ a; b, c }`, `{ a; b c }`, `{ ; }`, `{ a;; b }`, `{ a`, `{ a;`.
- Maps (`src/parser/tests/collections.rs`, rewritten): `[a -> 1]`,
  `[a -> 1,]`, `[a -> 1, b -> 2]`, nested `[a -> [b -> 2]]`, `[[k -> v] -> 1]`,
  `[a, b]`, `[]`; rejected `[a, b -> c]`, `[a -> 1, b]`, `[a ->]`, `[-> a]`;
  migration rows `{a: 1}`, `{a: 1, b: 2}`, `{"k": v}`, `{a: true}`,
  `{{x: 1}: 2}`; block rows `{ x: T }` and `{ x: Vec }`.
- `var`, closures, interpolation: `var x`, `var x: u32`, `var x = 1`,
  `(var a, var b) = f()`; `function(x: u32): u32 { x }`, `function() { }`;
  `"${a + b}"`, `"\${a}"` (escaped, no segment), `[|${x}|]`, nested
  `"${f("${y}")}"`, and unclosed `"${a"` (an error with an absolute span).
- Function bodies (`src/parser/tests/functions.rs` additions): braced,
  `= e`, `extern` without a body, a body with a parse error (absolute span),
  and the corpus.
- Kernel (`src/sema/scope_kernel.rs` tests and
  `tests/scope_kernel_bridge.rs`): table cases mirroring each `K-L` lemma, the
  bridge text comparison, and the differential property.
- Scope model (`src/sema/tests/scoping/`, split by topic, each file under
  400 lines): one case per `V-S` rule citing its upstream source; metamorphic
  properties; per-consumer cases constructing `Expr::Block` directly.

### Snapshot, public-API, and corpus tests

- `src/parser/tests/block_expression_snapshots.rs`: one inline `insta`
  snapshot rendering every accepted and rejected block and collection input as
  its source followed by `to_sexpr()` or `message @ start..end`, paired with
  semantic assertions on representative rows. Rendering: `{}` is `(block)`,
  `{ a }` is `(block (value a))`, `{ a; }` is `(block (stmts a))`, and
  `[a -> 1]` is `(map (entry a 1))`.
- `tests/semantic_scope_snapshots.rs`: an inline snapshot of the resolution
  graph (use name and span to declaration name and span) for three
  representative programs, paired with semantic assertions.
- `tests/block_expressions.rs`, a public-API integration test through
  `ddlint::parse`, `ddlint::sema::build`, and the linter `Runner`, over the
  program shown below. It asserts no parse errors, that `unused-relation` does
  not report `Checked` but does report `Witness`, and that the diagnostics
  match the parenthesized variant. Top-level `for (x in Items(x)) { Out(x) }.`
  keeps the existing unsupported-head diagnostic.
- `tests/upstream_corpus.rs`: every vendored program parses; every function
  body parses (EP-M6); `sema::build` succeeds and every variable use in the
  corpus resolves, because the corpus contains only programs the upstream
  compiler accepts.

Program used by `tests/block_expressions.rs`:

```ddlog
input relation Items(x: u32)
input relation Checked(x: u32)
input relation Witness(x: u32)
output relation Out(x: u32)
Out(x) :- Items(x), for (y in Items(y)) { Checked(y); Checked(x) }.
```

### Predicted `unused-variable` changes

On `tests/unused_variable_rule.rs` and `examples/`: head variables with no body
declaration stop being reported (they become unresolved uses, for roadmap
4.1.5); body-declared variables never used later are newly reported
(`R(x) :- S(x, y).` reports `y`); `for` and match pattern bindings unused in
their bodies are reported. Each existing case's new expectation is listed in
the EP-M8 commit message and checked against the behaviour tolerance.

### Red-Green-Refactor evidence

Recorded in `Artefacts and notes` per milestone: red transcripts, green
transcripts, seeded-fault transcripts, class-coverage counts, `cargo mutants`
summaries for `block.rs`, `scope_kernel.rs`, and the `sema` builder (survivors
killed or justified), Verus negative controls, and gate log paths.

Quality criteria: all suites above pass; `V-B*`, `V-M*`, and `V-S*` are
discharged by their tests; `K-L1` to `K-L6` are proved by `make verus` with no
trusted assumptions; formatting, type checking, lint, Markdown, and Mermaid
gates pass.

## Idempotence and recovery

Every step is an edit followed by a gate run; reruns are safe. Seeded faults
and Verus negative controls are applied in the working tree, recorded, and
reverted with `git restore`, never committed. `make install-verus` is
idempotent and verifies the checksum. To retry a milestone, return to its last
green commit after confirming no uncommitted work is needed.

## Artefacts and notes

Record here during implementation: Stage A findings, the corpus measurement,
red and green transcripts, class counts, seeded-fault and mutation summaries,
Verus run output and negative controls, approved snapshot texts, and gate log
paths.

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

`Expr` additions: `Block(BlockExpr)` and `VarDecl(String)`; `Closure` becomes
`{ params: Vec<ClosureParam>, body: Box<Expr> }` with
`pub struct ClosureParam { pub name: String, pub ty: Option<String> }`.
`StringLiteral` gains `interpolations: Vec<Expr>` (empty when not interpolated).
`Function` gains:

```rust
/// Parse this function's body, if it has one.
///
/// Error spans are source-absolute. Returns `None` for `extern` functions.
pub fn body(&self) -> Option<Result<FunctionBody, Vec<Simple<SyntaxKind>>>>;
```

`src/parser/ast/precedence.rs` gains
`pub(crate) const BLOCK_ITEM_MIN_BP: u8 = BP_SEQ + 1;`.

`src/parser/expression/block.rs` (private) defines `ItemPosition`,
`BlockFollow`, and
`classify_block_follow(position, Option<SyntaxKind>) -> BlockFollow`.

Message constants in `src/parser/error_messages.rs`, re-exported through
`src/test_util/mod.rs`: `BLOCK_ITEM_SEPARATOR_ERROR`, `EMPTY_BLOCK_ITEM_ERROR`,
`UNCLOSED_BLOCK_ERROR`, `BRACE_MAP_LITERAL_ERROR`,
`MIXED_COLLECTION_ENTRY_ERROR`, and `UNCLOSED_INTERPOLATION_ERROR`.

`src/sema/scope_kernel.rs` (crate-private, dependency-free):

```rust
pub(crate) struct KernelScope { pub(crate) parent: Option<usize>, pub(crate) anchor: u32 }
pub(crate) struct KernelDecl {
    pub(crate) name: u32, pub(crate) scope: usize, pub(crate) pos: u32, pub(crate) order: u32,
}
pub(crate) struct KernelBarrier { pub(crate) scope: usize, pub(crate) pos: u32, pub(crate) keep: Vec<u32> }
pub(crate) struct KernelUse { pub(crate) name: u32, pub(crate) scope: usize, pub(crate) pos: u32 }
pub(crate) fn resolve(
    scopes: &[KernelScope], decls: &[KernelDecl], barriers: &[KernelBarrier], use_site: &KernelUse,
) -> Option<usize>;
```

`sema` public additions: `ScopeKind::{Sequence, MatchArm, Function}`;
`SymbolOrigin::{AtomPattern, VarDeclaration, MatchPattern, FunctionParameter}`
(with `RuleHead` removed); `ScopeOrigin` variants for the new scopes; and the
facts `Symbol::shadows() -> Option<SymbolId>` and
`SemanticModel::redeclarations()`. Exact names are fixed in the EP-M8 red step.

Verus tooling: `tools/verus/{VERSION,SHA256SUMS,README.md}`,
`tools/rust-prover-tools/REF`, `verus/sema_scope_kernel.rs`, and the Makefile
targets `install-verus` and `verus`. No Cargo dependency is added.

## Related documentation and skills

Read before implementing: the specification sections listed in
`Conformance basis`; `docs/parser-implementation-notes.md`;
`docs/parser-conformance-register.md`; `docs/haskell-parser-analysis.md`;
`docs/building-an-error-recovering-parser-with-chumsky.md`;
`docs/rust-parser-testing-comprehensive-guide.md` (snapshots, error recovery,
Pratt tables, fuzzing); `docs/rust-testing-with-rstest-fixtures.md`;
`docs/ddlint-design.md` (sections 2, 3.2.1, 3.3, and 6);
`docs/complexity-antipatterns-and-refactoring-strategies.md`;
`docs/documentation-style-guide.md` (ADR template and numbering);
`docs/developers-guide.md` (googletest and rstest attribute order, fixture
lints, BDD steps); `docs/execplans/3-3-1-symbol-table-and-scope-resolution.md`;
and the 2.6.6.1 ExecPlan for the lazy-expression precedent.

Skills: `execplans`; `rust-router`, then `rust-unit-testing`, `proptest`,
`rust-types-and-apis`, and `rust-state-machines` (parser state);
`rust-verification` and `verus` (EP-M7); `arch-decision-records`;
`hexagonal-architecture` (pure cores, no new ports); `codegraph-mcp` (consumer
sweeps); `mapsplice`; `changelog`; `commit-message`; and `en-gb-oxendict-style`.

## Revision note

2026-10-18: initial draft.

2026-10-23: second draft after the first expert review (scoped `;` terminator,
specification reconciliation, consumer semantics, test design fixes).

2026-11-17: third draft after user direction. What changed: brace map literals
are dropped in favour of upstream `[k -> v]` with migration diagnostics; the
plan now models upstream lexical scope, which adds `var` expressions,
`function` closures, interpolation parsing, function-body parsing, a rule-model
inversion (bodies declare, heads use), sequence, match, and function scopes,
`group_by` barriers, and a Verus-proved scope kernel; an upstream corpus
milestone gates function-body parsing; the harness milestone is removed because
`main` provides it; ADR, roadmap, and path references follow `main`; and the
three earlier decisions are recorded as approved. Effect: the plan grows from
four to nine milestones and needs a second expert review before approval.
