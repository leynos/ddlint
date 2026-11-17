# Parser conformance register

This register tracks parser behaviour against the syntax specification.

- Normative syntax contract:
  `docs/differential-datalog-parser-syntax-spec-updated.md`
- Implementation detail companion: `docs/parser-implementation-notes.md`
- Scheduling and ownership: `docs/roadmap.md`

## Status legend

- `implemented`: code and tests align with the intended contract.
- `decision needed`: code/spec mismatch or ambiguous contract.
- `scheduled`: decision exists and is planned in `docs/roadmap.md`.

## Register

### 1. Qualified calls versus unresolved application

- Topic: parse-time call classification.
- Current behaviour (code): bare or non-qualified callees parse as
  `Expr::Apply`; only qualified lower-case terminal forms parse as `Expr::Call`
  (`src/parser/expression/pratt/mod.rs`, `src/parser/expression/qualified.rs`).
- Spec/target behaviour: this matches the current spec in section 2.2.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.4.5`.

### 2. String families, interning, and pattern restrictions

- Topic: literal coverage and pattern interpolation rejection.
- Current behaviour (code): tokenizer and literal parsing support standard,
  raw, and raw-interpolated strings, including interned forms; interpolated
  strings are rejected in patterns (`src/tokenizer.rs`,
  `src/parser/ast/string_literal.rs`, `src/parser/pattern/mod.rs`).
- Spec/target behaviour: matches sections 3.2 and 5.12.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.4.1`.

### 3. Numeric widths and fit checks

- Topic: shaped integer/float literal parsing.
- Current behaviour (code): numeric parsing records width/base/sign metadata and
  validates fit constraints (`src/parser/expression/numeric.rs`,
  `src/parser/ast/number.rs`).
- Spec/target behaviour: matches section 3.1.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.4.2`.

### 4. `apply` items

- Topic: top-level apply declaration support.
- Current behaviour (code): apply scanner and AST wrapper are implemented and
  tested (`src/parser/span_scanners/apply.rs`, `src/parser/ast/apply.rs`,
  `src/parser/tests/apply.rs`).
- Spec/target behaviour: matches section 5.7.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.5.5`.

### 5. Transformer extern-only validation

- Topic: rejecting non-extern transformer declarations.
- Current behaviour (code): diagnostics emitted for non-extern transformers
  (`src/parser/span_scanners/transformers.rs`,
  `src/parser/tests/transformers.rs`).
- Spec/target behaviour: matches section 5.4.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.5.5`.

### 6. Head by-reference lowering

- Topic: `&Rel{...}` in rule heads.
- Current behaviour (code): lowered to `ref_new(...)` in head context
  (`src/parser/ast/rule_head/mod.rs`).
- Spec/target behaviour: matches section 6.3.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.5.2`.

### 7. Legacy migration-plan omissions now covered by the spec

- Topic: host keyword reservation, control-flow statements, entrypoint docs.
- Current behaviour (code): lexer keyword policy and parser support align with
  spec sections 1.1.1, 2.3.1, and 5.10.
- Spec/target behaviour: migration gaps are closed.
- Decision status: `implemented`.
- Roadmap item: closed under phase 2 conformance tasks.

### 8. Top-level `for` desugaring

- Topic: spec requires top-level `for` to be desugared into rules.
- Current behaviour (code): parser-level lowering emits semantic rules for
  top-level `for` statements via `Parsed::semantic_rules()`. Rule-body `for`
  loops remain represented as `Expr::ForLoop` in explicit rules.
- Spec/target behaviour: section 6.5 documents top-level `for` desugaring
  into semantic rules with deterministic term ordering and targeted diagnostics
  for unsupported terminal statement forms.
- Known divergence: upstream parses a top-level `for` header as an atom with an
  optional `v in` binder, rejecting `for (a in A(a))`, and treats a rule-body
  `for` as an expression loop over a collection. ddlint's header, rule-body
  splitting at `=`, and term-only `if`/`for` branches differ; item 16 records
  the differences and schedules the alignment.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` items `2.5.4` and `2.6.1`.

### 9. Aggregation extraction stage boundary

- Topic: parse stage versus semantic stage for aggregation enforcement.
- Current behaviour (code): aggregation classification and validation are
  available via `Rule::body_terms()` and `Rule::flattened_body_terms()`, while
  `parse()` and `Parsed::errors()` do not enforce aggregation misuse as a
  global parse pipeline contract (`src/parser/ast/rule/mod.rs`,
  `src/parser/mod.rs`).
- Spec/target behaviour: sections 1, 6.1, 6.2, and 13 now describe
  aggregation handling as rule-body semantic extraction layered on top of the
  CST-backed parse result.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.6.2`.

### 10. Collection literal lowering stage boundary

- Topic: when vec/map literal lowering occurs.
- Current behaviour (code): `parse()` and `parse_expression()` preserve vector
  and map literals as raw `Expr::VecLit` and `Expr::MapLit` nodes in the CST
  (`src/parser/expression/data_structures.rs`, `src/parser/ast/expr/mod.rs`).
  `Parsed::semantic_rules()` remains limited to top-level `for` desugaring and
  does not lower collection literals.
- Spec/target behaviour: section 6.4 describes collection literal lowering as
  scheduled work owned by later lowering stages rather than the parser or
  current rule-analysis helpers. The literal surface itself is aligned with
  upstream under roadmap item `2.6.8`: the brace map literal is withdrawn, the
  empty vector literal `[]` is withdrawn, and `[k -> v, …]` becomes the map
  literal form.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.6.3`.

### 11. Index declaration grammar

- Topic: index grammar shape.
- Current behaviour (code): parser accepts
  `index Name(field: Type, ...) on Atom`, exposes typed fields via
  `Index::fields()`, and exposes the normalized `on` target via
  `Index::on_target()` (`src/parser/span_scanners/indexes.rs`,
  `src/parser/ast/index.rs`).
- Spec/target behaviour: matches section 5.6. The legacy shorthand
  `index Name on Relation(columns)` is rejected with a deterministic diagnostic
  requiring the typed field list before `on`.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.6.4`.

### 12. Transformer declaration grammar

- Topic: output signature requirements and transformer-name token class.
- Current behaviour (code): parser requires output list after `:`
  and emits a deterministic diagnostic when the signature is missing or empty.
  Transformer names must start with a lowercase letter or underscore; the
  parser rejects capitalized names (e.g., `Foo`) with a targeted diagnostic
  (`src/parser/span_scanners/transformers.rs`,
  `src/parser/tests/transformers.rs`). Malformed declarations recover by
  skipping to the next newline boundary through shared scanner utilities.
- Spec/target behaviour: matches section 5.4's extern-only transformer form
  with a mandatory `:` plus at least one ordered output identifier, and
  enforces lowercase-name class for transformer identifiers.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.6.5`.

### 13. Relation forms and role/kind grammar

- Topic: relation declaration variants.
- Current behaviour (code): scanner accepts optional role, optional kind,
  optional `&`, record bodies, bracket element-type bodies, and primary-key
  suffixes (`src/parser/span_scanners/relations/mod.rs`). The typed AST exposes
  `RelationRole`, `RelationKind`, `RelationBody`, keyword-presence flags,
  `is_ref()`, and typed primary-key access through `PrimaryKey`,
  `PrimaryKeyBinder`, and `PrimaryKeyExpression`
  (`src/parser/ast/relation/mod.rs`,
  `src/parser/ast/relation/primary_key/mod.rs`). Regression coverage includes
  the relation form matrix and property tests
  (`src/parser/tests/relations/mod.rs`,
  `src/parser/tests/relation_proptest.rs`), the primary-key unit tests
  (`src/parser/ast/relation/primary_key/tests.rs`), the primary-key properties
  and strategies (`src/parser/tests/relation_primary_key_props.rs`,
  `src/parser/tests/relation_primary_key_strategies.rs`), and the behavioural
  feature (`tests/relation_primary_key_bdd.rs`).
- Spec/target behaviour: matches section 5.5. Absence of a role means internal;
  `internal` is not reserved. Absence of a kind means `relation`. Primary keys
  are accepted only on `input` record relations; bracket-wrapped primary-key
  clauses are rejected with a deterministic diagnostic. Typed access to the
  trailing expression is implemented by `PrimaryKeyExpression`: binder names
  are independent of expression validity, and `parse()` reports source-absolute
  error spans.
- Known limitation: the binder list accepts any identifier token, so
  `primary key (Foo)` is accepted even though section 5.5 spells a binder as
  `LcName`. Transformer names already enforce the lowercase class, so this is
  an open conformance gap rather than an intended superset. The equivalent
  import-alias gap recorded in section 5.2 is the same class of gap. Both are
  tracked as roadmap item `2.6.9`.
- Known limitation: `PrimaryKeyExpression::parse()` returns
  `chumsky::error::Simple` values, so an internal parser type appears in the
  public API. Replacing it with a domain-owned error type is assigned to the
  ADR-001 crate split, as recorded in the ADR-002 amendment and plan decision
  `D3`. Issue #267 tracks the public `chumsky` diagnostic exposure, including
  this API and its source-absolute spans.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` items `2.6.6` and `2.6.6.1`.

### 14. Legacy token compatibility policy

- Topic: `typedef`, `as`, legacy type names, `#`, `<=>` policy completion.
- Current behaviour (code): tokenizer recognizes these tokens, and
  `src/parser/reserved_tokens.rs` single-sources parser diagnostics for
  unsupported uses. `type`, `import X as Y`, and `#[...]` remain accepted.
- Spec/target behaviour: section 9.1 records the closed policy matrix.
  `typedef`, legacy type names, bare `#`, and `<=>` are rejected with
  deterministic diagnostics; `as` and attribute `#` keep their supported
  grammar roles.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.6.7`.

### 15. Braces as block expressions

- Topic: policy for braced expression forms.
- Current behaviour (code): `{ expr }` parses as a ddlint brace-group
  expression, `{k: v}` as a brace map literal, and `{}` as an empty map
  (`src/parser/expression/data_structures.rs`).
- Spec/target behaviour: braces are always upstream block expressions, `{ … }`
  with `;` as the item separator and an optional final value, so `{}` is the
  empty block and `{ e }` is a block whose value is `e`. There are no brace
  group or brace map forms, and the colon-terminating map-key mode is deleted.
  The block is represented as `Expr::Block(BlockExpr)` and the `;` separator is
  kept from escaping a block item by a scoped parser state. Decision record:
  [ADR 006](./adrs/adr-0006-block-expressions-and-collection-syntax.md).
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.8`.

### 16. Sequencing and rule-level `for`

- Topic: the `;` operator outside braces and rule-level `for` forms.
- Current behaviour (code): `;` is the infix `BinaryOp::Seq`, accepted in any
  expression position, including `( a; b )`, call arguments, vector items,
  struct fields, match-arm bodies, and unbraced `for` bodies
  (`src/parser/ast/precedence.rs`). Top-level `for (a in A(a)) …` lowers to
  semantic rules, and rule-body `for` loops over relations are classified as
  `RuleBodyTerm::ForLoop` (`src/parser/top_level_for/`,
  `src/parser/ast/rule/classification.rs`). A rule-body
  `for (x in xs) y = f(x)` is split at the `=` and fails pattern parsing.
- Spec/target behaviour: upstream DDlog has `;` only inside block expressions,
  parses a top-level `for` header as an atom with an optional `v in` binder
  (rejecting `for (a in A(a))`), and treats a rule-body `for` as an expression
  loop over a collection. Upstream `if`/`for` branches are terms, so
  `{ if c { a } else { b } ++ x }` also differs.
- Decision status: `decision needed`.
- Roadmap item: `docs/roadmap.md` item `2.6.10`.

### 17. Struct-literal and struct-pattern syntax

- Topic: constructor syntax.
- Current behaviour (code): struct literals and patterns use `C { f: e }`
  (`src/parser/expression/data_structures.rs`, `src/parser/pattern/`). The
  struct-literal guard's `{ ident :` heuristic rejects the valid upstream
  `if ready { x: T }`.
- Spec/target behaviour: upstream uses `C{.f = e}` for named fields and
  `C{e}` for positional fields, in both expressions and patterns.
- Decision status: `decision needed`.
- Roadmap item: `docs/roadmap.md` item `2.6.11`.

### 18. Expression forms for variable scoping

- Topic: parser support that upstream scoping depends on.
- Current behaviour (code): `var x` is rejected as an expression
  (`unexpected token: K_VAR`); `function(…)` closures are unsupported; pipe
  closures take untyped parameters; `${…}` interpolation segments are not parsed
  (`src/parser/ast/string_literal.rs` records only a flag); ascription types
  are parsed as expressions (`parse_type`), so `x: Vec<u32>` misparses and
  `{a: 1}` parses as a block containing a nonsensical ascription; `Expr` nodes
  carry no source spans.
- Spec/target behaviour: upstream `evardcl`, `eclosure`, interpolated
  strings, and `typeSpecSimple`; identifier-bearing nodes carry source-absolute
  spans so semantic analysis can identify each occurrence.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.12`.

### 19. Function bodies

- Topic: parsing function bodies.
- Current behaviour (code): the function scanner records bodies as balanced
  brace spans that are never parsed (`src/parser/span_scanners/functions.rs`,
  `src/parser/ast/function.rs`); `function f(): T = expr` bodies are absorbed
  into the return-type scan; the scanner treats `function(…)` closures as
  malformed declarations.
- Spec/target behaviour: upstream bodies are `{ eseq }` or `= expr`, parsed as
  expressions, with no terminator after a declaration.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.13`.

### 20. Remaining upstream expression forms

- Topic: upstream forms missing from the specification.
- Current behaviour (code): `x @ pat` bindings, `v in R(…)` atom binders
  outside top-level `for`, and the `?` operator are not parsed; bit slices use
  `e[h, l]` (`src/parser/expression/pratt/postfix.rs`); adjacent string
  literals are not concatenated.
- Spec/target behaviour: upstream `ebinding`, atom binders, `ETry`,
  `e[h:l]` slices, and `many1 stringLit` concatenation.
- Decision status: `decision needed`.
- Roadmap item: `docs/roadmap.md` item `2.6.14`.

### 21. Variable scoping in the semantic model

- Topic: lexical scope semantics.
- Current behaviour (code): `src/sema/` declares rule-head variables and
  treats body occurrences as uses; it opens no scope for blocks, match arms, or
  functions, ignores match and expression-level `for` patterns, and cannot see
  `var` declarations inside expressions.
- Spec/target behaviour: section 8.1 of
  `docs/differential-datalog-parser-syntax-spec-updated.md` records upstream
  DDlog's scoping rules: body clauses declare and heads use, positive atoms
  declare implicitly, assignments declare only through `var`, `group_by` hides
  ungrouped variables, and sequence, match, `for`, closure, and function scopes
  nest.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` items `3.3.5` and `3.3.6`.

### 22. Type syntax versus upstream

- Topic: type keywords and primitive type names.
- Current behaviour (code): `typedef`, `bit<N>`, `signed<N>`, `bigint`,
  `double`, and `float` are rejected with deterministic diagnostics in favour of
  `type`, sized integers such as `u32`, and `f32`/`f64`
  (`src/parser/reserved_tokens.rs`; item 14).
- Spec/target behaviour: upstream DDlog's grammar uses `typedef` and the
  `bit`, `signed`, `bigint`, `double`, and `float` types. Whether ddlint's
  specification keeps its policy or aligns with upstream is undecided.
- Decision status: `decision needed`.
- Roadmap item: `docs/roadmap.md` item `2.6.15`.

## Stable diagnostic compatibility surface

Issue `#304` establishes a backend-neutral observability precursor to Phase 2 of
[Architectural Decision Record 001 (ADR-001)](./adrs/adr-0001-parser-crate-split.md).
`DiagnosticCode` freezes `D-REL-001` through `D-REL-008` as stable identifiers,
while `DiagnosticCategory` classifies every current scanner and parser
orchestration merge point. Existing code meanings do not change; adding a code
is additive. Consumers must not treat human-facing message text as an
identifier.

`parse_with_observer()` reports attempt and diagnostic records without changing
the `Parsed` result. `parse()` uses the no-op observer and remains independent
of any logging or metrics runtime. This does not complete roadmap item `2.8.1`,
which still requires the post-split public diagnostic contract and stage
modelling.

## Maintenance rules

When parser behaviour changes, update this register in the same change:

1. Update the matching topic entry.
2. Update `docs/roadmap.md` status for any affected scheduled item.
3. Update the syntax spec if the behavioural contract changed.
