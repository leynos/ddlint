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
  (`src/parser/expression/pratt.rs`, `src/parser/expression/qualified.rs`).
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
  (`src/parser/ast/rule_head.rs`).
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
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` items `2.5.4` and `2.6.1`.

### 9. Aggregation extraction stage boundary

- Topic: parse stage versus semantic stage for aggregation enforcement.
- Current behaviour (code): aggregation classification and validation are
  available via `Rule::body_terms()` and `Rule::flattened_body_terms()`, while
  `parse()` and `Parsed::errors()` do not enforce aggregation misuse as a
  global parse pipeline contract (`src/parser/ast/rule.rs`,
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
  (`src/parser/expression/data_structures.rs`, `src/parser/ast/expr.rs`).
  `Parsed::semantic_rules()` remains limited to top-level `for` desugaring and
  does not lower collection literals.
- Spec/target behaviour: section 6.4 describes collection literal lowering as
  scheduled work owned by later lowering stages rather than the parser or
  current rule-analysis helpers.
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
  optional `&`, record bodies, bracket element-type bodies, and opaque
  primary-key suffixes (`src/parser/span_scanners/relations.rs`). The typed AST
  exposes `RelationRole`, `RelationKind`, `RelationBody`, keyword-presence
  flags, `is_ref()`, and compatibility `primary_key()` binder/list access
  (`src/parser/ast/relation.rs`). Regression coverage includes the relation
  form matrix and property tests (`src/parser/tests/relations.rs`,
  `src/parser/tests/relation_proptest.rs`).
- Spec/target behaviour: matches section 5.5. Absence of a role means internal;
  `internal` is not reserved. Absence of a kind means `relation`. Primary keys
  are accepted only on `input` record relations; bracket-wrapped primary-key
  clauses are rejected with a deterministic diagnostic. Typed access to the
  trailing primary-key expression is deferred to roadmap item `2.6.6.1`, while
  the CST preserves the text.
- Decision status: `implemented`.
- Roadmap item: `docs/roadmap.md` item `2.6.6`.

### 14. Legacy token compatibility policy

- Topic: `typedef`, `as`, legacy type names, `#`, `<=>` policy completion.
- Current behaviour (code): tokenizer still recognizes these tokens and parser
  treatment is mixed (`src/tokenizer.rs`).
- Spec/target behaviour: section 9.1 records compatibility intent but not a
  fully closed policy matrix.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.7`.

### 15. Brace-group extension `{ expr }`

- Topic: extension policy.
- Current behaviour (code): `{ expr }` parses as a brace-group expression
  (`src/parser/expression/data_structures.rs`).
- Spec/target behaviour: this extension needs explicit support or rejection.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.8`.

## Maintenance rules

When parser behaviour changes, update this register in the same change:

1. Update the matching topic entry.
2. Update `docs/roadmap.md` status for any affected scheduled item.
3. Update the syntax spec if the behavioural contract changed.
