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
- Current behaviour (code): top-level rule scanning triggers do not provide a
  `K_FOR` entry path (`src/parser/span_scanners/rules.rs`).
- Spec/target behaviour: section 6.5 currently states desugaring.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.1`.

### 9. Aggregation extraction stage boundary

- Topic: parse stage versus semantic stage for aggregation enforcement.
- Current behaviour (code): aggregation classification is available via
  `Rule::body_terms()`, but not enforced as a global parse pipeline contract
  (`src/parser/ast/rule.rs`, `src/parser/mod.rs`).
- Spec/target behaviour: section 6.1 describes parse-stage extraction with
  validation semantics.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.2`.

### 10. Collection literal lowering stage boundary

- Topic: when vec/map literal lowering occurs.
- Current behaviour (code): parser emits raw `Expr::VecLit` and `Expr::MapLit`
  nodes (`src/parser/expression/data_structures.rs`).
- Spec/target behaviour: section 6.4 describes early lowering.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.3`.

### 11. Index declaration grammar

- Topic: index grammar shape.
- Current behaviour (code): parser expects `index Name on Relation(columns)`
  form (`src/parser/span_scanners/indexes.rs`).
- Spec/target behaviour: section 5.6 currently describes typed index field list
  and `on Atom` form.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.4`.

### 12. Transformer declaration grammar

- Topic: output signature requirements.
- Current behaviour (code): parser requires output list after `:`
  (`src/parser/span_scanners/transformers.rs`).
- Spec/target behaviour: section 5.4 currently documents extern-only
  transformer shape without equivalent output-list requirement.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.5`.

### 13. Relation forms and role/kind grammar

- Topic: relation declaration variants.
- Current behaviour (code): scanner supports `input relation`,
  `output relation`, and bare `relation` forms
  (`src/parser/span_scanners/relations.rs`).
- Spec/target behaviour: section 5.5 documents broader role/kind variants.
- Decision status: `scheduled`.
- Roadmap item: `docs/roadmap.md` item `2.6.6`.

### 14. Legacy token compatibility policy

- Topic: `typedef`, `as`, legacy type names, `#`, `<=>` policy completion.
- Current behaviour (code): tokenizer still recognises these tokens and parser
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
