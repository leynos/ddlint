# Parser implementation notes

This document captures parser implementation details that are non-obvious in
code review. It is intentionally non-normative.

- Language contract: `docs/differential-datalog-parser-syntax-spec-updated.md`
- Spec/code deltas and open decisions: `docs/parser-conformance-register.md`
- Delivery sequencing: `docs/roadmap.md`

## Status and ownership

- This document describes current implementation behaviour.
- `docs/ddlint-design.md` is the design-level source for the target split-crate
  architecture and public contracts.
- This document is the worked implementation guide for current module mapping,
  migration sequencing, and non-regression invariants.
- If behaviour differs from the syntax spec, track that mismatch in
  `docs/parser-conformance-register.md` rather than duplicating decision text
  here.
- Historical proposal content was archived from:
  `docs/archive/pratt-parser-for-ddlog-expressions.md`,
  `docs/archive/function-parsing-design.md`, and `docs/archive/parser-plan.md`.

## Parser architecture

The parser stack is split into three cooperating layers:

- Tokenization (`src/tokenizer.rs`): emits full-fidelity tokens with trivia.
- Span scanners (`src/parser/span_scanners/*`): identify top-level statement
  spans and structural boundaries.
- AST wrappers and expression parsing (`src/parser/ast/*`,
  `src/parser/expression/*`): build typed views and expression trees.

The expression parser is a bespoke Pratt parser over the token stream. It does
not use `chumsky::pratt`; operator binding powers are defined centrally in
`src/parser/ast/precedence.rs`.

## ADR-001 split-crate migration mapping

The current implementation still lives inside the `ddlint` crate, but ADR-001
defines the target extraction into dedicated parser crates. During that
migration, the current module layout should map to the future crates as follows:

- `ddlog-syntax`:
  `src/tokenizer.rs`, `src/language.rs`, `src/parser/token_stream.rs`,
  `src/parser/cst_builder/*`, `src/parser/span_scanners/*`, CST-oriented typed
  wrappers, and parse-stage diagnostic machinery.
- `ddlog-sema`:
  owned semantic data extracted from today's AST helper surface, semantic
  validation passes, dependency extraction logic, and planner-handoff export
  helpers.
- `ddlog-parser`:
  the top-level parse orchestration surface, syntax-to-semantics coordination,
  and any temporary compatibility shims needed while consumers migrate.

This mapping should be treated as a movement guide for existing modules rather
than a licence to preserve current internal structure unchanged. If a module
mixes syntax and semantic concerns today, split the responsibilities before or
while moving it.

## Migration invariants

The following invariants must hold throughout the crate split:

- Lossless CST fidelity must remain byte-accurate, including trivia
  preservation and existing span behaviour for parser-facing APIs.
- Parse-stage diagnostics must remain available without instantiating the
  semantic layer.
- Semantic nodes must carry provenance back to syntax spans for declarations,
  rule heads, body terms, aggregations, attributes, and adornments.
- Compiler-facing workflows must stop depending on CST-string extraction and
  reparsing once the semantic layer lands.
- Planner-relevant semantic collections must expose deterministic ordering so
  downstream dependency extraction and cache-key generation remain stable.
- Compatibility shims must live only in `ddlog-parser`, and each shim needs a
  deprecation target release plus a tracked removal issue.
- Phase exits must use the validation scopes defined in
  `docs/adr-001-parser-crate-split.md`: parser unit scope, integration scope,
  and the `tests/fixtures/phase3/` corpus introduced for semantic extraction.

## Expression parser invariants

### Qualified calls and unresolved application

- Qualified, lower-case terminal names parse as `Expr::Call`.
- Bare names and other non-qualified callees parse as `Expr::Apply`.
- This preserves deterministic parse-time behaviour and defers semantic
  disambiguation to name-resolution passes.

Primary implementation points:

- `src/parser/expression/pratt.rs`
- `src/parser/expression/qualified.rs`

### Struct-literal guard

`IDENT { ... }` is ambiguous in control-flow contexts. The parser uses a
struct-literal guard while parsing `if` conditions and similar prefixes to
avoid consuming branch braces as struct fields.

Guard suspension points are explicit:

- parenthesized sub-expressions,
- brace groups,
- closure bodies,
- contexts where struct literals are expected.

This behaviour is an implementation extension; policy status is tracked in the
conformance register.

### Recursion and delimiter safety

- Expression nesting is capped to avoid stack blowups.
- Delimiter tracking is depth-aware and keeps parsing recoverable after local
  syntax errors.
- Combined delimiter tokens such as `<<` are tracked using explicit unit counts
  in delimiter stack operations.

## Control-flow expression behaviour

### `if` and `else`

- Missing `else` yields unit tuple `Expr::Tuple(vec![])`.
- `else if` chains nest as right-leaning `Expr::IfElse` nodes.
- Missing branch expressions emit targeted diagnostics tied to the offending
  token span.

### `match`

- Scrutinee parsing uses normal expression parsing.
- Arm patterns are parsed via the dedicated pattern parser.
- At least one arm is required; optional trailing comma is accepted.

### `for`

- Rule-body `for` constructs parse as `Expr::ForLoop`.
- Binding uses the shared pattern parser.
- Optional `if` guard is represented as `Option<Box<Expr>>`.
- Top-level `for` statements are lowered into semantic rules exposed via
  `Parsed::semantic_rules()`.
- Lowering order is deterministic: iterable first, then optional guard, then
  nested iterable/guard pairs, then the terminal atom-like body as the rule
  head.
- Unsupported terminal body statement forms emit a targeted diagnostic:
  "top-level \`for\` body must end in an atom-like expression (for example
  \`Rel(args)\`)".

## Rule-body integration and term extraction

Rule bodies are split into comma-separated literal spans with delimiter-aware
tracking. Each literal is then validated through expression parsing.

The `Rule` AST wrapper exposes helpers that classify terms into:

- expression terms,
- assignment terms (`Pattern = Expr`),
- aggregation terms (`group_by` and legacy `Aggregate` forms).

Implementation points:

- `src/parser/span_scanners/rules.rs`
- `src/parser/ast/rule.rs`

Aggregation and lowering stage boundaries remain design-sensitive and are
tracked in the conformance register.

## Shared declaration parsing utilities

Declaration parsers for functions, relations, and transformers reuse helpers in
`src/parser/ast/parse_utils/`.

Important invariants:

- `parse_name_type_pairs` enforces `name: type` pairs with delimiter-aware type
  parsing.
- `parse_type_expr` is recursive and supports nested delimiters `()[]{}` and
  angle forms.
- Delimiter diagnostics preserve opening-token spans for unclosed/mismatched
  cases.
- Optional return type parsing (`parse_type_after_colon`) reuses the same type
  expression logic and trivia skipping.

These helpers are shared intentionally to keep declaration parsing consistent
across top-level constructs.

## Diagnostics policy

The parser prefers localized, span-precise diagnostics and recovery over hard
stops. This keeps CST and AST extraction usable in partially invalid files.

Representative diagnostics include:

- missing branch expressions in control-flow prefixes,
- malformed delimiters with expected-vs-found reporting,
- malformed aggregation signatures,
- invalid transformer declaration forms.

## File index

- Tokenization and keyword policy: `src/tokenizer.rs`
- Entry parse orchestration: `src/parser/mod.rs`
- Pratt parser: `src/parser/expression/pratt.rs`
- Prefix/infix helpers: `src/parser/expression/*.rs`
- Rule span scanning: `src/parser/span_scanners/rules.rs`
- Top-level scanners: `src/parser/span_scanners/*.rs`
- AST wrappers: `src/parser/ast/*.rs`
- Shared parse utilities: `src/parser/ast/parse_utils/*.rs`
