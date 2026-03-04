# Parser implementation notes

This document captures parser implementation details that are non-obvious in
code review. It is intentionally non-normative.

- Language contract: `docs/differential-datalog-parser-syntax-spec-updated.md`
- Spec/code deltas and open decisions: `docs/parser-conformance-register.md`
- Delivery sequencing: `docs/roadmap.md`

## Status and ownership

- This document describes current implementation behaviour.
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

Top-level `for` is unsupported in this parser generation. The rule scanner
emits a diagnostic ("top-level \`for\` is not supported; use \`for\` inside
rule bodies instead") when `K_FOR` appears at a top-level line-start position.
This decision is recorded in the conformance register (item 8).

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
