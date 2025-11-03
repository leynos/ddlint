# DDlint parser gap analysis

## Executive summary

- The expression engine is in **good shape**: a clean Pratt parser with tuples,
  struct literals, closures, if/match/for control-flow, decent delimiter
  diagnostics, and tidy test coverage
  (`src/parser/expression/*`, `src/parser/tests`, `tests/*`).
- The **surface language** in the spec is **wider** than what’s implemented
  today. Notable gaps: rich string forms & interning, width-qualified numeric
  literals, collection literals (map/vector) + their desugarings,
  `group_by`/`Aggregate` extraction, by-ref heads, multi-head rules with `@`
  locations on heads, and some item forms (`apply`, transformer extern-only
  guard). Several **deviations** also exist (e.g., `{ expr }` as an expression
  grouping, and how unqualified `name(…)` calls are parsed).
- Most gaps are tractable, but two require **design choices**: (1) whether to
  keep `{ expr }` as grouping (not in spec) and (2) whether to change call
  parsing to obey the spec’s “only fully qualified calls are parsed as calls”
  rule.

In this analysis, RHS (right-hand side) refers to the rule body, not the rule
head.

## Where the implementation deviates (or is missing) vs the spec

### 1) Lexical and literals

- **String families & interning**
  Spec: three families (normal `"…"`, raw `[| … |]`, interpolated raw
  `$[| … ${expr} … |]`), plus `i"…"`/`i[|…|]` interning and the ban on
  interpolated strings in patterns. Code today: literal parsing only recognizes
  **quoted strings** via `T_STRING` in the Pratt layer
  (`src/parser/expression/literals.rs`) and strips quotes; no raw/interpolated
  forms, no interning prefix handling. **Action:** extend tokenizer to emit
  distinct tokens for raw/interpolated strings and `i`-prefixed forms; extend
  Pratt literals to build the right Abstract Syntax Tree (AST) (and keep the
  “no interpolated strings in patterns” validation).

- **Numeric literal widths**
  Spec: width-qualified ints (`8'hFF`, `16'sd-1`) and floats (`…'f32|f64`) with
  **fit checks**. Code: numbers are parsed as **opaque text** `Literal::Number`
  with no width/signedness semantics. **Action:** teach the tokenizer to
  recognize width/base/signedness; add parse-time validation and a shaped
  abstract syntax tree representation (or rich literal node) so lints can
  reason about sizes.

### 2) Operators and precedence

- **Operator table completeness**
  Spec mandates an authoritative table incl. `++` (concat), bit-xor `^`,
  implication `=>`, the usual arithmetic/logic, and tight postfix
  (`f(…)`, `e[expr]`, `e.name`) precedence. Code: postfix/index/call/member
  access and most arithmetic/logic exist (see
  `src/parser/expression/infix.rs`, `src/parser/ast/precedence.rs`).
  Implication `=>` is tokenized (search shows `T_IMPLIES`), but tests do not
  exercise it under Pratt; concat `++`/xor `^` aren’t visible in tests either.
  **Action:** (a) ensure all spec’d tokens are wired into Pratt with correct
  binding power/associativity; (b) add focused tests for `=>`, `^`, `++` to
  lock precedence.

- **Call parsing for unqualified names**
  Spec: “Only **fully qualified** `module::func` parses as a function call at
  parse time; a bare `name(…)` parses as a variable application and is
  disambiguated by name resolution later”. Code: `foo()` becomes
  `Expr::Call { name: "foo", … }` immediately (see
  `tests/expression_var_and_call.rs`, `src/parser/tests/expression.rs`).
  **Action (policy choice):**

  - To **match the spec**: parse `name(…)` as `Variable("name")` followed by a
    **postfix apply** node (or keep `Call` but mark it “unresolved” unless
    qualified).
  - Alternatively, update the spec if the current simpler behaviour is the
    intended policy.

### 3) Expressions and control-flow

- **`match` expression shape**
  Spec defines **pattern grammar** (tuples, structs, typed patterns, wildcards,
  literals, etc.) and says patterns appear in three contexts
  (match/flatmap/for) with the **same surface syntax, different abstract syntax
  tree shapes**. Code: `match` is implemented (great!) but **stores arm
  patterns as raw strings** (`MatchArm { pattern: String, body: Expr }` in
  `src/parser/ast/expr.rs`) and collects them via delimiter-balanced slicing
  (`expression/pattern_collection.rs`). **Deviation:** the current code does
  not build the **pattern tree** the spec expects. **Action:** add a **pattern
  parser** that produces nodes like
  `PVar`, `PTuple`, `PStruct`, `PLit`, `PTyped`, … and use it for **match
  arms**, **for-loop bindings**, and **right-hand-side flatmap binds**.

- **`for` loops**
  Spec: `for (Pattern in Expr) if Guard? Statement` acts as an imperative form
  in rule bodies; top-level `for` is **desugared to rules**. Code: Pratt
  supports `for` as an **expression** (`Expr::ForLoop`) with pattern text
  slicing and an optional `if` guard (`src/parser/expression/control_flow.rs`).
  Top-level desugaring is not present. **Action:** (a) add **top-level**
  desugaring pass (“convertStatement”) to emit rules; (b) once the plan
  introduces a **pattern tree**, stop storing the binding as string.

- **`{ expr }` as grouping**
  Spec doesn’t define braces as an **expression grouping** (braces are for map
  literals or blocks in statements). Code: the parser accepts `{ expr }` as an
  **alternate grouping** (`parse_brace_group`). **Deviation:** extra surface
  form not in spec. **Action:** either remove it (strict conformance), or
  **bless it in the spec** as a tolerated extension and document
  precedence/ambiguities.

### 4) Collections and desugarings

- **Vector/Map literals + lowering**
  Spec: `[a, b, c]` and `{k:v,…}` with deterministic **lowerings** to builder
  calls (capacity/insert). Code: no collection literal parsing in the Pratt
  layer today. **Action:** add collection literal productions and (if the
  implementation keeps “early transforms in parser”) build the lowered form
  immediately or tag nodes for a tiny post-parse lowering pass.

- **`group_by` extraction & legacy `Aggregate`**
  Spec: at most **one** `group_by(project, key)` per right-hand-side
  expression; extracted to a dedicated abstract syntax tree node; legacy
  `Aggregate` lowered and warned as deprecated. Code: no `group_by`/`Aggregate`
  handling found beyond roadmap docs. **Action:** extend right-hand-side
  parsing to detect/extract `group_by`; add errors for multiple/wrong-arity;
  keep the `RHSGroupBy` node wired into checks later.

### 5) Rules, heads, and adornments

- **Multi-head rules and `@` location**
  Spec: multiple heads allowed (`Head, Head :- Body.`); `@ Expr` allowed **in
  heads only**; each head may carry delay `-<N>` and diff `'`. Code: rule
  Abstract syntax tree tests exist, but tests for **multi-head**, head
  **locations**, **delay**, or **diff mark** are absent. Token kinds exist
  (`T_AT`, delay/diff tokens at the lexer level), but parsing/validation isn’t
  visible. **Action:** add grammar for `RuleLHS` = `Atom Location?`,
  comma-separated; support `@` with head-only validation; parse `-<N>` and `'`
  adorners with range checks (u32 for delay).

- **Head by-ref lowering (`&Rel{…}` → `ref_new`)**
  Spec: `&Rel{…}` **in heads** lowers to `ref_new(Rel{…})`; in expressions,
  `&expr` remains a by-ref expression node. Code: no sign of this lowering.
  **Action:** add head-context check and rewrite before abstract syntax tree
  finalization.

- **Assignments in the right-hand side / flat-map binds**
  Spec allows **assignment-like binds** (pattern `=` expression) as distinct
  right-hand-side terms. Code: right-hand-side binder forms aren’t visible yet.
  **Action:** add term kinds for `RHSAssign` and validate pattern contexts.

### 6) Items (top-level constructs)

- **Transformer extern-only**
  Spec: `transformer` must be **extern**; non-extern rejected; attributes
  allowed only on `type`, `function`, and `relation`. Code: abstract syntax
  tree modules for transformers exist, but there aren’t tests asserting
  **non-extern = error** or **attribute placement** rules. **Action:**
  implement/validate these guards and add tests (e.g., attribute on `index`
  should error per spec).

- **Index declaration shape**
  Spec: `index Name (field: Type, …) on Atom;` (with full Atom grammar — may
  include delay/diff/ref/constructor forms). Code: index parsing exists and
  enforces `on`/parentheses balancing (see `src/parser/tests/indexes.rs`).
  **Gap:** **`on` Atom** variants (delayed/diff/ref/constructor) are missing.
  **Action:** extend the `on` target to parse full **Atom** and add tests that
  combine the adornments.

- **`apply` items**
  Spec includes an `Apply` top-level form. Code: no `apply` parsing/tests are
  present. **Action:** add `Apply` item parser plus abstract syntax tree
  coverage and a couple of fixtures.

### 7) Name & scoping rules

- **Uniqueness/overload rules**
  Spec: unique names for type/relation/index/transformer/import; function
  groups overloaded by **arity** only; reserved words not allowed as
  identifiers; attributes only in certain places. Code: tokenizer and abstract
  syntax tree structure are present, but enforcement tests for **duplicate
  definitions**, **function (name, arity)** uniqueness, or attribute placement
  beyond the function/param span tests are absent. **Action:** wire post-parse
  validation for uniqueness and produce **spanned diagnostics** per spec.

## Noteworthy *implementation-only* extensions (consider updating the spec)

- **`{ expr }` grouping** in expressions (`parse_brace_group`) – tidy, often
  ergonomic, but **unspecified**. Decide whether to keep and bless it.
- **Structural literal “guard”** (blocking `Name { … }` in certain contexts
  like `if` condition to avoid misparses). – pragmatic and well-documented in
  code/tests; add a line to the spec’s “portability notes” if that behaviour
  remains.

## Quick pointer map (where to change things)

- **Tokenizer & language tokens:** `src/tokenizer.rs`, `src/language.rs`
  (add raw/interpolated/interned string tokens, width-numeric tokens, ensure
  `=>`, `^`, `++`, delay/diff are emitted cleanly)
- **Pratt expression parser:** `src/parser/expression/*`

  - `literals.rs` (string families, numerics)
  - `infix.rs` + `ast/precedence.rs` (wire missing operators)
  - `data_structures.rs` (vector/map literals; keep struct literal guard)
- `control_flow.rs` & `pattern_collection.rs` (switch to a **pattern tree**
    rather than string slices)
- **Rules & items:** `src/parser/ast/*` and `src/parser/tests/*`
  (multi-head parsing, `@` location, delay/diff on heads, `&Rel{…}` lowering,
  `apply`, extern-only transformers)
- **Validation passes:** likely a **post-parse** module (or extend existing
  `ast/parse_utils`) for name uniqueness, attribute placement, and
  numerical/delay range checks.

## Suggested test additions (bite-sized)

- Operator precedence tables for `=>`, `^`, `++`, plus mixed cases
  (`a ++ b ^ c => d and e`).
- All string forms (incl. rejection of interpolated strings in patterns).
- Width-numeric acceptance and range rejections, and float width enforcement.
- `index … on <Atom-with-adornments>` matrix.
- Multi-head rules and head-only `@`, `-<N>`, `'` validation.
- `&Rel{…}` in **head** lowers; `&expr` in **body** remains unary operator.
- `group_by` 0/1/2+ cases (success, wrong arity, duplicate).
- Attribute placement errors and non-extern transformer error.
- `apply` item basic acceptance.

## Priority fix plan

1. **Strings & numbers** (token kinds + literal parsing + tests)
2. **Operator table completion** (`=>`, `^`, `++`)
3. **Pattern parsing** (introduce a proper pattern tree; switch match/for)
4. **Rules adornments** (multi-head, `@`, delay/diff, `&Rel` lowering)
5. **Collections + desugarings** (vector/map)
6. **`group_by`/`Aggregate` extraction**
7. **Top-level `for` desugaring** + `apply` item, extern-only `transformer`
   check
8. **Name/attribute validators** (uniqueness, placement, reserved words)

That ordering keeps risk low and unlocks the most downstream value (lints on
real programs) the fastest.
