# Differential Datalog parser and syntax specification (updated)

**Audience:** Implementing engineers porting or extending the frontend;
linter/IDE authors.

**Scope:** A precise, self‑contained reference for the concrete syntax, lexical
rules, operator table, and early semantic transformations (aka “desugarings”).
This document resolves prior ambiguities and defines the intended parser
contract, including validation/error cases.

## Status and source-of-truth policy

### Current implementation contract

- Behaviour implemented in code and tests is tracked in
  `docs/parser-conformance-register.md`.
- `docs/parser-implementation-notes.md` documents non-normative implementation
  details and invariants.

### Target and compatibility contract

- This document remains the normative syntax contract.
- If the current implementation diverges from this spec, record the delta in
  `docs/parser-conformance-register.md` and schedule resolution in
  `docs/roadmap.md`.
- Do not silently update either spec or code in isolation; keep both aligned
  through an explicit conformance change.

______________________________________________________________________

## 1. Overview

A Differential Datalog (DDlog) program consists of imports, type definitions,
functions, (extern) transformers, relation declarations, index declarations,
rules, and applies. The parser constructs a `DatalogProgram` from these
elements, performs a limited set of tree rewrites (e.g., `group_by` extraction,
legacy `Aggregate` lowering, literal lowering to builder calls), enforces
name‑uniqueness invariants, and records source provenance.

### 1.1 Entry points and products

- **Input:** a UTF‑8 source string (tabs are treated as spaces for positioning).
- **Output:** a `DatalogProgram` comprising:
  - `imports`, `typedefs`, `functions` (grouped by name; overload by arity
    permitted), `transformers`, `relations`, `indexes`, `rules`, `applys`.
- **Transformations performed pre‑Abstract Syntax Tree (AST):** `group_by` and
  `Aggregate` lowering; map/vector literal lowering; `&` in rule heads →
  `ref_new`.

#### 1.1.1 Expression‑only parsing

- Implementations may also expose an expression‑only parser entry point that
  accepts a single expression and returns an `Expr` AST. This entry point uses
  the same lexer and operator table as full‑program parsing and is intended for
  tests and interactive tooling.

______________________________________________________________________

## 2. Lexical structure

### 2.1 Whitespace and comments

- Whitespace separates tokens; newlines are not syntactically significant.
- `//` to end‑of‑line and `/* … */` block comments are supported; nested block
  comments are **not** required.
- Tabs are normalized to single spaces for position tracking.

### 2.2 Identifiers and case classes

- **Scoped identifiers** use Rust‑like `module::path::Name`.
- **Case constraints on final segment:**
  - **`UcScopedIdentifier`**: `^[A-Z][A-Za-z0-9_]*$` (constructors, relations,
    transformers, types that begin with capital).
  - **`LcScopedIdentifier`**: `^(_|[a-z])[A-Za-z0-9_]*$` (variables, fields,
    local functions if unqualified).
  - **`LcGlobalIdentifier`**: as above but **must** include at least one `::`
    segment (that is, fully qualified).

**Resolution rule:** Only **fully qualified** `module::func` names parse as
function calls at parse time. A bare `name(…)` parses as a variable application
and is disambiguated later during name resolution.

### 2.3 Reserved words and symbols

The following **keywords** and **reserved operators** cannot be used as
identifiers (final list should be kept 1:1 with the lexer):

- **Keywords:** `type`, `function`, `extern`, `transformer`, `input`, `output`,
  `internal`,`relation`,`stream`,`multiset`,`index`,`on`,`primary`,`key`,`apply`,`match`,`if`,`else`,`for`,`in`,`then`,`skip`,`true`,`false`,`var`,`mut`,`return`,`break`,`continue`.
- **Special tokens:** `@`, `:-`, `,`, `;`, `:`, `::`, `.`, `&`, `'` (diff
  marker), `-<` (delay introducer), `=>` (implies), brackets and braces
  `()[]{}`.

Reserved but not part of the grammar: `#`, `<=>`. Implementations must reject
their use with a clear diagnostic.

#### 2.3.1 Host‑language keyword reservation

In addition to DDlog keywords and operators above, implementations must reserve
Rust keywords as identifiers to avoid conflicts with code generation and
embedding. The complete reserved set is defined in the lexer; this spec
requires that using any host keyword as an identifier be rejected with a
diagnostic. Keep the lexer table as the source of truth.

______________________________________________________________________

## 3. Literals

### 3.1 Numeric literals

- **Integers with explicit width:** `'<w><base>digits` where `<w>` is a
  positive decimal width (bits), `<base>` is one of `b` (bin), `o` (oct), `d`
  (dec), `h` (hex). Signed variants use `sb`, `so`, `sd`, `sh`. Examples:
  `8'hFF`, `16'sd-1`, `1'b0`.
- **Floats with explicit width:** decimal floats suffixed with `'f32` or
  `'f64`. Only 32‑ and 64‑bit are supported.
- **Validation:** width > 0; for integers, value must fit width; for floats,
  only 32/64 allowed.

### 3.2 String literals and interning

Three families are supported:

1. **Standard strings**: `"…"` with backslash escapes and **`${expr}`
   interpolation**.
2. **Raw strings**: `[| … |]` (no escapes; no interpolation).
3. **Interpolated raw strings**: `$[| … ${expr} … |]`.

Prefixing any string form with `i` yields an **interned string**, desugared to
`intern("…")` (or `intern([|…|])`).

**Pattern restriction:** only constant (non‑interpolated) strings may appear in
**patterns**.

### 3.3 Collections

- **Vector literals:** `[e1, e2, …]` desugar to a builder sequence
  `vec_with_capacity(n); push(e)…`.
- **Map literals:** `{k1: v1, k2: v2, …}` desugar to
  `map_empty(); insert(k, v)…`.

______________________________________________________________________

## 4. Operator precedence and associativity

Higher rows bind tighter. All binary operators are left‑associative unless
noted.

| Precedence | Operators / Forms                                      | Associativity |
| ---------- | ------------------------------------------------------ | ------------- |
| 14         | Postfix call `f(…)`, index `e[expr]`, field `e.name`   | left          |
| 13         | Unary prefix: `- e`, `! e`, `~ e`, `& e`               | right         |
| 12         | `*`, `/`, `%`                                          | left          |
| 11         | `+`, `-`, `++` (concatenation)                         | left          |
| 10         | Shifts `<<`, `>>`                                      | left          |
| 9          | Bitwise `&`                                            | left          |
| 8          | Bitwise `^`                                            | left          |
| 7          | Bitwise or (`&#124;`)                                  | left          |
| 6          | Comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`          | non‑assoc     |
| 5          | Logical `and`                                          | left          |
| 4          | Logical `or`                                           | left          |
| 3          | Implication `=>`                                       | right         |
| 2          | Conditional expression `if … then … else …`            | n/a           |
| 1          | Assignment forms inside statements (see §6)            | right         |

**Note:** `++` (concatenation) and `^` (bit‑xor) are part of the operator table
and are recognized as operators. `&` in row 13 is expression-only; head
semantics are described in §7.3.

______________________________________________________________________

## 5. Grammar (mini‑Extended Backus–Naur Form (EBNF))

Notation: `A?` optional, `A*` zero or more, `A+` one or more, alternatives with
`|`. Terminals in `'single quotes'`.

### 5.1 Program

```ebnf
Program       ::= Item*
Item          ::= Import | Typedef | Function | Transformer | RelationDecl
               | IndexDecl | Rule | Apply | Attributed(Item)
Attributed(X) ::= Attribute+ X
Attribute     ::= '#[' AttrBody ']'
```

**Attribute placement:** Attributes are permitted **only** on `Typedef`,
`Function`, and `RelationDecl`. Any other placement is an error.

### 5.2 Imports and types

```ebnf
Import   ::= 'import' ScopedPath ';'
Typedef  ::= 'type' UcName TypeParams? '=' Type ';'
Type     ::= UcName TypeArgs? | TupleType | MapType | VecType | Primitive

TupleType ::= '(' (Type (',' Type)*)? ')'
MapType   ::= '{' Type ':' Type '}'
VecType   ::= '[' Type ']'
Primitive ::= 'bool' | 'i8' | 'u8' | 'i16' | … | 'u128'
            | 'f32' | 'f64' | 'string' | 'interned'
```

### 5.3 Functions and closures

```ebnf
Function  ::= 'function' LcName '(' Params? ')' RetType? Block
Params    ::= Param (',' Param)*
Param     ::= Mut? LcName (':' Type)?
Mut       ::= 'mut'
RetType   ::= ':' Type

Closure   ::= 'function' '(' Params? ')' RetType? Block
            | '|' Params? '|' RetType? Expr   // pipe form, expression body
```

### 5.4 Transformers (extern‑only)

```ebnf
Transformer ::= 'extern' 'transformer' UcName '(' Params? ')' ';'
```

A non‑extern `transformer` is rejected.

### 5.5 Relations

Two equivalent forms:

```ebnf
RelationDecl ::= Role? Kind? UcName '(' Fields? ')' PrimaryKey? ';'
               | Role? Kind? UcName '[' Type ']' ';'
Role         ::= 'input' | 'output' | 'internal'
Kind         ::= 'relation' | 'stream' | 'multiset'
Fields       ::= Field (',' Field)*
Field        ::= LcName ':' Type
PrimaryKey   ::= '[' 'primary' 'key' '(' LcName ')' Expr ']'
```

Notes:

- The first form synthesizes a record type for `UcName` with the listed fields.
- `&UcName` in rule **heads** triggers special semantics (see §7.3).
- `Role`/`Kind` affect runtime semantics but not parse shape.

### 5.6 Indexes

```ebnf
IndexDecl ::= 'index' UcName '(' IndexFieldList ')' 'on' Atom ';'
IndexFieldList ::= IndexField (',' IndexField)*
IndexField     ::= LcName ':' Type
```

`on` targets an `Atom` (see §5.9) that may include delay/diff/ref/constructor
forms.

### 5.7 Apply items

```ebnf
Apply        ::= 'apply' UcName '(' ApplyArgs? ')' '->' '(' ApplyOutputs? ')'
ApplyArgs    ::= ApplyArg (',' ApplyArg)*
ApplyArg     ::= UcName | LcName
ApplyOutputs ::= UcName (',' UcName)*
```

Notes:

- Inputs may name relations (uppercase) or functions (lowercase).
- Trailing commas are permitted to mirror the legacy DDlog parser.

### 5.8 Rules and bodies

```ebnf
Rule    ::= RuleLHS (',' RuleLHS)* ':-' RuleRHS '.'
RuleLHS ::= Atom Location? Delay?
Location::= '@' Expr

RuleRHS ::= RhsTerm (',' RhsTerm)*
RhsTerm ::= Atom Delay? | Condition | Assignment | StatementExpr
```

- **Multiple heads:** `RuleLHS` is a comma‑separated, non‑empty list.
- **Location:** D3log‑style location is permitted in **heads** as `@ Expr`.
- **Delay and diff:** permitted on atoms in heads and bodies (see §5.9).

### 5.9 Atoms

Two core shapes (constructor and bracket form), with optional adornments:

```ebnf
Atom      ::= RefMark? UcOrLc DiffMark? '(' ArgList? ')'
            | RefMark? UcOrLc DiffMark? '[' ArgList? ']'
RefMark   ::= '&'
Delay     ::= '-<' Unsigned32 '>'
DiffMark  ::= "'"  // single quote (after the name, before the arguments)
ArgList   ::= Expr (',' Expr)*
UcOrLc    ::= UcName | LcName | ScopedPath
```

### 5.10 Statements and desugaring

Imperative forms are permitted inside rule bodies, and top-level statements may
be desugared during parsing as described below.

```ebnf
Statement       ::= ForStmt | IfStmt | MatchStmt | Skip | BreakStmt
                  | ContinueStmt | ReturnStmt | Block | ExprInStmt
ForStmt         ::= 'for' '(' Pattern 'in' Expr ')' IfGuard? Statement
IfGuard         ::= 'if' Expr
IfStmt          ::= 'if' '(' Expr ')' Statement ('else' Statement)?
MatchStmt       ::= 'match' '(' Expr ')' '{' MatchArm (',' MatchArm)* '}'
MatchArm        ::= Pattern '->' Statement
Skip            ::= 'skip'
Block           ::= '{' Statement* '}'
ExprInStmt      ::= LhsAssign '=' Expr   // see note below
LhsAssign       ::= Pattern | Expr       // implementation permits pattern LHS
```

**Top‑level `for`:** A top‑level `for` statement is **converted into one or
more rules** during parsing (desugaring). This affects scoping, and it governs
where variables may appear.

**Break/Continue/Return:**

- `break` and `continue` are permitted only inside loop bodies.
- `return` is permitted in function/closure bodies; it is not valid in rule
  bodies. Misuse must be reported with a clear diagnostic.

### 5.11 Conditions and assignments in the rule right-hand side (RHS)

`Condition` is any expression in a boolean context. The parser accepts
assignment‑like forms (e.g., pattern `=` expression) inside RHS to support
**flatmap**‑like binds; these are represented distinctly in the AST.

### 5.12 Patterns

Patterns appear in three contexts with the same surface syntax but different
AST nodes:

- **Match patterns** (in `match` arms).
- **FlatMap patterns** (LHS of `=` in RHS assignments).
- **Loop patterns** (in `for (pat in expr)`).

```ebnf
Pattern     ::= '_' | VarPat | TuplePat | StructPat | TypedPat | LiteralPat
VarPat      ::= 'var'? LcName
TypedPat    ::= Pattern ':' Type
TuplePat    ::= '(' (Pattern (',' Pattern)*)? ')'
StructPat   ::= UcName '{' FieldPatList? '}'
FieldPatList::= FieldPat (',' FieldPat)*
FieldPat    ::= LcName ':' Pattern
LiteralPat  ::= Integer | StringConst | Bool
```

Only **constant** strings may appear in `LiteralPat` (no interpolation).

### 5.13 Conditional expressions

```ebnf
ExprIf ::= 'if' Expr 'then' TermExpr 'else' TermExpr
```

Each arm parses as a **term** to avoid precedence pitfalls.

______________________________________________________________________

## 6. Early semantic transformations (desugarings)

### 6.1 `group_by` extraction

- At most **one** `group_by(project, key)` call is permitted **per RHS
  expression**.
- Arity must be exactly **two**.
- The parser extracts `group_by` into an explicit `RHSGroupBy` node bound to
  the magic variable `__group`. The remainder of the expression is re‑emitted
  as a condition/assignment referencing `__group`.
- Multiple `group_by` occurrences, or the wrong arity, are parse‑time errors.

### 6.2 Legacy `Aggregate(…)`

- The legacy `Aggregate` form is still recognized and lowered into `RHSGroupBy`
  - a follow‑up condition over `__group`.
- It is **deprecated**; future grammar may remove it. Emit a warning in linters.

### 6.3 Head by‑reference (`&Rel{…}`) → `ref_new`

- In a **rule head**, `&Rel{…}` (or bracket form) is rewritten to a call to
  **`ref_new(Rel{…})`**, so downstream stages can materialize reference values.
- In a **rule body** or expression context, `&expr` remains a standard
  **by‑reference** expression node.

### 6.4 Literal lowering for vectors/maps

- `[a, b, c]` →
  `let v = vec_with_capacity(3); v.push(a); v.push(b); v.push(c); v`.
- `{k: v, …}` → `let m = map_empty(); m.insert(k, v); …; m`.

### 6.5 Top‑level `for` desugaring

- A top-level statement in the form:

  ```ddlog
  for (pat in iterable if guard?) body.
  ```

  desugars into one semantic rule by collecting header terms left-to-right:

  ```ddlog
  body :- iterable, guard?.
  ```

- Nested top-level loops flatten in lexical order:

  ```ddlog
  for (a in A(a) if ready(a)) for (b in B(b)) Head(a, b).
  ```

  becomes:

  ```ddlog
  Head(a, b) :- A(a), ready(a), B(b).
  ```

- The terminal `body` must be atom-like (`Rel(args)` after parse-time call
  classification). Unsupported terminal statement forms (for example `if`,
  `match`, or sequencing) are rejected with a targeted diagnostic.
- Rule-body `for` loops remain represented as `Expr::ForLoop` (section 5.10)
  and are unaffected by top-level desugaring.

______________________________________________________________________

## 7. Semantics of rule adornments

### 7.1 Multiple heads

A rule may specify **one or more heads** separated by commas. Each head is an
`Atom` and may carry its own location, delay, and diff marks. Multiple heads
are equivalent to emitting multiple single‑head rules sharing the body.

### 7.2 Location in heads

`@ Expr` after a head denotes a **D3log‑style location** for the produced
fact/collection. The expression must type‑check to a location type at later
stages.

### 7.3 Delay and diff marks

- **Delay:** `-<N>` applies a non‑negative 32‑bit unsigned delay to the atom
  (and in heads may appear after an optional location). Values outside the
  inclusive range `0..=2^32-1` are rejected.
- **Diff mark:** a single quote after the atom name (e.g., `Rel'(…)`) marks a
  **difference** atom.

______________________________________________________________________

## 8. Name‑uniqueness and scoping rules

- **Unique definitions** (per program/module): each **type**, **relation**,
  **index**, **transformer**, and **module import** name must be defined at
  most once.
- **Functions:** may be grouped and **overloaded by arity**. Each
  `(name, arity)` pair must be unique.
- **Attributes:** valid only on `type`, `function`, and `relation`; elsewhere,
  emit an error.
- **Keywords:** identifiers cannot be any reserved keyword or operator token.

______________________________________________________________________

## 9. Errors and diagnostics (normative samples)

Implementations **must** reject programs exhibiting any of the following with a
clear diagnostic that includes a span:

- **Misplaced attribute:** an attribute preceding any item other than
  `type`, `function`, or `relation`.
- **Non‑extern transformer:** `transformer` lacking the `extern` qualifier.
- **Duplicate definition:** repeated name for
  type/relation/index/transformer/import, or function with the same
  `(name, arity)`.
- **`group_by` misuse:** more than one `group_by` in an expression or wrong
  arity (≠ 2).
- **String pattern interpolation:** an interpolated string in a pattern context.
- **Numeric width errors:** width ≤ 0; integer value does not fit width;
  floating‑point width not `32` or `64`.
- **Delay out of range:** delay not in `0..=2^32-1`.
- **Illegal placement of location:** `@` used outside a rule head.
- **Use of reserved words as identifiers.**

______________________________________________________________________

## 9.1 Legacy and compatibility tokens

Implementations may encounter historical tokens from older DDlog parsers. This
spec defines their treatment to aid migration:

- `Aggregate(…)`: accepted and lowered to `RHSGroupBy`; emit a deprecation
  diagnostic.
- `FlatMap`/`Inspect`: not language keywords; represent flatmap via RHS pattern
  binds instead. If used as keywords, reject with a targeted message.
- `typedef`: not supported; use `type` definitions. Emit an error with a fix
  hint.
- Legacy type names such as `bigint`, `bit`, `double`, `float`, `signed`:
  not in the grammar. Use sized integer types (`iN`/`uN`), and `f32`/`f64` for
  floating‑point.
- `as`: not a keyword in the updated grammar; reject its use as a keyword.

______________________________________________________________________

## 10. AST crosswalk (informative)

This section maps grammar constructs to representative AST node names to aid
porting and testing.

- **Program:** `DatalogProgram { imports, typedefs, functions, transformers,
  relations, indexes, rules, applys }`.
- **Typedef:** `TypeDef { name, params, body }`.
- **Function:** `FuncDef { name, params, ret, body }`, collated into
  `FuncGroup` by name.
- **Transformer:** `TransformerDef { extern: true, name, params }`.
- **Apply:** `Apply { transformer, inputs, outputs }`.
- **RelationDecl:** `Relation { role, kind, name, typeOrFields, primaryKey? }`.
- **IndexDecl:** `Index { name, fields: [(name, type)], on: Atom }`.
- **Rule:** `Rule { heads: [RuleLHS], body: [RhsTerm] }`.
- **RuleLHS:** `RuleLHS { atom: Atom, location?: Expr }`.
- **Atom:** `Atom { ref?, delay?, diff?, name, args, bracketForm? }`.
- **RhsTerm:** `RHSAtom`, `RHSCond`, `RHSAssign`, `RHSGroupBy`.
- **Statement:** `SFor`, `SIf`, `SMatch`, `SSkip`, `SBlock`, `SExpr` (with
  top-level `SFor` lowered to rules).
- **Pattern:** `PVar`, `PTuple`, `PStruct`, `PTyped`, `PLit`, `PWildcard`.
- **Expr:** `ECall`, `EIndex`, `EField`, `EUnary`, `EBinary`, `EIf`, `EMapLit`,
  `EVecLit`, `ERef`, `EFunc` (fully qualified only).

______________________________________________________________________

## 11. Worked examples

### 11.1 Multi‑head with location, delay, and diff

```ddlog
// Two heads; first has a delay and location; second is a diff fact
A(a)@site(x) -<10>, B'(b) :-
    for (t in tasks()) if ready(t) {
        x = site_of(t),
        a = project_a(t),
        b = project_b(t)
    }.
```

Equivalent to two single‑head rules sharing the RHS.

### 11.2 `group_by` extraction

```ddlog
Totals(u, total) :-
    orders(u, amt),
    group_by(sum(amt), u),
    total = __group.
```

Only one `group_by` is allowed in the expression; the parser extracts it to
`RHSGroupBy(project=sum(amt), key=u)` and introduces `__group`.

### 11.3 Head by‑ref

```ddlog
&RefOrder{ id, amt } :- incoming_order(id, amt).
```

Lowered to:

```ddlog
ref_new(RefOrder{ id, amt }) :- incoming_order(id, amt).
```

### 11.4 Strings and interning

```ddlog
let s1 = "hello ${name}";
let s2 = [| raw string with ${no_interp} |];
let k  = i"USER_${id}";        // interned
let r  = i[| bucket_${n} |];    // interned raw with interpolation
```

### 11.5 Numbers with widths

```ddlog
let z  = 8'hFF;      // 255 fits width 8
let m1 = 16'sd-1;    // signed decimal width 16
let f  = 3.14159'f64; // permitted widths: 32 or 64
```

### 11.6 Collections

```ddlog
let v = [a, b, c];   // lowered to capacity+push sequence
let m = {k1: v1, k2: v2}; // lowered to empty+insert sequence
```

### 11.7 Index

```ddlog
index OrdersByUser (user: UserId, ts: Timestamp) on Orders[user, ts, amt].
```

______________________________________________________________________

## 12. Validation examples (should fail)

- **Multiple `group_by`:**

```ddlog
X(x) :- group_by(sum(x), k), group_by(count(x), k). // error: multiple group_by
```

- **Wrong arity:**

```ddlog
X(x) :- group_by(sum(x)). // error: expected 2 arguments
```

- **Non‑extern transformer:**

```ddlog
transformer Foo(); // error: transformer declarations must be extern
```

- **Attribute on index:**

```ddlog
#[cold]
index Ix(a: T) on A[a]. // error: attributes not permitted here
```

- **Delay out of range:**

```ddlog
A() -<4294967296> :- B(). // error: delay must fit u32
```

- **Interpolated string in pattern:**

```ddlog
match s { i"X_${n}" -> skip } // error: patterns require constant strings
```

______________________________________________________________________

## 13. Portability notes for implementers

- **Operator table:** adopt the table in §4 verbatim; divergence between lexer
  reserved operators and the table causes hard‑to‑diagnose parse drift.
- **Function resolution:** treat unqualified calls as variables until name
  resolution; only `module::func` is parsed as a function call token.
- **Desugaring boundaries:** keep `group_by` and `Aggregate` lowering in the
  parser (or immediately after), so later phases can assume a uniform
  representation (`RHSGroupBy`).
- **Tabs and positions:** perform any tab normalization before lexing and
  preserve a mapping for accurate diagnostics.
- **Compatibility policy:** legacy constructs accepted for compatibility must
  either be lowered to the canonical AST (e.g., `Aggregate`) or rejected with a
  precise diagnostic and fix hint. Implementations MUST NOT silently accept
  constructs that are not part of this specification.
- **Multiple heads:** either preserve the multi‑head form in the AST or expand
  into multiple single‑head rules consistently; this spec treats multi‑head as
  syntactic sugar for multiple rules with a shared body.

______________________________________________________________________

## 14. Change log (relative to previous draft)

- Corrected rule operator from `::-` to `:-`.
- Added authoritative operator table including `++` and `^` with precedence and
  associativity.
- Clarified attribute placement (types, functions, relations only).
- Specified extern‑only transformers.
- Fully specified index declaration shape
  (`index Name(field: Type, …) on Atom`).
- Documented both relation declaration forms and roles/kinds.
- Added explicit multi‑head LHS and location `@ Expr` semantics.
- Introduced formal grammar for statements, patterns, and atoms.
- Documented desugarings (`group_by`, legacy `Aggregate`, by‑ref head, literal
  lowering, top‑level `for`).
- Added numeric literal and string/interning details, including pattern
  restrictions.
- Added name‑uniqueness and scoping rules; clarified function vs variable
  parsing.
- Added validation/error section with concrete examples.
- Added AST crosswalk to guide ports and test fixture authorship.

______________________________________________________________________

*End of specification.*
