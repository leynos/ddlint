# Architectural decision record (ADR) 0006: block expressions and collection syntax

## Status

Accepted on 17 November 2026. Braces are normative block expressions, bracket
map literals replace brace map literals, and a scoped parser state keeps `;`
from escaping a block item.

## Date

2026-11-17.

## Context and problem statement

ddlint parses expressions with a handwritten Pratt parser that builds its own
abstract syntax tree (AST) from the lossless concrete syntax tree (CST). Two
surface forms had drifted from upstream Differential Datalog (DDlog):

- Braces `{ … }` were parsed as a ddlint extension: either an empty map
  literal, a single-expression group, or a colon-delimited map literal
  (`docs/parser-conformance-register.md` item 15, roadmap item 2.6.8).
- Map literals used the brace form `{k: v, …}` and vector literals accepted the
  empty form `[]`, neither of which upstream accepts.

Upstream, braces are a core expression term: `Parse.hs` parses `braces eseq` in
`term`, and `eseq` returns its single item unchanged, yields `ETuple []` when
empty, and folds `SEQ` for a semicolon-separated sequence. Map literals are
`[k -> v, …]` through `emap_literal`, and both `emap_literal` and
`evec_literal` require at least one element (`commaSepEnd1`). Braced `if`,
`else`, `for`, closure, match-arm, and function bodies are all block
expressions upstream. Evidence was read from
`vmware-archive/differential-datalog` at commit
`ca5ded823dc2c01bc33aac44e526c12d2fed03a6`
(`src/Language/DifferentialDatalog/Parse.hs`).

The syntax specification's source-of-truth policy (`SPEC-POLICY`, section 11 of
`docs/differential-datalog-parser-syntax-spec-updated.md`) requires the
normative contract and the implementation to agree. The compatibility-policy
bullet (`SPEC-13-COMPAT`) forbids silently accepting constructs that are not in
the specification. Keeping either ddlint extension therefore requires an
explicit specification entry; the user directed fidelity to upstream instead.

The specification records the compatibility decision: ddlint is unreleased and
owes no compatibility with its earlier behaviour.

## Decision drivers

- A linter must distinguish braces from parentheses; `{ e }` and `( e )` have
  different upstream meanings even though both collapse to `e`.
- The parser is a pure core. The decision must not add input/output (I/O),
  logging, or global state.
- Block structure must be observable in the AST so the semantic layer, lint
  rules, and tests can reason about statements and their value.
- The `;` separator must not be swallowed by an `if`, `for`, `return`, or
  closure tail that restarts at binding power 0.
- Fidelity is bounded: every other upstream divergence found during this work
  is recorded and scheduled rather than changed here.
- No new dependency may be introduced.

## Requirements

### Functional requirements

- `{ x }`, `{ a; b }`, `{ a; }`, `{}`, and
  `{ if c { a } else { b }; d }` parse as blocks with explicit statements and
  value.
- `[k1 -> v1, k2 -> v2]` parses as a map literal; braces never form a map, so
  `{a: 1, b: 2}` is an ordinary malformed block.
- `[]` is rejected, naming `vec_empty()` and `map_empty()`.
- Parenthesized forms are unchanged: `( e )` is a group, `()` is the empty
  tuple, and `(a, b)` is a tuple.

### Technical requirements

- No parse outside braces and bracket literals changes.
- No operator's binding power or associativity changes.
- New production code does not panic.
- The block and collection syntax is defined in the specification as
  normative grammar.

## Options considered

### Option A: keep braces as a ddlint extension

Retain brace groups and brace map literals, and document them as an intentional
superset.

Rejected. It contradicts the user's fidelity direction, keeps two spellings for
a map, and leaves `{ if c { a } else { b }; d }` unrepresentable as a sequence
of items, because the `if` tail consumes the separator.

### Option B: reuse `Expr::Group` for braces

Map `{ e }` to the existing `Expr::Group` node and add a statement list
elsewhere.

Rejected. A group is the parenthesized form and its meaning is fixed.
Overloading it would make braces and parentheses indistinguishable to every
consumer and would force a second node anyway for multi-item blocks.

### Option C: a block of `Seq` binary nodes

Represent `{ a; b }` as `Expr::Binary { op: Seq, … }` inside a wrapper.

Rejected. `Seq` is an operator with binding power 0, so its right-hand side
already swallows following constructs unless a special state is present. A flat
`Seq` chain also cannot express a trailing semicolon, an empty block, or the
distinction between statements and the block's value.

### Option D: a flat item list with a trailing-semicolon flag

Store `Vec<Expr>` plus `ends_with_semicolon: bool`.

Rejected. It makes an invalid value representable (`([], true)`) and forces
every consumer to recompute the value from the flag. The chosen representation
makes every value valid by construction.

### Option E: `Expr::Block(BlockExpr)` with statements and an optional value

Split the items into those followed by `;` and an optional final value.

Chosen. Every `(statements, value)` pair is a valid block, no invariant is
exposed, and `items()` reconstructs source order exactly.

| Topic                      | Option B (Group) | Option C (Seq) | Option D (flag) | Option E (BlockExpr) |
| -------------------------- | ---------------- | -------------- | --------------- | -------------------- |
| Braces distinct from `()`  | no               | yes            | yes             | yes                  |
| Empty block representable  | no               | no             | invalid states  | yes                  |
| Trailing `;` representable | no               | no             | yes             | yes                  |
| Consumer traversal cost    | n/a              | walk `Seq`     | flag arithmetic | `items()` iterator   |

_Table 1: Comparison of block representation options._

## Decision outcome / proposed direction

The following decisions are adopted, and are referred to as `BG-Dn` in
`docs/execplans/2-6-8-decide-brace-group-extension-policy.md`.

- `BG-D1` (codify): `{ … }` is a normative block expression.
- `BG-D2` (distinct node): a block is `Expr::Block(BlockExpr)` with public
  fields `statements: Vec<Expr>` (items followed by `;`) and
  `value: Option<Box<Expr>>` (the final item when not followed by `;`; `None`
  means unit, `()`). `Expr::Group` is reserved for parentheses. Every
  `(statements, value)` pair is a valid block, so the public fields expose no
  invariant.
- `BG-D3` (separator): inside braces `;` is a separator handled by the block
  parser, not the `BinaryOp::Seq` operator. A trailing `;` is accepted; an
  empty item (`{ ; }`, `{ a;; b }`) is rejected.
- `BG-D4` (empty braces): `{}` is the empty block, whose value is unit.
- `BG-D5` (braces always open a block): there are no brace map literals. Items
  after `{` are ordinary expressions, so `{ x: T }` is a block whose value is
  the ascription `x: T`, and the colon-terminating key mode is deleted.
- `BG-D6` (struct-literal guard): in an `if` condition or a `for` guard, an
  identifier followed by `{` is not a struct literal. `ident {}` is reported
  only when another `{` follows, so `if ready {} else { go() }` parses. The
  `{ ident :` heuristic is kept while ddlint's struct syntax is unchanged.
- `BG-D11` (`;` terminates tails): while block items are parsed, a scoped
  parser state makes the infix loop stop at `;`. Items are parsed at
  `BLOCK_ITEM_MIN_BP` (one above `;`). The state is kept through the `if`
  condition and both branches, the `for` body, the `return` value, and the
  pipe-closure body, and cleared and restored at every nested delimiter:
  parentheses, call arguments, bit slices, vector and map literals, nested
  blocks, struct fields, the `match` scrutinee and arms, and the `for` iterable.
- `BG-D12` (upstream map literals): `[k -> v, …]` is a map literal producing
  `Expr::MapLit`, with at least one entry and an optional trailing comma. `->`
  is neither an infix nor a postfix operator, so the first element's parse
  stops at it, reproducing upstream's `[ expr ->` lookahead without
  backtracking. A literal mixing elements and entries is rejected. Upstream
  vector literals also need at least one element, so `[]` is rejected with
  `EMPTY_VECTOR_LITERAL_ERROR`; upstream's standard library provides
  `vec_empty()` and `map_empty()`.
- `BG-D14` (types): the specification's `MapType ::= '{' Type ':' Type '}'` and
  `VecType ::= '[' Type ']'` are replaced by upstream's library types
  `Map<K, V>` and `Vec<T>`. ddlint stores type text, so this changes only the
  specification.

### Architectural rationale

`BlockExpr` carries no flag and no derived count. `from_items(items, t)` is
total except for `([], true)`, which cannot arise from source and returns
`None`; `items()` and `into_items()` are the only traversal order; and
`ends_with_semicolon()` is derived from the shape. The representation is
therefore a bijection with the abstract block model, which removes a class of
consumer bugs rather than relying on documentation.

`classify_block_follow` is a pure, total function over the position and the
next token. It keeps the block loop's control flow in one enumerated place, so
the parser's error messages and recovery decisions do not scatter through the
item loop.

The `;`-terminates state mirrors the existing struct-literal state: one counter
on the parser, a suspending wrapper for tails, and a clearing wrapper for
delimiters. Reusing that pattern keeps the parser's context discipline uniform
and makes the clearing points auditable against `BG-D11`.

## Goals and non-goals

Goals: outcomes 1 and 2 above, plus the specification and register entries that
close register items 10 and 15.

Non-goals, each recorded in `docs/parser-conformance-register.md` and scheduled
in `docs/roadmap.md`:

- `BG-D7`: no new `DiagnosticCode`; messages remain constants in
  `src/parser/error_messages.rs` until roadmap item 2.8.1.
- `BG-D8`, `BG-D9`: rule-level `for` forms and `;` sequencing outside braces
  (roadmap item 2.6.10, register item 16).
- `BG-D15`: upstream struct-literal and struct-pattern syntax (roadmap item
  2.6.11, register item 17).
- `BG-D16`: the expression forms upstream scoping needs (roadmap item 2.6.12,
  register item 18).
- `BG-D17`: function bodies (roadmap item 2.6.13, register item 19).
- `BG-D18`: `@` bindings, `in` atom binders, `?`, `e[h:l]` slices, and adjacent
  string concatenation (roadmap item 2.6.14, register item 20).
- `BG-D19`: upstream type syntax (roadmap item 2.6.15, register item 22).
- `LS-D0`: the upstream lexical-scope model and its proof (roadmap items 3.3.5
  and 3.3.6, register item 21).

## Consequences

- `Expr` gains a variant. Under ADR-001, expression AST ownership moves at
  roadmap item 2.7.1, and later variants need joint approval; this variant is
  added before the split.
- `sema` matches `Expr` exhaustively, relying on `Expr` not being
  `#[non_exhaustive]`. New variants must add arms there.
- `()` and `{}` are distinct nodes (`Tuple(vec![])` versus an empty block) with
  the same value, matching upstream's `eTuple []` versus an empty `eseq`.
- The block representation lemma is covered by a three-row `rstest` table and a
  `proptest` round trip rather than a Verus proof; the user approved that
  substitution.
- Removing brace maps rewrites existing ddlint test cases. Because ddlint is
  unreleased, no migration diagnostic, migration guide, or deprecation window
  is added.

## Known risks and limitations

- Ascription types are still parsed as expressions, so `{a: 1}` parses as a
  block whose value is a nonsensical ascription. This is pre-existing and is
  recorded in register item 18, to be fixed with the type grammar.
- The struct-literal guard's `{ ident :` heuristic wrongly rejects the valid
  `if ready { x: T }`. It is retained so struct syntax keeps parsing, and is
  recorded as a known false positive in register item 17.
- If `BG-D11` misses a tail or a delimiter, a separator is swallowed. Tables
  and generated cases cover every named tail and delimiter, and seeded faults
  prove the tests reject a missing propagation.

## Alternatives considered

- **Remove braces entirely.** Not upstream: upstream has block expressions.
- **Keep brace maps alongside bracket maps.** Two spellings for one form, and
  the colon form is not upstream.
- **Accept `[]` as a ddlint extension.** The user directed upstream fidelity;
  `vec_empty()` and `map_empty()` are the upstream spellings.
