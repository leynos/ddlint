# Parser design documentation analysis

## Scope and method

This analysis covers the following documents:

- `docs/parser-gap-analysis.md`
- `docs/differential-datalog-parser-syntax-spec-updated.md`
- `docs/differential-datalog-parser-syntax-spec-migration-plan.md`
- `docs/pratt-parser-for-ddlog-expressions.md`
- `docs/function-parsing-design.md`
- `docs/parser-plan.md`

Code was inspected to resolve document contradictions against the current
implementation, especially in:

- `src/parser/expression/*`
- `src/parser/ast/*`
- `src/parser/span_scanners/*`
- `src/parser/mod.rs`
- `src/tokenizer.rs`
- parser tests under `src/parser/tests/*` and `tests/*`

Total analysed documentation volume: 1,993 lines.

## Executive summary

- Overlap is high. Roughly 60-70% of content is duplicated, historical, or both.
- Contradictions are also high. Most are resolvable now by treating the code as
  source-of-truth for current behaviour.
- A smaller set of contradictions are true open design decisions; these should
  be scheduled in `docs/roadmap.md` before ADR-001 implementation planning.
- The six documents should be reduced to three active documents plus one
  historical archive to lower design overhead without losing guidance.

## 1) Overlap assessment

### Document-by-document overlap

| Document                                                         | Current role                                      | Overlap level                                                  | Recommended disposition                                                                             |
| ---------------------------------------------------------------- | ------------------------------------------------- | -------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| `docs/differential-datalog-parser-syntax-spec-updated.md`        | Normative syntax/spec reference                   | Medium (overlaps with Pratt and gap docs)                      | Keep as primary spec, but split clearly into "current behaviour" vs "target/compatibility" sections |
| `docs/parser-gap-analysis.md`                                    | Delta list between spec and implementation        | Very high (duplicates spec and roadmap, plus stale claims)     | Merge into a consolidated conformance register                                                      |
| `docs/differential-datalog-parser-syntax-spec-migration-plan.md` | Migration from Haskell-analysis-era docs          | Very high (much now superseded inside spec)                    | Merge decision log residue into conformance register; archive remainder                             |
| `docs/pratt-parser-for-ddlog-expressions.md`                     | Expression parser design and implementation notes | High (proposal and implementation mixed; duplicates spec/plan) | Keep only implementation notes; move proposal/history out                                           |
| `docs/function-parsing-design.md`                                | Parse-utils design notes                          | Low to medium                                                  | Merge into implementation notes as a dedicated section                                              |
| `docs/parser-plan.md`                                            | Initial Haskell-to-Rust porting plan              | Very high (mostly completed historical steps)                  | Archive as historical, retain only unresolved useful actions in roadmap                             |

### Primary overlap clusters

1. Expression grammar and operator precedence guidance appears in:
   `docs/differential-datalog-parser-syntax-spec-updated.md`,
   `docs/pratt-parser-for-ddlog-expressions.md`, and
   `docs/parser-gap-analysis.md`.
2. "What is still missing" guidance appears in both
   `docs/parser-gap-analysis.md` and
   `docs/differential-datalog-parser-syntax-spec-migration-plan.md`, but with
   different eras and assumptions.
3. Porting and implementation workflow guidance appears in both
   `docs/parser-plan.md` and `docs/pratt-parser-for-ddlog-expressions.md`, and
   is partly superseded by code and tests.

## 2) Contradictions resolvable via code inspection

The following contradictions can be removed now by aligning documents to the
implemented behaviour.

| Topic                                                                                    | Contradictory guidance                                                                                                      | Code reality                                                                                                                                                                                                                              | Action                                                                                 |
| ---------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| Qualified calls vs bare applications                                                     | `docs/parser-gap-analysis.md` says bare `foo()` is parsed as direct call (`Expr::Call`)                                     | Bare/non-qualified forms parse as `Expr::Apply`; only qualified lower-case terminal forms parse as `Expr::Call` (`src/parser/expression/pratt.rs:303`, `src/parser/expression/qualified.rs:8`)                                            | Remove outdated gap claim; keep one canonical statement in spec + implementation notes |
| String families and pattern interpolation                                                | `docs/parser-gap-analysis.md` reports missing raw/interpolated/interned support                                             | Tokenizer and AST string parser already support standard/raw/raw-interpolated and interned variants, with pattern interpolation rejection (`src/tokenizer.rs:31`, `src/parser/ast/string_literal.rs:87`, `src/parser/pattern/mod.rs:173`) | Remove stale gap text and keep behaviour in spec + tests references                    |
| Numeric literal widths/fit checks                                                        | `docs/parser-gap-analysis.md` reports opaque numbers as gap                                                                 | Structured numeric parsing with width/base/sign and fit checks is implemented (`src/parser/expression/numeric.rs:75`, `src/parser/ast/number.rs:31`)                                                                                      | Remove stale gap text                                                                  |
| Apply item support                                                                       | `docs/parser-gap-analysis.md` says `apply` parsing/tests absent                                                             | Apply scanner and AST wrapper are implemented and tested (`src/parser/span_scanners/apply.rs:15`, `src/parser/ast/apply.rs:15`, `src/parser/tests/apply.rs:1`)                                                                            | Remove stale gap text                                                                  |
| Transformer extern-only validation                                                       | `docs/parser-gap-analysis.md` says extern-only guard coverage absent                                                        | Non-extern transformer diagnostics implemented and tested (`src/parser/span_scanners/transformers.rs:13`, `src/parser/tests/transformers.rs:135`)                                                                                         | Remove stale gap text                                                                  |
| Head by-reference lowering                                                               | `docs/parser-gap-analysis.md` says no `&Rel{...}` head lowering                                                             | Head-only lowering to `ref_new(...)` is implemented (`src/parser/ast/rule_head.rs:184`)                                                                                                                                                   | Remove stale gap text                                                                  |
| Migration-plan "spec missing host keywords / break-continue-return / parser entrypoints" | `docs/differential-datalog-parser-syntax-spec-migration-plan.md` treats these as missing from spec                          | These sections are now present in spec (`docs/differential-datalog-parser-syntax-spec-updated.md:86`, `docs/differential-datalog-parser-syntax-spec-updated.md:286`, `docs/differential-datalog-parser-syntax-spec-updated.md:35`)        | Archive migration-plan sections as completed                                           |
| Pratt design architecture                                                                | `docs/pratt-parser-for-ddlog-expressions.md` starts with a `chumsky::pratt` proposal, then later says bespoke Pratt is used | Actual parser is bespoke Pratt over token stream (`src/parser/expression/mod.rs:1`)                                                                                                                                                       | Split proposal/history from current implementation notes                               |

## 3) Contradictions that are true open design items

These are not just stale text; they represent unresolved contract decisions and
should be scheduled in `docs/roadmap.md`.

| Topic                                                                | Spec/guidance side                                                                                                                                        | Implementation side                                                                                                                                       | Why it matters for ADR-001                                                   |
| -------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| Top-level `for` desugaring                                           | Spec requires top-level `for` -> rules (`docs/differential-datalog-parser-syntax-spec-updated.md:379`) and roadmap marks complete (`docs/roadmap.md:175`) | Top-level rule scanning only triggers from `T_IDENT`, `T_IMPLIES`, `T_AMP`; no top-level `K_FOR` entry path (`src/parser/span_scanners/rules.rs:328`)     | Defines whether this is syntax-layer or sema-layer behaviour in split crates |
| Aggregation extraction stage                                         | Spec describes parser-stage `RHSGroupBy` extraction and parse-time errors (`docs/differential-datalog-parser-syntax-spec-updated.md:350`)                 | Aggregation classification lives in `Rule::body_terms()` helper; `parse()` does not run it globally (`src/parser/ast/rule.rs:97`, `src/parser/mod.rs:47`) | Needed to define stable parse vs semantic contracts                          |
| Collection literal lowering stage                                    | Spec says parser lowers vec/map literals (`docs/differential-datalog-parser-syntax-spec-updated.md:373`)                                                  | Parser emits `Expr::VecLit`/`Expr::MapLit` raw AST nodes (`src/parser/expression/data_structures.rs:82`)                                                  | Needed to place lowering responsibilities in crate split                     |
| Index declaration grammar                                            | Spec grammar expects typed index fields and `on Atom` (`docs/differential-datalog-parser-syntax-spec-updated.md:227`)                                     | Parser expects `index Name on Relation(columns)` shape (`src/parser/span_scanners/indexes.rs:25`)                                                         | Compiler-facing schema must be explicit before split                         |
| Transformer declaration grammar                                      | Spec grammar shows extern transformer with no outputs (`docs/differential-datalog-parser-syntax-spec-updated.md:199`)                                     | Parser requires output list after `:` (`src/parser/span_scanners/transformers.rs:30`)                                                                     | API and semantic model shape diverge otherwise                               |
| Relation forms and role/kind grammar                                 | Spec includes role/kind variants and bracket form (`docs/differential-datalog-parser-syntax-spec-updated.md:212`)                                         | Scanner handles `input relation`, `output relation`, and bare `relation` only (`src/parser/span_scanners/relations.rs:110`)                               | Affects semantic completeness and parser crate scope                         |
| Legacy token policy (`typedef`, `as`, legacy type names, `#`, `<=>`) | Spec marks several as rejected/compatibility managed (`docs/differential-datalog-parser-syntax-spec-updated.md:446`)                                      | Tokenizer still recognizes them and parser behaviour is mixed (`src/tokenizer.rs:119`)                                                                    | Must be formalized as compatibility policy pre-split                         |
| Brace-group extension `{ expr }`                                     | Gap analysis flags as design choice (`docs/parser-gap-analysis.md:97`)                                                                                    | Implemented and tested as extension (`src/parser/expression/data_structures.rs:97`)                                                                       | Must be either codified or removed to avoid semantic drift                   |

## 4) Unactioned design items to schedule in `docs/roadmap.md`

The following items remain valuable and should be added (or re-opened) as
explicit roadmap work before ADR-001 execution.

1. Re-open 2.5.4 as unfinished: top-level `for` contract.
   - Decide either:
     - implement top-level `for` desugaring in parser/sema; or
     - remove from normative spec and document as unsupported.
   - Acceptance: one behaviour contract, tests for valid/invalid top-level
     `for`, and spec/roadmap alignment.

2. Add task: aggregation extraction contract and stage boundary.
   - Decide whether aggregation errors are parse-stage or semantic-stage.
   - Acceptance: `parse()` or explicit semantic pass enforces arity and
     uniqueness consistently; docs reflect the chosen stage.

3. Add task: collection literal lowering boundary.
   - Decide whether vec/map lowering occurs in syntax layer, semantic layer, or
     later lowering layer.
   - Acceptance: one documented boundary with tests proving stable output shape.

4. Add task: index grammar alignment.
   - Choose one canonical grammar (`index ... on Relation(...)` vs typed-fields
     plus `on Atom`) and align parser + spec.
   - Acceptance: parser scanners, AST wrappers, tests, and spec examples match.

5. Add task: transformer grammar alignment.
   - Resolve colon-output signature vs semicolon-only signature conflict.
   - Acceptance: one grammar, parser/tests/spec all aligned.

6. Add task: relation form coverage decision.
   - Either implement spec-listed role/kind/bracket variants or mark them as
     deferred/unsupported in spec.
   - Acceptance: explicit support matrix in spec with tests.

7. Add task: legacy token compatibility policy completion.
   - Close outstanding decisions for `typedef`, `as`, legacy numeric names,
     `#`, and `<=>`.
   - Acceptance: deterministic diagnostics and explicit compatibility table.

8. Add task: brace-group extension decision.
   - Either codify `{ expr }` as supported extension in spec or remove with a
     targeted diagnostic and migration note.
   - Acceptance: behaviour + documentation + tests aligned.

## 5) Recommended document consolidation before ADR-001

### Recommended target structure (active docs)

1. Keep `docs/differential-datalog-parser-syntax-spec-updated.md` as the sole
   language/spec contract.
2. Create `docs/parser-implementation-notes.md` by merging:
   - implementation-only sections from
     `docs/pratt-parser-for-ddlog-expressions.md`;
   - all of `docs/function-parsing-design.md`;
   - the still-useful non-historical parts of `docs/parser-plan.md`.
3. Create `docs/parser-conformance-register.md` by merging:
   - open deltas from `docs/parser-gap-analysis.md`;
   - unresolved decision log items from
     `docs/differential-datalog-parser-syntax-spec-migration-plan.md`.

### Historical retention (non-active docs)

1. Archive `docs/parser-plan.md` and the superseded portions of
   `docs/differential-datalog-parser-syntax-spec-migration-plan.md` under
   `docs/archive/` with clear banners and links to active docs.

### Expected reduction

- From six active docs (1,993 lines) to three active docs plus archive.
- Active decision surface should shrink by roughly 45-55% while preserving all
  relevant guidance.

## 6) Actionable restructuring instructions

1. Declare a single source-of-truth policy in
   `docs/differential-datalog-parser-syntax-spec-updated.md`:
   - add a short "status" header with "current implementation" and
     "target/compatibility" subsections.

2. Build `docs/parser-conformance-register.md` with this fixed template:
   - topic;
   - current behaviour (with code reference);
   - spec/target behaviour;
   - decision status (`implemented`, `decision needed`, `scheduled`);
   - roadmap item link.

3. Build `docs/parser-implementation-notes.md` with only non-obvious
   implementation behaviour and invariants (for example: struct-literal guard,
   expression depth limits, delimiter/error handling, rule-body expression
   slicing).

4. Replace `docs/parser-gap-analysis.md` with a short redirect banner to
   `docs/parser-conformance-register.md`, preserving the original in archive.

5. Replace `docs/differential-datalog-parser-syntax-spec-migration-plan.md`
   with a short "completed migration" note and a link to the conformance
   register decision log; archive detailed historical text.

6. Replace `docs/pratt-parser-for-ddlog-expressions.md` with a concise pointer
   to `docs/parser-implementation-notes.md`; move proposal-era content to
   archive.

7. Update `docs/contents.md` so active parser documentation lists only:
   - syntax spec;
   - parser implementation notes;
   - parser conformance register;
   - roadmap;
   - archived history links.

8. Add or re-open roadmap tasks listed in section 4 so unresolved parser design
   choices are explicitly scheduled before ADR-001 execution planning.

## 7) Constraint check

This restructuring plan adheres to requested constraints:

- No relevant design guidance is discarded; duplicated/historical guidance is
  consolidated or archived with pointers.
- ADR-001 implementation work is not performed here.
- Documentation volume and cognitive overhead are reduced while preserving all
  currently valuable parser design guidance.
