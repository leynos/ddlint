# Migration plan for the Differential Datalog parser spec

This document records the gaps between the historical Haskell parser analysis
and the updated, normative DDlog syntax specification, then sets out an
implementation plan to migrate any still‑relevant information into the spec, so
the Haskell analysis can be fully deprecated.

Terminology: references to the rule right-hand side (RHS) denote the rule body;
the left-hand side (LHS) denotes the head.

______________________________________________________________________

## Executive summary

The updated specification supersedes most grammar and precedence details and
fixes prior ambiguities (e.g., `:-` vs `::-`). However, several pieces of
useful information present in the Haskell analysis are either omitted or not
explicitly superseded in the spec:

- Reservation of host‑language (Rust) keywords as identifiers.
- Explicit handling status for legacy/compatibility tokens (e.g.,
  `Aggregate`,`FlatMap`,`Inspect`,`typedef`,`signed`,`bigint`,`bit`,`double`,`float`,`as`).
- Statement forms `break`, `continue`, and `return` (both the parsers and the
  keywords are present in code/roadmaps, yet they are not formalized in the
  Statements grammar).
- A few reserved operator tokens visible in the Haskell parser (`#`, `<=>`)
  that the spec neither reserves nor explicitly deprecates.
- An expression‑only parser entry point (useful for tools/tests) that the spec
  does not currently document.

These items should be incorporated into the spec as compatibility and
clarification notes. Once complete, the Haskell analysis document can be
deprecated as authoritative guidance and retained only as historical context.

______________________________________________________________________

## Findings: content not yet superseded by the spec

1. Host‑language keyword reservation

   - The Haskell analysis documents reserving a broad set of Rust keywords to
     avoid code‑generation and embedding conflicts.
   - The current spec lists only DDlog keywords and reserved operators; it does
     not state the host‑language reservation policy.

2. Legacy/compatibility tokens and spellings

   - The Haskell analysis lists tokens such as
     `Aggregate`, `FlatMap`, `Inspect`, `typedef`, `signed`, `bigint`, `bit`,
     `double`, `float`, and `as`.
   - The updated spec explicitly documents legacy `Aggregate` lowering and
     flatmap‑like binds via pattern assignments, but does not enumerate the
     full set of legacy tokens nor specify their acceptance/deprecation status
     (accept with lowering, parse‑time error, or reserved but unused).

3. Control‑flow statements: `break`, `continue`, `return`

   - The Haskell parser (and the published roadmap) recognize these constructs.
   - The spec’s Statements grammar covers `for`/`if`/`match`/`skip`/`block` and
     assignments, but does not include `break`, `continue`, or `return`.

4. Additional reserved operators in Haskell

   - The Haskell token set includes `#` and `<=>` as single tokens.
   - Neither the spec’s operator table nor its special tokens list mentions
     them, nor states whether they are reserved or removed.

5. Parser entry points

   - The Haskell module exposes both a full‑program parser and an
     expression‑only parser (useful for tests and tooling).
   - The spec documents only the full‑program input/output.

6. Implementation compatibility notes

   - The Haskell analysis spells out pre‑lexing tab normalization and position
     mapping. The spec references this but could explicitly tie it to entry
     points and diagnostics guarantees.

Already superseded (intentional changes)

- Rule head operator corrected to `:-` (not `::-`).
- `{ expr }` as expression grouping is intentionally excluded from the spec.
- Only fully‑qualified calls parse as calls at parse time (bare `name(…)`
  deferred to name resolution) — a deliberate divergence from the Haskell
  parser.

______________________________________________________________________

## Migration plan

Objective: integrate all still‑relevant details from the Haskell analysis into
the normative spec and implementation notes, then deprecate the Haskell
document.

1) Spec edits

   - Add an appendix “Reserved Identifiers and Host‑Language Keywords”.
     - State that the lexer reserves DDlog keywords and a set of Rust keywords.
     - Provide a compact, versioned list or reference to a single source of
       truth (the lexer table), with a note that updates to the host keyword
       set update the spec implicitly.
   - Add a “Legacy and Compatibility Tokens” subsection:
     - `Aggregate`: accepted; lowered to `RHSGroupBy`; deprecated with a
       diagnostic.
     - `FlatMap`: surface syntax represented via pattern binds on the right-hand
       side; no distinct keyword in the updated language.
     - `typedef`, `signed`, `bigint`, `bit`, `double`, `float`, `as`, `Inspect`
       (and any others present in the historical lexer): define each token's
       status as one of the following: accepted alias (with diagnostic),
       parse‑time error, or reserved (tokenized but rejected with a targeted
       message). If a token is not used today, mark it as “reserved, not part
       of the grammar”.
   - Extend the Statements grammar to include `break`, `continue`, and `return`:
     - `break`/`continue`: allowed only in loop bodies; error elsewhere.
     - `return`: allowed in function/closure bodies; not valid in rule bodies.
     - Call out diagnostic responsibilities when misused.
   - Clarify treatment of `#` and `<=>` operators:
     - Either mark them as “reserved but not in the grammar” (error on use) or
       “removed” (hard error) — decide and document.
   - Document an expression‑only entry point alongside the full‑program entry
     point under “Entry points and products”.
   - Expand “Portability notes” with a compatibility policy and explicit tab
     normalization guidance.

2) Lexer and parser alignment

   - Ensure the lexer’s reserved keyword set includes Rust keywords and any
     legacy tokens the project commits to reserve/diagnose.
   - Decide fate of `#`/`<=>` and implement consistent errors.

3) Tests and diagnostics

   - Add parser tests covering:
     - `break`/`continue`/`return` acceptance/errors in correct/incorrect
       contexts.
     - Legacy token handling (lowering, aliasing, or rejection with a precise
       message).
     - Reserved‑but‑unused operators (`#`, `<=>`) produce the intended
       diagnostic.
     - Host keyword reservation: using a Rust keyword as an identifier is
       rejected with a clear error.

4) Deprecate the Haskell analysis document

   - After spec and tests land, add a deprecation banner to
     `docs/haskell-parser-analysis.md` pointing to the spec, noting it is
     preserved only for historical context.
   - Remove any normative lists in the Haskell doc (or relabel as “historical
     snapshot”) to avoid confusion.

5) Optional: compatibility mode policy

   - If the project chooses to keep certain legacy spellings temporarily, gate
     them behind a “compat mode” configuration, documented in the spec’s
     compatibility appendix. Provide a deprecation timeline.

______________________________________________________________________

## Reference update list (occurrences to change after deprecation)

Search scope: repository tracked files.

- docs/roadmap.md:100
  - Text: “parser analysis (`docs/haskell-parser-analysis.md`).”
  - Action: Update reference to the spec (and/or this migration plan or a new
    “Implementation Notes” section in the spec).

- docs/pratt-parser-for-ddlog-expressions.md:270
  - Text: “parser analysis (`docs/haskell-parser-analysis.md`).”
  - Action: Update reference to the spec’s operator table and grammar, and the
    new appendix on host keyword reservations.

Note: re‑run `rg -n "haskell-parser-analysis.md"` prior to removing the old
document to capture any new references.

______________________________________________________________________

## Decision log (to be confirmed)

- `#` and `<=>` tokens: [DECIDE] reserved‑but‑unused vs removed.
- `typedef` handling: [DECIDE] alias to `type` with diagnostic vs hard error.
- Surface acceptance of legacy numerical type names (e.g., `double`, `float`):
  [DECIDE] accepted synonym vs error.

______________________________________________________________________

## QA checklist

- [ ] Spec updated with appendix and statement grammar changes.
- [ ] Lexer tables match the updated reserved sets.
- [ ] Tests added for statements, legacy tokens, reserved operators, and host
      keyword reservation.
- [ ] Haskell analysis doc bannered as deprecated.
- [ ] All repo references updated to the spec.
