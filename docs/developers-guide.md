# Developer guide

This guide records the parser module structure introduced by issue `#223`. It
is intentionally narrow and documents ownership boundaries rather than the full
parsing pipeline.

## Parser module structure

### `src/parser/ast/expr/sexpr.rs`

- Owns S-expression rendering helpers for `Expr`.
- Supports test and fixture comparisons without coupling callers to debug
  formatting.
- Should remain presentation-only; parsing and semantic classification belong
  elsewhere.

### `src/parser/ast/rule/classification.rs`

- Owns rule-body term classification for raw literals.
- Handles assignment parsing, aggregation detection, and `for`-loop lowering
  within the rule-body helper path.
- Keeps rule-body classification separate from the public `Rule` wrapper so
  `rule.rs` stays focused on the surface API.

### `src/parser/expression/pratt/postfix.rs`

- Owns postfix dispatch for the Pratt parser.
- Routes function calls, bit slices, field access, tuple indexing, method
  calls, and delay postfixes to the appropriate helper.
- Coordinates the pending diff-marker state across the postfix chain.

### `src/parser/expression/pratt/diff.rs`

- Owns diff-marker tracking and validation.
- Wraps completed postfix expressions in `Expr::AtomDiff` when a diff marker
  is pending.
- Emits the targeted diagnostics for duplicate, misplaced, or dangling diff
  markers.

### `src/parser/expression/pratt/delay.rs`

- Owns `expr -<N>` postfix parsing.
- Consumes the `-<` token pair, reads the delay literal, and returns
  `Expr::AtomDelay` on success.
- Keeps delay-specific validation separate from the generic postfix loop.

## Boundary rules

- Keep formatting helpers in `sexpr.rs` rather than mixing them into the core
  expression parser.
- Keep rule-body classification in `classification.rs` rather than adding
  helper-stage logic to `rule.rs`.
- Keep postfix dispatch in `postfix.rs`; add new postfix behaviour there only
  when it needs shared chain state.
- Keep diff-marker state and delay parsing in their dedicated submodules so
  `pratt.rs` remains the central parser entry point.
- Route reserved-token diagnostics through `src/parser/reserved_tokens.rs`.
  That module owns the parser-internal messages and the `rejection_for`
  predicate for unsupported legacy tokens. The canonical public policy table
  lives in `docs/differential-datalog-parser-syntax-spec-updated.md` section
  `9.1`; avoid duplicating it in code comments or local scanner modules.
## Spelling policy

The lint and Markdown gates run pinned `typos` 1.48.0 with British English and
Oxford `-ize` conventions. Before checking maintained Markdown, the generator
refreshes the shared estate dictionary into an untracked local cache only when
the authority is newer, then merges `typos.local.toml`. The generated
`typos.toml` is reviewed and committed so a clean, network-restricted checkout
can still enforce the last known-good policy.

Add repository-only proper names or quoted upstream terms to
`typos.local.toml`; never edit generated entries in `typos.toml` by hand. The
gate also runs the helper's Python 3.13 tests with at least 90% line coverage.

## Workflow pins and Dependabot

Dependabot owns the upgrade of GitHub Actions and reusable workflows,
including calls into `leynos/shared-actions`. Contract tests that assert a
caller's exact commit SHA create a lockstep dependency: every time
Dependabot opens a bump PR, the test fails until a human edits the pinned
constant to match. That defeats the purpose of automated dependency updates
and turns a routine bump into a manual chore.

Contract tests may still verify the *shape* of a reusable-workflow caller.
They must not verify the specific SHA value.

- Do assert the workflow references the correct reusable workflow path.
- Do assert the ref is pinned to a full 40-character commit SHA, not a
  mutable branch such as `main` or `rolling`.
- Do assert the expected `on:` triggers, least-privilege `permissions:`, and
  the inputs the caller relies on.
- Do not hard-code the current SHA value as an expected string. Match it
  with a pattern instead.
- Do not fail a test purely because Dependabot bumped the pinned SHA.

```python
import re

SHA_RE = re.compile(r"^[0-9a-f]{40}$")

def test_uses_pinned_full_sha(caller_step):
    ref = caller_step["uses"].split("@")[-1]
    assert SHA_RE.match(ref), f"expected a 40-hex commit SHA, got {ref!r}"
```

If a workflow's behaviour genuinely depends on a feature only present from a
particular commit onwards, express that as a comment or a changelog note,
not as a test assertion on the SHA string.
