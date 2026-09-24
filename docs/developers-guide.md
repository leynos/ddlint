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

### `src/parser/ast/relation.rs`

- Owns the typed relation declaration surface for role, kind, ref marker, and
  body form.
- Treats `role()` and `kind()` as the canonical APIs for new code.
  `is_input()` and `is_output()` are derived helpers kept for callers that only
  need role predicates.
- Uses `role_keyword_present()` and `kind_keyword_present()` when callers need
  source-fidelity rather than the defaulted semantic value.
- Exposes declaration-level reference relations through `is_ref()`. Do not
  infer ref status from raw `&` tokens in downstream callers.
- Exposes `body()`, `element_type()`, `columns()`, and `primary_key()` as
  fallible queries returning `Result<_, RelationParseErrors>`. A valid record
  body yields `Ok(RelationBody::Fields(..))` with `element_type()` as
  `Ok(None)`; a valid bracket body yields `Ok(RelationBody::ElementType(..))`
  with `columns()` as `Ok(Vec::new())`. Malformed or missing bodies return
  `Err(..)` rather than an empty `Fields` vector, so direct AST querying stays
  reliable for malformed or synthetic nodes.
- Keeps `primary_key()` focused on the binder/list names (`Ok(None)` when
  absent). Spec-form trailing primary-key expressions are preserved in the CST
  until roadmap follow-up `2.6.6.1` introduces typed access. `Parsed::errors()`
  remains the parser-level diagnostic channel.

### `src/parser/span_scanners/relations.rs`

- Owns top-level relation-candidate discovery, declaration orchestration,
  record and bracket body parsing, primary-key validation,
  relation-versus-rule/fact disambiguation, span collection, and recovery.
- Applies delimiter-depth-aware line-start filtering so only genuine top-level
  declarations are treated as candidates, and emits the `D-REL-*` diagnostics.
- Produces relation declaration spans for CST construction;
  `src/parser/ast/relation.rs` then exposes the typed AST accessors over the
  resulting nodes.

### `src/parser/span_scanners/relations/cursor.rs`

- Owns shared cursor movement, trivia handling, token inspection, and
  balanced-delimiter traversal (including delimiter-stack maintenance) for the
  relation scanner.
- Holds no grammar decisions; those remain in `relations.rs`.

### `src/parser/span_scanners/relations/preamble.rs`

- Parses only the optional role/kind preamble keywords and enforces
  `D-REL-001`, `D-REL-002`, and `D-REL-003`.
- Keeps relation names, ref markers, bodies, primary keys, candidate discovery,
  and recovery in `relations.rs`.

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
- Prefer `Relation::role()` and `Relation::kind()` for new relation-aware
  logic. Use `is_input()` and `is_output()` only as predicate conveniences.
- Prefer `Relation::body()` over combining `columns()` and `element_type()`
  when code must branch on relation body shape.
- Keep postfix dispatch in `postfix.rs`; add new postfix behaviour there only
  when it needs shared chain state.
- Keep diff-marker state and delay parsing in their dedicated submodules so
  `pratt.rs` remains the central parser entry point.
- Keep scanner grammar and recovery decisions — candidate discovery, body and
  suffix parsing, and span collection — in `relations.rs`.
- Keep cursor movement and balanced-block mechanics in `relations/cursor.rs`.
- Keep role/kind preamble parsing and its ordering/duplication diagnostics
  (`D-REL-001` through `D-REL-003`) in `relations/preamble.rs`.
- Keep typed consumer-facing relation metadata in `ast/relation.rs`, and
  inspection-only CST traversal in `ast/relation/inspect.rs`.

## Contributor workflow

Run these gates in order before committing:

1. `make fmt`
2. `make check-fmt`
3. `make lint`
4. `make test`
5. `make markdownlint`
6. `make nixie`

This is the required pre-commit sequence, matching `AGENTS.md`.

`make lint` includes the spelling gate, and `make markdownlint` also invokes
the spelling gate for tracked Markdown. `make markdownlint` lints every
Markdown source, and `make nixie` validates the Mermaid diagrams within them.

`make spelling` regenerates the `typos` configuration and checks tracked
Markdown for en-GB-oxendict spelling. See `AGENTS.md` for the underlying
command implementations; this guide does not duplicate them.

## Parser test helpers

A test asserts; a helper arranges. Arrangement can fail, so the arrangement
helpers in `src/parser/tests/helpers.rs` return `Option` and the calling test
body owns the unwrap. `parse_single_item` and its six wrappers,
`parse_relation`, `parse_index`, `parse_function`, `parse_transformer`,
`parse_import` and `parse_apply`, follow this shape, as does
`parse_single_rule`, which reaches the same result through `parse_ok` rather
than through `parse_single_item`.

The Whitaker lint `no_expect_outside_tests` enforces it, and it is stricter
than it looks. Proc-macro attributes are erased before the lint sees the code,
so a helper is indistinguishable from production code no matter where it lives.
A panicking helper also reports the wrong location: the failure points at the
helper and says `item missing`, with nothing about which test wanted what.

Two consequences follow.

- `clippy::expect_used` stays denied, and each intentional unwrap carries its
  own narrowly scoped `#[expect(clippy::expect_used, reason = "...")]`. Put
  that attribute on the `let` statement that owns the unwrap, not on the test
  function. `#[rstest]` moves a function-level attribute onto its generated
  per-case wrappers, so the base function holding the body stays unlinted and
  the expectation is reported unfulfilled. Module-wide `#![expect(...)]` is
  forbidden: it disarms the lint for code nobody has written yet.
- Shared rule assertions are macros, not functions. `assert_body_assignment`,
  `assert_body_terms_error` and `assert_multiple_aggregation_error` expand at
  the call site, so a failure names the calling test rather than the helper,
  and the arrangement's unwrap lands inside a test body. A function that wraps
  an assertion macro puts that unwrap back outside a test body, and the lint
  reports it again.

## Spelling policy

Run the spelling gate with `make spelling`. It enforces British English with
Oxford `-ize` conventions over tracked Markdown prose, and it also enforces
exact phrase corrections that Typos cannot match because it splits hyphenated
phrases into separate words.

The gate regenerates the tracked `typos.toml` on every run from the live shared
dictionary and the repository-specific `typos.local.toml` overlay. Because the
dictionary is live, `typos.toml` must never be drift checked in continuous
integration. The builder refreshes the estate dictionary into the untracked
`.typos-oxendict-base.toml` cache only when the authoritative copy is newer,
records refresh metadata in `.typos-oxendict-base.json`, and reuses a valid
cache when the network is unavailable.

Add repository-only proper names or quoted upstream terms to
`typos.local.toml`; never edit generated entries in `typos.toml` by hand.

## Workflow pins and Dependabot

Dependabot owns the upgrade of GitHub Actions and reusable workflows, including
calls into `leynos/shared-actions`. Contract tests that assert a caller's exact
commit SHA create a lockstep dependency: every time Dependabot opens a bump PR,
the test fails until a human edits the pinned constant to match. That defeats
the purpose of automated dependency updates and turns a routine bump into a
manual chore.

Contract tests may still verify the *shape* of a reusable-workflow caller. They
must not verify the specific SHA value.

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
particular commit onwards, express that as a comment or a changelog note, not
as a test assertion on the SHA string.

## Coverage publication

Pull-request continuous integration (CI) generates LCOV coverage and ratchets
it against the baseline written by `coverage-main.yml`. The pull-request lane
publishes no coverage artefact, never contacts CodeScene, and never receives
`CS_ACCESS_TOKEN`, so a change in CodeScene's application programming interface
(API) cannot hold a pull request.

`coverage-main.yml` is the only publisher. On each push to `main` it refreshes
the ratchet baseline and uploads the report to CodeScene. It also runs on
demand through `workflow_dispatch`, for merges that fire no push event: a
dispatch on `main` uploads a fresh report, but the shared action advances the
baseline only on a push, so the ratchet catches up at the next push to `main`.
No `env` binds `CS_ACCESS_TOKEN`: a check step writes whether the secret is
set, from an expression evaluated before its shell runs, and the upload step
receives the token only as its `access-token` input, because the uploader is a
composite action that would pass its step's `env` to its nested steps. The
upload runs only when the token is present and the ref is `refs/heads/main`, so
a dispatch from a branch cannot publish that branch's coverage as the trunk's.

Two gaps are known and accepted. Merges made by the Dependabot automerge
workflow use `GITHUB_TOKEN` and fire no push event, so they are measured only
at the next push to `main` or a manual dispatch. A dispatch that replaces a
pending push uploads the same or a newer commit, but leaves the ratchet
baseline one commit behind until the next push. Both are tracked in
leynos/shared-actions#518.

The publisher's concurrency group is keyed on the ref alone and never cancels a
run in progress, so runs on `main` never overlap, and a newer trigger replaces
an older pending run rather than queueing behind it. GitHub does not promise to
start runs in trigger order, so this does not guarantee commit order: an older
run that starts late can publish its commit's coverage after a newer one, and
the next push supersedes it. A manual re-run of an older run keeps its SHA and
its run id: it republishes that commit's coverage to CodeScene, but replaces no
ratchet baseline unless the original run saved none, because the shared action
saves each baseline under a key that includes the run id.

No other workflow a push starts, directly or through a local call, may generate
coverage outside the pull-request guard, so the publisher is the only baseline
writer. Both coverage steps select the same inputs at the same `shared-actions`
pin because the pull-request ratchet is only meaningful against a baseline
measured the same way.

`make test-workflow-contracts` holds this shape. The contract tests are
`codescene_pull_request_test.py`, `codescene_publisher_test.py` and
`codescene_token_test.py` under `tests/workflow_contracts/`, with the rules in
the `codescene_*_rules.py` modules beside them and the strict workflow reader in
`codescene_workflow_reader.py`. The rules read every workflow a pull request
can start, from its own events, reviews and comments, a merge queue, or a push
not confined to `main` or tags, following local reusable-workflow calls and
`workflow_run` chains, and refuse any mention of the CodeScene host, uploader,
client, or token there. They also refuse `continue-on-error` wherever it would
turn a failed ratchet or upload green. The upload guard is compared as an exact
set of conjuncts, so an `||` hidden inside an extra conjunct fails the
comparison without a separate scan. Each clause has a test that mutates the
workflows and expects the clause to refuse the result.
