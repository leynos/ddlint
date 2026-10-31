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

### `src/parser/span_scanners/typedefs.rs`

- Collects spans for `type` and `extern type` declarations; it finds statement
  boundaries rather than validating the complete type grammar. A top-level
  semicolon or newline ends a span, while nested delimiters keep internal
  newlines within it.
- Reuses `TypeDepths` and `adjust_type_depths` from
  `src/parser/span_scanners/indexes_support.rs` to track `()`, `[]`, `{}`, and
  `<>`; `>>` closes two angle levels. This support is shared with the index
  scanner. Legacy `typedef` diagnostics remain owned by
  `src/parser/reserved_tokens.rs`.

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
- Route reserved-token diagnostics through `src/parser/reserved_tokens.rs`.
  That module owns the parser-internal messages and the `rejection_for`
  predicate for unsupported legacy tokens. The canonical public policy table
  lives in `docs/differential-datalog-parser-syntax-spec-updated.md` section
  `9.1`; avoid duplicating it in code comments or local scanner modules.
- Keep expected-token types private to parser test fixtures:
  `ExpectedReservedToken` pairs lexemes with diagnostic constants, and
  `ReservedTokenScenario` owns `SourceText` plus that expectation;
  `DeclarationPosition` is a separate typed source builder. `SourceText`
  carries source input only, and tests must not call production `rejection_for`
  to derive expectations. Follow `SpannedErrorCase` for typed
  source-and-diagnostic-span fixtures.

## Parser observability

`parse` uses a no-op observer and remains independent of telemetry runtimes.
Applications that need parser-attempt and diagnostic callbacks can call
`parse_with_observer` with a `ParseObserver` implementation. The observer is a
read-only integration boundary: callbacks cannot affect parsing, and its
diagnostic context separates stable codes, categories, severity, spans, and
message text. Message text and spans must not become metric labels.

Top-level `Parser` completion includes every final `Parsed::errors()` entry,
including reserved-token diagnostics appended after child phases. Report only
that newly appended suffix through the existing
`observability::report_diagnostics` before the completion callback. Diagnostics
already reported by child phases are not emitted again. This reuses the current
reporter and adds no separate observability abstraction.

The optional `observability` feature exposes `TelemetryObserver`. It forwards
events to the `tracing` and `metrics` facades without installing a global
subscriber or recorder. Only host applications initialize exporters. See the
[parser observability guide](parser-observability.md) for the stable taxonomy,
adapter usage, and export/alert guidance.

The private `NonRuleScanResults` and `SpanBuildInput` types are single-call
pipeline carriers in `span_scanner.rs`. Keep them local to one parse so scanner
outputs and error order cannot be reused across parser calls.

## Lint execution

`Runner::new` records whether `Parsed::errors()` is non-empty. When parsing
reports errors, `Runner::run` skips registered rules and returns no lint
diagnostics; recovery CST nodes are not linted. Callers must report parser
errors separately from the returned lint diagnostics.

## Contributor workflow

Run these gates in order before committing:

1. `make fmt`
2. `make check-fmt`
3. `make lint`
4. `make test`
5. `make markdownlint`
6. `make nixie`

This is the required pre-commit sequence, matching `AGENTS.md`.

`make lint` first runs `make interrogate`, which needs `uv` and requires a
docstring on every Python definition under `tests` (it fails on any `MISSED`
row, because interrogate rounds its percentage to one decimal). It also
includes the spelling gate, and `make markdownlint` also invokes the spelling
gate for tracked Markdown. `make markdownlint` lints every Markdown source, and
`make nixie` validates the Mermaid diagrams within them.

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

- `clippy::expect_used` remains denied in production code. `clippy.toml` sets
  `allow-expect-in-tests = true`, which permits reasoned `.expect("reason")`
  calls in test code. Since `nightly-2026-10-30` Clippy treats every item in a
  test crate as test code, so it no longer rejects an `expect` in a fixture,
  setup helper or behaviour-driven development (BDD) step there (it used to);
  `make test-clippy-policy` pins that. The restriction on those items now rests
  on Whitaker's `no_expect_outside_tests` lint alone: keep them fallible.
  Module-wide lint allowances or expectations remain forbidden because they
  mask code beyond the test body.
- Shared rule assertions are macros, not functions. `assert_body_assignment`,
  `assert_body_terms_error` and `assert_multiple_aggregation_error` expand at
  the call site, so a failure names the calling test rather than the helper,
  and the arrangement's unwrap lands inside a test body. A function that wraps
  an assertion macro puts that unwrap back outside a test body, and the lint
  reports it again.

`make test-clippy-policy` measures this boundary with the repository's pinned
Clippy and existing `rstest` dependency. It runs separately from `cargo test`
to avoid a nested Cargo lock. `make lint`, and therefore CI's lint step, runs
it sequentially. The integration target `tests/clippy_expect_policy.rs` owns
the positive test bodies and the helper and fixture specimens, which the pinned
Clippy accepts as test code. Only this contract activates
`clippy_policy_negative`; ordinary test runs leave that module disabled. The
Python runner under `scripts/` checks lint codes and primary source locations,
refuses unrelated compiler errors, and removes the permission in a temporary
configuration as a negative control, which rejects all of them. These specimens
test the local invocation contract, rather than providing reusable setup
helpers.

The runner's private `_expect_locations` helper validates primary diagnostic
spans only for its Cargo JSON reader. Keep that boundary local to this policy
contract; other validation commands should use their own supported diagnostic
formats rather than depend on its specimen-specific checks.

### Rule-behaviour setup ownership

The rule-behaviour integration target owns its fallible rule extractor and
mixed-rule fixture under `tests/support/rule_behaviour.rs`. The extractor
reports parse and missing-rule failures; the fixture composes it with the
shared source used by the classification and flattened-order tests. Only those
two tests consume that fixture and retain their own assertions. Neither setup
boundary asserts or panics. Other test families keep their existing fixtures;
wider reuse requires an explicit ownership decision rather than extending this
file-local setup implicitly.

## Spelling policy

Run the spelling gate with `make spelling`. It enforces British English with
Oxford `-ize` conventions over tracked Markdown prose, and it also enforces
exact phrase corrections that Typos cannot match because it splits hyphenated
phrases into separate words.

`TYPOS_CONFIG_BUILDER_VERSION` in the `Makefile` pins the
`typos-config-builder` release the gate runs (currently `v0.1.3`). Raise it
together with the regenerated `typos.toml`, never on its own. The builder
requires Python 3.14 or newer, so the target passes `--python 3.14` and `uv`
fetches that interpreter when the host lacks one.

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
calls into `leynos/shared-actions`. For Dependabot-managed reusable-workflow
callers, contract tests that assert an exact commit SHA create a lockstep
dependency: every time Dependabot opens a bump PR, the test fails until a human
edits the pinned constant to match. That defeats the purpose of automated
dependency updates and turns a routine bump into a manual chore.

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

For these reusable-workflow callers, a dependency on a feature introduced in a
particular commit belongs in a comment or changelog note rather than an exact
SHA assertion.

### Direct Setup uv cache inputs

The direct Setup uv action keeps caching enabled and hashes the repository's
inline Python tool inputs: `Makefile`, `.github/workflows/ci.yml`,
`tests/workflow_contracts/**/*.py`, and `scripts/**/*.py`. Its default
dependency manifest patterns do not match this Rust repository's tracked files.

`make test-workflow-contracts` checks that every configured pattern matches
tracked repository files and that the inputs cover the dependency definitions
and owned Python contracts. It also checks the direct action's verified
immutable revision. Updating this consumer pin requires verifying that the new
revision supports `cache-dependency-glob` and updating the contract's checked
reference together. This interface check is separate from the reusable-workflow
pin convention above.

## Coverage publication

Pull-request continuous integration (CI) generates LCOV coverage and ratchets
it against the baseline written by `coverage-main.yml`. The pull-request lane
publishes no coverage artefact, never contacts CodeScene, and never receives
`CS_ACCESS_TOKEN`, so a change in CodeScene's application programming interface
(API) cannot hold a pull request.

The ratchet holds its stored reference within one percentage point: it fails
only when measured coverage is more than one point below the reference, and
advances the value only when measurement is more than one point above it. A new
cache generation can save the same numeric value. The restore `cache-hit` flag
records an exact-key hit, so a `restore=miss` metric can accompany a successful
fallback restore.

`coverage-main.yml` is the only publisher. On each push to `main` it refreshes
the ratchet baseline and uploads the report to CodeScene. It also runs on
demand through `workflow_dispatch`, for merges that fire no push event: a
dispatch on `main` uploads a fresh report, but the shared action advances the
baseline only on a push, so the ratchet catches up at the next push to `main`.
No `env` binds `CS_ACCESS_TOKEN`: a check step writes whether the secret is
set, from an expression evaluated before its shell runs, and the upload step
receives the token only as its `access-token` input, because the uploader is a
composite action that would pass its step's `env` to its nested steps. The
upload runs only when the token is present and the ref is `refs/heads/main`.
The job declares the protected `codescene` environment, whose deployment branch
policy admits `main` only, and administrative bypass is enabled. The token is
provisioned in that environment; it is absent from the repository's Actions
secrets. Branch dispatches remain subject to the job's environment protection,
and the upload step retains its independent main-ref guard.

The publisher's concurrency group is keyed on the workflow and ref and never
cancels a run in progress, so runs on `main` never overlap, and a newer trigger
replaces an older pending run rather than queueing behind it. GitHub does not
promise to start runs in trigger order, so this does not guarantee commit
order: an older run that starts late can publish its commit's coverage after a
newer one, and the next push supersedes it. A manual re-run of an older run
keeps its SHA and its run id: it republishes that commit's coverage to
CodeScene, but replaces no ratchet baseline while the original run's cache
entry survives, because the shared action saves each baseline under a key that
includes the run id. If that entry was never saved or has since been evicted, a
re-run of a push saves the older commit's baseline again, the shared action
restores the newest entry under the key prefix, and later ratchets read the
older baseline until the next push saves a newer one. That stale-order risk is
accepted. A re-run of a dispatch saves no baseline, since the shared action
saves one only on a push.

Two gaps are known and accepted, and both are tracked in
[shared-actions issue 518](https://github.com/leynos/shared-actions/issues/518):

- Merges made by the Dependabot automerge workflow use `GITHUB_TOKEN` and fire
  no push event, so they are measured only at the next push to `main` or a
  manual dispatch.
- A dispatch that replaces a pending push writes no baseline, since the shared
  action saves one only on a push, so the ratchet baseline stays behind until
  the next push. A dispatch made before a push can also reach the concurrency
  group after it, replace it and upload the older commit; that is part of the
  stale-order risk accepted above.

No other workflow a push starts, directly or through a local call, may generate
coverage outside the pull-request guard, and none, the publisher included, may
run a local action, whose `action.yml` the contract does not read, so the
publisher is the only baseline writer. Both coverage steps select the same
inputs at the same `shared-actions` pin because the pull-request ratchet is
only meaningful against a baseline measured the same way.

`make test-workflow-contracts` holds this shape by running
`cv005-contracts check`, the shared contract library in `leynos/shared-actions`
(`packages/cv005-contracts`), from a full commit named by `CV005_CONTRACTS_REF`
in the Makefile, and CI runs it as its own step. A fix to the rules is
therefore a pin bump. The target needs `uv`, which fetches the Python 3.13 the
library runs under. The repository's parameters are in `.github/cv005.toml`: its
`repository` name and the `[selection]` inputs the baseline measures, which
the publisher's generator must carry and every pull-request lane must match.
The target also runs consumer-specific regressions, including
`tests/workflow_contracts/coverage_publisher_test.py`. Its private strict YAML
reader checks only the publisher's exact workflow/ref concurrency and canonical
token-step contract; it is test-local support, not a reusable baseline auditor.
The shared library owns the transitive pull-request workflow closure,
secret-route, and baseline-writer rules. Those rules read every workflow a pull
request can start, from its own events, reviews and comments, a merge queue, or
a push not confined to `main` or tags, following local reusable-workflow calls,
`workflow_run` chains and local composite actions, and refuse any mention of
the CodeScene host, uploader, client, or token there. They also refuse
`continue-on-error` wherever it would turn a failed ratchet or upload green,
and they read workflows strictly, so a duplicate key is refused rather than
silently resolved.

## The build standard

Development, test, lint, and typecheck builds use the parallel `rustc` frontend
(`-Zthreads=8`) and, on Linux, the `mold` linker (`-Clink-arg=-fuse-ld=mold`).
These are defaults in `.cargo/config.toml`, which Cargo discovers on its own,
so a bare `cargo build` gets them. `mold` ships for Linux only, so the linker
flag lives in a Linux-only table and macOS and Windows keep their platform
linker. Cargo selects one `rustflags` source rather than merging them, so every
source repeats the same flags apart from the linker.

An assigned `RUSTFLAGS` replaces the configuration's flags, so the Makefile
recipes that set it compose the standard's flags onto any inherited value (CI's
`setup-rust` exports one). Two builds are deliberately excluded: coverage
assigns `RUSTFLAGS` without the fast flags, because a measurement should not
depend on them, and the release recipe and workflow keep the platform linker,
because they assign `RUSTFLAGS` (even an empty value displaces the
configuration). Cargo has no per-profile `rustflags`, so a direct
`cargo build --release` takes the configuration's flags unless `RUSTFLAGS` is
assigned too.

On Linux, install `mold` before building: the configuration names it, so a
build without it fails at link time. CI installs it through `setup-rust`'s
`install-mold` input. `tests/build_standard_contract.rs` holds the standard. It
reads the configuration sources, the commands `make -n` prints for each
development target on a Linux host and a macOS host (each keeping the caller's
own `RUSTFLAGS`) and for each coverage and release target on a Linux host, and
the `setup-rust` steps of the CI workflows (each must pass `install-mold`), so
a flag lost through a recipe or workflow edit fails there. The decision is
recorded in [ADR 003](adr-003-rust-build-standard.md).

### Development build footprint

The development profile sets `incremental = false` and `debug = 0` in
`Cargo.toml`, so a dev build keeps no incremental state or debug information.
To debug, set `CARGO_PROFILE_DEV_DEBUG=true`; to rebuild incrementally, set
`CARGO_INCREMENTAL=1` (unset any `sccache` wrapper first, which refuses
incremental builds). `.cargo/config.toml` also sets
`[unstable] embed-metadata = false`, the native Cargo key for nightlies from
2026-07-31, so Cargo coordinates metadata output itself. No raw rustc metadata
flags are used, and the pin is not moved to suit the key.
`tests/build_footprint_contract.rs` holds all three settings and proves them,
with both overrides, at the Cargo boundary by building a one-file crate under
the repository's own `[profile.dev]` table and Cargo configuration.

### Whitaker isolation

`make lint` runs Whitaker with the literal `RUSTFLAGS="-D warnings"`, after
clearing an inherited `CARGO_ENCODED_RUSTFLAGS`, which Cargo ranks above
`RUSTFLAGS`. Whitaker runs under the toolchain its installer manages, so it
takes none of the development standard (no parallel frontend, no `mold` linker
flag and no inherited `RUSTFLAGS`). The build-standard contract reads that
isolated command as exempt from the restatement clauses, and ADR 003 records
the exception. `tests/whitaker_isolation_contract.rs` reads the real
`make -n lint` output and fails unless Whitaker clears the encoded flags and
assigns the literal `RUSTFLAGS="-D warnings"`.

### Cranelift

Exception: Cranelift is not the development-profile backend. The repository pins
`nightly-2026-10-30` (no Cranelift measurement was taken on it, and the
exception has no deferral date: it holds while the release workflow builds on
stable), but the release workflow (`cross +stable build --release`) builds on a
stable toolchain against `.cargo/config.toml`, and stable Cargo refuses a
`[profile.dev] codegen-backend` key ("config profile `dev` is not valid"), so
selecting the backend would break that build (recorded 2026-09-29). That build
assigns `RUSTFLAGS`, so the nightly-only `-Zthreads` flag never reaches it.
Revisit if that build moves to the pinned nightly.
