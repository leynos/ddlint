# Adopt OrthoConfig v0.10.0 in the CLI configuration spine

This ExecPlan is a living document. Keep its progress, discoveries, decisions,
verification, outcomes, and conformance links current at each milestone.

Status: IN PROGRESS

## Purpose / big picture

Give the planned `ddlint` command line a tested configuration layer based on
the published `ortho_config` v0.10.0 release. Users will be able to configure
the planned lint command through TOML, `DDLINT_*` environment variables, and
command-line values, while the current placeholder binary and reusable `ddlint`
library retain their existing behaviour and dependency boundary.

This work executes roadmap item 5.1.5 as the configuration spine. It does not
implement the still-planned file-lint command, `rules`, or `explain` commands.

## Constraints

- Keep the imported OrthoConfig guides byte-for-byte identical to commit
  `38812c563e3953a490e3efc202609f37e21a0517` and preserve their recorded Secure
  Hash Algorithm 256-bit (SHA-256) checksums.
- Keep `ortho_config` out of the reusable root `ddlint` package and leave that
  package's dependency set and minimum supported Rust version (MSRV) unchanged.
- Put OrthoConfig types in an application-owned `ddlint-cli` workspace package
  with Rust 1.89 as its MSRV. Keep the published library surface out of that
  package.
- Preserve the current placeholder binary's exact no-configuration output,
  streams, and exit status. Do not implement CLI commands early.
- Use `ortho_config = "0.10.0"` with default features enabled. Do not use
  unreleased APIs or `default-features = false`.
- Use injected environment sources and explicit file sources in tests. Do not
  mutate the test process environment.
- Do not suppress warnings, add ignores, bypass the compiler wrapper, merge,
  or publish a release.

## Tolerances (exception triggers)

- Stop if v0.10.0 cannot keep the OrthoConfig dependency inside the application
  package without changing the reusable library's MSRV or dependency graph.
- Stop if the required source-injection and subcommand APIs are absent from the
  imported v0.10.0 documentation or fail against the published crate.
- Stop if preserving the placeholder's no-configuration behaviour requires
  changing its output or exit contract.
- Keep implementation within the planned configuration package, its tests,
  workspace/build wiring, and the directly related documentation. Escalate if
  unrelated runtime or domain changes become necessary.

## Risks

- Risk: subcommand source-aware loading has more than one supported entry
  point. Severity: medium; likelihood: medium. Mitigation: exercise the exact
  v0.10.0 public API with a real explicit TOML source and `MapEnv` before
  finalizing the module shape.
- Risk: workspace conversion may omit the application package from a canonical
  gate. Severity: high; likelihood: low. Mitigation: make workspace coverage
  explicit in `Makefile`, then inspect each generated gate command.
- Risk: imported guides may not pass this repository's Markdown gates as-is.
  Severity: medium; likelihood: low. Mitigation: preserve their bytes and
  report any incompatible gate rather than editing upstream text.
- Risk: the current application toolchain pin may be below the new package's
  Rust 1.89 floor. Severity: medium; likelihood: low. Mitigation: verify the
  pinned compiler before deciding whether to change the dated-nightly pin.

## Progress

- [x] (2026-10-26 12:16Z) Read repository instructions, confirmed a clean
  checkout at baseline `7450d4a031c54f56ce22e8801b015548f4a1ef82`, renamed the
  branch, and recorded `rustc 1.92.0-nightly` identity.
- [x] (2026-10-26 12:16Z) Confirmed issue #411 is open, the requested remote
  branch is absent, and no adoption pull request (PR) exists.
- [x] (2026-10-26 12:16Z) Fetched and resolved the upstream `v0.10.0` tag;
  imported both guides from the immutable commit and recorded their SHA-256
  checksums.
- [x] (2026-10-26 12:16Z) Read the imported user's guide and migration guide
  section 6. Firecrawl confirmed the documented `default-features = false`
  failure in upstream issue #627.
- [x] (2026-10-26) Inventory the existing and specified CLI surface, current
  tests, design and roadmap configuration contracts, compiler identity, and
  repository callers.
- [x] (2026-10-26) Baseline `make test` passed: 35 summaries, 1,274 passed,
  zero failed, zero ignored, including the trybuild compatibility contract.
- [x] (2026-10-26) Complete the configuration design, architectural decision
  record (ADR), roadmap update, and plan; `make fmt`, `make check-fmt`,
  `make markdownlint`, and `make nixie` pass.
- [x] (2026-10-26) Commit the documentation milestone and address the
  actionable findings from its initial CodeRabbit review.
- [x] (2026-10-26) Resolve and review M1 follow-up findings. Final CodeRabbit
  review of `b419496d8bd1626402989d4d83893f2a158a6456` completed with zero
  findings after formatting, Markdown/spelling lint, and Mermaid validation
  passed.
- [x] (2026-10-26) Start M2 only after M1 review clearance; the CLI
  configuration package and source-injected tests are now the implementation
  scope.
- [x] (2026-10-26) Add the `ddlint-cli` package, keep the root library
  dependency/MSRV boundary, move the unchanged placeholder, and implement the
  OrthoConfig-derived lint settings and source-aware loader. Focused tests
  cover defaults, TOML, `MapEnv`, explicit CLI overrides, malformed/unreadable
  and missing files, invalid values, unknown fields/rules, and the exact
  placeholder process contract. Initial red and green logs are recorded under
  `/tmp`.
- [x] (2026-10-26) Resolve two v0.10.0 integration details from tests:
  OrthoConfig's generated `config_path` participates in CLI merging, so the app
  consumes it internally while retaining unknown-key rejection; environment
  values are scalar strings, so `ignore_patterns` accepts a JSON-array string.
  These choices are documented in the design, ADR, and guides.
- [x] (2026-10-26) Run the complete sequential gate order on the corrected M2
  candidate: `make fmt`, `make check-fmt`, `make lint`, `make test`,
  `make markdownlint`, and `make nixie` all passed. The test gate reported 39
  suites, 1,288 passed, and zero failures or ignored tests. The exact logs use
  the `-4.out` suffix under `/tmp` for this worktree and branch.
- [x] (2026-10-26) Commit M2 as `536f23c` and run `coderabbit review --agent`
  after all deterministic gates passed. The review returned two findings about
  isolating automatic file discovery in tests (one duplicate recommendation)
  and one finding to mark roadmap item 5.1.5 complete.
- [x] (2026-10-26) Isolate automatic discovery in a child process rooted at an
  empty temporary directory, use explicit empty sources in error tests, and
  mark roadmap item 5.1.5 complete. The full sequential gates passed again;
  `make test` reported 39 suites, 1,288 passed, and zero failures or ignored
  tests. Commit: `833f08a`.
- [x] (2026-10-26) After the 55-minute cooldown, CodeRabbit started a review of
  `61b887a` but aborted with exit 134 after one heartbeat; no verdict or
  findings were produced. One retry on the same exact head was rate-limited
  with a reported 10-minute-43-second wait.
- [x] (2026-10-26 23:56Z) After another randomized 58-minute cooldown,
  CodeRabbit completed on `4b8c83b` with one trivial finding: the placeholder
  child process inherited the repository working directory despite clearing its
  environment.
- [x] (2026-10-27 00:09Z) Run the placeholder child from a fresh temporary
  directory while preserving its exact output, streams, and exit status.
  `make fmt`, `make check-fmt`, `make lint`, `make test`, `make markdownlint`,
  and `make nixie` all passed; the test gate reported 39 suites, 1,288 passed,
  zero failures, and zero ignored tests. Commit `841f7ea` followed these gates.
- [x] (2026-10-27) Commit the placeholder-test correction as `841f7ea` after
  the full sequential gate run.
- [x] (2026-10-27) CodeRabbit reviewed `1ebdbd5` and found that the defaults
  test still allowed host discovery through XDG and home-directory variables.
- [x] (2026-10-27) Pin `HOME`, `USERPROFILE`, `XDG_CONFIG_HOME`,
  `XDG_CONFIG_DIRS`, and `APPDATA` to the empty child directory. The full
  sequential gates passed again: 39 suites, 1,288 passed, zero failures. The
  source correction and this progress update are ready to commit.
- [ ] Commit the discovery-root correction and obtain a clean exact-head
  review before M3.
- [ ] Run final gates, commit, push the requested branch, and create a draft
  PR with the requested issue and Lody-session references.

## Surprises & discoveries

- Observation: the CLI is still a placeholder and the existing test inventory
  has no binary-level CLI tests. Evidence: `src/main.rs` only writes the
  placeholder line and exits 1; `rg` found no `assert_cmd` or `CARGO_BIN_EXE`
  tests. Impact: use a child-process regression test for the existing
  placeholder contract while validating the new configuration type separately.
- Observation: CodeGraph and Leta tools using the Model Context Protocol (MCP)
  are not exposed in this workspace. Evidence: tool discovery returned no
  CodeGraph or Language Server Protocol (LSP) tools. Impact: use the
  documented-path and literal-search fallback, and make no semantic-index
  claims.
- Observation: the generated configuration selector is merged as the internal
  `config_path` key, and the environment provider supplies scalar strings.
  Evidence: the explicit-file test initially failed strict root
  deserialization; an environment list initially failed sequence
  deserialization. Impact: the app captures the generated selector internally
  and parses ignore-pattern environment values as JSON arrays.
- Observation: CodeRabbit rate-limited the follow-up review of `833f08a` with
  the response `Rate limit exceeded` and a reported 8-minute-32-second service
  wait. The requested `vsleep` cooldown was randomized to 55 minutes and is
  completed. The review of `61b887a` then aborted with exit 134 after a single
  heartbeat, and its retry was rate-limited with a reported 10-minute-43-second
  wait. Impact: those two attempts produced no verdict; after the next
  cooldown, review completed on `4b8c83b` and found the placeholder test's
  inherited working directory. The test now uses a fresh temporary directory;
  all deterministic gates pass, and commit `841f7ea` records the correction. A
  follow-up review remains pending.
- Observation: the placeholder regression child cleared its environment but
  inherited the repository working directory. Evidence: CodeRabbit's review of
  `4b8c83b` identified that automatic config discovery could still see a
  repository file. Impact: launch that child from a fresh temporary directory
  and retain the exact output/stream/status assertions.
- Observation: injected `MapEnv` still needs every discovery variable pinned
  when automatic discovery is under test. Evidence: v0.10.0 documents
  `/etc/xdg` as the fallback for `XDG_CONFIG_DIRS`, and review of `1ebdbd5`
  identified host XDG/home discovery in the defaults test. Impact: pin `HOME`,
  `USERPROFILE`, `XDG_CONFIG_HOME`, `XDG_CONFIG_DIRS`, and `APPDATA` to the
  isolated root; the test now does so and the full gates pass.
- Observation: CodeRabbit described the 27 October progress records as future
  dated while the review completed across UTC midnight. Evidence: the review of
  `841f7ea` completed at 2026-10-27 00:15 UTC; the review of `4b8c83b`
  completed at 2026-10-26 23:56 UTC; and the full gate logs end at 2026-10-27
  00:09 UTC. Impact: record each review and validation using its actual UTC
  date.

## Decision log

- Decision: implement only the planned 5.1.5 configuration spine now; keep
  command execution and the 5.1.1–5.1.4 CLI surface for their roadmap work.
  Rationale: issue #411 expressly says not to start the CLI early, while also
  requiring code and validation beyond a design or dependency edit. The
  application-owned configuration type and source-injection tests are the
  concrete 5.1.5 implementation. Date/Author: 2026-10-26, Codex under the
  user's explicit implementation request.
- Decision: isolate application configuration in a non-published
  `ddlint-cli` workspace package. Rationale: the current root package also
  publishes the reusable library, and OrthoConfig v0.10.0 requires Rust 1.89. A
  package boundary preserves the library's dependency and MSRV boundary. The
  placeholder binary will move with its exact behaviour into that app package.
  Date/Author: 2026-10-26, Codex.
- Decision: use `DDLINT_`, `ddlint.toml`, TOML only, and the `cmds.lint`
  namespace. Nested environment keys use `__`, for example
  `DDLINT_CMDS__LINT__FORMAT`. File, environment, and explicit CLI values
  follow OrthoConfig's defaults < files < environment < CLI order. The lint
  format is configurable; environment ignore-pattern lists use JSON-array
  strings; file inputs and `--no-ignore` remain CLI-only. The generated
  `config_path` selector is an internal field, not a user configuration key.
  `rules` and `explain` have no settings yet and will not load configuration.
  Profiles and localization are not offered. Date/Author: 2026-10-26, Codex
  under the user's explicit implementation request.
- Decision: use first-wins file discovery at OrthoConfig's configured system,
  user, and current-project locations; do not search every parent directory. A
  selected `--config` path is exclusive and required. Rationale: these are the
  shipped v0.10.0 discovery semantics; no CLI has shipped a parent-search
  contract. Date/Author: 2026-10-26, Codex.

## Outcomes & retrospective

The upstream documentation import and its source identity are verified. The
baseline suite passed, and the design, roadmap, ADR, user and developer
guidance, and provenance are documented. Nine actionable documentation findings
were fixed; one reported missing reference was a false positive because its
label is not linked in the byte-preserved source. Compatibility pages preserve
the upstream relative paths, and the migration wrapper points all user-guide
links at the imported OrthoConfig guide. Earlier M1 reviews flagged
documentation clarity and link targets: DTO expansions, contents-index targets
and wrapping, imported-guide links, wrapper links, and source-link paragraph
wrapping. These were corrected without changing either canonical imported
guide. The review of `09b3f8b` found two missing targets for the imported
guide's cargo-orthohelp design and API links. Firecrawl verified both source
files at the recorded upstream commit; the new compatibility pages link to
those sources. An exhaustive scan found no other missing local links. The
review of `96d46a4` also flagged three compatibility links that used the movable
`v0.10.0` tag; the ADR and both migration wrappers now point to the immutable
commit recorded above. The review of `cdc334c` also found that generated
configuration selectors are hidden by default; the ADR now sets
`config_cli_visible = true` to match its documented help contract. The review of
`0363b17` found that the v0.9.0 migration compatibility page labelled its
upstream source as v0.10.0; the link label now matches the source version. The
final M1 review of `b419496d8bd1626402989d4d83893f2a158a6456` had zero
findings, and the documentation gates passed. M1 is clear; continue with the
application package and injected-source tests.

## Context and orientation

The root package currently combines a public Rust library and a placeholder
binary. `src/main.rs` prints one line to standard output and exits 1. The
design for future CLI commands and `ddlint.toml` is in `docs/ddlint-design.md`
§4.1–4.3; roadmap item 5.1 lists the still-planned command surface. There are
two public correctness rules in `src/linter/rules/correctness/`, but no
completed CLI or CLI integration suite.

## Baseline inventory

Baseline commit: `7450d4a031c54f56ce22e8801b015548f4a1ef82`. Both the installed
and repository-pinned compilers report
`rustc 1.92.0-nightly (a454fccb0 2025-09-15)`. The root manifest defines the
`ddlint` library and its placeholder binary; there is no `clap` or OrthoConfig
dependency.

| Surface                         | Existing behaviour or specified contract                                                                                                                                                                                                             |
| ------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Binary                          | `ddlint`; ignores arguments, writes `ddlint: CLI not yet implemented. Run with --help for planned options.` plus a newline to stdout, writes nothing to stderr, and exits 1.                                                                         |
| Planned default command         | `ddlint <FILES...>`; file operands, `--fix`, `--format` (`rich`, `json`, or `compact`, default `rich`), `--config PATH`/`-c PATH`, and `--no-ignore`.                                                                                                |
| Planned subcommands             | `ddlint rules` and `ddlint explain <RULE_NAME>`.                                                                                                                                                                                                     |
| Planned machine-readable output | `--format json` is specified as machine-readable; `rich` and `compact` are human-oriented. No output schema or exact exit-code matrix is specified yet.                                                                                              |
| Planned status/stream contract  | Design §4.1 says the future CLI will be integration-tested for exit codes and exact stdout/stderr, but does not yet assign status codes. It specifies success for valid input and failure for lint errors in the end-to-end plan (§4.9).             |
| Existing configuration          | None. The design had proposed `ddlint.toml`, parent-directory traversal, and `config-rs`; no environment variable names or migration aliases were specified. This decision replaces that proposal before the CLI ships.                              |
| Existing tests                  | `rg` found no CLI binary or argument integration tests (`assert_cmd`/`CARGO_BIN_EXE`). The baseline workspace `make test` passed; detailed results follow this table.                                                                                |
| Repository callers              | No CI workflow, Makefile, shell, Python, or Cargo manifest invokes `ddlint`/`cargo-ddlint`. `docs/parabellum/estate.yaml` registers the command by name but does not invoke it. No issue-linked adoption branch or PR was present at inventory time. |

The CLI surface above is still planned. This issue implements the 5.1.5
configuration model only; the default lint command and both subcommands remain
unimplemented until their existing roadmap items are delivered.

The baseline command `make test` exited 0. Its log contains 35 successful test
summaries with 1,274 passed, zero failed, and zero ignored cases. The
`parser_api_compatibility_contracts_are_checked_for_consumers` trybuild test
passed in 567.93 seconds. No CLI-specific test existed to run. The full log is
`/tmp/baseline-test-ddlint-issue-411.out`.

The upstream documents are `docs/ortho-config-users-guide.md` and
`docs/ortho-config-v0-10-0-migration-guide.md`. Their provenance and checksums
are recorded in `docs/ortho-config-provenance.md`.

## Conformance basis

- Issue #411, especially its requirements to use v0.10.0, keep the library
  free of OrthoConfig, build only the planned configuration spine, test
  precedence and failure cases, and preserve current no-configuration behaviour.
- Baseline `7450d4a031c54f56ce22e8801b015548f4a1ef82`.
- `docs/ddlint-design.md` §4.1–4.3 and `docs/roadmap.md` §5.1; these are the
  current project design artefacts. No Terms of Reference or earlier
  CLI-specific ADR existed.
- ADR 004 records the application/library boundary and configuration
  contract.
- Upstream `leynos/ortho-config` tag object
  `996e5a1c9f22ace4f55bf5dcc196d2c0524262bf`, peeled to commit
  `38812c563e3953a490e3efc202609f37e21a0517`.
- Repository `AGENTS.md` quality gates and Rust dependency/MSRV rules.

Trace links:

- ISSUE-411-APP-BOUNDARY -> ADR-004 -> `cli/Cargo.toml` and
  `cli/src/config/mod.rs` -> EP-M2 -> dependency-boundary tests and workspace
  gates.
- ISSUE-411-PRECEDENCE -> design §4.2 and ADR-004 -> `LintConfig` -> EP-M2 ->
  source-injected layer tests.
- ISSUE-411-ERRORS -> design §4.2 and ADR-004 -> application error mapping ->
  EP-M2 -> malformed, missing, invalid, and unknown-key tests.
- ISSUE-411-BASELINE -> `src/main.rs` baseline -> preserved app placeholder ->
  EP-M2 -> binary child-process regression test.
- ISSUE-411-PROVENANCE -> upstream immutable commit -> imported guides and
  provenance file -> EP-M1 -> checksum equality.

## Verification plan

No new mathematical invariant or concurrency protocol is introduced. The
configuration loader has finite source tiers and failure categories, so a table
of distinct, explicit witnesses is more direct than property or model checking.

### INV-PRECEDENCE

Obligation: `format` follows defaults < file < environment < explicit CLI;
`ignore_patterns` follows defaults < file < environment and supports the
command-line `--no-ignore` clear; rule severities follow defaults < file <
environment. No clap parser default may hide a lower layer.

Method: focused tests against OrthoConfig v0.10.0's real general
`load_from_iter_with_sources` API, using `MapEnv` and an explicit TOML file.
The general derive loader is required because its discovery attributes support
the chosen names and `--config` selector; the subcommand helper uses fixed
filename candidates.

Domain: distinct source values for the `lint` configuration namespace.

Artefact: `cli/tests/config_layers.rs`.

Evidence: `cargo test -p ddlint-cli --test config_layers`; pass only when each
layer's distinct value wins at its tier and explicit CLI wins over all lower
layers.

Non-vacuity: each supported source supplies a distinct value and the assertions
verify the winning tier, including the explicit CLI format and no-ignore
override. A changed expected tier must fail.

### INV-SUBCOMMAND-SCOPE

Obligation: lint settings load from `[cmds.lint]` and the `DDLINT_CMDS__LINT__`
environment namespace. Commands without settings do not load configuration.

Method: source-injected tests against the real derive and merge entry point.

Domain: the configured lint namespace and its matching environment variables.

Artefact: `cli/tests/config_layers.rs`.

Evidence: the focused test above; file and environment values under the lint
namespace resolve while the `rules` and `explain` commands remain unimplemented.

Non-vacuity: the values differ across source tiers and the file and environment
keys use the full lint namespace.

### INV-FAILURES

Obligation: an absent automatic file is optional; a missing explicitly chosen
file, malformed TOML, unreadable file, invalid enum value, and rejected unknown
key each produce an error and never silently become defaults.

Method: named integration cases with isolated temporary files and source maps;
verify optional automatic discovery in a child process rooted at an empty
temporary directory.

Domain: each listed failure plus a default-only control.

Artefact: `cli/tests/config_errors.rs`.

Evidence: focused integration test; each failure must return a typed or
actionable configuration error; the absent-file control must return defaults.

Non-vacuity: each file is actually selected and contains the failure trigger;
the control uses a distinct valid value and proves the optional path is
reachable.

### INV-BOUNDARY

Obligation: the root reusable package does not depend on OrthoConfig, while the
application package declares Rust 1.89 and the published dependency keeps its
default features enabled.

Method: inspect manifests and run workspace metadata, typecheck, tests, and
lint against the pinned compiler.

Domain: root package and app package dependency graphs.

Artefact: `Cargo.toml`, `cli/Cargo.toml`, and a manifest contract test if
needed.

Evidence: `cargo tree -p ddlint` has no `ortho_config`;
`cargo tree -p ddlint-cli` includes v0.10.0; `make typecheck` and canonical
gates pass.

Non-vacuity: query each package separately and verify the exact resolved
version and package `rust-version`.

### INV-PLACEHOLDER

Obligation: without configuration, the existing binary emits the same exact
placeholder line on stdout, leaves stderr empty, and exits 1.

Method: isolated child-process integration test with an empty working directory
and cleared child environment.

Domain: a no-argument invocation with an empty child environment and no config
file.

Artefact: `cli/tests/placeholder_cli.rs`.

Evidence: `cargo test -p ddlint-cli --test placeholder_cli` asserts exact
streams and status.

Non-vacuity: execute the actual built `ddlint` binary in a fresh empty
directory; the asserted output and status are non-empty and non-zero.

External axioms: OrthoConfig v0.10.0 implements its published derive,
discovery, source-injection, and merge APIs as documented; Cargo resolves the
published crate. This work verifies the integration, not third-party internals.

## Plan of work

Stage A: finish inventory and baseline. Record the command surface specified by
the design, the placeholder's actual behaviour, the absence of CLI tests, all
repository callers, compiler identity, and open issue/PR/branch state.

Stage B: write the design decision before code. Add ADR-004, update design
§4.1–4.3, update roadmap 5.1.5, add users' and developers' guidance, and keep
the imported guides unchanged. Run documentation gates, commit the coherent
documentation milestone, and request CodeRabbit review.

Stage C: add the `ddlint-cli` workspace application package, move the existing
placeholder binary without changing its behaviour, and implement the
OrthoConfig-derived lint settings and validated domain-facing representation.
Add tests before production configuration logic, then run the focused tests and
the full sequential gates. Commit the coherent code milestone and request
CodeRabbit review.

Stage D: resolve review concerns, re-run the affected gates, confirm final
checksums and exact branch head, push using SSH, and create a draft pull
request (PR). Do not merge or publish a release.

## Milestones and plateaus

### EP-M1: documented v0.10.0 design

Outcome: imported guides, provenance, ADR, design, roadmap, users' guide,
developers' guide, and a current ExecPlan describe one coherent contract.

Requirements: ISSUE-411-PROVENANCE, ISSUE-411-APP-BOUNDARY, and
ISSUE-411-PRECEDENCE.

Acceptance evidence: guide checksums match provenance; `make check-fmt`,
`make markdownlint`, and `make nixie` pass; CodeRabbit has no unresolved
concerns for this commit.

Conformance check: exact upstream sources retained; no code or dependency
change; design and roadmap agree; no actual CLI command has been started.

Recovery: restore the immutable imported files by `git show` from the recorded
commit and recompute checksums; revise only the affected documentation before
continuing.

Remaining gaps: executable configuration code and full repository gates.

Compatibility decision: preserve the placeholder binary's existing no-config
contract; no compatibility shim is introduced.

### EP-M2: tested application configuration spine

Outcome: the app package resolves its lint settings through OrthoConfig, with
hermetic tests for sources, errors, and namespace scope; the library remains
independent.

Requirements: ISSUE-411-APP-BOUNDARY, ISSUE-411-PRECEDENCE, ISSUE-411-ERRORS,
and ISSUE-411-BASELINE.

Acceptance evidence: focused tests, placeholder child-process test, package
dependency/MSRV inspection, and all sequential canonical gates pass. Current
gate evidence: 1,288 tests passed; lint and all documentation gates are green.

Conformance check: no CLI commands or lint execution implemented; v0.10.0
default features enabled; application errors remain at the app boundary; docs
and tests match ADR-004.

Recovery: revert only the app package and workspace-wiring commit; the M1
design and provenance commit remains independently coherent.

Remaining gaps: roadmap tasks 5.1.1–5.1.4 and runtime use of the configuration
spine are still future CLI work.

Compatibility decision: preserve the current placeholder output and status;
there are no existing command-line consumers to migrate.

### EP-M3: reviewed draft PR

Outcome: exact branch head is pushed and a draft PR closes issue #411 and
includes the Lody session link in terminal `## References`.

Acceptance evidence: current-head gates/review reconciled; PR metadata read
back; branch tracking points to the pushed head.

Recovery: no merge or release operation is performed; revise the same branch
after review and revalidate.

Compatibility decision: none beyond the M2 placeholder contract.

## Concrete steps

From the repository root, first record the actual baseline test result with
`make test` and a branch-specific log under `/tmp`. Then complete M1 and run
`make fmt`, `make check-fmt`, `make markdownlint`, and `make nixie`
sequentially. Do not alter the imported documents if formatting would change
their bytes.

For M2, add and run the focused tests before implementation. Then run
`make fmt`, `make check-fmt`, `make lint`, `make test`, `make markdownlint`, and
`make nixie` sequentially, capturing each output under `/tmp`. Inspect
failures from the cited log before deciding whether a source fix is needed.

For M3, verify `LODY_SESSION_ID`, authenticate SSH as `leynos`, push the
requested branch, refresh its `origin` tracking ref, and create the draft PR
with title `Adopt ortho_config v0.10.0 for the ddlint CLI (#411)`, summary
`Closes #411`, and a terminal `## References` section containing the session
URL.

## Validation and acceptance

Red-Green-Refactor: configuration tests must first fail because the planned app
crate/types are absent or because a deliberately configured source is not yet
resolved. After the minimal implementation, focused config tests must pass. A
changed expected precedence value and a malformed selected file are the
negative controls. Then run the full gates in the prescribed sequence.

The deterministic quality gates are formatting, format checking, lint,
workspace tests, Markdown lint, and Mermaid validation. `make lint` includes
Clippy, Whitaker, policy tests, and spelling. Review each command's captured
terminal result; partial output is not a pass.

## Idempotence and recovery

The upstream docs import refuses to overwrite existing files. To verify or
restore them, read the two source paths from commit
`38812c563e3953a490e3efc202609f37e21a0517` and compare SHA-256 values with
`docs/ortho-config-provenance.md`. All code and docs changes remain on the
requested issue branch; do not delete other worktrees or shared caches.

## Interfaces and dependencies

The future-facing application package will own `ddlint_cli::config::LintConfig`,
`OutputFormat`, a configuration severity enum, and a validated settings type
whose severity values map to `ddlint::linter::RuleLevel`. The resolver returns
errors; it does not print or terminate the process. The root library stays free
of `ortho_config` and `clap`.

The app manifest uses caret requirements for `ortho_config = "0.10.0"` and
Serde, keeps OrthoConfig default features enabled, and sets
`rust-version = "1.89"`. The existing dated nightly remains only if its
verified compiler meets that floor.

## Revision note

Recorded the M2 follow-up commits, successful gate runs, CodeRabbit tool
failures, and review findings. The placeholder child working directory is
isolated and its platform discovery roots are pinned; full gates pass again.
The correction commit and clean M2 review remain, as does all M3 delivery work.
Review and validation timestamps use UTC to make the midnight boundary explicit.
