# 2.6.7 legacy-token diagnostic follow-up

Status: IMPLEMENTATION COMPLETE; source review approved; delivery tracked in
[PR #354](https://github.com/leynos/ddlint/pull/354)

Formal review
[5497951857](https://github.com/leynos/ddlint/pull/354#pullrequestreview-5497951857)
approved source on `1e4737e` with no actionable code comments.
Developer-contract documentation reconciliation and merge are tracked
separately.

## PR #354 CodeScene repair

### Current follow-up (2026-10-13)

- Wyvern verified supported type-angle grammar. A focused case exposed the
  prior scanner boundary failure at `T_SHR`, span `29..31`; the existing
  `type_definition_missing_name_returns_none` accepts `type = string`. The
  scanner collects spans, not full grammar validity.
- The scanner reuses `TypeDepths` and `adjust_type_depths` from
  `indexes_support` for `()`, `[]`, `{}`, and `<>`; `>>` closes two angle
  levels. The prior local `balanced_block` approach is historical and
  superseded. Index scanning and import-alias behaviour are unchanged. The
  compact finite randomized property in `src/parser/tests/reserved_tokens.rs`
  reuses `ExpectedReservedToken::ALL` and `reserved_token_spans`, retaining all
  44 cases without the redundant length wrapper. Its oracle compares foreign
  error vectors in exact order and full `Simple` fields, expected 0/1 symbol
  sets, idempotence, and counts/spans across all eight enum classes. Randomized
  inputs exercise direct `complete_reserved_token_errors` and failing inputs
  through public `parse_expression`; they do not call public `parse()` or
  establish successful Expr API path coverage. Existing deterministic `parse()`
  /cast tests and source inspection cover both Pratt outcomes. The bounded
  randomized property makes no formal-proof or general type-grammar claim. The
  positive property passed once at
  `/tmp/ddlint-pr354-property-restored-positive.out`; index integration (5),
  generic boundary (5), and types (16) focused tests passed. Negative control
  #2 killed the collector as expected: the minimal Typedef case changed expected
  `30..37` to actual `[]`, recorded in
  `/tmp/ddlint-pr354-property-negative-control-2.log`. The exact original
  production line is restored. All seven final-model Make gates passed: 1,215
  tests across 32 suites and 16 workflow contracts. Gate logs follow this
  template:
  `/tmp/<target>-d0264bd8-554b-49e7-ab08-b42ea6541172-2-6-7-finalize-legacy-token-compatibility-policy-replacement-final-model-3.out`;
  formatting log: `/tmp/ddlint-pr354-final-model-fmt-3.out`.
- Net Rust delta is 799 lines across 52 files, within the unchanged 800-line
  and 54-file limits. The larger 132-line property draft is unused; the
  1,000-line approval request is superseded. Final documentation receipt
  validation is recorded in the delivery ledger.
- Focused local CodeScene checks for `typedefs.rs`, `reserved_tokens.rs`, and
  `fixtures.rs` scored 10.0; this is not a whole-branch score. Wyvern confirmed
  the two details arose in `353..e1c8da3` and remain unchanged from `e1c8da3`
  through `23b04d1` and this follow-up; categories and file scores are
  unchanged relative to `353`: Types Duplication (9.38) and Rename Group +
  Infix Complexity (9.68). The Types result reflects renamed test logic, which
  is unchanged. The Infix count rose from 9 to 11 on the earlier feature
  branch, not on main. Exact-head hosted CodeScene approval at E1/`23b04d1`
  covered these details. Neither the HEAD boundary block nor the current
  generic case changes the three named type-function bodies. These details are
  not regressions from this follow-up; no broader refactor is needed.
- Hosted CI
  [run 38324124185](https://github.com/leynos/ddlint/actions/runs/38324124185/job/115058756211)
  passed on clean head `df32`. CodeScene
  [result 7923543](https://codescene.io/projects/69278/delta/results/7923543)
  passed with approval `5497156449`. CodeRabbit
  [assessment 6135024630](https://github.com/leynos/ddlint/pull/354#issuecomment-6135024630)
  inspected exact head `df32` against base `353` and cleared implementation,
  boundary, and property concerns. These receipts do not establish formal
  review or merge approval.
- The E1 reviews (`5495972133`, `5495921966`) requested changes across four
  premerge rows and reported the P1; they predate and do not cover the current
  type-depth rewrite. Original P1 cases were cleared in `4252727667`; assessment
  `6135024630` cleared the new generic follow-up. Formal review and merge
  remain pending. Reply `6133461317` could not recover the reported 82
  denominator; independent enumeration found 79, so that numeric warning
  remains an unverified advisory. Documentation records 12 purposeful source
  docs. These documentation-only repairs do not alter the already-implemented
  `Runner` parse-error short-circuit. The `Unreleased` migration was clarified
  without inventing a `0.2` version. Review reconciliation remains pending.

- Implementation head: `d21d9e34d16f3ccf5bcde40f0d719927db6c8c9b`, based on
  `353bb48b4900bcaf9322246f3911c371c175140c`. The diff is 48 files and net +662
  Rust lines, within the 800-line limit; the earlier 54-file count remains
  historical. The typed-fixture repair changed no runtime or API behaviour.
  Separately, the PR changes the `Runner` parse-error contract. Hosted results
  below apply to this implementation head, not to these later documentation
  edits.
- At `71ebe4c`, hosted CI was green, but CodeScene check `7920932` failed:
  String Heavy Function Arguments was 84.6% across 17 functions against a 39%
  threshold. This was not cleared by retaining string fixtures; that earlier
  disposition is superseded by the typed fixture remedy below.
- Private `src/parser/tests/reserved_tokens/fixtures.rs` pairs each
  `ExpectedReservedToken` with its exact lexeme and diagnostic constant,
  independently of production `rejection_for`. `ReservedTokenScenario` owns
  `SourceText` plus `ExpectedReservedToken`; `DeclarationPosition` is a
  separate typed source builder. `SourceText` carries source input only. No
  `TOKEN` placeholder, suppression, or threshold change is used; all 44 cases
  remain.
- Local receipts record sequential gates passing: `make check-fmt`, `make lint`,
  `make test` (1,209 passed), `make typecheck`, workflow contracts (16),
  `make markdownlint`, and `make nixie`; parent `make fmt` passed. Focused
  CodeScene reviews of both reserved-token Rust test files scored 10.0.
  CodeScene found no introduced findings in the working-tree delta from
  `353bb48`; its staged-delta check was empty. Logs:
  `/tmp/ddlint-pr354-repair-*`.
- Exact-head hosted acceptance passed:
  [build-test](https://github.com/leynos/ddlint/actions/runs/38302396367/job/114988225290)
  and [CodeScene](https://codescene.io/projects/69278/delta/results/7921317).
  Hosted logs verify 44 reserved-token unit tests, 15 integration tests, and 16
  workflow contracts. The
  [CodeRabbit assessment reply](https://github.com/leynos/ddlint/pull/354#issuecomment-6131906604)
  clears all six implementation dispositions; local receipts were supplied and
  its hosted logs verified. The assessment is not formal PR approval. At this
  receipt checkpoint, formal review and merge remained pending.

## Rebase receipt (2026-10-12)

- Rebased 14 original branch commits onto target
  `353bb48b4900bcaf9322246f3911c371c175140c`; 12 commits were replayed. Old
  head: `a4302fe21e3f14e5615a7dcad901c831ad5bc458`; old base:
  `02fe39d66e45196df173a602150454446b08f4e7`; replay head:
  `2cf5b8f9d4f32596db3dd308cca4cb404a9a3361`. The first branch commit,
  `5990a92`, is the original plan. There was no stack or squash parent.
- Recovery refs:
  `refs/recovery/ddlint-legacy-20261012-a4302fe/{head,base,target}`. Native
  `zdiff3` was selected because Weave selection was global-only.
- Conflict resolutions retain main's fallible `Option` helpers and test-body
  macros over the old panic helpers in `helpers.rs` and rule files; preserve
  main's `normalise_tokens` name and scoped spelling patterns; and omit
  obsolete commits `0492a26` (rename/broad-word spelling patch) and `324c6ca`
  (generated-only archive patch), already represented on main. Commit `5a2` now
  contributes guide whitespace only. The branch diagnostic commit replayed
  unchanged. The resulting branch diff contains 47 files against target
  `353bb48b4900bcaf9322246f3911c371c175140c`; the earlier 54-file scope count
  is retained as historical pre-rebase context.
- Main's current baseline includes `-Zthreads=8` and mold flags,
  `missing_docs` denial, the Python interrogate gate, tracked and untracked
  `mdtablefix` checks, and `typos-config-builder` v0.1.3 in place of the
  deleted scripts.
- **Earlier replay-head local acceptance passed.** Integration coverage
  passed (15), the targeted `expression_prefix` retry passed (33), and focused
  library tests passed (44). `make fmt`, `make check-fmt`, `make lint`,
  `make typecheck`, workflow contracts (16), `make markdownlint`, and
  `make nixie` passed. The first `make test` run had 720 passes and 9 Rayon
  initialization failures (`EAGAIN`); bounded with
  `RAYON_NUM_THREADS=2 RUST_TEST_THREADS=1`, `make test` passed. A prior
  compiler-wrapper invocation exited 1 without a supervisor refusal or
  established cause; its targeted retry passed. The supervisor/daemon task
  cgroup limit near 8192 is consistent with possible environmental pressure but
  does not establish the earlier cause. The prior test-side CodeScene score of
  9.68 and its String Heavy Function Arguments finding did not satisfy the
  hosted gate; the retain-string-fixtures decision is superseded by the typed
  remedy above. Logs: `/tmp/ddlint-rebase-*.out`.
- **Historical pre-publication status at the rebase checkpoint:** hosted review
  and checks were pending then. The implementation-head hosted acceptance is
  recorded above; earlier local results for old head `a4302fe…` remain
  historical and do not apply to replay head `2cf5b8f…`.

This continuation supplements the
[original 2.6.7 plan](./2-6-7-finalize-legacy-token-compatibility-policy.md).
Its implementation baseline includes semantic fixture and example migrations.
The user authorized reconciling that baseline and the narrow diagnostic fixes
below; no additional line-budget approval is implied. At the latest pre-format
checkpoint, branch scope counting this continuation plan was 54 files and net
+564 Rust lines from the merge base.

## Current obligations

- Keep `complete_reserved_token_errors` at the final `parse()` and
  `parse_expression()` boundaries, after all diagnostics at each boundary have
  been produced.
- Pre-rebase behavioural acceptance on old head `a4302fe…` passed: 44 focused
  unit tests and 15 integration tests cover reserved-token policy,
  deduplication, diagnostic spans, and parser recovery. Replay-head acceptance
  is recorded in the receipt above.
- Pre-rebase local checks passed on old head `a4302fe…`: `make check-fmt`,
  `make lint`, `make test`, `make typecheck`, `make test-workflow-contracts` (6
  passed), explicit continuation-plan lint, and `make nixie`. These results are
  historical; replay-head results are recorded above.
- At the pre-rebase checkpoint, the initial `make fmt` attempt failed with an
  infrastructural EAGAIN thread/fork error; retry passed with exit code 0 at
  `/tmp/ddlint-diagnostics-final-fmt-2929239.out`. `make check-fmt`,
  `make nixie`, and explicit continuation-plan lint passed. The latest
  `markdownlint` found four spelling errors, now corrected. This is pre-rebase
  history; replay-head documentation gates passed as recorded above.
- Pre-rebase CodeScene's test-file score and finding remain historical; the
  current correct-path test assessment is recorded in the receipt above.
- At this receipt checkpoint, formal PR review and merge remained pending; the
  CodeRabbit assessment above is implementation evidence, not formal approval.
- Logs:
  `/tmp/*-d0264bd8-554b-49e7-ab08-b42ea6541172-2-6-7-finalize-legacy-token-compatibility-policy-replacement*`.

Acceptance coverage includes top-level `for` deduplication; repeated, mixed,
and bare-hash expression spans; recovery to later declarations; legacy tokens
in parameter and relation fields; and legacy-token and generic parse-error
sentinels.

## Scope and evidence

The authorized follow-up is limited to the diagnostic-path placement,
deduplication, and acceptance work above. Do not infer approval for broader
parser changes or a larger code-line budget. Implementation and exact-head
hosted acceptance are recorded above. At that receipt checkpoint, formal review
and merge remained pending; current review and repair status is recorded in the
current follow-up. Do not claim full PR completion.

## Remote baseline

At commit `00d695e`, GitHub `build-test` reported **SUCCESS**
([job 95013385605](https://github.com/leynos/ddlint/actions/runs/31885199898/job/95013385605))
and CodeScene health reported **FAILURE**
([result 7771601](https://codescene.io/projects/69278/delta/results/7771601)).
These are historical starting-baseline results. Exact-head hosted results for
the implementation head are recorded above; prior-head checks remain separate
from formal review.
