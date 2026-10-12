# 2.6.7 legacy-token diagnostic follow-up

Status: LOCAL REPAIR ACCEPTANCE PASSED; hosted review and checks pending

## PR #354 CodeScene repair

- Base: `353bb48b4900bcaf9322246f3911c371c175140c`; candidate: `71ebe4c` plus
  supervisor-owned typed-fixture edits. Current diff: 48 files and net +662
  Rust lines, within the 800-line limit. The earlier 54-file count is
  historical. Runtime and API code are unchanged.
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
- Local gates passed sequentially: `make check-fmt`, `make lint`, `make test`
  (1,209 passed), `make typecheck`, workflow contracts (16),
  `make markdownlint`, and `make nixie`; `make fmt` passed on the parent.
  Focused CodeScene reviews of both reserved-token Rust test files scored 10.0.
  CodeScene reported no introduced findings for the working-tree delta from
  `353bb48`; its staged-delta check was empty. Logs:
  `/tmp/ddlint-pr354-repair-*`.
- Hosted review and checks remain pending. These are local results, not hosted
  success; publication and hosted verification are separate delivery gates.

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
- **Hosted review and checks remain pending.** Scrutineer's final report
  confirms the local gates passed; this is not hosted success or full PR
  completion. Earlier local results recorded in the original plan belong to old
  head `a4302fe…` and remain historical, not evidence for replay head
  `2cf5b8f…`.

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
- Obtain a new hosted review; prior-head remote results below do not validate
  the current head.
- Logs:
  `/tmp/*-d0264bd8-554b-49e7-ab08-b42ea6541172-2-6-7-finalize-legacy-token-compatibility-policy-replacement*`.

Acceptance coverage includes top-level `for` deduplication; repeated, mixed,
and bare-hash expression spans; recovery to later declarations; legacy tokens
in parameter and relation fields; and legacy-token and generic parse-error
sentinels.

## Scope and evidence

The authorized follow-up is limited to the diagnostic-path placement,
deduplication, and acceptance work above. Do not infer approval for broader
parser changes or a larger code-line budget. Replay-head local acceptance
passed as recorded above; hosted review and checks remain pending. Do not claim
full PR completion.

## Remote baseline

At commit `00d695e`, GitHub `build-test` reported **SUCCESS**
([job 95013385605](https://github.com/leynos/ddlint/actions/runs/31885199898/job/95013385605))
and CodeScene health reported **FAILURE**
([result 7771601](https://codescene.io/projects/69278/delta/results/7771601)).
These are the starting baseline only. The local review is recorded above; new
remote results for the current head are pending. Prior-head checks remain
separate from follow-up review.
