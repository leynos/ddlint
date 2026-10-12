# 2.6.7 legacy-token diagnostic follow-up

Status: LOCAL ACCEPTANCE PASSED — remote review pending

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
- Local behavioural acceptance passed on 2026-10-12: 44 focused unit tests and
  15 integration tests cover reserved-token policy, deduplication, diagnostic
  spans, and parser recovery.
- Local checks passed on 2026-10-12: `make check-fmt`, `make lint`, `make test`,
  `make typecheck`, `make test-workflow-contracts` (6 passed), explicit
  continuation-plan lint, and `make nixie`.
- The initial `make fmt` attempt failed with an infrastructural EAGAIN
  thread/fork error; retry passed with exit code 0 at
  `/tmp/ddlint-diagnostics-final-fmt-2929239.out`. `make check-fmt`,
  `make nixie`, and explicit continuation-plan lint passed. The latest
  `markdownlint` found four spelling errors, now corrected. Final documentation
  validation is required before publication; remote review remains pending.
- Local CodeScene scored `9.68`, indicated no duplication, and left only String
  Heavy Function Arguments from parameterized string-based parser fixtures.
  This is a local disposition only.
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
parser changes or a larger code-line budget. Local behavioural acceptance and
final formatting have passed. Final documentation validation is required before
publication; remote review remains pending. Do not claim full PR completion.

## Remote baseline

At commit `00d695e`, GitHub `build-test` reported **SUCCESS**
([job 95013385605](https://github.com/leynos/ddlint/actions/runs/31885199898/job/95013385605))
and CodeScene health reported **FAILURE**
([result 7771601](https://codescene.io/projects/69278/delta/results/7771601)).
These are the starting baseline only. The local review is recorded above; new
remote results for the current head are pending. Prior-head checks remain
separate from follow-up review.
