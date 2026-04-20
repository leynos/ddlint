# Implement context variable lifecycle for correlation IDs

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & discoveries`, `Decision log`, and
`Outcomes & retrospective` must be kept up to date as work proceeds.

Status: DRAFT

PLANS.md is not present in this repository.

## Purpose / big picture

Implement lifecycle-safe contextual storage for correlation IDs using Python
`contextvars`, aligned with roadmap item 2.4.2 and design-doc section 3.3.4.
After this change, middleware request handling sets a correlation ID at the
start of `process_request`, preserves a reset token for deterministic cleanup,
and always resets in `process_response`, including failure paths. Success is
observable through unit tests (`pytest`), behavioural tests (`pytest-bdd`),
and full project quality gates.

## Constraints

- Implement the behaviour requested for 2.4.2 exactly:
  - set correlation ID in `process_request`;
  - store reset token for cleanup;
  - reset context variable in `process_response`;
  - guarantee cleanup when request processing fails.
- Keep public consumer behaviour stable except for explicitly documented
  correlation ID lifecycle semantics.
- Record design decisions in the design document named by the work item.
- Update `docs/users-guide.md` with any user-visible behaviour or API changes.
- Mark the roadmap item as complete only after all quality gates pass.
- Use Makefile targets and capture command output via `tee` with
  `set -o pipefail`.
- Do not add dependencies unless required and documented.

## Tolerances (exception triggers)

- Scope: if implementation requires touching more than 10 source files or more
  than 350 net new lines, stop and escalate.
- API surface: if public middleware constructor signatures or integration
  points must change, stop and escalate.
- Missing references: if roadmap section 2.4.2, design-doc section 3.3.4, or
  `docs/users-guide.md` cannot be located in the target codebase, stop and
  resolve source-of-truth paths before coding.
- Tooling mismatch: if `make typecheck` is not defined, stop and agree on the
  canonical type-check command before marking the work complete.
- Test instability: if test failures persist after two focused fix iterations,
  stop and escalate with captured logs.

## Risks

- Risk: cleanup logic may be skipped on exception paths, leaking correlation
  IDs across requests. Mitigation: centralize token handling and add explicit
  failure-path tests.
- Risk: concurrent request handling may expose context bleed between tasks.
  Mitigation: add isolation tests with parallel/concurrent request execution.
- Risk: behavioural tests may pass while token lifecycle is still wrong
  internally. Mitigation: add direct unit assertions around token set/reset.
- Risk: documentation drift between design doc, users guide, and roadmap.
  Mitigation: update all three in one change and validate before completion.

## Progress

- [x] (2026-02-20 00:00Z) Drafted ExecPlan for 2.4.2 with explicit
  implementation, test, docs, and validation milestones.
- [ ] Align referenced roadmap/design/users-guide paths in target codebase.
- [ ] Add failing unit and behavioural tests for lifecycle and isolation.
- [ ] Implement middleware contextvar lifecycle changes.
- [ ] Update design and user documentation, then mark roadmap item complete.
- [ ] Run and pass all required validation gates.

## Surprises & discoveries

- The current checked-out repository appears to be a Rust parser project and
  does not currently expose the referenced Falcon middleware docs or files.
  Execution must begin with a path alignment step to locate the intended
  Python middleware codebase sections before implementation.

## Decision log

- Decision: include an explicit pre-implementation alignment milestone as the
  first execution step.
  Rationale: the requested roadmap/design references and Python middleware
  symbols are not currently discoverable in this checkout, so execution must
  avoid guessing paths.
  Date/Author: 2026-02-20 (assistant)

## Outcomes & retrospective

Not executed yet. This section must be completed after implementation and
validation, including final test evidence and any follow-on refactor notes.

## Context and orientation

This plan targets middleware with `process_request` and `process_response`
hooks, where a correlation ID is stored in a `ContextVar` during request
handling and reset deterministically at request completion. The change requires
both correctness under success paths and robust cleanup under exceptions.

Expected implementation touchpoints in the target codebase:

- Middleware module containing `process_request` and `process_response`.
- Context storage module defining the correlation ID `ContextVar`.
- Unit test suite (`pytest`) covering middleware internals.
- Behavioural test suite (`pytest-bdd`) covering request lifecycle behaviour.
- `docs/falcon-correlation-id-middleware-design.md` (or equivalent design doc).
- `docs/users-guide.md`.
- `docs/roadmap.md`.

## Plan of work

Stage A: align source-of-truth references.

Locate the exact files and sections for roadmap 2.4.2 and design-doc 3.3.4,
and confirm where `process_request`, `process_response`, and correlation ID
context storage are implemented. If paths differ from this plan, update this
ExecPlan first, then proceed.

Stage B: add failing tests first.

Add `pytest` unit tests that prove:

- correlation ID is set during `process_request`;
- reset token is captured and retained for cleanup;
- `process_response` resets the context variable;
- cleanup also occurs when request handling fails.

Add `pytest-bdd` scenarios that prove:

- request-scoped correlation ID visibility during processing;
- no residual correlation ID after response completion;
- isolation between concurrent requests.

Stage C: implement lifecycle-safe middleware logic.

Update middleware so `process_request` sets the correlation ID via
`ContextVar.set(...)` and stores the returned token in request context state.
Update `process_response` to call `ContextVar.reset(token)` and clear stored
token state. Ensure failure paths invoke the same cleanup logic, either through
framework error hooks or a shared finalization path.

Stage D: document behaviour and decisions.

Record lifecycle decisions and failure-path semantics in the design document
section for contextvar lifecycle (section 3.3.4). Update `docs/users-guide.md`
with user-facing behaviour and integration expectations (for example,
request-scope visibility and cleanup guarantees). Mark roadmap item 2.4.2
complete once all validation passes.

Stage E: run quality gates and capture evidence.

Run required commands from repository root:

    set -o pipefail && make check-fmt 2>&1 | tee /tmp/2-4-2-check-fmt.log
    set -o pipefail && make typecheck 2>&1 | tee /tmp/2-4-2-typecheck.log
    set -o pipefail && make lint 2>&1 | tee /tmp/2-4-2-lint.log
    set -o pipefail && make test 2>&1 | tee /tmp/2-4-2-test.log

If docs are changed and project policy requires doc validation, also run:

    set -o pipefail && make markdownlint 2>&1 | tee /tmp/2-4-2-markdownlint.log

## Concrete steps

1. Locate middleware and context storage modules and update this plan with exact
   file paths.
2. Add failing `pytest` unit tests for set, token storage, reset, and
   failure-path cleanup.
3. Add failing `pytest-bdd` scenarios for in-request visibility, post-response
   clearing, and concurrent isolation.
4. Implement `process_request` contextvar set + token persistence.
5. Implement `process_response` reset + token cleanup.
6. Wire cleanup into failure paths so reset always occurs.
7. Make tests pass and keep assertions stable and deterministic.
8. Update design doc section 3.3.4 with lifecycle rationale and decisions.
9. Update `docs/users-guide.md` for consumer-facing behaviour/API details.
10. Mark roadmap item 2.4.2 complete in `docs/roadmap.md`.
11. Run all required Makefile quality gates with `tee` logging.
12. Record final evidence and outcomes in this ExecPlan.

## Validation and acceptance

Acceptance is met when all conditions below are true:

- Unit tests prove contextvar value is set during request processing.
- Unit tests prove contextvar value is reset after response processing.
- Unit tests prove reset runs on failure paths.
- Behavioural tests prove isolation between concurrent requests.
- `make check-fmt`, `make typecheck`, `make lint`, and `make test` pass.
- Design doc and users guide reflect final behaviour.
- Roadmap item 2.4.2 is checked off as complete.

## Idempotence and recovery

All plan stages are re-runnable. If partial implementation leaves token cleanup
inconsistent, revert only the incomplete lifecycle path and re-apply changes
with tests first. If validation fails, fix one failure class at a time and
re-run the failed command, then re-run the full gate set.
