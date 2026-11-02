"""Contract tests for the CI full-suite step binding.

The CI Test step must run `make test` exactly once, unconditionally, between
Lint and the workflow contract tests, and its job must not be gated.
"""

from __future__ import annotations

import copy
from collections.abc import Mapping
from typing import cast

import pytest

from build_provisioning import BuildProvisioningError, _soft_fails, read_repository_workflows


def _require_ci_full_suite_binding(
    workflows: Mapping[str, Mapping[object, object]],
) -> None:
    """Require CI's Test step to run once, unconditionally, between lint and contracts."""
    workflow = workflows.get("ci.yml")
    if not isinstance(workflow, Mapping):
        raise BuildProvisioningError("CI workflow is missing from the strict reader result")
    jobs = workflow.get("jobs")
    if not isinstance(jobs, Mapping):
        raise BuildProvisioningError("CI workflow jobs must be a mapping")
    job = jobs.get("build-test")
    if not isinstance(job, Mapping):
        raise BuildProvisioningError("CI build-test job is missing")
    steps = job.get("steps")
    if not isinstance(steps, list):
        raise BuildProvisioningError("CI build-test steps must be a list")

    test_index = _ordered_stage_indices(steps)
    _require_sole_make_test_step(steps, test_index)
    _require_unconditional_job(job)


def _ordered_stage_indices(steps: list[object]) -> int:
    """Require one Lint, Test and contracts step in that order, returning Test."""
    wanted = ("Lint", "Test", "Workflow contract tests")
    positions = {name: _stage_positions(steps, name) for name in wanted}
    _require_one_of_each(positions)
    lint_index, test_index, contracts_index = (positions[name][0] for name in wanted)
    _require_stage_order(lint_index, test_index, contracts_index)
    return test_index


def _stage_positions(steps: list[object], name: str) -> list[int]:
    """Return the indices of steps carrying one stage name."""
    return [
        index
        for index, step in enumerate(steps)
        if isinstance(step, Mapping) and step.get("name") == name
    ]


def _require_one_of_each(positions: Mapping[str, list[int]]) -> None:
    """Require exactly one step for every named stage."""
    if any(len(indices) != 1 for indices in positions.values()):
        raise BuildProvisioningError("CI needs one each of Lint, Test and workflow contracts")


def _require_stage_order(lint_index: int, test_index: int, contracts_index: int) -> None:
    """Require Test to follow Lint and precede the workflow contracts."""
    if not lint_index < test_index < contracts_index:
        raise BuildProvisioningError("CI Test must follow Lint and precede workflow contracts")


def _require_sole_make_test_step(steps: list[object], test_index: int) -> None:
    """Require the single `make test` step to be the unconditional named Test step."""
    _require_unconditional_test_step(steps[test_index])
    _require_single_make_test_step(steps, test_index)


def _is_gated(step: Mapping[object, object]) -> bool:
    """Report whether a step is conditional, an action call or soft-failing."""
    return "uses" in step or "if" in step or _soft_fails(step)


def _require_unconditional_test_step(test_step: object) -> None:
    """Require the named Test step to run `make test` without soft failure."""
    if not isinstance(test_step, Mapping) or test_step.get("run") != "make test":
        raise BuildProvisioningError("CI Test must run make test without soft failure")
    if _is_gated(test_step):
        raise BuildProvisioningError("CI Test must run make test without soft failure")


def _require_single_make_test_step(steps: list[object], test_index: int) -> None:
    """Require exactly one `make test` step, at the named Test position."""
    run_indices = [
        index
        for index, step in enumerate(steps)
        if isinstance(step, Mapping) and step.get("run") == "make test"
    ]
    if run_indices != [test_index]:
        raise BuildProvisioningError("CI must have exactly one make test step named Test")


def _require_unconditional_job(job: Mapping[object, object]) -> None:
    """Require the CI job itself to be neither conditional nor soft-failing."""
    if "if" in job or _soft_fails(job):
        raise BuildProvisioningError("CI build-test must not be conditional or soft-fail")


def test_ci_full_suite_is_bound_to_one_unconditional_make_test_step() -> None:
    """The approved CI suite is one unconditional make test after lint."""
    _require_ci_full_suite_binding(read_repository_workflows())


@pytest.mark.parametrize(
    "mutation",
    ["removed", "step-if-false", "step-continue-on-error", "job-if-false", "job-continue-on-error"],
)
def test_ci_full_suite_cannot_be_removed_or_soft_skipped(mutation: str) -> None:
    """Removing or gating the full Test step breaks the CI binding contract."""
    workflows = copy.deepcopy(read_repository_workflows())
    workflow = cast("dict[object, object]", workflows["ci.yml"])
    jobs = cast("dict[object, object]", workflow["jobs"])
    job = cast("dict[object, object]", jobs["build-test"])
    steps = cast("list[object]", job["steps"])
    test_index = next(
        index for index, value in enumerate(steps)
        if isinstance(value, Mapping) and value.get("name") == "Test"
    )
    test_step = cast("dict[object, object]", steps[test_index])
    if mutation == "removed":
        steps.pop(test_index)
    elif mutation == "step-if-false":
        test_step["if"] = False
    elif mutation == "step-continue-on-error":
        test_step["continue-on-error"] = True
    elif mutation == "job-if-false":
        job["if"] = False
    else:
        job["continue-on-error"] = True
    with pytest.raises(BuildProvisioningError):
        _require_ci_full_suite_binding(workflows)
