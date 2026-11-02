"""Assert that every reachable Linux Rust suite proves its tool setup.

A suite must follow an unconditional pinned ``setup-rust`` step, and every
Make route must reach the provisioning target before its Rust suite.
"""

from __future__ import annotations

from collections.abc import Mapping

from build_provisioning_common import (
    MOLD_VERSION,
    PROVISIONING_TARGET,
    SETUP_RUST,
    BuildProvisioningError,
    MakeEvaluation,
    SuiteConsumer,
    _is_true,
    _mapping,
)
from build_provisioning_make import evaluate_make_goal
from build_provisioning_workflows import discover_linux_suites


def _soft_fails(node: Mapping[object, object]) -> bool:
    """Report whether a node can fail without failing its workflow."""
    return node.get("continue-on-error") not in (None, False, "false", "False")


def _is_bound_setup(step: object, context: str) -> bool:
    """Report whether a step is the unconditional pinned setup-rust this gate accepts."""
    setup = _mapping(step, context)
    if setup.get("uses") != SETUP_RUST or "if" in setup:
        return False
    if _soft_fails(setup):
        return False
    inputs = _mapping(setup.get("with", {}), f"{context}.setup-rust.with")
    return (
        _is_true(inputs.get("install-mold"))
        and inputs.get("mold-version", MOLD_VERSION) == MOLD_VERSION
    )


def _assert_job_provisioning(
    workflow_name: str,
    job_name: str,
    job_suites: list[SuiteConsumer],
    workflows: Mapping[str, Mapping[object, object]],
) -> None:
    """Require one job's suites to follow the bound setup and their Make preflight."""
    steps = _job_steps(workflow_name, job_name, workflows)
    _require_prior_setup(workflow_name, job_name, steps, job_suites)
    _require_make_preflights(job_suites)


def _job_steps(
    workflow_name: str,
    job_name: str,
    workflows: Mapping[str, Mapping[object, object]],
) -> list[object]:
    """Return one job's step list, refusing a non-list value."""
    job = _mapping(workflows[workflow_name]["jobs"], f"{workflow_name}.jobs")[job_name]
    steps = _mapping(job, f"{workflow_name}:{job_name}").get("steps", [])
    if not isinstance(steps, list):
        raise BuildProvisioningError(f"{workflow_name}:{job_name}.steps must be a list")
    return steps


def _require_prior_setup(
    workflow_name: str,
    job_name: str,
    steps: list[object],
    job_suites: list[SuiteConsumer],
) -> None:
    """Require an unconditional setup-rust step before the first suite step."""
    first_suite_index = min(
        suite.step_index for suite in job_suites if suite.step_index is not None
    )
    if not any(
        _is_bound_setup(step, f"{workflow_name}:{job_name}.steps[{index}]")
        for index, step in enumerate(steps[:first_suite_index])
    ):
        raise BuildProvisioningError(
            f"{workflow_name}:{job_name} needs unconditional prior setup-rust with mold {MOLD_VERSION}"
        )


def _require_make_preflights(job_suites: list[SuiteConsumer]) -> None:
    """Require every Make suite route to reach the provisioning target first."""
    for suite in job_suites:
        if suite.route == "make":
            evaluation = evaluate_make_goal(suite.make_goal)
            _require_make_preflight(evaluation, f"Make goal {suite.make_goal!r}")


def _require_make_preflight(evaluation: MakeEvaluation, context: str) -> None:
    """Require the evaluated preflight target to precede a Rust suite."""
    if not evaluation.suite_lines:
        raise BuildProvisioningError(f"{context} does not reach a Rust suite")
    if not evaluation.provisioning_lines or min(evaluation.provisioning_lines) >= min(evaluation.suite_lines):
        raise BuildProvisioningError(
            f"{context} must reach {PROVISIONING_TARGET} before its Rust suite"
        )


def assert_suite_provisioning(workflows: Mapping[str, Mapping[object, object]]) -> None:
    """Require each reachable Linux suite to have an unconditional prior setup.

    Parameters
    ----------
    workflows : Mapping[str, Mapping[object, object]]
        Workflow documents keyed by file name.

    Returns
    -------
    None
        Returns when every reachable Linux suite proves its tool setup.

    Raises
    ------
    BuildProvisioningError
        If discovery fails, or a job's suites lack an unconditional prior
        ``setup-rust`` step or the Make preflight they depend on.
    """
    grouped: dict[tuple[str, str], list[SuiteConsumer]] = {}
    for suite in discover_linux_suites(workflows):
        grouped.setdefault((suite.workflow, suite.job), []).append(suite)

    for (workflow_name, job_name), job_suites in grouped.items():
        if all(suite.route == "mutation-cargo" for suite in job_suites):
            continue
        _assert_job_provisioning(workflow_name, job_name, job_suites, workflows)


def assert_make_goal_provisioning(goal: str) -> MakeEvaluation:
    """Require one Make goal's evaluated Rust suite to reach tool preflight.

    Parameters
    ----------
    goal : str
        Make goal whose Rust suite must follow the provisioning check.

    Returns
    -------
    MakeEvaluation
        The evaluated trace used to prove the suite and preflight order.

    Raises
    ------
    BuildProvisioningError
        If GNU Make cannot evaluate the goal, or the goal reaches a Rust
        suite without first reaching the provisioning check.
    """
    evaluation = evaluate_make_goal(goal)
    _require_make_preflight(evaluation, f"Make goal {goal!r}")
    return evaluation
