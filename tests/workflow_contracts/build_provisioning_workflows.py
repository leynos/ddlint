"""Record the Rust suite routes declared directly by a workflow job's steps.

Coverage actions, direct Cargo invocations and evaluated Make goals are read
here; the topology walk that reaches these steps lives in
``build_provisioning_topology``.
"""

from __future__ import annotations

from collections.abc import Mapping

from build_provisioning_common import (
    COVERAGE_ACTION,
    BuildProvisioningError,
    StepLocation,
    SuiteConsumer,
    _mapping,
)
from build_provisioning_make import _cargo_routes, _make_goals, evaluate_make_goal


def _step_suites(
    workflow_name: str,
    job_name: str,
    job: Mapping[object, object],
    has_default_working_directory: bool,
) -> tuple[SuiteConsumer, ...]:
    """Return suite steps found through direct commands and evaluated Make."""
    steps_value = job.get("steps", [])
    if not isinstance(steps_value, list):
        raise BuildProvisioningError(f"{workflow_name}:{job_name}.steps must be a list")
    suites: list[SuiteConsumer] = []
    for index, step_value in enumerate(steps_value):
        location = StepLocation(workflow_name, job_name, index)
        step = _mapping(step_value, location.label)
        suites.extend(_coverage_suite(location, step))
        suites.extend(_run_step_suites(location, step, has_default_working_directory))
    return tuple(suites)


def _coverage_suite(
    location: StepLocation, step: Mapping[object, object]
) -> tuple[SuiteConsumer, ...]:
    """Return the coverage route for a step, refusing any unpinned variant."""
    uses = step.get("uses")
    if not isinstance(uses, str) or not uses.startswith(COVERAGE_ACTION.rsplit("@", 1)[0]):
        return ()
    if uses != COVERAGE_ACTION:
        raise BuildProvisioningError(
            f"{location.workflow}:{location.job} uses an unapproved coverage action pin"
        )
    return (SuiteConsumer(location.workflow, location.job, "coverage-action", location.index),)


def _run_step_suites(
    location: StepLocation,
    step: Mapping[object, object],
    has_default_working_directory: bool,
) -> tuple[SuiteConsumer, ...]:
    """Return the direct and Make suite routes declared by one `run` step."""
    run = step.get("run")
    if run is None:
        return ()
    if not isinstance(run, str):
        raise BuildProvisioningError(f"{location.label}.run must be text")
    direct = tuple(
        SuiteConsumer(location.workflow, location.job, route, location.index)
        for route in _cargo_routes(run, location.label)
    )
    return direct + _make_step_suites(location, step, run, has_default_working_directory)


def _make_step_suites(
    location: StepLocation,
    step: Mapping[object, object],
    run: str,
    has_default_working_directory: bool,
) -> tuple[SuiteConsumer, ...]:
    """Return the Make suite routes declared by one `run` step."""
    goals = _make_goals(run)
    _reject_custom_working_directory(location, step, goals, has_default_working_directory)
    return tuple(
        SuiteConsumer(location.workflow, location.job, "make", location.index, goal)
        for goal in goals
        if evaluate_make_goal(goal).suite_lines
    )


def _reject_custom_working_directory(
    location: StepLocation,
    step: Mapping[object, object],
    goals: tuple[str, ...],
    has_default_working_directory: bool,
) -> None:
    """Refuse a Make call that could run outside the repository root."""
    has_step_directory = "working-directory" in step
    if goals and (has_default_working_directory or has_step_directory):
        raise BuildProvisioningError(
            f"{location.workflow}:{location.job} Make call has a custom working directory"
        )
