"""Walk the workflow topology and record every reachable Linux Rust suite.

Local reusable workflows are followed to their checked-in child, while the
two pinned external boundaries are classified explicitly so an unknown call
cannot hide a suite.
"""

from __future__ import annotations

from collections.abc import Mapping
from pathlib import PurePosixPath

from build_provisioning_common import (
    COVERAGE_ACTION,
    DEPENDABOT_AUTOMERGE,
    MUTATION_CARGO,
    BuildProvisioningError,
    StepLocation,
    SuiteConsumer,
    _is_true,
    _mapping,
)
from build_provisioning_make import _cargo_routes, _make_goals, evaluate_make_goal
from build_provisioning_matrix import linux_runner_rows


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
    if goals and (has_default_working_directory or "working-directory" in step):
        raise BuildProvisioningError(
            f"{location.workflow}:{location.job} Make call has a custom working directory"
        )
    return tuple(
        SuiteConsumer(location.workflow, location.job, "make", location.index, goal)
        for goal in goals
        if evaluate_make_goal(goal).suite_lines
    )


def _has_working_directory(defaults: object, context: str) -> bool:
    """Check workflow or job run defaults for a custom Make working directory."""
    run = _mapping(_mapping(defaults, context).get("run", {}), f"{context}.run")
    return "working-directory" in run


def _local_workflow_name(uses: str) -> str:
    """Resolve a local reusable workflow and refuse paths outside the tree."""
    path = PurePosixPath(uses)
    if (
        not uses.startswith("./.github/workflows/")
        or len(path.parts) != 3
        or path.suffix.casefold() not in {".yml", ".yaml"}
    ):
        raise BuildProvisioningError(f"invalid local reusable workflow {uses!r}")
    return path.name


def _reusable_job_routes(
    workflow_name: str,
    job_name: str,
    job: Mapping[object, object],
    uses: str,
) -> tuple[SuiteConsumer, ...]:
    """Classify a reusable-workflow job, rejecting unknown or unpinned calls."""
    if uses.startswith("./"):
        return ()
    if uses == MUTATION_CARGO:
        inputs = _mapping(job.get("with", {}), f"{workflow_name}:{job_name}.with")
        if not _is_true(inputs.get("install-mold")):
            raise BuildProvisioningError(
                f"{workflow_name}:{job_name} must pass install-mold: true"
            )
        return (SuiteConsumer(workflow_name, job_name, "mutation-cargo", None),)
    if uses == DEPENDABOT_AUTOMERGE:
        # This immutable shared workflow calls the GitHub API and has no consumer checkout.
        return ()
    raise BuildProvisioningError(
        f"{workflow_name}:{job_name} has an unknown reusable workflow {uses!r}"
    )


class _SuiteCollector:
    """Walk the workflow topology once, following local reusable calls."""

    def __init__(self, workflows: Mapping[str, Mapping[object, object]]) -> None:
        """Record the topology that later visits resolve reusable calls against."""
        self._workflows = workflows
        self.suites: list[SuiteConsumer] = []

    def visit(self, workflow_name: str, stack: tuple[str, ...]) -> None:
        """Visit one workflow and every local reusable workflow it calls."""
        if workflow_name in stack:
            raise BuildProvisioningError(
                f"local reusable workflow cycle: {(*stack, workflow_name)!r}"
            )
        workflow = self._workflows.get(workflow_name)
        if workflow is None:
            raise BuildProvisioningError(f"local reusable workflow {workflow_name!r} is missing")
        jobs = _mapping(workflow.get("jobs"), f"{workflow_name}.jobs")
        if not jobs:
            raise BuildProvisioningError(f"{workflow_name}.jobs is empty")
        for job_name, job_value in jobs.items():
            self._visit_job(workflow_name, workflow, job_name, job_value, stack)

    def _visit_job(
        self,
        workflow_name: str,
        workflow: Mapping[object, object],
        job_name: object,
        job_value: object,
        stack: tuple[str, ...],
    ) -> None:
        """Record one job's suites, descending into a local reusable workflow."""
        if not isinstance(job_name, str):
            raise BuildProvisioningError(f"{workflow_name} has a non-text job name {job_name!r}")
        job = _mapping(job_value, f"{workflow_name}:{job_name}")
        uses = job.get("uses")
        if "uses" in job and not isinstance(uses, str):
            raise BuildProvisioningError(f"{workflow_name}:{job_name}.uses must be text")
        if isinstance(uses, str):
            self.suites.extend(_reusable_job_routes(workflow_name, job_name, job, uses))
            if uses.startswith("./"):
                self.visit(_local_workflow_name(uses), (*stack, workflow_name))
            return
        self.suites.extend(_job_suites(workflow_name, job_name, workflow, job))


def _job_suites(
    workflow_name: str,
    job_name: str,
    workflow: Mapping[object, object],
    job: Mapping[object, object],
) -> tuple[SuiteConsumer, ...]:
    """Return a job's suite routes when the job runs on a Linux runner."""
    has_directory = _has_working_directory(
        workflow.get("defaults", {}), f"{workflow_name}.defaults"
    ) or _has_working_directory(job.get("defaults", {}), f"{workflow_name}:{job_name}.defaults")
    job_suites = _step_suites(workflow_name, job_name, job, has_directory)
    if not job_suites:
        return ()
    if not linux_runner_rows(job, f"{workflow_name}:{job_name}"):
        return ()
    return job_suites


def discover_linux_suites(
    workflows: Mapping[str, Mapping[object, object]],
) -> tuple[SuiteConsumer, ...]:
    """Find Linux suite routes, resolving every local reusable workflow call.

    Parameters
    ----------
    workflows : Mapping[str, Mapping[object, object]]
        Workflow documents keyed by file name.

    Returns
    -------
    tuple[SuiteConsumer, ...]
        Distinct Linux Rust suite routes reachable from the topology.

    Raises
    ------
    BuildProvisioningError
        If no topology is supplied, a reusable workflow call cannot be
        resolved, or the topology exposes no Linux Rust suite.
    """
    if not workflows:
        raise BuildProvisioningError("no workflow topology was supplied")
    collector = _SuiteCollector(workflows)
    for name in workflows:
        collector.visit(name, ())
    if not collector.suites:
        raise BuildProvisioningError("no Linux Rust suite was found in the workflow topology")
    return tuple(dict.fromkeys(collector.suites))
