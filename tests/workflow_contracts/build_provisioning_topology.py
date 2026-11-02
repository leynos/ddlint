"""Walk the workflow topology and record every reachable Linux Rust suite.

Local reusable workflows are followed to their checked-in child, while the
two pinned external boundaries are classified explicitly so an unknown call
cannot hide a suite. Step-level routes come from ``build_provisioning_workflows``.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import PurePosixPath

from build_provisioning_common import (
    DEPENDABOT_AUTOMERGE,
    MUTATION_CARGO,
    BuildProvisioningError,
    SuiteConsumer,
    _is_true,
    _mapping,
)
from build_provisioning_matrix import linux_runner_rows
from build_provisioning_workflows import _step_suites


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


@dataclass(frozen=True)
class _JobRef:
    """Identify one job together with the workflow document that declares it."""

    workflow_name: str
    workflow: Mapping[object, object]
    name: str
    body: Mapping[object, object]

    def defaults_label(self) -> str:
        """Return the location of this job's own run defaults."""
        return f"{self.workflow_name}:{self.name}.defaults"


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
            self._visit_job(_job_ref(workflow_name, workflow, job_name, job_value), stack)

    def _visit_job(self, ref: _JobRef, stack: tuple[str, ...]) -> None:
        """Record one job's suites, descending into a local reusable workflow."""
        uses = ref.body.get("uses")
        if "uses" in ref.body and not isinstance(uses, str):
            raise BuildProvisioningError(f"{ref.workflow_name}:{ref.name}.uses must be text")
        if isinstance(uses, str):
            self.suites.extend(_reusable_job_routes(ref.workflow_name, ref.name, ref.body, uses))
            if uses.startswith("./"):
                self.visit(_local_workflow_name(uses), (*stack, ref.workflow_name))
            return
        self.suites.extend(_job_suites(ref))


def _job_ref(
    workflow_name: str,
    workflow: Mapping[object, object],
    job_name: object,
    job_value: object,
) -> _JobRef:
    """Build the reference for one job, refusing a non-text job name."""
    if not isinstance(job_name, str):
        raise BuildProvisioningError(f"{workflow_name} has a non-text job name {job_name!r}")
    job = _mapping(job_value, f"{workflow_name}:{job_name}")
    return _JobRef(workflow_name, workflow, job_name, job)


def _job_suites(ref: _JobRef) -> tuple[SuiteConsumer, ...]:
    """Return a job's suite routes when the job runs on a Linux runner."""
    has_directory = _has_working_directory(
        ref.workflow.get("defaults", {}), f"{ref.workflow_name}.defaults"
    ) or _has_working_directory(ref.body.get("defaults", {}), ref.defaults_label())
    job_suites = _step_suites(ref.workflow_name, ref.name, ref.body, has_directory)
    if not job_suites:
        return ()
    if not linux_runner_rows(ref.body, f"{ref.workflow_name}:{ref.name}"):
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
