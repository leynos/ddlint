"""Shared model, constants and reader for the provisioning workflow contracts.

Discovery, Make evaluation and the provisioning assertions all speak in these
types and pinned values, so they live here rather than in any one of them.
CV-005 remains the strict workflow reader and canonical policy owner.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import cast

from cv005_contracts.loading import read_workflows

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS_DIRECTORY = ROOT / ".github" / "workflows"
# Keep this in step with the single full-SHA pin the workflows use. Dependabot
# moves that pin as one group, so a stale copy here fails these cases loudly
# instead of silently accepting a mixed set of revisions.
APPROVED_SHARED_ACTIONS_REF = "872f59f53717b5bebe8e24b2f4a7496b089a939a"
SHARED_ACTION_PREFIX = "leynos/shared-actions/.github"
SETUP_RUST = f"{SHARED_ACTION_PREFIX}/actions/setup-rust@{APPROVED_SHARED_ACTIONS_REF}"
MUTATION_CARGO = f"{SHARED_ACTION_PREFIX}/workflows/mutation-cargo.yml@{APPROVED_SHARED_ACTIONS_REF}"
DEPENDABOT_AUTOMERGE = f"{SHARED_ACTION_PREFIX}/workflows/dependabot-automerge.yml@{APPROVED_SHARED_ACTIONS_REF}"
COVERAGE_ACTION = f"{SHARED_ACTION_PREFIX}/actions/generate-coverage@{APPROVED_SHARED_ACTIONS_REF}"
PROVISIONING_TARGET = "check-build-tools"
# The pinned mold release this repository provisions, read from its own record
# so the workflow contract and the Makefile installer cannot drift apart.
MOLD_VERSION = (ROOT / "tools" / "mold" / "VERSION").read_text(encoding="utf-8").strip()


class BuildProvisioningError(AssertionError):
    """Raised when a reachable Linux Rust suite has no proven tool setup."""


@dataclass(frozen=True)
class SuiteConsumer:
    """Identify one Rust suite route reachable from a repository workflow."""

    workflow: str
    job: str
    route: str
    step_index: int | None
    make_goal: str | None = None


@dataclass(frozen=True)
class MakeEvaluation:
    """Keep the evaluated Make trace needed by this consumer contract."""

    suite_lines: tuple[int, ...]
    provisioning_lines: tuple[int, ...]


@dataclass(frozen=True)
class StepLocation:
    """Identify one workflow step for suite records and failure messages."""

    workflow: str
    job: str
    index: int

    @property
    def label(self) -> str:
        """Return the step's location as it appears in failure messages."""
        return f"{self.workflow}:{self.job}.steps[{self.index}]"


def read_repository_workflows() -> dict[str, dict[object, object]]:
    """Read every checked-in workflow through the pinned CV-005 reader.

    Returns
    -------
    dict[str, dict[object, object]]
        Workflow documents keyed by file name.

    Raises
    ------
    WorkflowReadingError
        If the workflow directory cannot be listed, holds no workflow, or a
        workflow cannot be read or parsed.
    """
    return cast("dict[str, dict[object, object]]", read_workflows(WORKFLOWS_DIRECTORY))


def _mapping(value: object, context: str) -> Mapping[object, object]:
    """Return a workflow mapping or fail closed with its location."""
    if not isinstance(value, Mapping):
        raise BuildProvisioningError(f"{context} must be a mapping, got {value!r}")
    return value


def _is_true(value: object) -> bool:
    """Accept YAML booleans and GitHub's quoted true input spelling."""
    return value is True or value in ("true", "True", "TRUE")
