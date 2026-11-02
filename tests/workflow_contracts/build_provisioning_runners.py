"""Classify resolved runner labels as Linux or another operating system.

Every resolved label must name a known operating system, so an unrecognized
runner cannot hide a suite from the provisioning audit.
"""

from __future__ import annotations

from collections.abc import Mapping
import re

from build_provisioning_common import BuildProvisioningError
from build_provisioning_matrix import MATRIX_VALUE, _matrix_rows

RUNNER_OS = re.compile(r"(?:^|[^a-z])(ubuntu|linux|windows|macos|osx)(?:-|$)")


def linux_runner_rows(job: Mapping[object, object], context: str) -> tuple[dict[str, object], ...]:
    """Return Linux legs and reject runner labels whose operating system is unknown.

    Parameters
    ----------
    job : Mapping[object, object]
        Workflow job whose matrix selects the runner legs.
    context : str
        Location used in failure messages.

    Returns
    -------
    tuple[dict[str, object], ...]
        Resolved matrix rows that select a Linux runner.

    Raises
    ------
    BuildProvisioningError
        If the job has no ``runs-on`` value, its matrix is unsupported, or a
        resolved runner label names no operating system or conflicting ones.
    """
    if "runs-on" not in job:
        raise BuildProvisioningError(f"{context} has a suite but no runs-on value")
    runner = job["runs-on"]
    return tuple(
        row for row in _matrix_rows(job, context) if _is_linux_runner(runner, row, context)
    )


def _is_linux_runner(runner: object, row: Mapping[str, object], context: str) -> bool:
    """Report whether one resolved matrix row selects Linux, refusing conflicts."""
    labels = _runner_labels(runner, row, context)
    systems = {match.group(1) for label in labels if (match := RUNNER_OS.search(label.lower()))}
    if not systems:
        raise BuildProvisioningError(f"{context} has an unclassified runner {labels!r}")
    if systems & {"ubuntu", "linux"} and systems & {"windows", "macos", "osx"}:
        raise BuildProvisioningError(f"{context} has conflicting runner systems {labels!r}")
    return bool(systems & {"ubuntu", "linux"})


def _runner_label(value: str, row: Mapping[str, object], context: str) -> str:
    """Resolve one runner label, following a matrix reference when present."""
    expression = MATRIX_VALUE.fullmatch(value)
    if expression:
        return _matrix_runner_label(expression.group(1), row, context)
    if "${{" in value or "}}" in value:
        raise BuildProvisioningError(f"{context} has an unresolved runner expression {value!r}")
    return value


def _matrix_runner_label(name: str, row: Mapping[str, object], context: str) -> str:
    """Resolve a matrix-referenced runner label to the value in its row."""
    resolved = row.get(name)
    if not isinstance(resolved, str):
        raise BuildProvisioningError(f"{context} cannot resolve matrix runner {name!r}")
    return resolved


def _runner_labels(value: object, row: Mapping[str, object], context: str) -> tuple[str, ...]:
    """Resolve runner labels from scalar, list, or group-and-label mappings."""
    if isinstance(value, str):
        return (_runner_label(value, row, context),)
    if isinstance(value, list):
        labels: list[str] = []
        for item in value:
            labels.extend(_runner_labels(item, row, context))
        return tuple(labels)
    if isinstance(value, Mapping) and "labels" in value:
        return _runner_labels(value["labels"], row, context)
    raise BuildProvisioningError(f"{context}.runs-on has unsupported shape {value!r}")