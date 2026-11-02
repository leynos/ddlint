"""Resolve a workflow job's runner matrix to its finite legacy rows.

Only a single finite axis is supported; matrix modifiers and empty axes fail
closed, so an unsupported matrix cannot erase a suite from the audit. Runner
label classification lives in ``build_provisioning_runners``.
"""

from __future__ import annotations

from collections.abc import Mapping
import re

from build_provisioning_common import BuildProvisioningError, _mapping

MATRIX_VALUE = re.compile(r"^\$\{\{\s*matrix\.([A-Za-z0-9_-]+)\s*\}\}$")


def _matrix_rows(job: Mapping[object, object], context: str) -> tuple[dict[str, object], ...]:
    """Resolve a finite single-axis matrix used to select suite runners."""
    strategy = _mapping(job.get("strategy", {}), f"{context}.strategy")
    if "matrix" not in strategy:
        return ({},)
    matrix = _mapping(strategy["matrix"], f"{context}.strategy.matrix")
    name, values = _single_finite_axis(matrix, context)
    return tuple({name: value} for value in values)


def _single_finite_axis(
    matrix: Mapping[object, object], context: str
) -> tuple[str, list[object]]:
    """Require one finite non-empty matrix axis without include or exclude."""
    _reject_matrix_modifiers(matrix, context)
    if len(matrix) != 1:
        raise BuildProvisioningError(f"{context} suite matrix must have one finite axis")
    name, values = next(iter(matrix.items()))
    return _resolved_axis(name, values, context)


def _reject_matrix_modifiers(matrix: Mapping[object, object], context: str) -> None:
    """Refuse matrix include and exclude entries this contract cannot resolve."""
    if any(key in matrix for key in ("include", "exclude")):
        raise BuildProvisioningError(f"{context} suite matrix include/exclude is unsupported")


def _resolved_axis(name: object, values: object, context: str) -> tuple[str, list[object]]:
    """Return a text axis name with its non-empty list of values."""
    match name, values:
        case str() as axis, list() as items if items:
            return axis, items
        case _:
            raise BuildProvisioningError(f"{context} has an unresolved matrix axis {name!r}")