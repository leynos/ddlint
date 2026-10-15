"""Guard this repository's direct uv cache inputs, not shared cache policy.

Python tool requirements live inline in Make rather than a dependency manifest.
These private helpers serve this caller contract only. Path.glob interprets
positive repository-relative patterns; intersecting its results with git's
tracked paths excludes machine-local and untracked files from the evidence.
Run through the existing ``make test-workflow-contracts`` target.
"""

from __future__ import annotations

from copy import deepcopy
from pathlib import Path
import subprocess
from typing import Any

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ".github/workflows/ci.yml"
# This caller's cache input was verified at this immutable action revision.
# A pin update must verify its interface and update this contract together;
# this is a consumer interface check, not the estate's action allowlist.
VERIFIED_SETUP_UV = "astral-sh/setup-uv@c18668ad3cf93ea998bef934396af7bb5c839dc7"
PATTERNS = (
    "Makefile",
    WORKFLOW,
    "tests/workflow_contracts/**/*.py",
    "scripts/**/*.py",
)


class UniqueKeyLoader(yaml.SafeLoader):
    """Reject duplicate workflow mappings instead of silently overwriting them."""


def _unique_mapping(loader: UniqueKeyLoader, node: yaml.MappingNode) -> dict[object, Any]:
    """Read one unambiguous workflow mapping."""
    mapping = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if key in mapping:
            raise ValueError(f"duplicate workflow key: {key}")
        mapping[key] = loader.construct_object(value_node)
    return mapping


UniqueKeyLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _unique_mapping
)


def _direct_step(document: dict[str | bool, Any]) -> dict[str, Any]:
    """Locate the actual direct action, without inspecting nested Setup Rust."""
    steps = [
        step
        for job in document["jobs"].values()
        for step in job.get("steps", [])
        if str(step.get("uses", "")).startswith("astral-sh/setup-uv@")
    ]
    assert len(steps) == 1, "CI must contain exactly one direct Setup uv step"
    step = steps[0]
    assert step["uses"] == VERIFIED_SETUP_UV, "Setup uv must use the verified cache-input interface"
    return step


@pytest.fixture
def workflow() -> dict[str | bool, Any]:
    """Read the actual workflow with duplicate-key rejection."""
    return yaml.load((ROOT / WORKFLOW).read_text(encoding="utf-8"), Loader=UniqueKeyLoader)


@pytest.fixture
def tracked_paths() -> set[str]:
    """Read this checkout's tracked paths without compiling or changing it."""
    result = subprocess.run(
        ["/usr/bin/git", "ls-files", "-z"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    return set(result.stdout.rstrip("\0").split("\0"))


def _validate(step: dict[str, Any], tracked: set[str]) -> None:
    """Require non-vacuous inputs covering this caller's inline tool sources."""
    inputs = step.get("with", {})
    assert inputs.get("enable-cache") in (True, "true"), "direct uv cache must stay enabled"
    raw = inputs.get("cache-dependency-glob")
    assert isinstance(raw, str) and raw.strip(), "direct uv cache needs explicit dependency inputs"
    patterns = [line.strip() for line in raw.splitlines() if line.strip()]
    covered = set()
    for pattern in patterns:
        assert not pattern.startswith(("!", "/")) and ".." not in Path(pattern).parts
        matches = {path.relative_to(ROOT).as_posix() for path in ROOT.glob(pattern)} & tracked
        assert matches, f"cache input pattern has no tracked matches: {pattern}"
        covered.update(matches)
    required = {"Makefile", WORKFLOW, "scripts/clippy_expect_policy_test.py"}
    required.update(
        path for path in tracked
        if path.startswith("tests/workflow_contracts/") and path.endswith(".py")
    )
    assert required <= covered, f"cache inputs omit owned tool sources: {sorted(required - covered)}"


def test_direct_uv_cache_covers_owned_tool_inputs(workflow: dict[str | bool, Any], tracked_paths: set[str]) -> None:
    """The actual direct step must hash real inline dependency and contract sources."""
    _validate(_direct_step(workflow), tracked_paths)


@pytest.mark.parametrize("removed", PATTERNS)
def test_removing_an_input_family_is_rejected(
    workflow: dict[str | bool, Any], tracked_paths: set[str], removed: str
) -> None:
    """Dropping any owned input family must fail the caller contract."""
    step = deepcopy(_direct_step(workflow))
    step["with"]["cache-dependency-glob"] = "\n".join(
        pattern for pattern in PATTERNS if pattern != removed
    )
    with pytest.raises(AssertionError, match="omit owned tool sources"):
        _validate(step, tracked_paths)


@pytest.mark.parametrize("replacement", ["", "requirements-does-not-exist.txt"])
def test_empty_or_unmatched_inputs_are_rejected(
    workflow: dict[str | bool, Any], tracked_paths: set[str], replacement: str
) -> None:
    """An empty input or placeholder dependency file cannot hide the warning."""
    step = deepcopy(_direct_step(workflow))
    step["with"]["cache-dependency-glob"] = replacement
    with pytest.raises(AssertionError, match="explicit dependency inputs|no tracked matches"):
        _validate(step, tracked_paths)


def test_disabling_cache_is_rejected(workflow: dict[str | bool, Any], tracked_paths: set[str]) -> None:
    """Removing the useful cache must not substitute for fixing its inputs."""
    step = deepcopy(_direct_step(workflow))
    step["with"].update({"enable-cache": False, "cache-dependency-glob": "\n".join(PATTERNS)})
    with pytest.raises(AssertionError, match="must stay enabled"):
        _validate(step, tracked_paths)


def test_unverified_action_revision_is_rejected(workflow: dict[str | bool, Any]) -> None:
    """A different full SHA needs cache-input interface verification first."""
    changed = deepcopy(workflow)
    _direct_step(changed)["uses"] = "astral-sh/setup-uv@" + "0" * 40
    with pytest.raises(AssertionError, match="verified cache-input interface"):
        _direct_step(changed)
