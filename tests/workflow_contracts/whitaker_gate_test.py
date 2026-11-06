"""Contract tests for the Whitaker Dylint gate in CI and the Makefile.

`make lint` must run Whitaker over the workspace with the literal warning
policy, and the gate must be binding: CI installs the pinned suite at the
shared full-SHA revision before it runs `make lint`, the aggregate `lint`
target depends on a dedicated `lint-whitaker` target, and that target keeps
Whitaker's package selection configurable. These tests read the workflow and
the Makefile, so a dropped prerequisite, an unpinned installer, a version
override or a lost package variable fails here rather than silently weakening
the gate.

Run via ``make test-workflow-contracts``.
"""

from __future__ import annotations

import re

import pytest
import yaml

from build_provisioning_common import APPROVED_SHARED_ACTIONS_REF, ROOT

MAKEFILE = ROOT / "Makefile"
CI = ROOT / ".github" / "workflows" / "ci.yml"
INSTALL_WHITAKER = "leynos/shared-actions/.github/actions/install-whitaker"
INSTALL_WHITAKER_AT_PIN = f"{INSTALL_WHITAKER}@{APPROVED_SHARED_ACTIONS_REF}"
# Inputs that would let CI run a different suite or installer revision than
# the pinned action commit, so the gate could change with no repository diff.
FORBIDDEN_INPUTS = {"suite-version", "installer-version", "allow-suite-pin"}


@pytest.fixture(scope="module")
def makefile_text() -> str:
    """Read the Makefile once at the test boundary, failing the test readably."""
    try:
        return MAKEFILE.read_text(encoding="utf-8")
    except OSError as error:
        raise AssertionError(
            f"could not read the Makefile at {MAKEFILE}: {error}"
        ) from error


def _variable(makefile: str, name: str) -> str:
    """Return the value a `NAME ?=` or `NAME =` assignment gives, joined."""
    pattern = re.compile(rf"^{name}\s*\??=\s*((?:.*\\\n)*.*)$", re.MULTILINE)
    match = pattern.search(makefile)
    assert match, f"the Makefile must assign {name}"
    return re.sub(r"\\\n\s*", " ", match.group(1)).strip()


def _prerequisites(makefile: str, target: str) -> list[str]:
    """Return the prerequisites declared for *target*."""
    pattern = re.compile(rf"^{re.escape(target)}:([^=\n]*)$", re.MULTILINE)
    match = pattern.search(makefile)
    assert match, f"the Makefile must declare a {target} target"
    return match.group(1).split("##")[0].split()


def _recipe(makefile: str, target: str) -> str:
    """Return the tab-indented recipe lines of *target*, or an empty string."""
    pattern = re.compile(
        rf"^{re.escape(target)}:[^\n]*\n((?:\t[^\n]*\n)*)", re.MULTILINE
    )
    match = pattern.search(makefile)
    assert match, f"the Makefile must declare a {target} target"
    return match.group(1)


def _steps(workflow: dict) -> list[tuple[str, list[dict]]]:
    """Yield each job name with its steps."""
    return [(name, job.get("steps", [])) for name, job in workflow["jobs"].items()]


def _install_steps(steps: list[dict]) -> list[dict]:
    """Return the steps that use the Whitaker installer, at any revision."""
    return [
        step for step in steps if str(step.get("uses", "")).startswith(INSTALL_WHITAKER + "@")
    ]


def test_ci_pins_the_installer_at_the_shared_full_sha() -> None:
    """A floating tag or a local action would escape the reviewed pin."""
    workflow = yaml.safe_load(CI.read_text(encoding="utf-8"))
    installed = [
        step["uses"]
        for _, steps in _steps(workflow)
        for step in _install_steps(steps)
    ]
    assert installed == [INSTALL_WHITAKER_AT_PIN], (
        "CI must install only the approved Whitaker action pin"
    )


def test_ci_passes_no_suite_or_installer_version_override() -> None:
    """The pinned commit is the only source of the suite revision."""
    workflow = yaml.safe_load(CI.read_text(encoding="utf-8"))
    for job, steps in _steps(workflow):
        for step in _install_steps(steps):
            inputs = set(step.get("with", {}))
            overrides = inputs & FORBIDDEN_INPUTS
            assert not overrides, f"job {job} overrides {sorted(overrides)}"


def test_ci_installs_whitaker_before_it_runs_make_lint() -> None:
    """An install after the lint step leaves the gate without its suite."""
    workflow = yaml.safe_load(CI.read_text(encoding="utf-8"))
    checked = 0
    for job, steps in _steps(workflow):
        lint = [i for i, step in enumerate(steps) if str(step.get("run", "")).strip() == "make lint"]
        if not lint:
            continue
        install_steps = _install_steps(steps)
        install = [
            index for index, step in enumerate(steps) if step in install_steps
        ]
        assert install, f"job {job} runs make lint without installing Whitaker"
        assert min(install) < lint[0], f"job {job} installs Whitaker after make lint"
        checked += 1
    assert checked, "no CI job runs `make lint`"


def test_lint_is_a_serial_composite_of_the_gates(makefile_text: str) -> None:
    """Dropping a prerequisite removes that gate from `make lint`."""
    assert _prerequisites(makefile_text, "lint") == [
        "interrogate",
        "lint-clippy",
        "lint-whitaker",
        "spelling",
    ], "`make lint` must keep the ordered gate prerequisites"


def test_not_parallel_keeps_the_composite_ordered(makefile_text: str) -> None:
    """Without this, `-j` could overlap the Cargo-producing gates."""
    match = re.search(r"^\.NOTPARALLEL:\s*(.+)$", makefile_text, re.MULTILINE)
    assert match, "the Makefile must declare .NOTPARALLEL"
    ordered = match.group(1).split()
    for target in ("lint", "lint-clippy", "lint-whitaker"):
        assert target in ordered, f".NOTPARALLEL does not cover {target}"


def test_the_gate_target_keeps_the_literal_policy_and_packages(
    makefile_text: str,
) -> None:
    """The gate must clear inherited flags and select the whole workspace."""
    recipe = _recipe(makefile_text, "lint-whitaker")
    assert (
        'unset CARGO_ENCODED_RUSTFLAGS && RUSTFLAGS="-D warnings" '
        "$(WHITAKER) --all $(WHITAKER_PACKAGES) -- --all-targets --all-features"
    ) in recipe, (
        "lint-whitaker must keep the literal warning policy and package selection"
    )


def test_the_packages_variable_defaults_to_the_workspace(makefile_text: str) -> None:
    """A package-less suite would silently check fewer crates."""
    assert _variable(makefile_text, "WHITAKER_PACKAGES") == "--workspace", (
        "the default must stay the whole workspace"
    )


def test_the_composite_runs_no_cargo_itself(makefile_text: str) -> None:
    """The Rust gates belong to the split targets, not to the aggregate."""
    assert "$(CARGO)" not in _recipe(makefile_text, "lint"), (
        "the aggregate must not invoke Cargo directly"
    )

    recipe = _recipe(makefile_text, "lint-clippy")
    for expected in ("$(CARGO) clippy", "$(CARGO) doc", "test-clippy-policy"):
        assert expected in recipe, f"lint-clippy must keep {expected}"