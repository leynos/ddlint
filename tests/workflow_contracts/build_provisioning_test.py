"""Contract tests for Linux Rust suite provisioning and consumer reachability.

The local helper owns only the route from repository workflow jobs to Rust
suites and their required tool setup. CV-005 remains the strict workflow reader
and canonical policy owner; this test runs via ``make test-workflow-contracts``.
"""

from __future__ import annotations

import copy
from collections.abc import Mapping
from pathlib import Path
import re
from types import SimpleNamespace
from typing import cast
from unittest.mock import Mock

import pytest

from cv005_contracts.loading import WorkflowReadingError, load_workflow, read_workflows
import build_provisioning as contract
from build_provisioning import (
    BuildProvisioningError,
    DEPENDABOT_AUTOMERGE,
    MakeEvaluation,
    MUTATION_CARGO,
    SETUP_RUST,
    assert_make_goal_provisioning,
    assert_suite_provisioning,
    discover_linux_suites,
    linux_runner_rows,
    read_repository_workflows,
)

ROOT = Path(__file__).resolve().parents[2]
MAKEFILE = ROOT / "Makefile"
CV005_CONTRACTS_REF = "88977798a5c3bae1549afb99642529488c665276"


def _workflow(mutation: str | None = None) -> dict[object, object]:
    """Build one strict-reader fixture with the requested setup mutation."""
    setup = ""
    if mutation != "removed":
        setup_uses = "actions-rust-lang/setup-rust-toolchain@main" if mutation == "wrong-pin" else SETUP_RUST
        condition = "        if: always()\n" if mutation == "conditional" else ""
        soft_fail = "        continue-on-error: true\n" if mutation == "continue" else ""
        install_mold = "false" if mutation == "mold-disabled" else "'true'"
        mold_version = "          mold-version: '2.40.0'\n" if mutation == "mold-version" else ""
        if mutation == "explicit-mold-version":
            mold_version = "          mold-version: '2.41.0'\n"
        setup = (
            f"      - uses: {setup_uses}\n{condition}{soft_fail}"
            f"        with:\n          install-mold: {install_mold}\n{mold_version}"
        )
    suite = "      - run: cargo test --all-targets\n"
    steps = suite + setup if mutation == "after" else setup + suite
    runner = "unallocated-pool" if mutation == "unknown-runner" else "ubuntu-latest"
    extra_job = ""
    if mutation == "new-suite-job":
        extra_job = """  added-suite:
    runs-on: ubuntu-24.04
    steps:
      - run: cargo test
"""
    return load_workflow(
        f"""name: provisioning fixture
on: workflow_dispatch
jobs:
  build:
    runs-on: {runner}
    steps:
{steps}{extra_job}"""
    )


def test_actual_workflow_topology_has_expected_nonempty_suite_routes() -> None:
    """CI's full all-features suite coexists with coverage and mutation routes."""
    suites = discover_linux_suites(read_repository_workflows())
    routes = {(suite.workflow, suite.job, suite.route, suite.make_goal) for suite in suites}
    assert routes == {
        ("ci.yml", "build-test", "make", "lint"),
        ("ci.yml", "build-test", "make", "test"),
        ("ci.yml", "build-test", "coverage-action", None),
        ("coverage-main.yml", "coverage-upload", "coverage-action", None),
        ("mutation-testing.yml", "mutation", "mutation-cargo", None),
    }


def test_actual_workflow_suites_require_provisioning_before_execution() -> None:
    """Each checked-in Linux suite must have its approved setup and preflight."""
    assert_suite_provisioning(read_repository_workflows())


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

    positions = {
        name: [index for index, step in enumerate(steps)
               if isinstance(step, Mapping) and step.get("name") == name]
        for name in ("Lint", "Test", "Workflow contract tests")
    }
    if any(len(indices) != 1 for indices in positions.values()):
        raise BuildProvisioningError("CI needs one each of Lint, Test and workflow contracts")
    lint_index, test_index, contracts_index = (positions[name][0] for name in (
        "Lint", "Test", "Workflow contract tests"
    ))
    if not lint_index < test_index < contracts_index:
        raise BuildProvisioningError("CI Test must follow Lint and precede workflow contracts")

    test_step = steps[test_index]
    if (
        not isinstance(test_step, Mapping)
        or test_step.get("run") != "make test"
        or "uses" in test_step
        or "if" in test_step
        or test_step.get("continue-on-error") not in (None, False, "false", "False")
    ):
        raise BuildProvisioningError("CI Test must run make test without soft failure")
    run_indices = [
        index for index, step in enumerate(steps)
        if isinstance(step, Mapping) and step.get("run") == "make test"
    ]
    if run_indices != [test_index]:
        raise BuildProvisioningError("CI must have exactly one make test step named Test")
    if "if" in job or job.get("continue-on-error") not in (None, False, "false", "False"):
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
    workflow = cast(dict[object, object], workflows["ci.yml"])
    jobs = cast(dict[object, object], workflow["jobs"])
    job = cast(dict[object, object], jobs["build-test"])
    steps = cast(list[object], job["steps"])
    test_index = next(
        index for index, value in enumerate(steps)
        if isinstance(value, Mapping) and value.get("name") == "Test"
    )
    test_step = cast(dict[object, object], steps[test_index])
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


@pytest.mark.parametrize(
    "runner",
    [
        pytest.param("ubuntu-latest", id="scalar"),
        pytest.param(["self-hosted", "Linux", "x64"], id="list"),
        pytest.param({"group": "linux", "labels": ["ubuntu-24.04", "x64"]}, id="mapping"),
    ],
)
def test_runner_shapes_identify_linux_suites(runner: object) -> None:
    """Scalar, list and group/label runners all resolve as Linux."""
    assert linux_runner_rows({"runs-on": runner}, "fixture:build") == ({},)


def test_matrix_runner_resolves_each_finite_linux_leg() -> None:
    """A runner matrix resolves Linux legs while recognizing macOS legs."""
    job = {
        "runs-on": "${{ matrix.os }}",
        "strategy": {"matrix": {"os": ["ubuntu-22.04", "macos-14"]}},
    }
    assert linux_runner_rows(job, "fixture:build") == ({"os": "ubuntu-22.04"},)


@pytest.mark.parametrize(
    "matrix",
    [
        pytest.param({"os": []}, id="empty-axis"),
        pytest.param({"os": ["ubuntu"], "arch": ["x64"]}, id="multiple-axes"),
        pytest.param({"include": [{"os": "ubuntu"}]}, id="include"),
        pytest.param({"os": [{"runner": "ubuntu"}]}, id="unresolved-runner"),
        pytest.param(None, id="null-matrix"),
    ],
)
def test_unresolvable_suite_matrices_fail_closed(matrix: dict[str, object] | None) -> None:
    """An unsupported or empty matrix cannot erase a suite from the audit."""
    job = {"runs-on": "${{ matrix.os }}", "strategy": {"matrix": matrix}}
    with pytest.raises(BuildProvisioningError):
        linux_runner_rows(job, "fixture:build")


def test_public_reader_rejects_an_empty_workflow_directory(tmp_path: Path) -> None:
    """The actual workflow reader prevents an empty directory from passing."""
    with pytest.raises(WorkflowReadingError):
        read_workflows(tmp_path)


@pytest.mark.parametrize(
    "workflows",
    [pytest.param({}, id="no-workflows"), pytest.param({"empty.yml": {"jobs": {}}}, id="no-jobs")],
)
def test_empty_workflow_topology_fails_closed(workflows: dict[str, object]) -> None:
    """No supplied workflow or job set can yield a vacuous success."""
    with pytest.raises(BuildProvisioningError):
        discover_linux_suites(workflows)


def test_local_reusable_workflow_closure_reaches_its_linux_suite() -> None:
    """A local reusable caller inherits the checked child workflow suite."""
    caller = load_workflow(
        """on: workflow_dispatch
jobs:
  suite:
    uses: ./.github/workflows/child.yml
"""
    )
    assert_suite_provisioning({"caller.yml": caller, "child.yml": _workflow()})


@pytest.mark.parametrize(
    "uses",
    [
        pytest.param("./.github/workflows/missing.yml", id="missing-local"),
        pytest.param("example/build.yml@0123456789abcdef0123456789abcdef01234567", id="unknown-remote"),
    ],
)
def test_unknown_reusable_workflow_routes_are_rejected(uses: str) -> None:
    """Only the checked local closure and two pinned boundaries are supported."""
    caller = load_workflow(f"jobs:\n  call:\n    uses: {uses}\n")
    with pytest.raises(BuildProvisioningError):
        discover_linux_suites({"caller.yml": caller})


@pytest.mark.parametrize(
    "mutation",
    [
        "removed",
        "after",
        "conditional",
        "continue",
        "mold-disabled",
        "wrong-pin",
        "mold-version",
        "unknown-runner",
        "new-suite-job",
    ],
)
def test_missing_or_invalid_suite_provisioning_is_rejected(mutation: str) -> None:
    """Provisioning mutations cannot hide an affected Linux suite."""
    with pytest.raises(BuildProvisioningError):
        assert_suite_provisioning({"fixture.yml": _workflow(mutation)})


def test_default_and_explicit_approved_mold_version_are_accepted() -> None:
    """The action default and explicit 2.41.0 input both satisfy the contract."""
    assert_suite_provisioning({"default.yml": _workflow()})
    assert_suite_provisioning({"explicit.yml": _workflow("explicit-mold-version")})


def test_pinned_mutation_reusable_route_requires_install_mold() -> None:
    """The immutable mutation workflow is accepted only with its setup input."""
    caller = load_workflow(
        f"""jobs:
  mutation:
    uses: {MUTATION_CARGO}
    with:
      install-mold: 'true'
"""
    )
    assert discover_linux_suites({"mutation.yml": caller})[0].route == "mutation-cargo"


def test_mutation_reusable_route_rejects_disabled_mold_installation() -> None:
    """The known external boundary cannot be called without forwarding true."""
    caller = load_workflow(
        f"""jobs:
  mutation:
    uses: {MUTATION_CARGO}
    with:
      install-mold: false
"""
    )
    with pytest.raises(BuildProvisioningError):
        discover_linux_suites({"mutation.yml": caller})


def test_dependabot_api_only_boundary_is_not_counted_as_a_rust_suite() -> None:
    """The exact pinned API-only reusable is an exception, not a suite route."""
    workflows = read_repository_workflows()
    assert DEPENDABOT_AUTOMERGE in str(workflows["dependabot-automerge.yml"])
    assert all(suite.workflow != "dependabot-automerge.yml" for suite in discover_linux_suites(workflows))


def test_multi_goal_make_call_keeps_suite_after_the_first_goal(monkeypatch: pytest.MonkeyPatch) -> None:
    """A later goal in a supported Make goal list remains reachable."""
    evaluations = {"fmt": MakeEvaluation((), ()), "test": MakeEvaluation((2,), ())}
    monkeypatch.setattr(contract, "evaluate_make_goal", evaluations.__getitem__)
    workflow = load_workflow(
        """jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - run: make fmt test
"""
    )
    suites = discover_linux_suites({"multi.yml": workflow})
    assert [suite.make_goal for suite in suites] == ["test"]


@pytest.mark.parametrize(
    "call",
    [
        "make -f alternate.mk test",
        "make RUSTFLAGS=-Dwarnings test",
        "cd subdir && make test",
        "echo hi\ncd nested\nmake test",
        "MAKEFLAGS=-fother.mk make test",
    ],
)
def test_unsupported_make_overrides_fail_closed(call: str) -> None:
    """The local evaluator refuses alternate files, cwd and flag overrides."""
    with pytest.raises(BuildProvisioningError):
        contract._make_goals(call)


@pytest.mark.parametrize("scope", ["step", "job", "workflow"])
def test_make_calls_with_custom_working_directory_fail_closed(scope: str) -> None:
    """Step, job and workflow working-directory settings cannot redirect Make."""
    step = {"run": "make test"}
    job: dict[str, object] = {"runs-on": "ubuntu-latest", "steps": [step]}
    workflow: dict[str, object] = {"jobs": {"build": job}}
    if scope == "step":
        step["working-directory"] = "subdir"
    elif scope == "job":
        job["defaults"] = {"run": {"working-directory": "subdir"}}
    else:
        workflow["defaults"] = {"run": {"working-directory": "subdir"}}
    with pytest.raises(BuildProvisioningError):
        discover_linux_suites({"custom-directory.yml": workflow})


@pytest.mark.parametrize("goal", ["build", "check", "lint", "test", "typecheck"])
def test_each_supported_make_suite_reaches_tool_preflight(goal: str) -> None:
    """Every public development build goal reaches provisioning first."""
    assert_make_goal_provisioning(goal)


def test_make_evaluation_failure_propagates_to_the_contract(monkeypatch: pytest.MonkeyPatch) -> None:
    """A failed Make metadata query remains a contract failure."""
    monkeypatch.setattr(contract.shutil, "which", lambda name: "make")
    monkeypatch.setattr(
        contract.subprocess,
        "run",
        Mock(return_value=SimpleNamespace(returncode=2, stdout="", stderr="missing goal")),
    )
    with pytest.raises(BuildProvisioningError, match="cannot evaluate goal"):
        assert_make_goal_provisioning("build-provisioning-failure-propagation")


def test_make_uses_the_same_pinned_cv005_package_for_cli_and_pytest() -> None:
    """The installed public reader and CLI share the approved immutable pin."""
    makefile = MAKEFILE.read_text(encoding="utf-8")
    variable = re.search(r"(?m)^CV005_CONTRACTS_REQUIREMENT = (.+)$", makefile)
    assert variable is not None
    requirement = "$(CV005_CONTRACTS_REQUIREMENT)"
    assert f"@$(CV005_CONTRACTS_REF)\\#subdirectory=packages/cv005-contracts" in variable.group(1)
    assert f"--from '{requirement}'" in makefile
    assert f"--with 'pytest>=8' --with 'pyyaml>=6' --with '{requirement}' pytest" in makefile
    assert f"CV005_CONTRACTS_REF ?= {CV005_CONTRACTS_REF}" in makefile
