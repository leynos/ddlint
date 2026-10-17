"""Regression tests for versioned GNU Make preflight trace diagnostics."""

from __future__ import annotations

import os
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

import build_provisioning as contract
from build_provisioning import BuildProvisioningError

SUITE_TRACE = "Makefile:161: cargo test --all-targets --all-features"


def _stub_make_output(
    monkeypatch: pytest.MonkeyPatch,
    stdout: str,
    *,
    returncode: int = 0,
) -> Mock:
    """Mock Make's process boundary while preserving evaluator behaviour."""
    monkeypatch.setattr(contract.shutil, "which", lambda _: "/usr/bin/make")
    result = SimpleNamespace(returncode=returncode, stdout=stdout, stderr="")
    runner = Mock(return_value=result)
    monkeypatch.setattr(contract.subprocess, "run", runner)
    return runner


@pytest.mark.parametrize(
    ("goal", "preflight_trace"),
    [
        pytest.param(
            "_trace_fixture_gnu_43",
            "Makefile:54: target 'check-build-tools' does not exist",
            id="gnu-4.3-missing-target",
        ),
        pytest.param(
            "_trace_fixture_gnu_441",
            "Makefile:54: update target 'check-build-tools' due to: target is .PHONY",
            id="gnu-4.4.1-phony-target",
        ),
        pytest.param(
            "_trace_fixture_legacy_dependency",
            "Makefile:54: update target 'check-build-tools' due to: prerequisite-a",
            id="legacy-dependency-update",
        ),
        pytest.param(
            "_trace_fixture_legacy_bare_update",
            "Makefile:54: update target 'check-build-tools'",
            id="legacy-update-without-reason",
        ),
    ],
)
def test_supported_make_trace_formats_reach_preflight_before_suite(
    monkeypatch: pytest.MonkeyPatch,
    goal: str,
    preflight_trace: str,
) -> None:
    """Recognize supported trace lines through the real Make evaluator."""
    runner = _stub_make_output(monkeypatch, f"{preflight_trace}\n{SUITE_TRACE}")

    evaluation = contract.assert_make_goal_provisioning(goal)

    assert evaluation.provisioning_lines == (0,)
    assert evaluation.suite_lines == (1,)
    command = runner.call_args.args[0]
    assert runner.call_args.kwargs["cwd"] == contract.ROOT
    assert "-f" not in command
    assert "--file" not in command


@pytest.mark.parametrize(
    ("goal", "preflight_trace"),
    [
        pytest.param(
            "_trace_fixture_unrelated_target",
            "Makefile:54: update target 'test' due to: target is .PHONY",
            id="unrelated-target",
        ),
        pytest.param(
            "_trace_fixture_echo_line",
            'echo "Makefile:54: update target \'check-build-tools\' due to: target is .PHONY"',
            id="echo-is-not-a-trace-event",
        ),
        pytest.param(
            "_trace_fixture_lookalike_target",
            "Makefile:54: update target 'check-build-tools-extra' due to: target is .PHONY",
            id="lookalike-target",
        ),
        pytest.param(
            "_trace_fixture_missing_event",
            None,
            id="missing-preflight-event",
        ),
        pytest.param(
            "_trace_fixture_trailing_output",
            "Makefile:54: target 'check-build-tools' does not exist extra",
            id="trailing-text-is-not-a-complete-event",
        ),
    ],
)
def test_untrusted_make_output_does_not_prove_preflight(
    monkeypatch: pytest.MonkeyPatch,
    goal: str,
    preflight_trace: str | None,
) -> None:
    """Reject unrelated, unanchored, incomplete, and absent trace events."""
    output = f"{preflight_trace}\n{SUITE_TRACE}" if preflight_trace else SUITE_TRACE
    runner = _stub_make_output(monkeypatch, output)

    with pytest.raises(BuildProvisioningError, match="must reach check-build-tools"):
        contract.assert_make_goal_provisioning(goal)

    assert runner.called


def test_preflight_trace_after_suite_does_not_satisfy_ordering(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """A valid preflight event still has to precede the Rust suite."""
    runner = _stub_make_output(
        monkeypatch,
        f"{SUITE_TRACE}\nMakefile:54: target 'check-build-tools' does not exist",
    )

    with pytest.raises(BuildProvisioningError, match="must reach check-build-tools"):
        contract.assert_make_goal_provisioning("_trace_fixture_preflight_after_suite")

    assert runner.called


def test_nonzero_make_result_rejects_otherwise_valid_trace(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """A failed Make query cannot prove provisioning even with valid output."""
    runner = _stub_make_output(
        monkeypatch,
        f"Makefile:54: target 'check-build-tools' does not exist\n{SUITE_TRACE}",
        returncode=2,
    )

    with pytest.raises(BuildProvisioningError, match="cannot evaluate goal"):
        contract.assert_make_goal_provisioning("_trace_fixture_nonzero_make")

    assert runner.called


def test_make_trace_sets_only_child_locale_and_preserves_environment(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Force English trace text while retaining every caller environment key."""
    monkeypatch.setenv("RUSTC_WRAPPER", "/example/rustc-wrapper")
    monkeypatch.setenv("RUSTDOC", "/example/rustdoc-wrapper")
    monkeypatch.setenv("RUSTFLAGS", "-D warnings")
    monkeypatch.setenv("MAKEFLAGS", "--warn-undefined-variables")
    monkeypatch.setenv("LC_ALL", "test-non-C-locale")
    caller_environment = dict(os.environ)
    runner = _stub_make_output(
        monkeypatch,
        f"Makefile:54: target 'check-build-tools' does not exist\n{SUITE_TRACE}",
    )

    contract.assert_make_goal_provisioning("_trace_fixture_child_locale")

    assert os.environ["LC_ALL"] == "test-non-C-locale"
    child_environment = runner.call_args.kwargs["env"]
    assert child_environment == {**caller_environment, "LC_ALL": "C"}
    assert child_environment["RUSTC_WRAPPER"] == caller_environment["RUSTC_WRAPPER"]
    assert child_environment["RUSTDOC"] == caller_environment["RUSTDOC"]
    assert child_environment["RUSTFLAGS"] == caller_environment["RUSTFLAGS"]
    assert child_environment["MAKEFLAGS"] == caller_environment["MAKEFLAGS"]
