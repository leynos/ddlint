"""Discover local Linux Rust suite consumers.

Checks their approved tool setup and Make prerequisites. CV-005 remains the
strict workflow reader and canonical workflow-policy owner.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path, PurePosixPath
import os
import re
import shutil
import subprocess
from typing import cast

from cv005_contracts.loading import read_workflows

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS_DIRECTORY = ROOT / ".github" / "workflows"
APPROVED_SHARED_ACTIONS_REF = "e693d81d176d2305e2b736a0be0f9038085dec7f"
SHARED_ACTION_PREFIX = "leynos/shared-actions/.github"
SETUP_RUST = f"{SHARED_ACTION_PREFIX}/actions/setup-rust@{APPROVED_SHARED_ACTIONS_REF}"
MUTATION_CARGO = f"{SHARED_ACTION_PREFIX}/workflows/mutation-cargo.yml@{APPROVED_SHARED_ACTIONS_REF}"
DEPENDABOT_AUTOMERGE = f"{SHARED_ACTION_PREFIX}/workflows/dependabot-automerge.yml@{APPROVED_SHARED_ACTIONS_REF}"
COVERAGE_ACTION = f"{SHARED_ACTION_PREFIX}/actions/generate-coverage@{APPROVED_SHARED_ACTIONS_REF}"
PROVISIONING_TARGET = "check-build-tools"
MATRIX_VALUE = re.compile(r"^\$\{\{\s*matrix\.([A-Za-z0-9_-]+)\s*\}\}$")
MAKE_CALL = re.compile(r"\bmake\b([^;&|\n]*)")
CARGO_CALL = re.compile(
    r"\b(?P<tool>cargo|cross)\s+(?:\+\S+\s+)?"
    r"(?P<command>[A-Za-z][A-Za-z0-9_-]*)\b(?P<arguments>[^;&|\n]*)"
)
RUST_SUITE_COMMANDS = {"bench", "build", "check", "clippy", "doc", "nextest", "run", "test"}
NON_SUITE_CARGO_COMMANDS = {
    "clean",
    "fetch",
    "fmt",
    "help",
    "install",
    "metadata",
    "package",
    "update",
    "version",
}
UPDATE_TARGET = re.compile(
    rf"^Makefile:\d+: (?:update target ['\"]{re.escape(PROVISIONING_TARGET)}['\"](?: due to: .*)?"
    rf"|target ['\"]{re.escape(PROVISIONING_TARGET)}['\"] does not exist)$"
)
RUNNER_OS = re.compile(r"(?:^|[^a-z])(ubuntu|linux|windows|macos|osx)(?:-|$)")


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


def read_repository_workflows() -> dict[str, dict[object, object]]:
    """Read every checked-in workflow through the pinned CV-005 reader."""
    return cast(dict[str, dict[object, object]], read_workflows(WORKFLOWS_DIRECTORY))


def _mapping(value: object, context: str) -> Mapping[object, object]:
    """Return a workflow mapping or fail closed with its location."""
    if not isinstance(value, Mapping):
        raise BuildProvisioningError(f"{context} must be a mapping, got {value!r}")
    return value


def _is_true(value: object) -> bool:
    """Accept YAML booleans and GitHub's quoted true input spelling."""
    return value is True or value in ("true", "True", "TRUE")


def _matrix_rows(job: Mapping[object, object], context: str) -> tuple[dict[str, object], ...]:
    """Resolve a finite single-axis matrix used to select suite runners."""
    strategy = _mapping(job.get("strategy", {}), f"{context}.strategy")
    if "matrix" not in strategy:
        return ({},)
    matrix = _mapping(strategy["matrix"], f"{context}.strategy.matrix")
    if any(key in matrix for key in ("include", "exclude")):
        raise BuildProvisioningError(f"{context} suite matrix include/exclude is unsupported")
    if len(matrix) != 1:
        raise BuildProvisioningError(f"{context} suite matrix must have one finite axis")
    name, values = next(iter(matrix.items()))
    if not isinstance(name, str) or not isinstance(values, list) or not values:
        raise BuildProvisioningError(f"{context} has an unresolved matrix axis {name!r}")
    return tuple({name: value} for value in values)


def _runner_labels(value: object, row: Mapping[str, object], context: str) -> tuple[str, ...]:
    """Resolve runner labels from scalar, list, or group-and-label mappings."""
    if isinstance(value, str):
        expression = MATRIX_VALUE.fullmatch(value)
        if expression:
            name = expression.group(1)
            resolved = row.get(name)
            if not isinstance(resolved, str):
                raise BuildProvisioningError(f"{context} cannot resolve matrix runner {name!r}")
            return (resolved,)
        if "${{" in value or "}}" in value:
            raise BuildProvisioningError(f"{context} has an unresolved runner expression {value!r}")
        return (value,)
    if isinstance(value, list):
        labels: list[str] = []
        for item in value:
            labels.extend(_runner_labels(item, row, context))
        return tuple(labels)
    if isinstance(value, Mapping):
        labels = value.get("labels")
        if labels is None:
            raise BuildProvisioningError(f"{context} runner mapping has no labels")
        return _runner_labels(labels, row, context)
    raise BuildProvisioningError(f"{context}.runs-on has unsupported shape {value!r}")


def linux_runner_rows(job: Mapping[object, object], context: str) -> tuple[dict[str, object], ...]:
    """Return Linux legs and reject runner labels whose operating system is unknown."""
    if "runs-on" not in job:
        raise BuildProvisioningError(f"{context} has a suite but no runs-on value")
    runner = job["runs-on"]
    rows = _matrix_rows(job, context)
    linux_rows = []
    for row in rows:
        labels = _runner_labels(runner, row, context)
        systems = {match.group(1) for label in labels if (match := RUNNER_OS.search(label.lower()))}
        if not systems:
            raise BuildProvisioningError(f"{context} has an unclassified runner {labels!r}")
        if systems & {"ubuntu", "linux"} and systems & {"windows", "macos", "osx"}:
            raise BuildProvisioningError(f"{context} has conflicting runner systems {labels!r}")
        if systems & {"ubuntu", "linux"}:
            linux_rows.append(row)
    return tuple(linux_rows)


def _make_goals(script: str) -> tuple[str | None, ...]:
    """Find simple Make goal lists, including the repository's default goal."""
    normalized = script.replace("\\\n", " ")
    if MAKE_CALL.search(normalized) and re.search(r"(?:^|[\r\n;&|]\s*)(?:cd|pushd)\b", normalized):
        raise BuildProvisioningError("Make suite calls must run from the repository root")
    if MAKE_CALL.search(normalized) and re.search(r"\b(?:MAKEFLAGS|GNUMAKEFLAGS|MAKEFILES)\s*=", normalized):
        raise BuildProvisioningError("Make environment overrides are unsupported")
    goals: list[str | None] = []
    for line in normalized.splitlines():
        command_line = line.partition("#")[0]
        for match in MAKE_CALL.finditer(command_line):
            tokens = match.group(1).split()
            if any(token.startswith("-") or "=" in token for token in tokens):
                raise BuildProvisioningError(
                    f"unsupported Make options or variable assignments in {command_line.strip()!r}"
                )
            goals.extend(tokens or [None])
    return tuple(goals)


def _rust_suite_line(line: str) -> bool:
    """Recognize development Rust suite commands in Make's printed recipe."""
    for match in CARGO_CALL.finditer(line):
        command = match.group("command")
        arguments = match.group("arguments")
        if command not in RUST_SUITE_COMMANDS and command not in NON_SUITE_CARGO_COMMANDS:
            raise BuildProvisioningError(f"unclassified Cargo command in Make output: {line.strip()}")
        if command in RUST_SUITE_COMMANDS and (command != "build" or "--release" not in arguments):
            return True
    return False


@lru_cache(maxsize=None)
def evaluate_make_goal(goal: str | None) -> MakeEvaluation:
    """Read actual Make target closure and recipe order without executing it."""
    make = shutil.which("make")
    if make is None:
        raise BuildProvisioningError("GNU Make is required to inspect suite prerequisites")
    command = [make, "--no-print-directory", "--dry-run", "--trace", "--always-make"]
    if goal is not None:
        command.append(goal)
    result = subprocess.run(
        command,
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
        env={**os.environ, "LC_ALL": "C"},
    )
    output = result.stdout + result.stderr
    if result.returncode != 0:
        raise BuildProvisioningError(f"Make cannot evaluate goal {goal!r}: {output.strip()}")
    lines = output.splitlines()
    suite_lines = tuple(index for index, line in enumerate(lines) if _rust_suite_line(line))
    provisioning_lines = tuple(index for index, line in enumerate(lines) if UPDATE_TARGET.search(line))
    return MakeEvaluation(suite_lines, provisioning_lines)


def _cargo_routes(script: str, context: str) -> tuple[str, ...]:
    """Identify direct suite commands and reject unknown Cargo routes."""
    routes = []
    for match in CARGO_CALL.finditer(script):
        command = match.group("command")
        arguments = match.group("arguments")
        if command not in RUST_SUITE_COMMANDS and command not in NON_SUITE_CARGO_COMMANDS:
            raise BuildProvisioningError(f"{context} has an unclassified Cargo command {command!r}")
        if command in RUST_SUITE_COMMANDS and (command != "build" or "--release" not in arguments):
            routes.append(f"{match.group('tool')}:{command}")
    if "cargo" in script and not CARGO_CALL.search(script):
        raise BuildProvisioningError(f"{context} has an unresolved Cargo command")
    return tuple(routes)


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
        step = _mapping(step_value, f"{workflow_name}:{job_name}.steps[{index}]")
        uses = step.get("uses")
        if isinstance(uses, str) and uses.startswith(COVERAGE_ACTION.rsplit("@", 1)[0]):
            if uses != COVERAGE_ACTION:
                raise BuildProvisioningError(f"{workflow_name}:{job_name} uses an unapproved coverage action pin")
            suites.append(SuiteConsumer(workflow_name, job_name, "coverage-action", index))

        run = step.get("run")
        if run is None:
            continue
        if not isinstance(run, str):
            raise BuildProvisioningError(f"{workflow_name}:{job_name}.steps[{index}].run must be text")
        for route in _cargo_routes(run, f"{workflow_name}:{job_name}.steps[{index}]"):
            suites.append(SuiteConsumer(workflow_name, job_name, route, index))
        goals = _make_goals(run)
        if goals and (has_default_working_directory or "working-directory" in step):
            raise BuildProvisioningError(f"{workflow_name}:{job_name} Make call has a custom working directory")
        for goal in goals:
            evaluation = evaluate_make_goal(goal)
            if evaluation.suite_lines:
                suites.append(
                    SuiteConsumer(workflow_name, job_name, "make", index, goal)
                )
    return tuple(suites)


def _has_working_directory(defaults: object, context: str) -> bool:
    """Check workflow or job run defaults for a custom Make working directory."""
    run = _mapping(_mapping(defaults, context).get("run", {}), f"{context}.run")
    return "working-directory" in run


def _require_make_preflight(evaluation: MakeEvaluation, context: str) -> None:
    """Require the evaluated preflight target to precede a Rust suite."""
    if not evaluation.suite_lines:
        raise BuildProvisioningError(f"{context} does not reach a Rust suite")
    if not evaluation.provisioning_lines or min(evaluation.provisioning_lines) >= min(evaluation.suite_lines):
        raise BuildProvisioningError(
            f"{context} must reach {PROVISIONING_TARGET} before its Rust suite"
        )


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


def discover_linux_suites(
    workflows: Mapping[str, Mapping[object, object]],
) -> tuple[SuiteConsumer, ...]:
    """Find Linux suite routes, resolving every local reusable workflow call."""
    if not workflows:
        raise BuildProvisioningError("no workflow topology was supplied")
    suites: list[SuiteConsumer] = []

    def visit(workflow_name: str, stack: tuple[str, ...]) -> None:
        """Visit one workflow and every local reusable workflow it calls."""
        if workflow_name in stack:
            raise BuildProvisioningError(f"local reusable workflow cycle: {(*stack, workflow_name)!r}")
        workflow = workflows.get(workflow_name)
        if workflow is None:
            raise BuildProvisioningError(f"local reusable workflow {workflow_name!r} is missing")
        jobs = _mapping(workflow.get("jobs"), f"{workflow_name}.jobs")
        if not jobs:
            raise BuildProvisioningError(f"{workflow_name}.jobs is empty")
        for job_name, job_value in jobs.items():
            if not isinstance(job_name, str):
                raise BuildProvisioningError(f"{workflow_name} has a non-text job name {job_name!r}")
            job = _mapping(job_value, f"{workflow_name}:{job_name}")
            uses = job.get("uses")
            if "uses" in job and not isinstance(uses, str):
                raise BuildProvisioningError(f"{workflow_name}:{job_name}.uses must be text")
            if isinstance(uses, str):
                if uses.startswith("./"):
                    child = _local_workflow_name(uses)
                    visit(child, (*stack, workflow_name))
                elif uses == MUTATION_CARGO:
                    inputs = _mapping(job.get("with", {}), f"{workflow_name}:{job_name}.with")
                    if not _is_true(inputs.get("install-mold")):
                        raise BuildProvisioningError(
                            f"{workflow_name}:{job_name} must pass install-mold: true"
                        )
                    suites.append(SuiteConsumer(workflow_name, job_name, "mutation-cargo", None))
                elif uses == DEPENDABOT_AUTOMERGE:
                    # This immutable shared workflow calls the GitHub API and has no consumer checkout.
                    continue
                else:
                    raise BuildProvisioningError(
                        f"{workflow_name}:{job_name} has an unknown reusable workflow {uses!r}"
                    )
                continue

            has_directory = _has_working_directory(
                workflow.get("defaults", {}), f"{workflow_name}.defaults"
            ) or _has_working_directory(job.get("defaults", {}), f"{workflow_name}:{job_name}.defaults")
            job_suites = _step_suites(workflow_name, job_name, job, has_directory)
            if not job_suites:
                continue
            linux_rows = linux_runner_rows(job, f"{workflow_name}:{job_name}")
            if linux_rows:
                suites.extend(job_suites)
    for name in workflows:
        visit(name, ())
    if not suites:
        raise BuildProvisioningError("no Linux Rust suite was found in the workflow topology")
    return tuple(dict.fromkeys(suites))


def assert_suite_provisioning(workflows: Mapping[str, Mapping[object, object]]) -> None:
    """Require each reachable Linux suite to have an unconditional prior setup."""
    suites = discover_linux_suites(workflows)
    grouped: dict[tuple[str, str], list[SuiteConsumer]] = {}
    for suite in suites:
        grouped.setdefault((suite.workflow, suite.job), []).append(suite)

    for (workflow_name, job_name), job_suites in grouped.items():
        if all(suite.route == "mutation-cargo" for suite in job_suites):
            continue
        job = _mapping(workflows[workflow_name]["jobs"], f"{workflow_name}.jobs")[job_name]
        steps = _mapping(job, f"{workflow_name}:{job_name}").get("steps", [])
        if not isinstance(steps, list):
            raise BuildProvisioningError(f"{workflow_name}:{job_name}.steps must be a list")
        first_suite_index = min(
            suite.step_index for suite in job_suites if suite.step_index is not None
        )
        for index, step_value in enumerate(steps):
            if index >= first_suite_index:
                continue
            setup = _mapping(step_value, f"{workflow_name}:{job_name}.steps[{index}]")
            if setup.get("uses") != SETUP_RUST or "if" in setup:
                continue
            if setup.get("continue-on-error") not in (None, False, "false", "False"):
                continue
            inputs = _mapping(setup.get("with", {}), f"{workflow_name}:{job_name}.setup-rust.with")
            if not _is_true(inputs.get("install-mold")) or inputs.get("mold-version", "2.41.0") != "2.41.0":
                continue
            break
        else:
            raise BuildProvisioningError(
                f"{workflow_name}:{job_name} needs unconditional prior setup-rust with mold 2.41.0"
            )

        for suite in job_suites:
            if suite.route == "make":
                evaluation = evaluate_make_goal(suite.make_goal)
                _require_make_preflight(evaluation, f"Make goal {suite.make_goal!r}")


def assert_make_goal_provisioning(goal: str) -> MakeEvaluation:
    """Require one Make goal's evaluated Rust suite to reach tool preflight."""
    evaluation = evaluate_make_goal(goal)
    _require_make_preflight(evaluation, f"Make goal {goal!r}")
    return evaluation
