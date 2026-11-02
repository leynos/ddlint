"""Evaluate Make goals and classify the Cargo commands they reach.

GNU Make is asked for a dry-run trace so the contract can prove that the
provisioning target precedes a Rust suite without executing the recipe.
"""

from __future__ import annotations

from functools import lru_cache
import os
import re
import shutil
import subprocess

from build_provisioning_common import (
    BuildProvisioningError,
    MakeEvaluation,
    PROVISIONING_TARGET,
    ROOT,
)

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


def _make_goals(script: str) -> tuple[str | None, ...]:
    """Find simple Make goal lists, including the repository's default goal."""
    normalized = script.replace("\\\n", " ")
    _reject_unsupported_make_context(normalized)
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


def _reject_unsupported_make_context(normalized: str) -> None:
    """Refuse Make suites that change directory or override Make's environment."""
    if MAKE_CALL.search(normalized) and re.search(r"(?:^|[\r\n;&|]\s*)(?:cd|pushd)\b", normalized):
        raise BuildProvisioningError("Make suite calls must run from the repository root")
    if MAKE_CALL.search(normalized) and re.search(r"\b(?:MAKEFLAGS|GNUMAKEFLAGS|MAKEFILES)\s*=", normalized):
        raise BuildProvisioningError("Make environment overrides are unsupported")


def _rust_suite_line(line: str) -> bool:
    """Recognize development Rust suite commands in Make's printed recipe."""
    for match in CARGO_CALL.finditer(line):
        command = match.group("command")
        arguments = match.group("arguments")
        if command not in RUST_SUITE_COMMANDS and command not in NON_SUITE_CARGO_COMMANDS:
            raise BuildProvisioningError(f"unclassified Cargo command in Make output: {line.strip()}")
        if _is_suite_invocation(command, arguments):
            return True
    return False


@lru_cache(maxsize=None)
def evaluate_make_goal(goal: str | None) -> MakeEvaluation:
    """Read the Make target closure and recipe order without executing it.

    Parameters
    ----------
    goal : str | None
        Make goal to evaluate, or ``None`` for the default goal.

    Returns
    -------
    MakeEvaluation
        Trace line indices of Rust suite commands and preflight events.

    Raises
    ------
    BuildProvisioningError
        If GNU Make is missing or cannot evaluate the goal.
    """
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


def _is_suite_invocation(command: str, arguments: str) -> bool:
    """Report whether one Cargo command is a development suite invocation."""
    return command in RUST_SUITE_COMMANDS and (command != "build" or "--release" not in arguments)


def _cargo_routes(script: str, context: str) -> tuple[str, ...]:
    """Identify direct suite commands and reject unknown Cargo routes."""
    routes = [
        route
        for match in CARGO_CALL.finditer(script)
        if (route := _classify_cargo_match(match, context)) is not None
    ]
    if "cargo" in script and not CARGO_CALL.search(script):
        raise BuildProvisioningError(f"{context} has an unresolved Cargo command")
    return tuple(routes)


def _classify_cargo_match(match: re.Match[str], context: str) -> str | None:
    """Return the suite route for one Cargo match, or None for other commands."""
    command = match.group("command")
    if command not in RUST_SUITE_COMMANDS and command not in NON_SUITE_CARGO_COMMANDS:
        raise BuildProvisioningError(f"{context} has an unclassified Cargo command {command!r}")
    if not _is_suite_invocation(command, match.group("arguments")):
        return None
    return f"{match.group('tool')}:{command}"
