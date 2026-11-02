"""Classify the Cargo commands a workflow step or Make recipe invokes.

Only development suite commands count as Rust suite routes; every other Cargo
command must be a known non-suite command, so an unrecognized invocation fails
closed instead of hiding a suite.
"""

from __future__ import annotations

import re

from build_provisioning_common import BuildProvisioningError

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
