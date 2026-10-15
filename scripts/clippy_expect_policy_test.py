"""Measure the repository's Clippy test-body boundary without nesting Cargo in Rust tests."""

from __future__ import annotations

import json
import os
from pathlib import Path
import tomllib
from typing import TypedDict

from plumbum import local
import pytest

ROOT = Path(__file__).resolve().parents[1]
SPECIMEN = ROOT / "tests" / "clippy_expect_policy.rs"


class _DiagnosticSpan(TypedDict):
    """Cargo diagnostic fields needed to identify the handwritten specimen."""

    is_primary: bool
    file_name: str
    line_start: int
    line_end: int


def _expect_lines(*reasons: str) -> set[int]:
    """Locate required source specimens, refusing missing or duplicate markers."""
    source = SPECIMEN.read_text(encoding="utf-8").splitlines()
    lines = set()
    for reason in reasons:
        matches = [index for index, line in enumerate(source, 1) if reason in line]
        assert len(matches) == 1, f"expected one specimen for {reason!r}: {matches}"
        lines.add(matches[0])
    return lines


def _expect_locations(spans: list[_DiagnosticSpan]) -> set[tuple[int, int]]:
    """Validate primary expect spans for the private Cargo diagnostic reader."""
    primary = [span for span in spans if span["is_primary"]]
    assert primary, "expect_used must identify its source location"
    locations = set()
    for span in primary:
        assert Path(span["file_name"]).name == SPECIMEN.name, span
        locations.add((span["line_start"], span["line_end"]))
    return locations


def _diagnostics(output: str) -> tuple[set[tuple[int, int]], list[str], bool]:
    """Return expect lint locations and all other compiler errors from Cargo JSON."""
    locations = set()
    unexpected = []
    builds = []
    for line in output.splitlines():
        event = json.loads(line)
        if event.get("reason") == "build-finished":
            builds.append(event["success"])
        if event.get("reason") != "compiler-message":
            continue
        message = event["message"]
        if message["level"] != "error":
            continue
        code = (message.get("code") or {}).get("code")
        if code != "clippy::expect_used":
            unexpected.append(message["message"])
            continue
        locations.update(_expect_locations(message["spans"]))
    assert len(builds) == 1, f"expected one completed Cargo build: {builds}"
    return locations, unexpected, builds[0]


@pytest.mark.parametrize("mode", ["accepted", "helpers_rejected", "permission_removed"])
def test_clippy_expect_boundary(mode: str, tmp_path: Path) -> None:
    """Check positive bodies, negative helpers, and a real configuration mutation."""
    config_text = (ROOT / "clippy.toml").read_text(encoding="utf-8")
    assert tomllib.loads(config_text)["allow-expect-in-tests"] is True
    manifest = tomllib.loads((ROOT / "Cargo.toml").read_text(encoding="utf-8"))
    assert manifest["lints"]["clippy"]["expect_used"] == "deny"
    if mode == "permission_removed":
        config_text = config_text.replace(
            "allow-expect-in-tests = true", "allow-expect-in-tests = false"
        )
    (tmp_path / "clippy.toml").write_text(config_text, encoding="utf-8")
    args = [
        "clippy", "--locked", "--test", "clippy_expect_policy",
        "--message-format=json", "--", "-D", "warnings",
    ]
    if mode == "helpers_rejected":
        args.extend(["--cfg", "clippy_policy_negative"])
    cargo = local[os.environ["CLIPPY_POLICY_CARGO"]]
    with local.cwd(ROOT), local.env(CLIPPY_CONF_DIR=str(tmp_path)):
        status, output, errors = cargo[args].run(retcode=None)
    locations, unexpected, succeeded = _diagnostics(output)
    assert not unexpected, (unexpected, errors)
    expected = set()
    if mode == "helpers_rejected":
        expected = _expect_lines("negative ordinary helper specimen", "negative fixture specimen")
    elif mode == "permission_removed":
        expected = _expect_lines("the test input is a valid byte", "the case supplies a valid byte")
    # A formatter can split the method chain before the reason string. Match
    # the primary source span to that exact marker rather than its first line.
    matched = [{line for line in expected if start <= line <= end} for start, end in locations]
    assert len(locations) == len(expected), (mode, locations, expected, errors)
    assert all(len(lines) == 1 for lines in matched), (mode, matched, errors)
    assert set().union(*matched) == expected, (mode, matched, expected, errors)
    assert status == (101 if expected else 0), (mode, status, errors)
    assert succeeded is (not expected), (mode, succeeded, errors)
