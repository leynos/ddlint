"""Consumer contracts for the protected CodeScene coverage publisher."""

from __future__ import annotations

import re
from pathlib import Path

import pytest
import yaml
from yaml.constructor import ConstructorError
from yaml.nodes import MappingNode


WORKFLOW_PATH = (
    Path(__file__).parents[2] / ".github" / "workflows" / "coverage-main.yml"
)
TOKEN_STEP_NAME = "Check CodeScene token"
UPLOAD_STEP_NAME = "Upload coverage data to CodeScene"
TOKEN_CHECK_COMMAND = (
    'echo "available=${{ secrets.CS_ACCESS_TOKEN != \'\' }}" '
    '>> "$GITHUB_OUTPUT"'
)
TOKEN_OUTPUT_GUARD = (
    "steps.codescene-token.outputs.available == 'true' && "
    "github.ref == 'refs/heads/main'"
)
WORKFLOW_REF_GROUP = "${{ github.workflow }}-${{ github.ref }}"
ACCESS_TOKEN_EXPRESSION = "${{ secrets.CS_ACCESS_TOKEN }}"


class _UniqueSafeLoader(yaml.SafeLoader):
    """Parse GitHub's `on` key as text and reject duplicate workflow keys."""

    yaml_implicit_resolvers = {
        initial: [
            (tag, pattern)
            for tag, pattern in resolvers
            if tag != "tag:yaml.org,2002:bool"
        ]
        for initial, resolvers in yaml.SafeLoader.yaml_implicit_resolvers.items()
    }

    def construct_mapping(
        self, node: MappingNode, deep: bool = False
    ) -> dict[str, object]:
        """Build string-keyed mappings and reject repeated keys."""
        self.flatten_mapping(node)
        mapping: dict[str, object] = {}
        for key_node, value_node in node.value:
            key = self.construct_object(key_node, deep=deep)
            if not isinstance(key, str):
                raise ConstructorError(
                    "while constructing a workflow mapping",
                    node.start_mark,
                    "found a non-string mapping key",
                    key_node.start_mark,
                )
            if key in mapping:
                raise ConstructorError(
                    "while constructing a workflow mapping",
                    node.start_mark,
                    f"found duplicate key {key!r}",
                    key_node.start_mark,
                )
            mapping[key] = self.construct_object(value_node, deep=deep)
        return mapping


_UniqueSafeLoader.add_implicit_resolver(
    "tag:yaml.org,2002:bool",
    re.compile(r"^(?:true|True|TRUE|false|False|FALSE)$"),
    list("tTfF"),
)


@pytest.fixture(scope="module")
def coverage_workflow_text() -> str:
    """Read the one publisher document under test."""
    return WORKFLOW_PATH.read_text(encoding="utf-8")


def _load_workflow(workflow_text: str) -> dict[str, object]:
    """Load a single workflow document with strict mapping keys."""
    workflow = yaml.load(workflow_text, Loader=_UniqueSafeLoader)
    if not isinstance(workflow, dict) or any(
        not isinstance(key, str) for key in workflow
    ):
        raise AssertionError("workflow root must be a string-keyed mapping")
    return {key: value for key, value in workflow.items() if isinstance(key, str)}


def _mapping_field(
    mapping: dict[str, object], key: str, description: str
) -> dict[str, object]:
    """Return a required string-keyed mapping field."""
    return _string_keyed_mapping(
        mapping.get(key), f"{description} must be a mapping"
    )


def _string_keyed_mapping(value: object, failure_message: str) -> dict[str, object]:
    """Validate and return a mapping whose keys are all strings."""
    if not isinstance(value, dict) or any(
        not isinstance(key, str) for key in value
    ):
        raise AssertionError(failure_message)
    return {key: item for key, item in value.items() if isinstance(key, str)}


def _workflow_steps(workflow: dict[str, object]) -> list[dict[str, object]]:
    """Return the coverage publisher job's workflow steps."""
    jobs = _mapping_field(workflow, "jobs", "jobs")
    publisher = _mapping_field(jobs, "coverage-upload", "coverage publisher job")
    raw_steps = publisher.get("steps")
    if not isinstance(raw_steps, list):
        raise AssertionError("coverage publisher steps must be a sequence")
    return [
        _string_keyed_mapping(step, "coverage publisher steps must be mappings")
        for step in raw_steps
    ]


def _find_unique_step(
    steps: list[dict[str, object]], field: str, value: str, description: str
) -> tuple[int, dict[str, object]]:
    """Find exactly one step by an identifying field."""
    matching = [
        (index, step) for index, step in enumerate(steps) if step.get(field) == value
    ]
    if len(matching) != 1:
        raise AssertionError(f"exactly one {description} is required")
    return matching[0]


def _require_equal(actual: object, expected: object, message: str) -> None:
    """Raise a contract failure when one publisher value differs."""
    if actual != expected:
        raise AssertionError(message)


def _assert_publisher_contract(workflow_text: str) -> None:
    """Check the publisher's workflow/ref serialization and token boundary."""
    workflow = _load_workflow(workflow_text)
    _mapping_field(workflow, "on", "event triggers")

    concurrency = _mapping_field(workflow, "concurrency", "concurrency")
    _require_equal(
        concurrency.get("group"),
        WORKFLOW_REF_GROUP,
        "concurrency group must include workflow and ref",
    )
    if concurrency.get("cancel-in-progress") is not False:
        raise AssertionError("publisher runs must not cancel in progress")

    steps = _workflow_steps(workflow)
    token_index, token_step = _find_unique_step(
        steps, "id", "codescene-token", "codescene-token step"
    )
    _require_equal(
        token_step.get("name"),
        TOKEN_STEP_NAME,
        "token check must use its canonical name",
    )
    _require_equal(
        token_step.get("run"),
        TOKEN_CHECK_COMMAND,
        "token presence command must match the canonical check",
    )

    upload_index, upload_step = _find_unique_step(
        steps, "name", UPLOAD_STEP_NAME, "CodeScene upload step"
    )
    if token_index >= upload_index:
        raise AssertionError("token check must precede the upload step")
    _require_equal(
        upload_step.get("if"),
        TOKEN_OUTPUT_GUARD,
        "upload guard must require the token and main ref",
    )
    upload_inputs = _mapping_field(upload_step, "with", "upload inputs")
    _require_equal(
        upload_inputs.get("access-token"),
        ACCESS_TOKEN_EXPRESSION,
        "upload must pass the secret through access-token",
    )


def _replace_once(workflow_text: str, old: str, new: str) -> str:
    """Apply one anchored in-memory mutation, failing on ambiguous anchors."""
    if workflow_text.count(old) != 1:
        raise AssertionError("mutation anchor must occur exactly once")
    return workflow_text.replace(old, new, 1)


def _move_token_check_after_upload(workflow_text: str) -> str:
    """Swap the two adjacent steps without writing the workflow file."""
    lines = workflow_text.splitlines(keepends=True)

    def step_range(name: str) -> tuple[int, int]:
        """Find one named sequence item and the next item's boundary."""
        header = f"      - name: {name}\n"
        starts = [index for index, line in enumerate(lines) if line == header]
        if len(starts) != 1:
            raise AssertionError("step-order mutation anchor must be unique")
        start = starts[0]
        end = next(
            (
                index
                for index in range(start + 1, len(lines))
                if lines[index].startswith("      - ")
            ),
            len(lines),
        )
        return start, end

    token_start, token_end = step_range(TOKEN_STEP_NAME)
    upload_start, upload_end = step_range(UPLOAD_STEP_NAME)
    if token_end != upload_start:
        raise AssertionError("token and upload steps must be adjacent for mutation")
    return "".join(
        lines[:token_start]
        + lines[upload_start:upload_end]
        + lines[token_start:token_end]
        + lines[upload_end:]
    )


def test_current_publisher_satisfies_its_consumer_contract(
    coverage_workflow_text: str,
) -> None:
    """Accept the main publisher's workflow/ref guard and token boundary."""
    _assert_publisher_contract(coverage_workflow_text)


@pytest.mark.parametrize(
    ("old", "new", "failure"),
    [
        pytest.param(
            "group: ${{ github.workflow }}-${{ github.ref }}",
            "group: coverage-main-${{ github.ref }}",
            "concurrency group must include workflow and ref",
            id="reject-legacy-ref-only-group",
        ),
        pytest.param(
            "cancel-in-progress: false",
            "cancel-in-progress: true",
            "publisher runs must not cancel in progress",
            id="reject-cancellation",
        ),
        pytest.param(
            "cancel-in-progress: false",
            "cancel-in-progress: 0",
            "publisher runs must not cancel in progress",
            id="reject-non-boolean-cancellation-value",
        ),
        pytest.param(
            "- name: Check CodeScene token",
            "- name: Check coverage token",
            "token check must use its canonical name",
            id="reject-token-name-change",
        ),
        pytest.param(
            "id: codescene-token",
            "id: coverage-token",
            "exactly one codescene-token step is required",
            id="reject-token-id-change",
        ),
        pytest.param(
            'secrets.CS_ACCESS_TOKEN != \'\'',
            'secrets.CS_ACCESS_TOKEN == \'\'',
            "token presence command must match the canonical check",
            id="reject-token-command-change",
        ),
        pytest.param(
            "github.ref == 'refs/heads/main'",
            "github.ref == 'refs/heads/main' "
            "|| github.event_name == 'workflow_dispatch'",
            "upload guard must require the token and main ref",
            id="reject-dispatch-guard-bypass",
        ),
        pytest.param(
            "access-token: ${{ secrets.CS_ACCESS_TOKEN }}",
            "access-token: ${{ env.CS_ACCESS_TOKEN }}",
            "upload must pass the secret through access-token",
            id="reject-token-input-change",
        ),
    ],
)
def test_rejects_publisher_contract_mutations(
    coverage_workflow_text: str, old: str, new: str, failure: str
) -> None:
    """Reject finite mutations to the publisher's required local contract."""
    mutated = _replace_once(coverage_workflow_text, old, new)
    with pytest.raises(AssertionError, match=re.escape(failure)):
        _assert_publisher_contract(mutated)


def test_rejects_token_check_after_upload(coverage_workflow_text: str) -> None:
    """Reject moving the token check below its guarded uploader."""
    mutated = _move_token_check_after_upload(coverage_workflow_text)
    with pytest.raises(
        AssertionError, match=re.escape("token check must precede the upload step")
    ):
        _assert_publisher_contract(mutated)


def test_rejects_duplicate_workflow_mapping_keys(coverage_workflow_text: str) -> None:
    """Reject duplicate YAML keys before contract assertions can pass."""
    group = "  group: ${{ github.workflow }}-${{ github.ref }}\n"
    mutated = _replace_once(
        coverage_workflow_text,
        group,
        group + "  group: coverage-main-${{ github.ref }}\n",
    )
    with pytest.raises(
        ConstructorError, match=re.escape("found duplicate key 'group'")
    ):
        _assert_publisher_contract(mutated)


@pytest.mark.parametrize(
    ("invalid_step", "failure"),
    [
        pytest.param(
            None,
            "coverage publisher steps must be mappings",
            id="reject-non-mapping-step",
        ),
        pytest.param(
            {1: "invalid step key"},
            "coverage publisher steps must be mappings",
            id="reject-non-string-step-key",
        ),
    ],
)
def test_rejects_invalid_step_mappings(invalid_step: object, failure: str) -> None:
    """Reject malformed step values before checking publisher policy."""
    workflow: dict[str, object] = {
        "jobs": {"coverage-upload": {"steps": [invalid_step]}}
    }
    with pytest.raises(AssertionError, match=re.escape(failure)):
        _workflow_steps(workflow)
