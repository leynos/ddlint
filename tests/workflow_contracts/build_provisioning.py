"""Discover local Linux Rust suite consumers.

Checks their approved tool setup and Make prerequisites. CV-005 remains the
strict workflow reader and canonical workflow-policy owner. The implementation
is split by concern across sibling modules; this module is the stable import
surface for the contract tests.
"""

from __future__ import annotations

from build_provisioning_checks import (
    _soft_fails,
    assert_make_goal_provisioning,
    assert_suite_provisioning,
)
from build_provisioning_common import (
    APPROVED_SHARED_ACTIONS_REF,
    COVERAGE_ACTION,
    DEPENDABOT_AUTOMERGE,
    MOLD_VERSION,
    MUTATION_CARGO,
    PROVISIONING_TARGET,
    ROOT,
    SETUP_RUST,
    SHARED_ACTION_PREFIX,
    WORKFLOWS_DIRECTORY,
    BuildProvisioningError,
    MakeEvaluation,
    StepLocation,
    SuiteConsumer,
    read_repository_workflows,
)
from build_provisioning_make import _make_goals, evaluate_make_goal
from build_provisioning_matrix import linux_runner_rows
from build_provisioning_workflows import discover_linux_suites

__all__ = [
    "APPROVED_SHARED_ACTIONS_REF",
    "COVERAGE_ACTION",
    "DEPENDABOT_AUTOMERGE",
    "MOLD_VERSION",
    "MUTATION_CARGO",
    "PROVISIONING_TARGET",
    "ROOT",
    "SETUP_RUST",
    "SHARED_ACTION_PREFIX",
    "WORKFLOWS_DIRECTORY",
    "BuildProvisioningError",
    "MakeEvaluation",
    "StepLocation",
    "SuiteConsumer",
    "_make_goals",
    "_soft_fails",
    "assert_make_goal_provisioning",
    "assert_suite_provisioning",
    "discover_linux_suites",
    "evaluate_make_goal",
    "linux_runner_rows",
    "read_repository_workflows",
]
