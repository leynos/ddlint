//! Private local contracts for build-tool installation and preflight.
//!
//! These cases are called only by `build_standard_contract.rs` and exercise
//! Make through injected Cargo and inert child tools.

use camino::Utf8Path;
use cap_std::{ambient_authority, fs_utf8::Dir};
use rstest::rstest;
use std::process::Output;

#[path = "provisioning_fixtures.rs"]
mod fixtures;
#[path = "provisioning_installation_tests.rs"]
mod installation_tests;
use fixtures::{PathPolicy, Sandbox, ToolScenario};

/// Combines child stdout and stderr for useful contract failure messages.
fn output_text(output: &Output) -> String {
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

/// Runs one Make goal in a staged sandbox, returning it, its success, and its transcript.
fn run_goal(
    scenario: ToolScenario,
    goal: &str,
    variables: &[&str],
) -> std::io::Result<(Sandbox, bool, String)> {
    let sandbox = Sandbox::new(scenario)?;
    let output = sandbox.run_make(Some(goal), variables, PathPolicy::IncludeSystemTools)?;
    let transcript = output_text(&output);
    Ok((sandbox, output.status.success(), transcript))
}

/// Confirms both approved Linux mold archives remain pinned in repository data.
#[test]
fn pinned_mold_data_names_both_approved_linux_archives() {
    let repository = Dir::open_ambient_dir(
        Utf8Path::new(env!("CARGO_MANIFEST_DIR")),
        ambient_authority(),
    )
    .expect("repository root should be readable through a capability");
    let version = repository
        .read_to_string("tools/mold/VERSION")
        .expect("pinned mold version should be present");
    let checksums = repository
        .read_to_string("tools/mold/SHA256SUMS")
        .expect("pinned mold checksums should be present");
    assert_eq!(version.trim(), "2.41.0");
    for expected in [
        "a3696680d99e692970590a178bc3a33d78d60d1c6dc9db7a11b557b02b751f5d  mold-2.41.0-x86_64-linux.tar.gz",
        "946de2774b06a71346bd59b55fddba610b65b8d93c3a4a1559cc84e103472710  mold-2.41.0-aarch64-linux.tar.gz",
    ] {
        assert!(
            checksums.lines().any(|line| line == expected),
            "missing pinned checksum entry: {expected}\n{checksums}"
        );
    }
}

/// Keeps bare Make on the development Cargo route despite a fresh target file.
#[test]
fn bare_make_stays_a_development_build_when_its_output_is_assumed_old() {
    let sandbox = Sandbox::new(ToolScenario::Development)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(
            None,
            &["-o", "target/debug/ddlint"],
            PathPolicy::SandboxOnly,
        )
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(output.status.success(), "bare make failed:\n{transcript}");
    assert!(
        transcript.contains("cargo-marker build") && transcript.contains("--bin ddlint"),
        "bare make did not invoke the development Cargo build:\n{transcript}"
    );
}

/// Ensures the explicit build target invokes Cargo even when its output is old.
#[test]
fn build_runs_cargo_even_when_make_is_told_its_binary_is_old() {
    let sandbox = Sandbox::new(ToolScenario::Development)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(
            Some("build"),
            &["-o", "target/debug/ddlint"],
            PathPolicy::SandboxOnly,
        )
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(output.status.success(), "make build failed:\n{transcript}");
    assert!(
        transcript.contains("cargo-marker build") && transcript.contains("--bin ddlint"),
        "make build skipped the Cargo invocation for an assumed-old binary:\n{transcript}"
    );
}

/// Requires each development route to stop at missing-mold preflight.
#[rstest]
#[case::development_build("build")]
#[case::unit_tests("test")]
#[case::typecheck("typecheck")]
#[case::lint_suite("lint")]
#[case::clippy_policy("test-clippy-policy")]
fn build_routes_fail_preflight_before_the_injected_cargo(#[case] target: &str) {
    let sandbox = Sandbox::new(ToolScenario::MissingMold)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(Some(target), &[], PathPolicy::SandboxOnly)
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(
        !output.status.success(),
        "missing mold passed preflight:\n{transcript}"
    );
    assert!(
        transcript.contains("mold"),
        "failure did not identify mold:\n{transcript}"
    );
    assert!(
        !transcript.contains("cargo-marker"),
        "{target} reached Cargo after preflight failed:\n{transcript}"
    );
    if target == "lint" {
        assert!(
            transcript.contains("interrogate"),
            "lint no longer runs interrogate before its build route:\n{transcript}"
        );
    }
    if target == "test-clippy-policy" {
        assert!(
            !transcript.contains("scripts/clippy_expect_policy_test.py"),
            "test-clippy-policy reached its script after preflight failed:\n{transcript}"
        );
    }
}

/// Requires actionable guidance when the available mold has the wrong version.
#[test]
fn check_build_tools_rejects_a_wrong_mold_version_with_an_actionable_error() {
    let (_sandbox, success, transcript) =
        run_goal(ToolScenario::WrongMold, "check-build-tools", &[])
            .expect("check-build-tools should run in the repository");
    assert!(!success, "wrong mold version passed:\n{transcript}");
    assert!(
        transcript.contains("mold") && transcript.contains("2.41.0"),
        "wrong-version guidance should name mold and its pinned version:\n{transcript}"
    );
}

/// Requires actionable guidance when the pinned nightly is unavailable.
#[test]
fn check_build_tools_rejects_a_missing_pinned_toolchain_actionably() {
    let (_sandbox, success, transcript) =
        run_goal(ToolScenario::MissingNightly, "check-build-tools", &[])
            .expect("check-build-tools should run in the repository");
    assert!(!success, "missing nightly passed:\n{transcript}");
    assert!(
        transcript.contains("nightly-2025-09-16"),
        "missing-toolchain guidance should name the pinned nightly:\n{transcript}"
    );
}

/// Confirms simulated non-Linux preflight succeeds without a mold check.
#[test]
fn check_build_tools_has_no_mold_requirement_for_a_non_linux_make_host() {
    let (_sandbox, success, transcript) = run_goal(
        ToolScenario::MissingMold,
        "check-build-tools",
        &["BUILD_HOST_OS=Darwin"],
    )
    .expect("check-build-tools should run in the repository");
    assert!(
        success,
        "non-Linux preflight was unavailable or failed:\n{transcript}"
    );
    assert!(
        !transcript.contains("mold"),
        "non-Linux preflight still checked the Linux-only linker:\n{transcript}"
    );
}

/// Preserves the existing interrogate, Cargo, script, Whitaker, spelling order.
#[test]
fn lint_keeps_interrogate_cargo_whitaker_and_spelling_order() {
    let sandbox = Sandbox::new(ToolScenario::Development)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(Some("lint"), &[], PathPolicy::IncludeSystemTools)
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(
        output.status.success(),
        "injected lint route failed:\n{transcript}"
    );
    let interrogate = transcript
        .find("interrogate-marker")
        .expect("lint did not run the interrogate prerequisite");
    let cargo = transcript
        .find("cargo-marker clippy")
        .expect("lint did not reach the injected Cargo command");
    let script = transcript
        .find("scripts/clippy_expect_policy_test.py")
        .expect("lint did not reach the Clippy policy script");
    let whitaker = transcript
        .find("--all -- --all-targets --all-features")
        .expect("lint did not reach the injected Whitaker command");
    let spelling = transcript
        .find("gate --repository .")
        .expect("lint did not reach the injected spelling command");
    assert!(
        interrogate < cargo && cargo < script && script < whitaker && whitaker < spelling,
        "lint command order changed:\n{transcript}"
    );
}
