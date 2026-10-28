//! Installer ordering and toolchain provisioning contracts.
//!
//! This private child module keeps installer cases separate from preflight
//! contracts while reusing their inert, capability-rooted tool fixture.

use super::{PathPolicy, Sandbox, ToolScenario, output_text};

/// Finds the first trace line containing every requested fragment.
fn line_position(trace: &str, fragments: &[&str]) -> Option<usize> {
    trace
        .lines()
        .position(|line| fragments.iter().all(|fragment| line.contains(fragment)))
}

/// Returns the host-pinned mold archive name and digest.
fn host_mold_archive() -> (&'static str, &'static str) {
    match std::env::consts::ARCH {
        "x86_64" => (
            "mold-2.41.0-x86_64-linux.tar.gz",
            "a3696680d99e692970590a178bc3a33d78d60d1c6dc9db7a11b557b02b751f5d",
        ),
        "aarch64" => (
            "mold-2.41.0-aarch64-linux.tar.gz",
            "946de2774b06a71346bd59b55fddba610b65b8d93c3a4a1559cc84e103472710",
        ),
        architecture => panic!("unsupported installer test architecture: {architecture}"),
    }
}

/// Asserts the installer verified the archive before extracting it.
fn assert_verified_check_mode(trace: &str) {
    let flag = trace
        .lines()
        .find_map(|line| line.strip_prefix("sha256sum:"))
        .and_then(|arguments| {
            arguments
                .split_ascii_whitespace()
                .find(|argument| *argument == "-c" || *argument == "--check")
        });
    assert!(
        flag.is_some(),
        "sha256sum was not invoked in check mode:\n{trace}"
    );
}

/// Asserts the pinned mold archive and checksum entry reached the download stage.
fn assert_pinned_archive(trace: &str, archive: &str, digest: &str) {
    let checksum_entry = format!("checksum-entry:{digest}  {archive}");
    assert!(
        trace.contains(archive),
        "installer did not request the host archive {archive}:\n{trace}"
    );
    assert!(
        trace.lines().any(|line| line == checksum_entry.as_str()),
        "sha256sum did not receive the pinned entry {checksum_entry}:\n{trace}"
    );
}

/// Asserts the existing pinned toolchain was reused and mold landed locally.
fn assert_reused_toolchain_and_local_mold(trace: &str, installed_mold: &str) {
    assert!(
        !trace.contains("toolchain install"),
        "installer reinstalled an existing pinned toolchain:\n{trace}"
    );
    assert!(
        installed_mold.contains("mold 2.41.0"),
        "installer did not place the pinned mold in its local prefix:\n{installed_mold}"
    );
}

/// Verifies checksum-before-extraction and component addition for old mold.
#[test]
fn installer_verifies_before_extracting_and_adds_the_component_to_an_existing_nightly() {
    let sandbox = Sandbox::new(ToolScenario::UpgradeMoldAndAddAnalyzer)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(
            Some("install-build-tools"),
            &[],
            PathPolicy::IncludeSystemTools,
        )
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(
        output.status.success(),
        "offline installer failed:\n{transcript}"
    );
    assert!(
        !transcript.contains("cargo-marker"),
        "tool installation must not invoke Cargo:\n{transcript}"
    );
    let trace = match sandbox.trace() {
        Ok(trace) => trace,
        Err(error) => panic!("could not read installer trace:\n{transcript}\n{error}"),
    };
    let download = line_position(&trace, &["curl:"]).expect("installer did not download mold");
    let verification = line_position(&trace, &["sha256sum:"])
        .expect("installer did not verify the downloaded archive");
    let extraction = line_position(&trace, &["tar:"]).expect("installer did not extract mold");
    let component = line_position(
        &trace,
        &[
            "rustup:",
            "component add",
            "rust-analyzer",
            "nightly-2025-09-16",
        ],
    )
    .expect("installer did not add rust-analyzer to the pinned nightly");
    assert_verified_check_mode(&trace);
    let installed_mold = sandbox
        .installed_mold()
        .expect("installer should place mold under its private HOME prefix");
    let (archive, digest) = host_mold_archive();
    assert!(
        download < verification && verification < extraction && extraction < component,
        "installer stages were out of order:\n{trace}"
    );
    assert_pinned_archive(&trace, archive, digest);
    assert_reused_toolchain_and_local_mold(&trace, &installed_mold);
}

/// Reuses a correctly versioned CI mold and adds only its missing component.
#[test]
fn installer_reuses_correct_ci_tools_and_only_adds_the_missing_component() {
    let sandbox = Sandbox::new(ToolScenario::ReuseCiMoldAndAddAnalyzer)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(
            Some("install-build-tools"),
            &[],
            PathPolicy::IncludeSystemTools,
        )
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(
        output.status.success(),
        "preinstalled CI tools were rejected:\n{transcript}"
    );
    let trace = sandbox.trace().expect("fake tool calls should be recorded");
    assert!(
        !trace.contains("curl:") && !trace.contains("tar:"),
        "installer redownloaded or re-extracted the correct CI mold:\n{trace}"
    );
    assert!(
        trace.contains("component add") && trace.contains("rust-analyzer"),
        "installer did not add the missing pinned component:\n{trace}"
    );
    assert!(
        !trace.contains("toolchain install"),
        "installer duplicated the existing setup-rust toolchain:\n{trace}"
    );
}

/// Installs the pinned nightly and analyzer when neither is available.
#[test]
fn installer_sets_up_a_missing_nightly_with_rust_analyzer() {
    let sandbox = Sandbox::new(ToolScenario::InstallMissingNightly)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(
            Some("install-build-tools"),
            &[],
            PathPolicy::IncludeSystemTools,
        )
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(
        output.status.success(),
        "offline nightly setup failed:\n{transcript}"
    );
    let trace = sandbox.trace().expect("fake tool calls should be recorded");
    let toolchain = line_position(
        &trace,
        &["rustup:", "toolchain install", "nightly-2025-09-16"],
    )
    .expect("installer did not create the missing pinned nightly");
    let toolchain_line = trace
        .lines()
        .nth(toolchain)
        .expect("toolchain install trace position should identify a line");
    let component_add = line_position(
        &trace,
        &[
            "rustup:",
            "component add",
            "rust-analyzer",
            "nightly-2025-09-16",
        ],
    );
    assert!(
        toolchain_line.contains("rust-analyzer")
            || component_add.is_some_and(|position| position > toolchain),
        "installer did not provision rust-analyzer with the missing nightly:\n{trace}"
    );
}

/// Prevents archive extraction and rustup after a failed checksum.
#[test]
fn failed_archive_checksum_stops_before_extraction_and_rustup() {
    let sandbox = Sandbox::new(ToolScenario::BadArchiveChecksum)
        .expect("isolated fake tools should be staged successfully");
    let output = sandbox
        .run_make(
            Some("install-build-tools"),
            &[],
            PathPolicy::IncludeSystemTools,
        )
        .expect("GNU make should run in the repository");
    let transcript = output_text(&output);
    assert!(
        !output.status.success(),
        "bad archive checksum passed:\n{transcript}"
    );
    let trace = match sandbox.trace() {
        Ok(trace) => trace,
        Err(error) => panic!("could not read checksum-failure trace:\n{transcript}\n{error}"),
    };
    assert!(
        trace.contains("curl:"),
        "installer did not download the fixture:\n{trace}"
    );
    assert!(
        trace.contains("sha256sum:"),
        "installer did not check the fixture checksum:\n{trace}"
    );
    assert!(
        !trace.contains("tar:") && !trace.contains("component add"),
        "failed checksum reached extraction or rustup:\n{trace}"
    );
}
