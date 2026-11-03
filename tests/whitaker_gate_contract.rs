//! Contract for the Whitaker gate as its own `make` target.
//!
//! `make lint` enforces Whitaker only while two things hold: the gate runs
//! Whitaker over the workspace with the literal warning policy, and both the
//! gate target and the aggregate `lint` target fail when Whitaker fails. These
//! tests run the real Makefile with a controlled failing Whitaker, and read
//! `make -n` and the make database for the wiring that cannot be executed
//! cheaply.

// The contract drives the Makefile with generated `/bin/sh` control executables
// and sets their mode with `PermissionsExt`, both of which are Unix-only, so
// the target is compiled only on Unix rather than failing the whole
// `cargo test` build on Windows.
#![cfg(unix)]

#[path = "whitaker_gate_support/controlled.rs"]
mod controlled;

use controlled::{
    Behaviour,
    CARGO_PAST_CLIPPY_MARKER,
    CLIPPY_FORBIDDEN,
    CLIPPY_MARKER,
    ControlledTool,
    Invocation,
    SPELLING_MARKER,
    Stopping,
    ToolSpec,
    WHITAKER_MARKER,
    assert_fails,
    assert_stops_after,
};

/// Scenario: the gate target with a controlled failing Whitaker.
///
/// Invariant: the tool's failure reaches the target's exit status, so nothing
/// in the recipe swallows it.
#[test]
fn the_gate_target_fails_when_whitaker_fails() -> Result<(), String> {
    let controlled = ControlledTool::new(ToolSpec {
        name: "whitaker",
        marker: WHITAKER_MARKER,
        behaviour: Behaviour::AlwaysFails,
    })?;
    let whitaker = format!("WHITAKER={}", controlled.path());
    assert_fails(
        &Invocation::new(["lint-whitaker", whitaker.as_str()]),
        WHITAKER_MARKER,
    )
}

/// Scenario: the aggregate `lint` with a failing Whitaker, holding back the
/// gates that would compile the workspace.
///
/// Invariant: `lint` propagates the Whitaker failure, so the suite is part of
/// the aggregate rather than an optional extra.
#[test]
fn the_aggregate_fails_when_the_gate_target_fails() -> Result<(), String> {
    let controlled = ControlledTool::new(ToolSpec {
        name: "whitaker",
        marker: WHITAKER_MARKER,
        behaviour: Behaviour::AlwaysFails,
    })?;
    let whitaker = format!("WHITAKER={}", controlled.path());
    assert_fails(
        &Invocation::new([
            "-j4",
            "lint",
            "-o",
            "interrogate",
            "-o",
            "lint-clippy",
            "-o",
            "spelling",
            whitaker.as_str(),
        ]),
        WHITAKER_MARKER,
    )
}

/// Scenario: the `lint-clippy` target with a controlled Cargo that fails only
/// for `clippy`.
///
/// Invariant: the target fails on the Clippy failure itself. The pass marker
/// would appear if the recipe reached `cargo doc` instead, so its absence
/// proves the target stopped at Clippy.
#[test]
fn the_clippy_gate_fails_when_clippy_fails() -> Result<(), String> {
    let cargo = ControlledTool::new(ToolSpec {
        name: "cargo",
        marker: CLIPPY_MARKER,
        behaviour: Behaviour::FailsForClippyOnly(CARGO_PAST_CLIPPY_MARKER),
    })?;
    let cargo_variable = format!("CARGO={}", cargo.path());
    assert_stops_after(
        &Invocation::new(["lint-clippy", cargo_variable.as_str()]),
        Stopping {
            expected: CLIPPY_MARKER,
            forbidden: &[CARGO_PAST_CLIPPY_MARKER],
        },
    )
}

/// Scenario: the aggregate `lint` with a controlled failing Clippy gate.
///
/// Invariant: a failing Clippy run stops the aggregate, so the later Whitaker
/// and spelling gates never start and the first real failure is the one
/// reported.
#[test]
fn the_aggregate_stops_after_a_failing_clippy_gate() -> Result<(), String> {
    let cargo = ControlledTool::new(ToolSpec {
        name: "cargo",
        marker: CLIPPY_MARKER,
        behaviour: Behaviour::FailsForClippyOnly(CARGO_PAST_CLIPPY_MARKER),
    })?;
    let whitaker = ControlledTool::new(ToolSpec {
        name: "whitaker",
        marker: WHITAKER_MARKER,
        behaviour: Behaviour::AlwaysFails,
    })?;
    let spelling = ControlledTool::new(ToolSpec {
        name: "typos-config-builder",
        marker: SPELLING_MARKER,
        behaviour: Behaviour::AlwaysFails,
    })?;
    let cargo_variable = format!("CARGO={}", cargo.path());
    let whitaker_variable = format!("WHITAKER={}", whitaker.path());
    let spelling_variable = format!("TYPOS_CONFIG_BUILDER={}", spelling.path());
    assert_stops_after(
        &Invocation::new([
            "-j4",
            "lint",
            cargo_variable.as_str(),
            whitaker_variable.as_str(),
            spelling_variable.as_str(),
        ]),
        Stopping {
            expected: CLIPPY_MARKER,
            forbidden: &CLIPPY_FORBIDDEN,
        },
    )
}

/// Scenario: the printed gate command with a package selection override.
///
/// Invariant: `WHITAKER_PACKAGES` reaches Whitaker, so the suite's scope is
/// configurable without editing the recipe.
#[test]
fn the_packages_variable_reaches_the_gate() -> Result<(), String> {
    let stdout = Invocation::new([
        "-n",
        "-B",
        "lint-whitaker",
        "WHITAKER_PACKAGES=--package=ddlint",
    ])
    .printed()?;
    let expected = "whitaker --all --package=ddlint --";
    if stdout.contains(expected) {
        Ok(())
    } else {
        Err(format!("`make -n lint-whitaker` never runs {expected}"))
    }
}

/// The targets `.NOTPARALLEL` must name for the aggregate to stay serial.
const SERIAL_TARGETS: [&str; 3] = ["lint", "lint-clippy", "lint-whitaker"];

/// The gate commands, in the order the aggregate must run them.
const GATE_MARKERS: [&str; 6] = [
    "interrogate -vv --fail-under",
    "cargo clippy",
    "cargo doc",
    "test-clippy-policy",
    "whitaker --all",
    "typos-config-builder gate",
];

/// The make database printed for the aggregate `lint` target.
struct MakeDatabase(String);

impl MakeDatabase {
    /// Runs `make -p -n -B lint` and captures the printed database.
    fn load() -> Result<Self, String> {
        Invocation::new(["-p", "-n", "-B", "lint"])
            .printed()
            .map(Self)
    }

    /// Asserts the `.NOTPARALLEL` declaration covers every gate target.
    fn assert_serial(&self) -> Result<(), String> {
        let serial = self
            .0
            .lines()
            .find(|line| line.starts_with(".NOTPARALLEL:"))
            .ok_or_else(|| "the make database declares no .NOTPARALLEL".to_owned())?;
        let declared: Vec<&str> = serial.split_whitespace().collect();
        SERIAL_TARGETS
            .iter()
            .find(|target| !declared.contains(target))
            .map_or(Ok(()), |missing| {
                Err(format!(".NOTPARALLEL does not cover {missing}: {serial}"))
            })
    }
}

/// The recipes `make -n -B lint` schedules for the aggregate target, so a
/// stored recipe for a target `lint` never reaches cannot satisfy an ordering
/// assertion.
struct DryRun(String);

impl DryRun {
    /// Runs `make -n -B lint` and captures the printed recipes.
    fn load() -> Result<Self, String> { Invocation::new(["-n", "-B", "lint"]).printed().map(Self) }

    /// Asserts the gate commands appear in their declared order.
    fn assert_gate_order(&self) -> Result<(), String> {
        GATE_MARKERS.iter().try_fold(None, |previous, marker| {
            let at = self
                .0
                .find(*marker)
                .ok_or_else(|| format!("`make -n lint` never runs {marker}"))?;
            if previous.is_some_and(|earlier| at <= earlier) {
                return Err(format!("`make -n lint` runs {marker} out of order"));
            }
            Ok(Some(at))
        })?;
        Ok(())
    }
}

/// Scenario: the aggregate's printed recipes under `-j4`.
///
/// Invariant: the prerequisites run in the declared order, which the dry run
/// reports for the aggregate; the make database reports the serial targets.
#[test]
fn the_aggregate_runs_the_gates_in_the_declared_order() -> Result<(), String> {
    let database = MakeDatabase::load()?;
    database.assert_serial()?;
    DryRun::load()?.assert_gate_order()
}
