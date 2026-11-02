//! Contract tests for the Makefile targets that build under the standard.
//!
//! These read the repository's own `make -n` output rather than a fixture, so
//! they are split from the contract root to keep each module within the module
//! size budget. The readers they exercise live in [`super::make`] and
//! [`super::process`].

use rstest::rstest;

use super::{
    Fixture,
    flags,
    make::{
        Host,
        commands_from,
        development_problems,
        held_out_problems,
        held_out_target_count,
        test_policy_problem,
    },
    none_of,
    process::real_make,
};
use crate::config::{Pin, THREADS_FLAG, TOOLCHAIN};

/// Scenario: a recipe continued over lines, beside an `echo` and another
/// command.
///
/// Invariant: the continued command is one command, and lines that are not a
/// Cargo or Whitaker command are ignored.
#[test]
fn a_continued_command_is_one_command() -> Result<(), String> {
    let joined = commands_from(concat!(
        "RUSTFLAGS=\"-A\" \\\n",
        "cargo test\necho cargo test\nmake other\n"
    ))?;
    if joined == vec![flags(&["-A"], false)] {
        Ok(())
    } else {
        Err(format!("read wrongly: {joined:?}"))
    }
}

#[test]
fn development_targets_restate_the_flags_on_linux() -> Result<(), String> {
    let (problems, read) = development_problems(real_make, Host::Linux, Pin::read(TOOLCHAIN)?)?;
    none_of(&problems)?;
    if read == 0 {
        return Err(
            "no development target assigns RUSTFLAGS, so the check proves nothing".to_owned(),
        );
    }
    Ok(())
}

#[test]
fn development_targets_keep_the_frontend_but_not_the_linker_elsewhere() -> Result<(), String> {
    none_of(&development_problems(real_make, Host::Darwin, Pin::read(TOOLCHAIN)?)?.0)
}

/// Coverage measures and release ships, so both stay on the default flags. A
/// repository that lists no such target has nothing local to hold out, and the
/// check then reads no commands; otherwise it must read at least one.
#[test]
fn coverage_and_release_take_neither_flag() -> Result<(), String> {
    let (problems, read) = held_out_problems(real_make)?;
    none_of(&problems)?;
    if held_out_target_count() > 0 && read == 0 {
        return Err(
            "the held-out targets run no cargo command, so the check proves nothing".to_owned(),
        );
    }
    Ok(())
}

/// Scenario: the `test` target's assigned commands, with and without the warning
/// policy, beside a command that never carried it.
///
/// Invariant: `make test` must keep `-D warnings` in at least one of its assigned
/// commands (dropping `$(RUST_FLAGS)` from the command that runs the tests would
/// lose it everywhere), while a version probe or prerequisite build that never
/// carried the policy is not held to it, and another target is not held to it.
#[rstest]
#[case::test_keeps_the_policy(Fixture("test"), &[&["-D", "warnings", THREADS_FLAG][..]], 0)]
#[case::test_spelled_joined(Fixture("test"), &[&["-Dwarnings", THREADS_FLAG][..]], 0)]
#[case::test_drops_the_policy(Fixture("test"), &[&[THREADS_FLAG][..]], 1)]
#[case::test_denies_nothing_useful(Fixture("test"), &[&["-A", "warnings", THREADS_FLAG][..]], 1)]
#[case::a_probe_may_omit_it_beside_the_run(Fixture("test"), &[&[THREADS_FLAG][..], &["-D", "warnings", THREADS_FLAG][..]], 0)]
#[case::build_may_omit_the_policy(Fixture("build"), &[&[THREADS_FLAG][..]], 0)]
#[case::test_assigns_no_command(Fixture("test"), &[], 1)]
fn the_test_target_keeps_the_warning_policy(
    #[case] target: Fixture,
    #[case] commands: &[&[&str]],
    #[case] expected: usize,
) {
    let assigned = commands
        .iter()
        .map(|words| flags(words, true))
        .collect::<Vec<_>>();
    let found = test_policy_problem(target.0, Host::Darwin, &assigned)
        .into_iter()
        .count();
    assert_eq!(found, expected, "target {}: {commands:?}", target.0);
}
