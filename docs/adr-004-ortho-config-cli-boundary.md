# Architectural decision record (ADR) 004: OrthoConfig CLI boundary

## Status

Accepted on 26 October 2026. The planned CLI configuration spine uses
OrthoConfig v0.10.0 in a separate application package.

## Date

2026-10-26.

## Context and problem statement

The root `ddlint` package contains both a reusable Rust library and a
placeholder binary. The CLI design and roadmap plan file linting, `rules`, and
`explain` commands, but those commands are not implemented. The design names
`config-rs` for `ddlint.toml`; issue #411 asks to use the published
`ortho_config` v0.10.0 release instead.

OrthoConfig v0.10.0 has a Rust 1.89 floor and provides layered TOML,
environment, Clap integration, subcommand namespaces, and source injection.
Adding it to the root package would expose its dependency and minimum supported
Rust version (MSRV) requirements to users of the reusable library. The
configuration work must also remain part of the roadmapped CLI item rather than
introducing command execution ahead of the planned surface.

## Decision drivers

- Keep reusable parser and linter consumers independent of CLI configuration
  frameworks and their MSRV.
- Use OrthoConfig v0.10.0's shipped APIs only, with default features enabled.
- Implement and test the configuration layer without starting CLI command
  execution early.
- Preserve the placeholder binary's no-configuration output, streams, and exit
  status.
- Make configuration precedence, discovery, environment names, and errors
  stable and testable without process environment mutation.

## Options considered

- Keep `config-rs` in the root package. This preserves the previous design but
  declines the requested adoption and still mixes application configuration
  with the reusable package.
- Add OrthoConfig to the root package as an optional feature. This still makes
  the reusable package's dependency graph and feature surface responsible for
  application configuration.
- Add a separate, non-published `ddlint-cli` application package. This keeps
  OrthoConfig and Rust 1.89 scoped to the application boundary and leaves the
  root library unchanged. This is the chosen option.

## Decision outcome / proposed direction

The workspace will have a non-published `ddlint-cli` package. It owns the
configuration types, loading, validation, and later the command-line entry
point. The current placeholder moves into that package without a change to its
observable behaviour. The root package remains the reusable `ddlint` library
and does not depend on OrthoConfig or Clap.

The app package declares `rust-version = "1.89"` and `ortho_config = "0.10.0"`,
with default features enabled. The existing dated nightly is retained if its
compiler is at least Rust 1.89. No direct `ortho_l10n` dependency is needed
because the CLI is not localized.

The application-owned `AppConfig` derives `OrthoConfig` and loads the complete
configuration document, including its nested `cmds.lint` table. A separate Clap
data transfer object (DTO) carries command-line-only fields and explicit
settings; the app applies only explicitly supplied values after loading
configuration. The general source-aware loader supports the planned file name
and explicit `--config` selector. The subcommand-specific loader has fixed
filename candidates, so it is not the entry point for this contract. The derive
macro configures `app_name = "ddlint"`, `config_file_name = "ddlint.toml"`,
`dotfile_name = ".ddlint.toml"`, `project_file_name = "ddlint.toml"`,
`config_cli_long = "config"`, `config_cli_short = 'c'`, and
`config_cli_visible = true` so the generated selector appears in help.

The planned lint configuration uses:

- `ddlint.toml` as the application-directory and project filename, and
  `.ddlint.toml` as a dotfile candidate in XDG Base Directory Specification
  (XDG), native, and home configuration locations;
- automatic first-wins discovery using the XDG, native platform, home, and
  current-project locations, without ancestor traversal; project discovery
  checks `ddlint.toml` only;
- explicit `--config <PATH>` or `-c <PATH>` selection, with
  `DDLINT_CONFIG_PATH` as the environment selector; a selected path is
  exclusive and required;
- the `DDLINT_` environment prefix, with nested keys separated by `__`, and the
  `[cmds.lint]` namespace;
- defaults < files < environment < explicit command-line values;
- TOML only, with OrthoConfig v0.10.0 `extends` support;
- configurable `format`, `ignore_patterns`, and rule severity overrides;
- no Clap parser default for `format`; the configuration model supplies `rich`
  and the optional parser DTO overrides it only when explicitly supplied; and
- rejection of unknown configuration fields and unknown rule names.

File operands, `--config <PATH>`/`-c <PATH>`, `--no-ignore`, and future `--fix`
behaviour are command-line-only. `rules` and `explain` have no configuration
fields and will not load configuration. Named profiles are not offered. Help
and parse errors remain English. This is not a Cargo subcommand.

Configuration loading returns errors to the application boundary. The app
converts configuration into validated settings and passes only domain types,
including `ddlint::linter::RuleLevel`, to the reusable library. The library
does not print errors or terminate the process.

## Goals and non-goals

The goal is a tested OrthoConfig configuration spine for roadmap item 5.1.5.
The scope includes the app package, layered settings, explicit-source tests,
documentation, and preserving the placeholder contract.

This decision does not implement file discovery for lint operands, lint
execution, diagnostic rendering, autofixing, `rules`, or `explain`. Those
remain roadmap work. The placeholder remains until the CLI command tasks are
implemented.

## Consequences

- `ddlint-cli` carries the OrthoConfig v0.10.0 dependency and Rust 1.89 floor;
  the root library keeps its current dependency graph and toolchain floor.
- Configuration for the default lint command lives under `[cmds.lint]`, even
  when its eventual user-facing invocation has no explicit `lint` token.
- Automatic project discovery uses the current working directory only. Users
  who want a different root can select a file with `--config` or use the
  documented XDG locations.
- `format` is represented in the configuration layer before the output
  backends are implemented. Runtime use follows the CLI roadmap.
- Tests use OrthoConfig's `_with_sources` APIs, `MapEnv`, explicit file
  sources, and child processes. They do not mutate the test process environment.

## Migration plan

1. Import and checksum the v0.10.0 guides, then update the design, roadmap, and
   users' and developers' guides.
2. Add the app package and configuration model, with source-aware tests for
   precedence, command namespace, and failure handling.
3. Keep roadmap items 5.1.1–5.1.4 open until their command parsing and runtime
   behaviour are implemented against this configuration spine.
