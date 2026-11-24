# Dependency warning qualification

This ledger qualifies the compiler diagnostics that package verification of
`ddlint` reports from Cargo registry sources. It exists because a successful
package build does not mean those diagnostics are resolved: Cargo passes
`--cap-lints warn` to dependency `rustc` invocations, which overrides the
`-D warnings` the consumer requested. Every row below therefore has an owner
and a disposition rather than an implied waiver.

The ticket is [issue 400][issue-400].

## Scope

In scope: qualifying the diagnostics that arise in resolved registry crates
when the packaged crate is verified.

Out of scope, and not part of this qualification:

- dependency upgrades beyond a caret-compatible resolution;
- vendored patches and `[patch]` overrides;
- lint suppression in `ddlint` sources;
- `--cap-lints` changes;
- compiler-date changes; and
- the separate missing package-metadata defect tracked in pull request 311,
  which is not one of the dependency diagnostics.

No file outside `docs/` changes for the qualification itself. See
[Consumer changes](#consumer-changes) for why no manifest edit was warranted.

## Validation route

The ticket's v6 route names ten commands, but its list was not supplied for
this candidate. The reconstruction below records nine in order and does not
identify the missing one; the qualification therefore covers these nine steps
and makes no claim about the tenth. They run from the repository root in this
order:

1. `make fmt`
2. `make check-fmt`
3. `make typecheck`
4. `make lint`
5. `make test`
6. `make markdownlint`
7. `make nixie`
8. resolved-graph capture (`cargo tree`)
9. non-development package verification

The run records do not each repeat all nine steps. Steps 1-7 are evidenced by
the sequential-gates table, step 8 by the resolved-graph record, and step 9 by
the package record. A run record covers the steps it names and claims nothing
about the others.

The gates keep the shared caches the `Makefile` sets (`UV_CACHE_DIR=.uv-cache`,
`UV_TOOL_DIR=.uv-tools`) and the existing `CARGO_HOME` and `target/` directory.

Package verification is explicit and non-development:

```plaintext
RUSTFLAGS="-D warnings" cargo package -vv
```

Assigning the `RUSTFLAGS` environment variable replaces the
`.cargo/config.toml` rustflags table, so the warning denial is stated in full
instead of being inherited. `-vv` is required because dependency `rustc`
invocations and their `--cap-lints warn` diagnostics are only visible in very
verbose output.

Table 1. Toolchain and tool identity.

| Component         | Identity                                        |
| ----------------- | ----------------------------------------------- |
| Toolchain channel | `nightly-2025-09-16`                            |
| `rustc` version   | `1.92.0-nightly (a454fccb0 2025-09-15)`         |
| `rustc` commit    | `a454fccb02df9d361f1201b747c01257f58a8b37`      |
| `cargo` version   | `1.92.0-nightly (24bb93c38 2025-09-10)`         |
| Whitaker          | `cargo-dylint 6.3.0`, suite `0.3.0-9-g45a436bc` |

Whitaker is a rolling release taken from the default-branch tip. The identity
above is the local suite used for the recorded gates; a later run takes
whatever the tip then offers, so a Whitaker finding is not evidence about this
ledger's dependency inventory.

The toolchain pin, `.cargo/config.toml`, the `Makefile` warning flags, the
Whitaker invocation, and the `[lints]` table are unchanged. `make lint` and
`make test` still deny warnings on `ddlint`'s own sources, and Whitaker still
runs.

## Resolved dependency paths

`Cargo.lock` is ignored, so the graph resolves inside the caret ranges below.
The constraints named here are the edges that decide which version `ddlint`
actually builds.

Table 2. Resolved versions and the constraint edges that pin them.

| Crate            | Resolved | Constraint that admits it                                           | Dependency path                                                                         |
| ---------------- | -------- | ------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| `allocator-api2` | `0.2.21` | `hashbrown 0.14.5` requires `0.2.9`                                 | `ddlint -> chumsky 0.9.3 -> hashbrown 0.14.5 -> allocator-api2`                         |
| `ahash`          | `0.8.13` | `hashbrown 0.14.5` requires `0.8.7`                                 | `ddlint -> chumsky 0.9.3 -> hashbrown 0.14.5 -> ahash` (via `hashbrown` defaults)       |
| `hashbrown`      | `0.14.5` | `chumsky 0.9.3` requires `0.14.2`; `rowan 0.17.0` requires `0.14.3` | `ddlint -> chumsky 0.9.3 / rowan 0.17.0 -> hashbrown`                                   |
| `memoffset`      | `0.9.1`  | `rowan 0.17.0` requires `0.9`                                       | `ddlint -> rowan 0.17.0 -> memoffset`                                                   |
| `regex-syntax`   | `0.6.29` | `logos-codegen 0.13.0` requires `0.6`                               | `ddlint -> logos 0.13.0 -> logos-derive 0.13.0 -> logos-codegen 0.13.0 -> regex-syntax` |
| `chumsky`        | `0.9.3`  | `ddlint` requires `0.9`                                             | `ddlint -> chumsky`                                                                     |
| `logos-codegen`  | `0.13.0` | `logos-derive 0.13.0` requires `0.13`                               | `ddlint -> logos 0.13.0 -> logos-derive 0.13.0 -> logos-codegen`                        |
| `thiserror-impl` | `1.0.69` | `thiserror 1.0.69` requires `1.0.69`                                | `ddlint -> thiserror 1.0.69 -> thiserror-impl`                                          |

Three observations follow from the constraint column. First, the fresh
resolution selects the newest release in every caret range. `ahash 0.8.13`
(2026-11-23) is newer than the `0.8.12` the ticket recorded, but it leaves all
four `ahash` diagnostics in place; `chumsky`, `logos`, `hashbrown 0.14`,
`memoffset`, `regex-syntax 0.6`, `allocator-api2 0.2`, and `thiserror 1.x` are
already at the newest release in their lines. Second, several crates are pinned
indirectly: `memoffset` and `regex-syntax 0.6` cannot move without a `rowan` or
`logos` change. Third, `hashbrown 0.14.5` has two independent holders.
`chumsky 0.9.3` is one, but `rowan 0.17.0` requires `hashbrown ^0.14.3` on its
own, so a Chumsky migration alone does not remove that edge. This matters for
the `hashbrown` and `ahash` rows below.

## Diagnostics and dispositions

Each row is one diagnostic group. "Owner" names the upstream project that must
ship the fix; `@leynos` tracks every row from the `ddlint` side.

The disposition vocabulary is: "awaiting upstream release", "contribute
upstream fix", "resolved by separate major migration", and "fixed in-range".

Table 3. Diagnostics, sites, paths, owners, and dispositions.

| Crate            | Version  | Lint                                                    | Verbatim message                                                                                                     | Site                                                          | Path and constraint                                              | Upstream owner                | Tracking owner | Disposition                          | Upstream evidence                                                                                                                                                                                                                                                                                                                                                                                                       |
| ---------------- | -------- | ------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | ---------------------------------------------------------------- | ----------------------------- | -------------- | ------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `allocator-api2` | `0.2.21` | `dangerous_implicit_autorefs`                           | ``implicit autoref creates a reference to the dereference of a raw pointer``                                         | `src/stable/vec/mod.rs:1902`                                  | `chumsky -> hashbrown -> allocator-api2`; `hashbrown ^0.2.9`     | `zakarumych/allocator-api2`   | `@leynos`      | awaiting upstream release            | [issue 41][a2-41] closed; [PR 42][a2-42] merged `930b1695` fixes this exact expression. The fix first appears in a published release at `0.3.1`, not `0.4.0`, and every published `0.3+` release is outside `hashbrown`'s `^0.2.9`. No open report covers a `0.2.x` backport.                                                                                                                                           |
| `ahash`          | `0.8.13` | `unexpected_cfgs` (`condition_value = "128"`)           | ``unexpected `cfg` condition value: `128```                                                                          | `src/fallback_hash.rs:179`                                    | `chumsky -> hashbrown -> ahash`; `hashbrown ^0.8.7`              | `tkaitchuck/aHash`            | `@leynos`      | contribute upstream fix              | No exact match in the tracker. [PR 269][ahash-269] only converts `feature = "specialize"`/`"folded_multiply"` cfgs to custom cfgs and registers those two. `0.8.13` adds `vaes` to the registered set and moves this site, but it still declares `target_pointer_width = "128"` and still leaves `fuzzing` unregistered. The consumer path is the [chumsky migration][t-chumsky], which removes `ahash` from the graph. |
| `ahash`          | `0.8.13` | `unexpected_cfgs` (`condition_name = "fuzzing"`, twice) | ``unexpected `cfg` condition name: `fuzzing```                                                                       | `src/random_state.rs:57`, `src/random_state.rs:77`            | `chumsky -> hashbrown -> ahash`; `hashbrown ^0.8.7`              | `tkaitchuck/aHash`            | `@leynos`      | contribute upstream fix              | Same. `fuzzing` is never registered through `cargo::rustc-check-cfg`, in `0.8.12` or `0.8.13`. The consumer path is the [chumsky migration][t-chumsky].                                                                                                                                                                                                                                                                 |
| `ahash`          | `0.8.13` | `dead_code` (`methods`)                                 | ``methods `read_u128x2`, `read_u128x4`, `read_last_u16`, `read_last_u128x2`, and `read_last_u128x4` are never used`` | `src/convert.rs:67`                                           | `chumsky -> hashbrown -> ahash`; `hashbrown ^0.8.7`              | `tkaitchuck/aHash`            | `@leynos`      | contribute upstream fix              | `0.8.13` is the newest `0.8` release and still carries these methods. `ahash` enters only because `chumsky` enables `hashbrown`'s default features; the [chumsky migration][t-chumsky] removes it, while `rowan`'s own `hashbrown` edge does not enable it.                                                                                                                                                             |
| `hashbrown`      | `0.14.5` | `dead_code` (`struct`, twice)                           | ``struct `RawIterHash` is never constructed`` and ``struct `RawIterHashInner` is never constructed``                 | `src/raw/mod.rs:4442`, `src/raw/mod.rs:4447`                  | `chumsky` and `rowan -> hashbrown`; `^0.14.2`, `^0.14.3`         | `rust-lang/hashbrown`         | `@leynos`      | awaiting upstream release            | The two types are gated behind `#[cfg(feature = "raw")]`, which the resolved graph does not enable. `hashbrown 0.16.1` and later restructure the module so the site disappears. Held here by `rowan 0.17.0` as well as `chumsky 0.9.3`, and no published `rowan` moves off `0.14`; see [rowan review][t-rowan].                                                                                                         |
| `memoffset`      | `0.9.1`  | `unexpected_cfgs` (24)                                  | ``unexpected `cfg` condition name: `…```                                                                             | `src/lib.rs:59`, `src/lib.rs:62`, `src/offset_of.rs:23-383`   | `rowan -> memoffset`; `rowan ^0.9`                               | `Gilnaa/memoffset`            | `@leynos`      | contribute upstream fix              | `0.9.1` is the newest release and the repository has been dormant since 2024-03-26. Its `build.rs` emits `cargo:rustc-cfg=<name>` for `tuple_ty`, `allow_clippy`, `maybe_uninit`, `doctests`, `raw_ref_macros`, `stable_const`, and `stable_offset_of` without the matching `cargo::rustc-check-cfg`.                                                                                                                   |
| `regex-syntax`   | `0.6.29` | `dead_code` (`method`)                                  | ``method `symmetric_difference` is never used``                                                                      | `src/hir/interval.rs:423`                                     | `logos -> logos-derive -> logos-codegen -> regex-syntax`; `^0.6` | `rust-lang/regex`             | `@leynos`      | resolved by separate major migration | `0.6.29` is the newest `0.6` release. The whole crate leaves the graph once `logos` moves past `0.13`. See [logos migration][t-logos].                                                                                                                                                                                                                                                                                  |
| `regex-syntax`   | `0.6.29` | `noop_method_call`                                      | ``call to `.borrow()` on a reference in this situation does nothing``                                                | `src/ast/parse.rs:379`                                        | `logos -> logos-derive -> logos-codegen -> regex-syntax`; `^0.6` | `rust-lang/regex`             | `@leynos`      | resolved by separate major migration | Same; no fix exists on the `0.6` line.                                                                                                                                                                                                                                                                                                                                                                                  |
| `chumsky`        | `0.9.3`  | `mismatched_lifetime_syntaxes`                          | ``hiding a lifetime that's elided elsewhere is confusing``                                                           | `src/recursive.rs:31`                                         | direct dependency; `ddlint ^0.9`                                 | `zesterer/chumsky` (Codeberg) | `@leynos`      | resolved by separate major migration | `0.9.3` is the newest `0.9` release. `0.13.0` and `0.14.2` clear this site but each raises one unrelated `unused_import` warning instead. See [chumsky migration][t-chumsky].                                                                                                                                                                                                                                           |
| `logos-codegen`  | `0.13.0` | `mismatched_lifetime_syntaxes`                          | ``hiding a lifetime that's elided elsewhere is confusing``                                                           | `src/generator/tables.rs:25`                                  | `logos -> logos-derive -> logos-codegen`; `logos-derive ^0.13`   | `maciejhirsz/logos`           | `@leynos`      | resolved by separate major migration | `0.13.x` is the newest line `logos 0.13` admits. `0.15.1` and `0.16.1` clear this site. See [logos migration][t-logos].                                                                                                                                                                                                                                                                                                 |
| `thiserror-impl` | `1.0.69` | `mismatched_lifetime_syntaxes` (9)                      | ``hiding a lifetime that's elided elsewhere is confusing``                                                           | `src/attr.rs:48`, `src/prop.rs:7, 11, 15, 19, 53, 57, 61, 65` | `thiserror -> thiserror-impl`; `thiserror =1.0.69`               | `dtolnay/thiserror`           | `@leynos`      | resolved by separate major migration | `2.0.21` clears all nine hits, but with a crate-level `#![allow(unknown_lints, mismatched_lifetime_syntaxes)]` rather than a source fix. `1.0.69` is the newest `1.x` release. See [thiserror migration][t-thiserror].                                                                                                                                                                                                  |

Every row above has a named upstream owner and an actionable disposition.
Cargo's lint cap is not treated as a waiver anywhere in this table.

## Upstream ownership searches

The exact-owner searches were run on 2026-10-28 and repeated on 2026-11-23.

- `dtolnay/thiserror`: `mismatched_lifetime_syntaxes` and `"hiding a lifetime"`
  returned no issue or pull request.
- `maciejhirsz/logos`: the same two queries returned no issue or pull
  request.
- `rust-lang/hashbrown`: `RawIterHash` returned related iterator work but
  nothing about the `raw` feature gating producing dead code;
  `"never constructed"` returned nothing.
- `rust-lang/regex`: `symmetric_difference "never used"` returned nothing.
- `Gilnaa/memoffset`: `rustc-check-cfg` over all issues and pull requests
  returned nothing. Issues were disabled when the ticket was written and are
  enabled again as of 2026-11-23.
- `tkaitchuck/aHash`: `fuzzing unexpected_cfgs` returned nothing;
  [PR 269][ahash-269] covers only the feature cfgs.
- `zesterer/chumsky`, now on Codeberg: `mismatched_lifetime_syntaxes`
  returned zero open, closed, or all results. The GitHub repository was
  archived on 2 April 2026.
- `zakarumych/allocator-api2`: [issue 41][a2-41] and [PR 42][a2-42] are the
  existing coverage; no `0.2.x` backport request existed before this update.

A zero-result search is not proof that no owner exists; it records what was
searched and when.

## Migration tracking

Each major migration has its own review ticket. The three Cargo migrations
already have Dependabot change vehicles; the tickets are the review record, not
duplicate bumps.

Table 4. Migration tickets and their change vehicles.

| Migration                       | Ticket                   | Change vehicle                |
| ------------------------------- | ------------------------ | ----------------------------- |
| `thiserror 1.0 -> 2.0`          | [issue 468][t-thiserror] | [PR 155][pr-155] (Dependabot) |
| `logos 0.13 -> 0.16`            | [issue 469][t-logos]     | [PR 276][pr-276] (Dependabot) |
| `chumsky 0.9 -> 0.14`           | [issue 470][t-chumsky]   | [PR 465][pr-465] (Dependabot) |
| `rowan 0.17 -> a later release` | [issue 471][t-rowan]     | None; review only             |

## What a major migration would and would not buy

To keep the migration reviews evidence-based, the pinned toolchain was used to
compile two scratch consumers against candidate releases. The result is a mixed
picture, so no migration may be sold as a clean sweep.

Table 5. Measured effect of each candidate migration.

| Change                           | Measured effect on this inventory                                                                                                                          |
| -------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `logos 0.13 -> 0.15.1`           | Removes both `regex-syntax 0.6.29` diagnostics and the `logos-codegen` lifetime diagnostic, but adds `lazy_static 1.5.1` with its own lifetime diagnostic. |
| `logos 0.13 -> 0.16.1`           | Same three removals, but pulls `regex-automata 0.4` and `aho-corasick 1.1.5`, adding 5 warnings across those two crates.                                   |
| `thiserror 1.0.69 -> 2.0.21`     | Removes all nine `thiserror-impl` diagnostics.                                                                                                             |
| `chumsky 0.9.3 -> 0.13.0`        | Removes the `chumsky` lifetime diagnostic but adds one `unused_import` warning. Moves `hashbrown` to `0.15.5`.                                             |
| `chumsky 0.9.3 -> 0.14.2`        | Same one-for-one trade, with `hashbrown 0.16.1`.                                                                                                           |
| `rowan 0.17 -> 0.18.0-alpha.2`   | No effect; `0.18.0-alpha.2` still declares `hashbrown 0.14.3` and `memoffset 0.9`.                                                                         |
| `hashbrown 0.14.5 -> 0.17.2`     | Removes the two `hashbrown` diagnostics, but still resolves `allocator-api2 0.2.21` transitively and cannot help while `rowan` pins `0.14.3`.              |
| `allocator-api2 0.2.21 -> 0.4.0` | Removes the diagnostic at the `0.4.0` source site, but stays outside `hashbrown`'s `^0.2.9`, so it cannot be resolved by the consumer.                     |
| `memoffset 0.9.1`                | Unchanged; `0.9.1` is the newest release and `rowan 0.18.0-alpha.2` still requires `0.9`.                                                                  |

A scratch consumer built from `chumsky 0.14.2`, `logos 0.16.1`,
`thiserror 2.0.21`, `rowan 0.18.0-alpha.2`, `hashbrown 0.17.2`,
`allocator-api2 0.4.0`, and `memoffset 0.9.1` reported 33 diagnostics across
six crates: fewer than 44, but with different owners, and still carrying
`allocator-api2 0.2.21`, `hashbrown 0.14.5`, and `memoffset 0.9.1` through
`rowan`. That is a migration problem to be reviewed on its own terms, not a
warning-cleanup that can be smuggled into this ticket.

## Consumer changes

No consumer change was applied, and the ledger therefore records "no consumer
change".

The reason is the constraint table above. A caret-compatible fix would have to
come from a newer release inside an existing range, and
`cargo update --dry-run` already resolves to the newest version of every
affected crate. The remaining options are major API migrations, which the
ticket requires to be reviewed separately.

## Upstream contacts

No upstream issue or pull request was opened by this qualification. The
dispositions are recorded above with the contact that already exists for each:

- `allocator-api2`: [issue 41][a2-41] and [PR 42][a2-42] already cover the
  defect; the remaining ask is a `0.2.x` release, recorded as a comment on that
  thread rather than a new report: [issue comment 7109286384][a2-comment].
- `ahash`: no existing owner, so a fix is to be contributed.
- `memoffset`: issues are disabled, so a fix is to be contributed.
- `chumsky`, `logos`, `hashbrown`, and `thiserror`: covered by the migration
  tickets in Table 4.

Record each new contact URL here once it exists.

## Run records

Each run is bound to the content tree it validated. A warning inventory is not
transferred validation for a later candidate.

### Historical evidence

The ticket's own inventory was captured on content tree
`da7bde9cc360dcd598b00eecbaae7740239dd283`, log `package.log`, SHA-256
`6d7dd56f51452c0c55830396ba8d79d5031ae5db06a4721d60709c0b73ac3b15`. It is
historical evidence only and does not validate any later tree.

### Package verification, tree `768f6172`

Table 6. Package verification record.

| Field                 | Value                                                              |
| --------------------- | ------------------------------------------------------------------ |
| Commit                | `ccb6e748aa3a93a662a66f8f73769997b609357b`                         |
| Content tree          | `768f6172cf7e2e307add6767e0fd480f56eeb817`                         |
| Package command       | `RUSTFLAGS="-D warnings" cargo package -vv`                        |
| Command exit          | `0`                                                                |
| `tee` exit            | `0`                                                                |
| `package.log` SHA-256 | `cf6f2a0fae743dbd48466b6dd78ba040bf3d76502738ada6081e4a2b4a7ebc89` |
| Declared warnings     | `44`                                                               |

The run resolved `ahash 0.8.13`, published on the day of the run, rather than
the `0.8.12` recorded by the ticket. The dependency set and the counts are
otherwise unchanged.

Per-crate counts, as the toolchain itself reports them:

Table 7. Warnings per crate.

| Crate            | Warnings |
| ---------------- | -------- |
| `allocator-api2` | 1        |
| `ahash`          | 4        |
| `chumsky`        | 1        |
| `hashbrown`      | 2        |
| `logos-codegen`  | 1        |
| `memoffset`      | 24       |
| `regex-syntax`   | 2        |
| `thiserror-impl` | 9        |
| **Total**        | **44**   |

Attribution note: `cargo` builds crates in parallel and interleaves their
diagnostics, so a diagnostic may be printed far from its crate's summary line.
Attribution here comes from each diagnostic's own `-->` path, which reconciles
to the same 44 the toolchain declares.

Edits to this page after the recorded commit are documentation-only. The
package route is bound to the tree above; the later documentation gates are
recorded separately.

### Resolved graph, tree `768f6172`

`Cargo.lock` is ignored, so the graph is captured rather than committed. The
capture uses the pinned toolchain, the resolved lock, and the normal and build
edges:

```shell
cargo tree --locked --edges normal,build
```

Table 8. Resolved-graph capture record.

| Field          | Value                                                              |
| -------------- | ------------------------------------------------------------------ |
| Content tree   | `768f6172cf7e2e307add6767e0fd480f56eeb817`                         |
| Command exit   | `0`                                                                |
| `tee` exit     | `0`                                                                |
| Output lines   | `80`                                                               |
| SHA-256 digest | `ddbdf3857a75222c7c775e27eae256507eb1c847051d4e0cd17f67bab9fea073` |

The capture resolves the eight crates in this ledger: `allocator-api2 0.2.21`,
`ahash 0.8.13`, `hashbrown 0.14.5`, `memoffset 0.9.1`, `regex-syntax 0.6.29`,
`chumsky 0.9.3`, `logos-codegen 0.13.0`, and `thiserror-impl 1.0.69`. It also
shows the two `hashbrown` holders directly.

This is point-in-time evidence, not a reproducible guarantee. `Cargo.lock` is
ignored, so a later registry index could select a different caret-compatible
version while this tree stays unchanged. The digest therefore identifies what
was captured on that tree and day; it does not assert what a future resolution
will choose.

### Candidate scope

The candidate tree pins `nightly-2025-09-16`, the compiler date the ticket
specifies. `main` has since moved to `nightly-2026-11-09`, and that newer
compiler raises additional deprecation diagnostics in the same registry crates.
Those results are not transferred validation for this tree, and a separate
re-qualification is required after the branch is rebased onto the newer pin.
The toolchain pin is deliberately not changed here, as the ticket requires.

### Migration probes

Both probes are auxiliary evidence for Table 5, not package verification of
`ddlint`. Each is a scratch crate with a `[workspace]` table so it resolves
independently, built with the pinned toolchain under `RUSTFLAGS="-D warnings"`.

Table 9. Probe records.

| Probe                | Dependencies                                                                                                                                | Command exit | Warnings | Log SHA-256                                                        |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | ------------ | -------- | ------------------------------------------------------------------ |
| Newest-release probe | `chumsky 0.14.2`, `logos 0.16.1`, `thiserror 2.0.21`, `rowan 0.18.0-alpha.2`, `hashbrown 0.17.2`, `allocator-api2 0.4.0`, `memoffset 0.9.1` | `0`          | `33`     | `fac6be05eca777ade41c325b9ece9a57de4028ac068e740d757804d0fdd9c279` |
| Intermediate probe   | `chumsky 0.13.0`, `logos 0.15.1`                                                                                                            | `0`          | `3`      | `8797ba1da7c1c6d2fd75cb99d5c716936cf9f8020762dc925b04d36914c7fae2` |

### Sequential gates

The gates below ran sequentially on the documented content tree, each with
command exit `0` and `tee` exit `0`.

Table 10. Sequential gate results.

| Gate                | Exit | Scope                                                            |
| ------------------- | ---- | ---------------------------------------------------------------- |
| `make fmt`          | `0`  | Formatting applied; `markdownlint --fix` reported 0 issues       |
| `make check-fmt`    | `0`  | `cargo fmt --check` and `mdtablefix --check`, 57 files unchanged |
| `make typecheck`    | `0`  | Workspace, all targets and features                              |
| `make lint`         | `0`  | Interrogate, Clippy, Clippy policy, Whitaker, and spelling       |
| `make test`         | `0`  | 35 suites passed, none failed                                    |
| `make markdownlint` | `0`  | 57 files, 0 issues, plus the spelling gate                       |
| `make nixie`        | `0`  | Every Mermaid diagram validated                                  |

`make lint` ran the rolling Whitaker suite, which reported
`Checking with toolchain nightly-2026-05-28` and completed without a finding.

## Differences from the baseline

The total is unchanged at 44, across the same eight crates and with the same
per-crate counts. Two details differ from the ticket's recorded run:

- `ahash` resolves to `0.8.13` (2026-11-23) rather than `0.8.12`. The release
  registers `vaes` and moves the three `ahash` sites, but it leaves the `128`
  condition value and the `fuzzing` condition name unregistered and keeps the
  dead-code methods, so all four `ahash` diagnostics remain.
- Consequently the recorded sites are `src/fallback_hash.rs:179`,
  `src/random_state.rs:57`, `src/random_state.rs:77`, and `src/convert.rs:67`.

Re-attributing the package log by diagnostic path reproduces the per-crate
counts exactly.

[issue-400]: https://github.com/leynos/ddlint/issues/400
[a2-41]: https://github.com/zakarumych/allocator-api2/issues/41
[a2-42]: https://github.com/zakarumych/allocator-api2/pull/42
[a2-comment]: https://github.com/zakarumych/allocator-api2/issues/41#issuecomment-7109286384
[ahash-269]: https://github.com/tkaitchuck/aHash/pull/269
[t-thiserror]: https://github.com/leynos/ddlint/issues/468
[t-logos]: https://github.com/leynos/ddlint/issues/469
[t-chumsky]: https://github.com/leynos/ddlint/issues/470
[t-rowan]: https://github.com/leynos/ddlint/issues/471
[pr-155]: https://github.com/leynos/ddlint/pull/155
[pr-276]: https://github.com/leynos/ddlint/pull/276
[pr-465]: https://github.com/leynos/ddlint/pull/465
