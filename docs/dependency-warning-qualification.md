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
- the separate missing package-metadata defect tracked in PR 311, which is not
  one of the dependency diagnostics.

No file outside `docs/` changes for the qualification itself. See
[Consumer changes](#consumer-changes) for why no manifest edit was warranted.

## Validation route

The route below is a reconstruction of the ticket's ordered route, because the
original ten-command list was not supplied for this candidate. Every run in
this ledger uses it in this order, from the repository root:

1. `make fmt`
2. `make check-fmt`
3. `make typecheck`
4. `make lint`
5. `make test`
6. `make markdownlint`
7. `make nixie`
8. resolved-graph capture (`cargo tree`)
9. non-development package verification

The gates keep the shared caches the `Makefile` sets (`UV_CACHE_DIR=.uv-cache`,
`UV_TOOL_DIR=.uv-tools`) and the existing `CARGO_HOME` and `target/` directory.

Package verification is explicit and non-development:

```plaintext
RUSTFLAGS="-D warnings" cargo package -vv
```

An assigned `RUSTFLAGS` replaces the `.cargo/config.toml` rustflags table, so
the warning denial is stated in full instead of being inherited. `-vv` is
required, because dependency `rustc` invocations and their `--cap-lints warn`
diagnostics are only visible in very verbose output.

| Component         | Identity                                   |
| ----------------- | ------------------------------------------ |
| Toolchain channel | `nightly-2025-09-16`                       |
| `rustc` version   | `1.92.0-nightly (a454fccb0 2025-09-15)`    |
| `rustc` commit    | `a454fccb02df9d361f1201b747c01257f58a8b37` |
| `cargo` version   | `1.92.0-nightly (24bb93c38 2025-09-10)`    |
| Whitaker          | `cargo-dylint 6.0.1`                       |

The toolchain pin, `.cargo/config.toml`, the `Makefile` warning flags, the
Whitaker invocation, and the `[lints]` table are unchanged. `make lint` and
`make test` still deny warnings on `ddlint`'s own sources, and Whitaker still
runs.

## Resolved dependency paths

`Cargo.lock` is ignored, so the graph resolves inside the caret ranges below.
The constraints named here are the edges that decide which version `ddlint`
actually builds.

| Crate            | Resolved | Constraint that admits it                                           | Dependency path                                                                         |
| ---------------- | -------- | ------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| `allocator-api2` | `0.2.21` | `hashbrown 0.14.5` requires `0.2.9`                                 | `ddlint -> chumsky 0.9.3 -> hashbrown 0.14.5 -> allocator-api2`                         |
| `ahash`          | `0.8.12` | `hashbrown 0.14.5` requires `0.8.7`                                 | `ddlint -> chumsky 0.9.3 / rowan 0.17.0 -> hashbrown 0.14.5 -> ahash`                   |
| `hashbrown`      | `0.14.5` | `chumsky 0.9.3` requires `0.14.2`; `rowan 0.17.0` requires `0.14.3` | `ddlint -> chumsky 0.9.3 / rowan 0.17.0 -> hashbrown`                                   |
| `memoffset`      | `0.9.1`  | `rowan 0.17.0` requires `0.9`                                       | `ddlint -> rowan 0.17.0 -> memoffset`                                                   |
| `regex-syntax`   | `0.6.29` | `logos-codegen 0.13.0` requires `0.6`                               | `ddlint -> logos 0.13.0 -> logos-derive 0.13.0 -> logos-codegen 0.13.0 -> regex-syntax` |
| `chumsky`        | `0.9.3`  | `ddlint` requires `0.9`                                             | `ddlint -> chumsky`                                                                     |
| `logos-codegen`  | `0.13.0` | `logos-derive 0.13.0` requires `0.13`                               | `ddlint -> logos 0.13.0 -> logos-derive 0.13.0 -> logos-codegen`                        |
| `thiserror-impl` | `1.0.69` | `thiserror 1.0.69` requires `1.0.69`                                | `ddlint -> thiserror 1.0.69 -> thiserror-impl`                                          |

Two observations follow from the constraint column. First, no caret-compatible
newer release exists for any affected crate: `cargo update --dry-run` locks
zero packages, and `cargo info` reports each resolved version as the newest in
its line. Second, several crates are pinned indirectly. `memoffset` and
`regex-syntax 0.6` cannot move without a `rowan` or `logos` change, and
`hashbrown 0.14.5` is held by both `chumsky` and `rowan`.

## Diagnostics and dispositions

Each row is one diagnostic group. "Owner" names the upstream project that must
ship the fix; `@leynos` tracks every row from the `ddlint` side.

The disposition vocabulary is: "awaiting upstream release", "contribute
upstream fix", "resolved by separate major migration", and "fixed in-range".

| Crate            | Version  | Lint                                                    | Verbatim message                                                                                                     | Site                                                          | Disposition                          | Upstream evidence                                                                                                                                                                                                             |
| ---------------- | -------- | ------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | ------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `allocator-api2` | `0.2.21` | `dangerous_implicit_autorefs`                           | ``implicit autoref creates a reference to the dereference of a raw pointer``                                         | `src/stable/vec/mod.rs:1902`                                  | Awaiting upstream release            | [issue 41][a2-41] closed; [PR 42][a2-42] merged `930b1695` fixes this expression but is released only in `0.4.0`, outside `hashbrown`'s `^0.2.9`                                                                              |
| `ahash`          | `0.8.12` | `unexpected_cfgs` (`condition_value = "128"`)           | ``unexpected `cfg` condition value: `128```                                                                          | `src/fallback_hash.rs:164`                                    | Contribute upstream fix              | No exact match in the tracker; [PR 269][ahash-269] changes feature cfgs to custom cfgs and does not cover `target_pointer_width = "128"`                                                                                      |
| `ahash`          | `0.8.12` | `unexpected_cfgs` (`condition_name = "fuzzing"`, twice) | ``unexpected `cfg` condition name: `fuzzing```                                                                       | `src/random_state.rs:48`, `src/random_state.rs:68`            | Contribute upstream fix              | Same; `fuzzing` is never registered through `cargo::rustc-check-cfg`                                                                                                                                                          |
| `ahash`          | `0.8.12` | `dead_code` (`methods`)                                 | ``methods `read_u128x2`, `read_u128x4`, `read_last_u16`, `read_last_u128x2`, and `read_last_u128x4` are never used`` | `src/convert.rs:69`                                           | Contribute upstream fix              | No exact match found (searched 2026-10-28)                                                                                                                                                                                    |
| `hashbrown`      | `0.14.5` | `dead_code` (`struct`, twice)                           | ``struct `RawIterHash` is never constructed`` and ``struct `RawIterHashInner` is never constructed``                 | `src/raw/mod.rs:4442`, `src/raw/mod.rs:4447`                  | Resolved by separate major migration | Fixed by `hashbrown` `0.15`/`0.16`; held here by `chumsky 0.9.3` and `rowan 0.17.0`, so it clears with the Chumsky and Rowan reviews                                                                                          |
| `memoffset`      | `0.9.1`  | `unexpected_cfgs` (24)                                  | ``unexpected `cfg` condition name: `…```                                                                             | `src/lib.rs:59`, `src/lib.rs:62`, `src/offset_of.rs:23–383`   | Contribute upstream fix              | Issues disabled. `build.rs` emits `cargo:rustc-cfg=<name>` for `tuple_ty`, `allow_clippy`, `maybe_uninit`, `doctests`, `raw_ref_macros`, `stable_const`, and `stable_offset_of` without the matching `cargo::rustc-check-cfg` |
| `regex-syntax`   | `0.6.29` | `dead_code` (`method`)                                  | ``method `symmetric_difference` is never used``                                                                      | `src/hir/interval.rs:423`                                     | Resolved by separate major migration | Whole crate leaves the graph once `logos` moves past `0.13`, which depends on `regex-syntax 0.8`                                                                                                                              |
| `regex-syntax`   | `0.6.29` | `noop_method_call`                                      | ``call to `.borrow()` on a reference in this situation does nothing``                                                | `src/ast/parse.rs:379`                                        | Resolved by separate major migration | Same; no fix on the `0.6` line                                                                                                                                                                                                |
| `chumsky`        | `0.9.3`  | `mismatched_lifetime_syntaxes`                          | ``hiding a lifetime that's elided elsewhere is confusing``                                                           | `src/recursive.rs:31`                                         | Resolved by separate major migration | `0.13.0` compiles this site cleanly but raises an unrelated `unused_import`; `0.9.3` is the newest `0.9` release                                                                                                              |
| `logos-codegen`  | `0.13.0` | `mismatched_lifetime_syntaxes`                          | ``hiding a lifetime that's elided elsewhere is confusing``                                                           | `src/generator/tables.rs:25`                                  | Resolved by separate major migration | `0.16.1` compiles this site cleanly; `0.13.x` is the newest line `logos 0.13` admits                                                                                                                                          |
| `thiserror-impl` | `1.0.69` | `mismatched_lifetime_syntaxes` (9)                      | ``hiding a lifetime that's elided elsewhere is confusing``                                                           | `src/attr.rs:48`, `src/prop.rs:7, 11, 15, 19, 53, 57, 61, 65` | Resolved by separate major migration | `2.0.21` clears all nine hits; `1.0.69` is the newest `1.x` release                                                                                                                                                           |

Every row above has a named upstream owner and an actionable disposition.
Cargo's lint cap is not treated as a waiver anywhere in this table.

## What a major migration would and would not buy

To keep the migration reviews evidence-based, the pinned toolchain was used to
compile a scratch consumer against the newest available release of every
affected crate. The result is a mixed picture, so no migration may be sold as a
clean sweep:

| Change                           | Effect on this inventory                                                                                                                     |
| -------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `logos 0.13 -> 0.16.1`           | Removes both `regex-syntax 0.6.29` diagnostics and the `logos-codegen` lifetime diagnostic                                                   |
| `thiserror 1.0.69 -> 2.0.21`     | Removes all nine `thiserror-impl` diagnostics                                                                                                |
| `rowan 0.17 -> 0.18`             | No effect; `0.18.0-alpha` still depends on `memoffset 0.9`                                                                                   |
| `chumsky 0.9.3 -> 0.13.0`        | Removes the `chumsky` lifetime diagnostic but adds an unrelated `unused_import` warning                                                      |
| `hashbrown 0.14.5 -> 0.16.1`     | Removes the two `hashbrown` diagnostics and both `ahash` diagnostics, but adds `aho-corasick`, `getrandom`, and `regex-automata` diagnostics |
| `allocator-api2 0.2.21 -> 0.4.0` | Does not remove the diagnostic; the `0.4.0` tree still reports the same implicit autoref                                                     |
| `memoffset 0.9.1`                | Unchanged; `0.9.1` is the newest release and `0.18.0-alpha` rowan still requires `0.9`                                                       |
| `ahash 0.8.12`                   | Unchanged; `0.8.12` is the newest `0.8` release                                                                                              |

The scratch consumer reported 36 diagnostics across seven crates: fewer than
44, but with three new crate owners. That is a migration problem to be reviewed
on its own terms, not a warning-cleanup that can be smuggled into this ticket.

## Consumer changes

No consumer change was applied, and the ledger therefore records "no consumer
change".

The reason is the constraint table above. A caret-compatible fix would have to
come from a newer release inside an existing range, and
`cargo update --dry-run` already resolves to the newest version of every
affected crate. The remaining options are major API migrations, which the
ticket requires to be reviewed separately.

## Upstream contacts

None yet. The dispositions call for a comment on the `allocator-api2` thread
rather than a duplicate report, upstream fixes for `ahash` and `memoffset`, and
separate migration reviews for `chumsky`, `hashbrown`, `logos`, and
`thiserror`. Record each contact URL here once it exists.

## Run records

Each run is bound to the content tree it validated. A warning inventory is not
transferred validation for a later candidate.

### Historical evidence

The ticket's own inventory was captured on content tree
`da7bde9cc360dcd598b00eecbaae7740239dd283`, log `package.log`, SHA-256
`6d7dd56f51452c0c55830396ba8d79d5031ae5db06a4721d60709c0b73ac3b15`. It is
historical evidence only and does not validate any later tree.

### Baseline, tree `4a6c908`

| Field                 | Value                                                              |
| --------------------- | ------------------------------------------------------------------ |
| Content tree          | `4a6c908414c6ee2068bc5a88a73d1ad85ad74c3e`                         |
| Package command       | `RUSTFLAGS="-D warnings" cargo package -vv`                        |
| Command exit          | `0`                                                                |
| `package.log` SHA-256 | `85b62b3097dbb80aa1fd522db11eb988869b626b3ee9b1389a79dd58570615cd` |
| Declared warnings     | `44`                                                               |

Per-crate counts, as the toolchain itself reports them:

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

### Final candidate, tree `736fc8e`

`cargo package` refuses a working tree with uncommitted changes, so
verification runs on a committed tree. The candidate exercised here is the
commit that introduced this ledger, whose content is the same as the current
tip apart from later edits to this page. Amending any commit rewrites its
identifier, so this row is a record of the run, not a pointer to the branch tip.

| Field                 | Value                                                              |
| --------------------- | ------------------------------------------------------------------ |
| Content tree          | `736fc8e115bc0211401d315866fdd593c24bb815`                         |
| Package command       | `RUSTFLAGS="-D warnings" cargo package -vv`                        |
| Command exit          | `0`                                                                |
| `package.log` SHA-256 | `0767f218719a260e9543a0b2fb8d2c2c70e9d431de13400278786e7e34253509` |
| Declared warnings     | `44`                                                               |

The per-crate counts are unchanged from the baseline. The order in which crates
report varies between runs because `cargo` builds them in parallel; the counts
do not.

### Sequential gates

All gates passed on the final candidate, each with command exit `0`:

| Gate                | Exit |
| ------------------- | ---- |
| `make fmt`          | `0`  |
| `make check-fmt`    | `0`  |
| `make typecheck`    | `0`  |
| `make lint`         | `0`  |
| `make test`         | `0`  |
| `make markdownlint` | `0`  |
| `make nixie`        | `0`  |

## Differences from the baseline

None. This record introduces documentation only, so the warning inventory on
the final candidate is identical to the baseline: the same eight crates, the
same 44 diagnostics, and the same per-crate counts. Re-attributing the final
`package.log` by diagnostic path reproduces the baseline attribution exactly.

[issue-400]: https://github.com/leynos/ddlint/issues/400
[a2-41]: https://github.com/zakarumych/allocator-api2/issues/41
[a2-42]: https://github.com/zakarumych/allocator-api2/pull/42
[ahash-269]: https://github.com/tkaitchuck/aHash/pull/269
