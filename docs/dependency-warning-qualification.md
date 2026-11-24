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
| Toolchain channel | `nightly-2026-11-09`                            |
| `rustc` version   | `1.102.0-nightly (5b7b37aef 2026-11-08)`        |
| `rustc` commit    | `5b7b37aefcb5b2fba3c2392bc9065b9e4602d494`      |
| `cargo` version   | `1.102.0-nightly (c07ac2c4d 2026-11-06)`        |
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

The table is the current candidate's inventory. The ticket's 44-warning
inventory is preserved as the baseline in
[Differences from the baseline](#differences-from-the-baseline), because the
compiler change makes the two lists incomparable.

Table 3. Diagnostics, sites, owners, and dispositions.

| Crate            | Version  | Lint                                   | Count | Site (first of group)                                                     | Path and constraint                                   | Upstream owner                | Disposition                          |
| ---------------- | -------- | -------------------------------------- | ----- | ------------------------------------------------------------------------- | ----------------------------------------------------- | ----------------------------- | ------------------------------------ |
| `num-traits`     | `0.2.19` | `deprecated` (constants and modules)   | 364   | `src/bounds.rs:3`, `src/cast.rs:4`, `src/lib.rs:243`, `src/float.rs:803`  | direct dependency; `ddlint ^0.2`                      | `rust-num/num-traits`         | awaiting upstream release            |
| `rayon`          | `1.12.0` | `unused_imports`                       | 44    | `src/iter/blocks.rs:1`, `src/iter/chain.rs:1`, `src/iter/empty.rs:1`      | direct dependency; `ddlint ^1.10`                     | `rayon-rs/rayon`              | awaiting upstream release            |
| `rayon`          | `1.12.0` | `deprecated` (`fetch_update`)          | 4     | `src/iter/par_bridge.rs:113`, `src/iter/skip_any.rs:105`                  | direct dependency; `ddlint ^1.10`                     | `rayon-rs/rayon`              | awaiting upstream release            |
| `num-integer`    | `0.1.47` | `deprecated` (constants and functions) | 42    | `src/roots.rs:259`, `src/lib.rs:512`                                      | `num-bigint -> num-integer`; `num-bigint ^0.1`        | `rust-num/num-integer`        | awaiting upstream release            |
| `memoffset`      | `0.9.1`  | `unexpected_cfgs`                      | 24    | `src/lib.rs:59`, `src/offset_of.rs:23`, `src/raw_field.rs:22`             | `rowan -> memoffset`; `rowan ^0.9`                    | `Gilnaa/memoffset`            | contribute upstream fix              |
| `regex-syntax`   | `0.6.29` | `deprecated`                           | 9     | `src/ast/mod.rs:236`, `src/ast/parse.rs:1537`, `src/hir/interval.rs:5`    | `logos-codegen -> regex-syntax`; `logos-codegen ^0.6` | `rust-lang/regex`             | resolved by separate major migration |
| `regex-syntax`   | `0.6.29` | `dead_code`, `noop_method_call`        | 2     | `src/hir/interval.rs:423`, `src/ast/parse.rs:379`                         | `logos-codegen -> regex-syntax`; `logos-codegen ^0.6` | `rust-lang/regex`             | resolved by separate major migration |
| `thiserror-impl` | `1.0.69` | `mismatched_lifetime_syntaxes`         | 9     | `src/attr.rs:48`, `src/prop.rs:7`                                         | `thiserror -> thiserror-impl`; `=1.0.69`              | `dtolnay/thiserror`           | resolved by separate major migration |
| `logos-codegen`  | `0.13.0` | `deprecated` (`usize::max_value`)      | 4     | `src/graph/meta.rs:85`, `src/graph/meta.rs:104`                           | `logos-derive -> logos-codegen`; `^0.13`              | `maciejhirsz/logos`           | resolved by separate major migration |
| `logos-codegen`  | `0.13.0` | `mismatched_lifetime_syntaxes`         | 1     | `src/generator/tables.rs:25`                                              | `logos-derive -> logos-codegen`; `^0.13`              | `maciejhirsz/logos`           | resolved by separate major migration |
| `ahash`          | `0.8.13` | `unexpected_cfgs`, `dead_code`         | 4     | `src/fallback_hash.rs:179`, `src/random_state.rs:57`, `src/convert.rs:67` | `chumsky -> hashbrown -> ahash`; `hashbrown ^0.8.7`   | `tkaitchuck/aHash`            | contribute upstream fix              |
| `hashbrown`      | `0.14.5` | `dead_code`                            | 2     | `src/raw/mod.rs:4442`, `src/raw/mod.rs:4447`                              | `chumsky` and `rowan -> hashbrown`                    | `rust-lang/hashbrown`         | awaiting upstream release            |
| `allocator-api2` | `0.2.21` | `dangerous_implicit_autorefs`          | 1     | `src/stable/vec/mod.rs:1902`                                              | `chumsky -> hashbrown -> allocator-api2`; `^0.2.9`    | `zakarumych/allocator-api2`   | awaiting upstream release            |
| `text-size`      | `1.1.1`  | `deprecated` (`std::u32`)              | 1     | `src/size.rs:8`                                                           | `rowan -> text-size`; `rowan ^1.1`                    | `rust-analyzer/text-size`     | awaiting upstream release            |
| `chumsky`        | `0.9.3`  | `mismatched_lifetime_syntaxes`         | 1     | `src/recursive.rs:31`                                                     | direct dependency; `ddlint ^0.9`                      | `zesterer/chumsky` (Codeberg) | resolved by separate major migration |

The four largest groups are new since the ticket was written. They are
`deprecated` and `unused_imports` diagnostics that the newer compiler date
introduced, not sites that changed in the registry crates. Ranking by count:

Table 4. Groups that appeared with the newer compiler.

| Crate                 | Gained | Class            | Explanation                                                                     |
| --------------------- | ------ | ---------------- | ------------------------------------------------------------------------------- |
| `num-traits`          | 364    | `deprecated`     | `core::<int>::MAX`/`MIN` and `std::u32`-style paths that the compiler now flags |
| `rayon`               | 48     | `unused_imports` | `use super::plumbing::*` in files that no longer reference it                   |
| `num-integer`         | 42     | `deprecated`     | the same numeric-constant and `min_value` deprecations                          |
| `regex-syntax 0.6.29` | +9     | `deprecated`     | `std::u8`/`std::u32` module and constant paths                                  |
| `logos-codegen`       | +4     | `deprecated`     | `usize::max_value` replaced by the `MAX` associated constant                    |
| `text-size`           | +1     | `deprecated`     | `std::u32` module path                                                          |

None of these has an in-range fix: `num-traits 0.2.19`, `rayon 1.12.0`,
`num-integer 0.1.47`, and `text-size 1.1.1` are each the newest release in
their line. Every one therefore carries "awaiting upstream release" rather than
a waiver.

Neither the `rayon` nor the `num-traits` family is a `ddlint` defect: both are
direct dependencies whose registry sources use constructs the compiler
deprecates. The `rayon` unused imports are a genuine upstream cleanup, but they
cannot be fixed from this repository.

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

Table 5. Migration tickets and their change vehicles.

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

Table 6. Measured effect of each candidate migration.

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
- `memoffset`: issues are enabled again as of 2026-11-23, but no fix or pull
  request exists and the repository has been dormant since 2024-03-26. An issue
  report, then a contributed `rustc-check-cfg` fix, is the available route.
- `chumsky`, `logos`, `hashbrown`, and `thiserror`: covered by the migration
  tickets in Table 5.

Record each new contact URL here once it exists.

## Run records

Each run is bound to the content tree it validated. A warning inventory is not
transferred validation for a later candidate.

### Historical evidence

The ticket's own inventory was captured on content tree
`da7bde9cc360dcd598b00eecbaae7740239dd283`, log `package.log`, SHA-256
`6d7dd56f51452c0c55830396ba8d79d5031ae5db06a4721d60709c0b73ac3b15`. It is
historical evidence only and does not validate any later tree.

### Package verification, tree `65f1e651`

Table 7. Package verification record.

| Field                 | Value                                                              |
| --------------------- | ------------------------------------------------------------------ |
| Commit                | `a108de736d6de44819186ae73eebdc62145b4b22`                         |
| Content tree          | `65f1e6519cd2acb94ee1770f16a00b516aa4cf91`                         |
| Package command       | `RUSTFLAGS="-D warnings" cargo package -p ddlint -vv`              |
| Command exit          | `0`                                                                |
| `tee` exit            | `0`                                                                |
| `package.log` SHA-256 | `9cd7bbcada15bf507bd199892153ca8e6414b3c104c05c9c244ef6cbc81326d7` |
| Declared warnings     | `513`                                                              |

The workspace-wide `cargo package` form is blocked on this tree by the separate
`cli` package-metadata defect: its `ddlint` dependency declares a `path` with
no version, so Cargo refuses to verify that manifest (`command_exit=101`, log
SHA-256 `36177edfea33fef714a0332af917a04bdc220a009987a2cc44108e1eafed4a3b`). The
`-p ddlint` selector packages the crate this ledger qualifies and is otherwise
identical.

Per-crate counts, as the toolchain itself reports them:

Table 8. Warnings per crate.

| Crate            | Warnings |
| ---------------- | -------- |
| `num-traits`     | 365      |
| `rayon`          | 48       |
| `num-integer`    | 42       |
| `memoffset`      | 24       |
| `regex-syntax`   | 11       |
| `thiserror-impl` | 9        |
| `logos-codegen`  | 5        |
| `ahash`          | 4        |
| `hashbrown`      | 2        |
| `allocator-api2` | 1        |
| `text-size`      | 1        |
| `chumsky`        | 1        |
| **Total**        | **513**  |

Attribution note: `cargo` builds crates in parallel and interleaves their
diagnostics. Attribution here comes from each diagnostic's own `-->` path,
which reconciles to the same 513 the toolchain declares.

Edits to this page after the recorded commit are documentation-only. The
package route is bound to the tree above; the later documentation gates are
recorded separately.

### Resolved graph, tree `65f1e651`

`Cargo.lock` is ignored, so the graph is captured rather than committed. The
capture uses the pinned toolchain, the resolved lock, and the normal and build
edges:

```shell
cargo tree --locked --edges normal,build
```

Table 9. Resolved-graph capture record.

| Field          | Value                                                              |
| -------------- | ------------------------------------------------------------------ |
| Content tree   | `65f1e6519cd2acb94ee1770f16a00b516aa4cf91`                         |
| Command exit   | `0`                                                                |
| `tee` exit     | `0`                                                                |
| Output lines   | `273`                                                              |
| SHA-256 digest | `2cee172f4ebf84c065b2ff67bb6b2378d4f5c35b3c0fd07473b8eeae849ae0ff` |

The capture resolves the twelve crates in Table 8, including
`allocator-api2 0.2.21`, `ahash 0.8.13`, `hashbrown 0.14.5`, `memoffset 0.9.1`,
`regex-syntax 0.6.29`, `text-size 1.1.1`, `chumsky 0.9.3`,
`logos-codegen 0.13.0`, and `thiserror-impl 1.0.69`.

This is point-in-time evidence, not a reproducible guarantee. `Cargo.lock` is
ignored, so a later registry index could select a different caret-compatible
version while this tree stays unchanged. The digest therefore identifies what
was captured on that tree and day; it does not assert what a future resolution
will choose.

### Candidate scope

The candidate tree is the merged result of this branch and `main`, and pins
`nightly-2026-11-09`. That is later than the `nightly-2025-09-16` the ticket
names, because `main` moved the pin while this work was in progress. Merging
was unavoidable: the branch conflicted with `main`, and a conflicting pull
request runs no CI at all.

The compiler change is the reason the inventory grew from 44 to 513. The newer
compiler enables deprecation diagnostics the older one did not report, so the
two numbers measure different compilers rather than a regression in `ddlint` or
its dependencies. Table 4 separates the groups that appeared with the newer
compiler.

### Migration probes

Both probes are auxiliary evidence for Table 6, not package verification of
`ddlint`. Each is a scratch crate with a `[workspace]` table so it resolves
independently, built with the pinned toolchain under `RUSTFLAGS="-D warnings"`.

Table 10. Probe records.

| Probe                | Dependencies                                                                                                                                | Command exit | Warnings | Log SHA-256                                                        |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | ------------ | -------- | ------------------------------------------------------------------ |
| Newest-release probe | `chumsky 0.14.2`, `logos 0.16.1`, `thiserror 2.0.21`, `rowan 0.18.0-alpha.2`, `hashbrown 0.17.2`, `allocator-api2 0.4.0`, `memoffset 0.9.1` | `0`          | `33`     | `fac6be05eca777ade41c325b9ece9a57de4028ac068e740d757804d0fdd9c279` |
| Intermediate probe   | `chumsky 0.13.0`, `logos 0.15.1`                                                                                                            | `0`          | `3`      | `8797ba1da7c1c6d2fd75cb99d5c716936cf9f8020762dc925b04d36914c7fae2` |

In the newest-release probe, `ahash` is never compiled. `chumsky 0.14.2` uses
`hashbrown 0.16.1`, and `rowan 0.18.0-alpha.2` compiles `hashbrown 0.14.5` with
default features off, so the `ahash` feature is never enabled. The probe log
contains `ahash` only inside `hashbrown`'s `--check-cfg` feature list, which is
why the four `ahash` diagnostics clear under the [chumsky migration][t-chumsky]
while the two `hashbrown 0.14.5` diagnostics remain under the
[rowan review][t-rowan].

### Sequential gates

The gates below ran sequentially on the documented content tree. The Rust
gates ran on `65f1e6519cd2acb94ee1770f16a00b516aa4cf91`; the documentation
gates were re-run after the later prose edits.

Table 11. Sequential gate results.

| Gate                | Exit | Scope                                                           |
| ------------------- | ---- | --------------------------------------------------------------- |
| `make fmt`          | `0`  | Formatting applied; `markdownlint --fix` reported 0 issues       |
| `make check-fmt`    | `0`  | `cargo fmt --check` and `mdtablefix --check`; 74 files unchanged |
| `make typecheck`    | `0`  | Workspace, all targets and features                             |
| `make lint`         | `0`  | Interrogate, Clippy, Clippy policy, Whitaker, and spelling       |
| `make test`         | `0`  | Full workspace suite, none failed                               |
| `make markdownlint` | `0`  | 74 files, 0 issues, plus the spelling gate                      |
| `make nixie`        | `0`  | Every Mermaid diagram validated                                 |

## Differences from the baseline

The ticket's baseline is 44 warnings across eight crates on
`nightly-2025-09-16`. The current candidate reports 513 across twelve crates on
`nightly-2026-11-09`. The difference is almost entirely the compiler date, which
`main` moved while this work was in progress.

Table 12. Baseline versus current candidate.

| Crate            | Baseline | Current | Change   | Cause                                   |
| ---------------- | -------- | ------- | -------- | --------------------------------------- |
| `num-traits`     | —        | 365     | +365     | newer compiler's `deprecated` lints     |
| `rayon`          | —        | 48      | +48      | newer compiler, plus `unused_imports`   |
| `num-integer`    | —        | 42      | +42      | newer compiler's `deprecated` lints     |
| `memoffset`      | 24       | 24      | 0        | unchanged                               |
| `regex-syntax`   | 2        | 11      | +9       | newer compiler's `deprecated` lints     |
| `thiserror-impl` | 9        | 9       | 0        | unchanged                               |
| `logos-codegen`  | 1        | 5       | +4       | newer compiler's `deprecated` lints     |
| `ahash`          | 4        | 4       | 0        | `0.8.13` moves the sites; no net change |
| `hashbrown`      | 2        | 2       | 0        | unchanged                               |
| `allocator-api2` | 1        | 1       | 0        | unchanged                               |
| `text-size`      | —        | 1       | +1       | newer compiler's `deprecated` lint      |
| `chumsky`        | 1        | 1       | 0        | unchanged                               |
| **Total**        | **44**   | **513** | **+469** | compiler date                           |

Every original group survives at the same count. `ahash` resolves to `0.8.13`
(2026-11-23), which registers `vaes` and moves the three sites, but leaves the
`128` condition value, the `fuzzing` condition name, and the dead-code methods
in place, so its four diagnostics are unchanged in number.

The `+469` is not a dependency regression and must not be waived. It is
recorded as 469 diagnostics from five crates that are each at their newest
in-range release, with "awaiting upstream release" as the disposition. A
compiler-date rollback would hide them again; that would be exactly the kind of
suppression the ticket forbids.

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
