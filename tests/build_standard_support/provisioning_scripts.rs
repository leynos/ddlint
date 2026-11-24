//! Shell shims used by the build provisioning contract fixtures.
//!
//! Each shim records its invocation and stays inside the fixture sandbox; the
//! toolchain shims accept only the channel pinned by `rust-toolchain.toml`,
//! which the fixture supplies as `FAKE_PINNED_TOOLCHAIN`.

/// Logs and fulfils downloads with inert bytes kept inside the fixture root.
pub(super) const CURL_SCRIPT: &str = r#"#!/bin/sh
printf 'curl:%s\n' "$*" >> "$TRACE_FILE"
destination=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --output|-o)
      shift
      destination=${1-}
      ;;
    --output=*) destination=${1#--output=} ;;
    -o*) destination=${1#-o} ;;
  esac
  shift
done
if [ -n "$destination" ] && [ "$destination" != "-" ]; then
  case "$destination" in
    "$SANDBOX_ROOT"/*) printf 'offline fixture archive\n' > "$destination" ;;
    *) printf 'refusing download outside the test sandbox\n' >&2; exit 73 ;;
  esac
else
  printf 'offline fixture archive\n'
fi
"#;

/// Rejects fallback downloads so installer cases remain offline.
pub(super) const WGET_SCRIPT: &str = r#"#!/bin/sh
printf 'wget-refused:%s\n' "$*" >> "$TRACE_FILE"
printf 'only the offline curl fixture is permitted\n' >&2
exit 73
"#;

/// Records checksum arguments/input and can inject a verification failure.
pub(super) const CHECKSUM_SCRIPT: &str = r#"#!/bin/sh
printf 'sha256sum:%s\n' "$*" >> "$TRACE_FILE"
printf 'checksum-entry:%s\n' "$(/usr/bin/cat)" >> "$TRACE_FILE"
if [ "${FAKE_CHECKSUM_FAIL:-false}" = true ]; then
  exit 1
fi
exit 0
"#;

/// Extracts a minimal mold tree only beneath the private fixture root.
pub(super) const TAR_SCRIPT: &str = r#"#!/bin/sh
printf 'tar:%s\n' "$*" >> "$TRACE_FILE"
destination=
while [ "$#" -gt 0 ]; do
  case "$1" in
    -C|--directory)
      shift
      destination=${1-}
      ;;
    -C*) destination=${1#-C} ;;
    --directory=*) destination=${1#--directory=} ;;
  esac
  shift
done
if [ -z "$destination" ]; then
  destination=$(pwd)
fi
case "$destination" in
  "$SANDBOX_ROOT"|"$SANDBOX_ROOT"/*) ;;
  *) printf 'refusing extraction outside the test sandbox\n' >&2; exit 73 ;;
esac
archive_root="$destination/mold-2.41.0-$FAKE_TARGET_ARCH-linux"
for binary_directory in "$archive_root/bin" "$destination/bin"; do
  mkdir -p "$binary_directory"
  printf '#!/bin/sh\nprintf "mold 2.41.0\\n"\n' > "$binary_directory/mold"
  chmod 755 "$binary_directory/mold"
done
exit 0
"#;

/// Models pinned toolchain and component discovery without installing them.
pub(super) const RUSTUP_SCRIPT: &str = r#"#!/bin/sh
printf 'rustup:%s\n' "$*" >> "$TRACE_FILE"
if [ -z "${FAKE_PINNED_TOOLCHAIN:-}" ]; then
  printf 'fixture has no pinned toolchain\n' >&2
  exit 75
fi
case "$*" in
  *"toolchain install"*)
    case "$*" in
      *"$FAKE_PINNED_TOOLCHAIN"*) : > "$RUSTUP_TOOLCHAIN_MARKER" ;;
      *) printf 'refusing unpinned toolchain install\n' >&2; exit 74 ;;
    esac
    case "$*" in
      *rust-analyzer*) : > "$RUST_ANALYZER_MARKER" ;;
    esac
    ;;
  *"$FAKE_PINNED_TOOLCHAIN"*)
    if [ "${FAKE_TOOLCHAIN_PRESENT:-true}" != true ] && [ ! -f "$RUSTUP_TOOLCHAIN_MARKER" ]; then
      printf 'toolchain %s is missing\n' "$FAKE_PINNED_TOOLCHAIN" >&2
      exit 1
    fi
    ;;
esac
case "$*" in
  *"toolchain list"*)
    if [ "${FAKE_TOOLCHAIN_PRESENT:-true}" = true ] || [ -f "$RUSTUP_TOOLCHAIN_MARKER" ]; then
      printf '%s-%s-unknown-linux-gnu\n' "$FAKE_PINNED_TOOLCHAIN" "$FAKE_TARGET_ARCH"
    fi
    ;;
  *"component list"*)
    if [ -f "$RUST_ANALYZER_MARKER" ] || [ "${FAKE_COMPONENT_PRESENT:-true}" = true ]; then
      printf 'rust-analyzer-%s-unknown-linux-gnu\n' "$FAKE_TARGET_ARCH"
    fi
    ;;
  *"component add"*rust-analyzer*)
    : > "$RUST_ANALYZER_MARKER"
    ;;
esac
exit 0
"#;

/// Reports the pinned compiler and rejects an absent fake toolchain.
pub(super) const RUSTC_SCRIPT: &str = r#"#!/bin/sh
if [ -z "${FAKE_PINNED_TOOLCHAIN:-}" ]; then
  printf 'fixture has no pinned toolchain\n' >&2
  exit 75
fi
if [ "${FAKE_TOOLCHAIN_PRESENT:-true}" != true ] && [ ! -f "$RUSTUP_TOOLCHAIN_MARKER" ]; then
  printf 'toolchain %s is missing\n' "$FAKE_PINNED_TOOLCHAIN" >&2
  exit 1
fi
printf 'rustc 1.90.0-nightly (%s)\n' "$FAKE_PINNED_TOOLCHAIN"
"#;

/// Delegates read-only text utilities to their host implementations.
pub(super) const TEXT_UTILITY_SCRIPT: &str = r#"#!/bin/sh
exec "/usr/bin/${0##*/}" "$@"
"#;

/// Supplies the current Linux architecture to the fake installer.
pub(super) const UNAME_SCRIPT: &str = r#"#!/bin/sh
case "$*" in
  -m) printf '%s\n' "$FAKE_TARGET_ARCH" ;;
  -sm|-ms) printf 'Linux %s\n' "$FAKE_TARGET_ARCH" ;;
  *) printf 'Linux\n' ;;
esac
"#;
