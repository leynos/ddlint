.DEFAULT_GOAL := build

.PHONY: help all clean test build release check lint typecheck fmt check-fmt \
        markdownlint spelling nixie tools \
        test-workflow-contracts test-clippy-policy interrogate \
        check-build-tools install-build-tools

APP ?= ddlint
CARGO ?= $(or $(shell command -v cargo 2>/dev/null),$(HOME)/.cargo/bin/cargo)
BUILD_JOBS ?=
CLIPPY_FLAGS ?= --all-targets --all-features -- -D warnings
MDLINT ?= $(shell command -v markdownlint-cli2 2>/dev/null || printf '%s' "$$HOME/.bun/bin/markdownlint-cli2")
# `make fmt` and `make check-fmt` call mdtablefix directly. `--git` selects the
# Markdown files Git tracks and `--include-untracked` adds the untracked files
# Git does not ignore, so a new document is formatted before it is staged.
# Both modes need mdtablefix 0.6.0 or later; CI pins the version at the
# install-mdtablefix step.
MDTABLEFIX ?= mdtablefix
MDTABLEFIX_SELECT = --git --include-untracked
MDTABLEFIX_RULES = --wrap --renumber --breaks --ellipsis --fences
NIXIE ?= nixie
TYPOS_CONFIG_BUILDER_VERSION ?= v0.1.3
TYPOS_CONFIG_BUILDER = uv tool run --python 3.14 --from \
	"git+https://github.com/leynos/typos-config-builder.git@$(TYPOS_CONFIG_BUILDER_VERSION)" \
	typos-config-builder
WHITAKER ?= whitaker

UV ?= uv
UV_ENV = UV_CACHE_DIR=.uv-cache UV_TOOL_DIR=.uv-tools
# The CV-005 CodeScene contracts live in shared-actions and run from a full
# commit, so a fix is a pin bump. `.github/cv005.toml` holds this repository's
# only parameters.
CV005_CONTRACTS_REF ?= 88977798a5c3bae1549afb99642529488c665276
CV005_CONTRACTS_REQUIREMENT = git+https://github.com/leynos/shared-actions@$(CV005_CONTRACTS_REF)\#subdirectory=packages/cv005-contracts
CV005_CONTRACTS = $(UV_ENV) $(UV) tool run --python 3.13 \
	--from '$(CV005_CONTRACTS_REQUIREMENT)' \
	cv005-contracts

# The development build standard (concordat rule `rust-build-defaults`):
# the parallel rustc frontend and, on Linux, the mold linker. An assigned
# RUSTFLAGS replaces every `rustflags` table in .cargo/config.toml, so each
# recipe that sets it composes these onto any inherited value (CI's
# setup-rust exports one), except coverage, which stays on LLVM and the
# platform linker.
BUILD_HOST_OS := $(shell uname -s)
BUILD_HOST_ARCH := $(shell uname -m)
STANDARD_RUSTFLAGS := -Zthreads=8$(if $(filter Linux,$(BUILD_HOST_OS)), -Clink-arg=-fuse-ld=mold)
RUST_TOOLCHAIN := $(shell sed -n 's/^channel = "\(.*\)"/\1/p' rust-toolchain.toml)
MOLD_VERSION := $(shell sed -n '1p' tools/mold/VERSION)
MOLD_INSTALL_DIR = $(HOME)/.local/share/ddlint/build-tools/mold-$(MOLD_VERSION)
MOLD_INSTALL_BIN = $(MOLD_INSTALL_DIR)/bin
export PATH := $(MOLD_INSTALL_BIN):$(PATH)

check-build-tools: ## Check the pinned local development build tools
	@set -eu; \
	  toolchain="$(RUST_TOOLCHAIN)"; \
	  if ! command -v rustup >/dev/null 2>&1; then \
	    printf '%s\n' 'rustup is required; install Rustup, then run make install-build-tools' >&2; \
	    exit 1; \
	  fi; \
	  if ! rustup run "$$toolchain" rustc --version >/dev/null 2>&1; then \
	    printf 'Rust toolchain %s is unavailable; run make install-build-tools\n' "$$toolchain" >&2; \
	    exit 1; \
	  fi; \
	  if [ "$(BUILD_HOST_OS)" = Linux ]; then \
	    if ! command -v mold >/dev/null 2>&1; then \
	      printf 'mold %s is required on Linux; run make install-build-tools\n' "$(MOLD_VERSION)" >&2; \
	      exit 1; \
	    fi; \
	    if ! mold_output=$$(mold --version 2>/dev/null); then \
	      printf 'mold %s version check failed; run make install-build-tools\n' "$(MOLD_VERSION)" >&2; \
	      exit 1; \
	    fi; \
	    mold_version=$$(printf '%s\n' "$$mold_output" | \
	      sed -n 's/^mold \([0-9][0-9.]*\).*/\1/p'); \
	    if [ "$$mold_version" != "$(MOLD_VERSION)" ]; then \
	      printf 'mold %s is required on Linux (found %s); run make install-build-tools\n' \
	        "$(MOLD_VERSION)" "$$mold_version" >&2; \
	      exit 1; \
	    fi; \
	  fi

install-build-tools: ## Install the pinned local Rust and Linux linker tools
	@set -eu; \
	  toolchain="$(RUST_TOOLCHAIN)"; \
	  if ! command -v rustup >/dev/null 2>&1; then \
	    printf '%s\n' 'rustup is required; install Rustup before provisioning repository tools' >&2; \
	    exit 1; \
	  fi; \
	  if [ "$(BUILD_HOST_OS)" = Linux ]; then \
	    case "$(BUILD_HOST_ARCH)" in \
	      x86_64|aarch64) ;; \
	      *) \
	        printf 'mold %s provisioning supports Linux x86_64 and aarch64 (found %s)\n' \
	          "$(MOLD_VERSION)" "$(BUILD_HOST_ARCH)" >&2; \
	        exit 1 ;; \
	    esac; \
	    mold_version=; \
	    if command -v mold >/dev/null 2>&1; then \
	      if mold_output=$$(mold --version 2>/dev/null); then \
	        mold_version=$$(printf '%s\n' "$$mold_output" | \
	          sed -n 's/^mold \([0-9][0-9.]*\).*/\1/p'); \
	      fi; \
	    fi; \
	    if [ "$$mold_version" != "$(MOLD_VERSION)" ]; then \
	      temporary_root=$${TMPDIR:-/tmp}; \
	      workdir=$$(mktemp -d "$$temporary_root/ddlint-build-tools.XXXXXX"); \
	      trap 'rm -rf "$$workdir"' 0; \
	      trap 'exit 1' HUP INT TERM; \
	      repository_root="$(CURDIR)"; \
	      archive_name="mold-$(MOLD_VERSION)-$(BUILD_HOST_ARCH)-linux.tar.gz"; \
	      install_dir="$(MOLD_INSTALL_DIR)"; \
	      mkdir -p "$$install_dir"; \
	      cd "$$workdir"; \
	      curl --fail --location --silent --show-error \
	        --output "$$workdir/$$archive_name" \
	        "https://github.com/rui314/mold/releases/download/v$(MOLD_VERSION)/$$archive_name"; \
	      checksum_entry=$$(awk -v archive="$$archive_name" \
	        '$$2 == archive { if (++matches == 1) print; else exit 1 } \
	          END { if (matches != 1) exit 1 }' \
	        "$$repository_root/tools/mold/SHA256SUMS"); \
	      printf '%s\n' "$$checksum_entry" | sha256sum --check -; \
	      tar -xzf "$$archive_name" --directory "$$install_dir" --strip-components=1; \
	      if [ ! -f "$$install_dir/bin/ld.mold" ]; then \
	        ln -s mold "$$install_dir/bin/ld.mold"; \
	      fi; \
	      if installed_output=$$("$${install_dir}/bin/mold" --version 2>/dev/null); then \
	        installed_version=$$(printf '%s\n' "$$installed_output" | \
	          sed -n 's/^mold \([0-9][0-9.]*\).*/\1/p'); \
	      else \
	        printf 'installed mold %s could not report its version\n' \
	          "$(MOLD_VERSION)" >&2; \
	        exit 1; \
	      fi; \
	      if [ "$$installed_version" != "$(MOLD_VERSION)" ]; then \
	        printf 'installed mold must report version %s (found %s)\n' \
	          "$(MOLD_VERSION)" "$$installed_version" >&2; \
	        exit 1; \
	      fi; \
	    fi; \
	  fi; \
	  installed_toolchains=$$(rustup toolchain list); \
	  if printf '%s\n' "$$installed_toolchains" | \
	    awk -v channel="$$toolchain" \
	      '$$1 == channel || index($$1, channel "-") == 1 { found = 1 } END { exit !found }'; then \
	    rustup component add rustfmt clippy rust-analyzer --toolchain "$$toolchain"; \
	  else \
	    rustup toolchain install "$$toolchain" --profile minimal \
	      --component rustfmt --component clippy --component rust-analyzer; \
	  fi
	+$(MAKE) check-build-tools

build: | check-build-tools ## Build debug binary
	RUSTFLAGS="$${RUSTFLAGS:+$$RUSTFLAGS }$(STANDARD_RUSTFLAGS)" $(CARGO) build $(BUILD_JOBS) --bin $(APP)

release: ## Build release binary
	RUSTFLAGS="$${RUSTFLAGS-}" $(CARGO) build $(BUILD_JOBS) --release --bin $(APP)

all: release ## Build release binary

check:
	+$(MAKE) check-fmt
	+$(MAKE) typecheck
	+$(MAKE) lint
	+$(MAKE) test

clean: ## Remove build artifacts
	$(CARGO) clean
	rm -rf .coverage .pytest_cache
	rm -f .typos-oxendict-base.json .typos-oxendict-base.toml

test: | check-build-tools ## Run tests with warnings treated as errors
	RUSTFLAGS="$${RUSTFLAGS:+$$RUSTFLAGS }-D warnings $(STANDARD_RUSTFLAGS)" $(CARGO) test --all-targets --all-features $(BUILD_JOBS)

test-workflow-contracts: ## Validate the mutation-testing and CodeScene coverage workflow contracts
	$(CV005_CONTRACTS) check --repository .
	uv run --with 'pytest>=8' --with 'pyyaml>=6' --with '$(CV005_CONTRACTS_REQUIREMENT)' pytest tests/workflow_contracts -q

# Run outside cargo test: a nested Cargo process could wait on its parent's lock.
test-clippy-policy: | check-build-tools ## Prove Clippy accepts test bodies and rejects panicking helpers
	CLIPPY_POLICY_CARGO="$(CARGO)" RUSTFLAGS="$${RUSTFLAGS:+$$RUSTFLAGS }$(STANDARD_RUSTFLAGS)" $(UV) run --with 'pytest>=8' --with 'plumbum>=1.9,<2' pytest scripts/clippy_expect_policy_test.py -q


# Interrogate measures docstring presence over every Python file the repository
# owns. Add a new Python directory to INTERROGATE_TARGETS. No `--python` pin: a
# managed interpreter would be downloaded, and a PyO3 build could link it.
INTERROGATE_VERSION ?= 1.7.0
INTERROGATE_TARGETS ?= tests scripts
# Level 2 prints a MISSED row per undocumented definition, which the
# `interrogate` target fails on: the rounded percentage alone passes a lone gap.
INTERROGATE_FLAGS ?= -vv --fail-under 100
INTERROGATE = $(UV_ENV) $(UV) tool run \
	--from 'interrogate==$(INTERROGATE_VERSION)' interrogate

# Interrogate rounds its percentage to one decimal, so `--fail-under 100` alone
# passes a lone gap in a large repository; fail on any MISSED row as well.
interrogate: ## Enforce complete Python docstring coverage
	@report="$$($(INTERROGATE) $(INTERROGATE_FLAGS) $(INTERROGATE_TARGETS))"; \
	  status=$$?; \
	  printf '%s\n' "$$report"; \
	  test "$$status" -eq 0 || exit "$$status"; \
	  if printf '%s\n' "$$report" | grep -q '| *MISSED *|'; then \
	    printf '%s\n' 'interrogate: undocumented definitions remain; see the MISSED rows above' >&2; \
	    exit 1; \
	  fi

lint: interrogate ## Run Clippy and the Whitaker Dylint suite with warnings denied
	+$(MAKE) check-build-tools
	RUSTFLAGS="$${RUSTFLAGS:+$$RUSTFLAGS }$(STANDARD_RUSTFLAGS)" $(CARGO) clippy $(CLIPPY_FLAGS)
	+$(MAKE) test-clippy-policy
	RUSTFLAGS="$${RUSTFLAGS:+$$RUSTFLAGS }-D warnings $(STANDARD_RUSTFLAGS)" $(WHITAKER) --all -- --all-targets --all-features
	+$(MAKE) spelling

typecheck: | check-build-tools ## Typecheck all workspace targets and features
	CARGO_CACHE_RUSTC_INFO=0 RUSTFLAGS="$${RUSTFLAGS:+$$RUSTFLAGS }$(STANDARD_RUSTFLAGS)" $(CARGO) check --workspace --all-targets \
		--all-features $(BUILD_JOBS)
# Macro ensuring a tool exists in PATH
define ensure_tool
$(if $(shell command -v $(1) >/dev/null 2>&1 && echo y),,\
$(error $(1) is required but not installed))
endef

# Ensure essential formatting tools exist to avoid missing-command errors
tools:
	$(call ensure_tool,mdtablefix)
	$(call ensure_tool,$(CARGO))
	$(call ensure_tool,rustfmt)
fmt: tools ## Format Rust and Markdown sources
	$(CARGO) fmt --all
	$(MDTABLEFIX) --in-place $(MDTABLEFIX_SELECT) $(MDTABLEFIX_RULES)
	$(MDLINT) --fix "**/*.md"

check-fmt: ## Verify formatting
	$(CARGO) fmt --all -- --check
	$(MDTABLEFIX) --check $(MDTABLEFIX_SELECT) $(MDTABLEFIX_RULES)

markdownlint: ## Lint Markdown files
	$(MDLINT) '**/*.md'
	+$(MAKE) spelling

spelling: ## Enforce en-GB-oxendict spelling in Markdown prose
	$(TYPOS_CONFIG_BUILDER) gate --repository .

nixie: ## Validate Mermaid diagrams
	find . -type f -name '*.md' -not -path './target/*' -not -path './.uv-cache/*' -not -path './.uv-tools/*' -print0 | xargs -0 $(NIXIE)

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?##' $(MAKEFILE_LIST) | \
	awk 'BEGIN {FS=":"; printf "Available targets:\n"} {printf "  %-20s %s\n", $$1, $$2}'
