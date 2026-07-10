.PHONY: help all clean test build release check lint typecheck fmt check-fmt \
        markdownlint spelling spelling-helper-test nixie tools \
        test-workflow-contracts

APP ?= ddlint
CARGO ?= $(or $(shell command -v cargo 2>/dev/null),$(HOME)/.cargo/bin/cargo)
BUILD_JOBS ?=
CLIPPY_FLAGS ?= --all-targets --all-features -- -D warnings
MDLINT ?= markdownlint
NIXIE ?= nixie
TYPOS_VERSION ?= 1.48.0
TYPOS := uv tool run typos@$(TYPOS_VERSION)

build: target/debug/$(APP) ## Build debug binary
release: target/release/$(APP) ## Build release binary

all: release ## Default target builds release binary

check: check-fmt typecheck lint test

clean: ## Remove build artifacts
	$(CARGO) clean
	rm -rf .coverage .pytest_cache scripts/__pycache__ scripts/tests/__pycache__
	rm -f .typos-oxendict-base.json .typos-oxendict-base.toml

test: ## Run tests with warnings treated as errors
	RUSTFLAGS="-D warnings" $(CARGO) test --all-targets --all-features $(BUILD_JOBS)

test-workflow-contracts: ## Validate the mutation-testing caller contract
	uv run --with 'pytest>=8' --with 'pyyaml>=6' pytest tests/workflow_contracts -q

target/%/$(APP): ## Build binary in debug or release mode
	$(CARGO) build $(BUILD_JOBS) $(if $(filter release,$*),--release) --bin $(APP)

lint: ## Run Clippy with warnings denied
	$(CARGO) clippy $(CLIPPY_FLAGS)
	+$(MAKE) spelling

typecheck: ## Typecheck all workspace targets and features
	CARGO_CACHE_RUSTC_INFO=0 $(CARGO) check --workspace --all-targets \
		--all-features $(BUILD_JOBS)
# Macro ensuring a tool exists in PATH
define ensure_tool
$(if $(shell command -v $(1) >/dev/null 2>&1 && echo y),,\
$(error $(1) is required but not installed))
endef

# Ensure essential formatting tools exist to avoid missing-command errors
tools:
	$(call ensure_tool,mdformat-all)
	$(call ensure_tool,$(CARGO))
	$(call ensure_tool,rustfmt)
fmt: tools ## Format Rust and Markdown sources
	$(CARGO) fmt --all
	mdformat-all

check-fmt: ## Verify formatting
	$(CARGO) fmt --all -- --check

markdownlint: ## Lint Markdown files
	git rev-parse --verify origin/main >/dev/null
	@set -e; \
	tmp=$$(mktemp); \
	trap 'rm -f "$$tmp"' EXIT; \
	git diff --name-only -z --diff-filter=ACMRT origin/main...HEAD -- \
		'*.md' '*.markdown' '*.mdx' > "$$tmp"; \
	if [ -s "$$tmp" ]; then xargs -0 $(MDLINT) < "$$tmp"; fi
	+$(MAKE) spelling

spelling: spelling-helper-test ## Enforce en-GB-oxendict spelling in Markdown prose
	@uv run scripts/generate_typos_config.py
	@git ls-files -z '*.md' | \
		xargs -0 -r $(TYPOS) --config typos.toml --force-exclude

spelling-helper-test: ## Validate the shared spelling-policy integration
	@PYTHONPATH=scripts uv run --python 3.13 \
		--with pytest==9.0.2 --with pytest-cov==7.0.0 \
		python -m pytest scripts/tests/test_typos_rollout.py \
		--cov=generate_typos_config --cov=typos_rollout \
		--cov=typos_rollout_cache --cov-fail-under=90

nixie: ## Validate Mermaid diagrams
	find . -type f -name '*.md' -not -path './target/*' -print0 | xargs -0 $(NIXIE)

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?##' $(MAKEFILE_LIST) | \
	awk 'BEGIN {FS=":"; printf "Available targets:\n"} {printf "  %-20s %s\n", $$1, $$2}'
