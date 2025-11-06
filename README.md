# Differential datalog linter (ddlint)

This is a generated project using [Copier](https://copier.readthedocs.io/).

## Logging

`ddlint` uses the [`log`](https://docs.rs/log/) crate to emit parser warnings.
Initialize a logger in your `main` function to display these messages. The
[`env_logger`](https://docs.rs/env_logger/) crate is a simple option:

```rust,no_run
fn main() {
    // Cargo.toml: env_logger = "0.11"
    env_logger::init();
    // run ddlint or your integration here
}
```

Control the verbosity with the `RUST_LOG` environment variable. For example,
you can set `RUST_LOG=warn` to display warnings.
