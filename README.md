# Differential Datalog Linter (ddlint)

This is a generated project using [Copier](https://copier.readthedocs.io/).

## Logging

`ddlint` uses the [`log`](https://docs.rs/log/) crate to emit parser warnings.
Initialise a logger in your `main` function to display these messages. The
[`env_logger`](https://docs.rs/env_logger/) crate is a simple option:

```rust
fn main() {
    env_logger::init();
    // run ddlint or your integration here
}
```

Control the verbosity with the `RUST_LOG` environment variable. For example,
`RUST_LOG=warn` will show warnings.
