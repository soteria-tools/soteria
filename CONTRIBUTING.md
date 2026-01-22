# Contributing to Soteria

Thank you for your interest in contributing to Soteria! This document provides guidelines and instructions for contributing to the project.

Soteria is developed and maintained by [Soteria Tools Ltd](https://soteria-tools.com). The core team makes final decisions on project direction, but we value community input and aim to be transparent about our decision-making process.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Code Style and Formatting](#code-style-and-formatting)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Coding Guidelines](#coding-guidelines)
- [Contributor License Agreement](#contributor-license-agreement)

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```sh
   git clone https://github.com/<your-username>/soteria.git
   cd soteria
   ```
3. **Set up the development environment** (see [Development Setup](#development-setup))
4. **Create a branch** for your changes:
   ```sh
   git checkout -b my-feature-branch
   ```

## Development Setup

### Prerequisites

- OCaml >= 5.4.0 with opam
- Z3 (SMT solver)
- For Soteria Rust: Rust nightly and Obol or Charon (see [README](./README.md#installing-rust-frontends))

### Setting Up the Environment

Create a local opam switch with all development dependencies:

```sh
make switch
```

This installs:
- All project dependencies
- `ocamlformat` (code formatter)
- `ocaml-lsp-server` (IDE support)
- `odig` (documentation tools)

If you already have a switch, install dependencies with:

```sh
make ocaml-deps
```

### Building

```sh
dune build         # Build the project
dune test          # Run all tests (requires both Rust frontends)
dune build @doc    # Build documentation
```

To run tests without Rust frontends installed:

```sh
dune test soteria      # Core library tests only
dune test soteria-c    # Soteria-C tests only
```

## Code Style and Formatting

### OCaml Formatting

We use **ocamlformat** to ensure consistent code style, please make sure you have the correct version installed (see `.ocamlformat`).

### Formatting Your Code

Before committing, format your code:

```sh
dune fmt
```

To check if your code is properly formatted (without modifying files):

```sh
dune build @fmt
```

The CI will reject PRs with improperly formatted code.

## Testing

### Running Tests

To run all tests (requires both Rust frontends to be installed):

```sh
dune test
```

To run tests for specific packages without Rust frontends:

```sh
dune test soteria      # Core library tests only
dune test soteria-c    # Soteria-C tests only
```

### Writing Tests

- Place tests alongside the code they test or in a dedicated `test/` directory
- Use [Alcotest](https://github.com/mirage/alcotest) for unit test
- Use [cram testing](https://dune.readthedocs.io/en/stable/reference/cram.html) for end-to-end tests.
- Ensure new features have corresponding tests
- Ensure bug fixes include a regression test

### Testing Soteria Rust

For Soteria Rust specific testing:

```sh
# Run against the Kani test suite
soteria-rust/scripts/test.py kani

# Run against the Miri test suite
soteria-rust/scripts/test.py miri
```

## Submitting Changes

### Before Submitting

1. **Format your code**: Run `dune fmt`
2. **Run tests**: Ensure all tests pass with `dune test` (or `dune test soteria soteria-c` if you don't have Rust frontends)
3. **Check formatting**: Run `dune build @fmt`

### Pull Request Process

1. Push your branch to your fork
2. Open a Pull Request against `main`
3. Fill out the PR description explaining your changes. Explain *what* and *why*, not just *how*.
4. Wait for CI to pass
5. Address any review feedback

## Coding Guidelines

### Exception Handling

The library and executables should **never** raise uncaught exceptions unless they correspond to an internal error that should be fixed in the tool.

**Acceptable exceptions:**
- Patterns that are believed to be unreachable (assertions for invariants)
- Internal errors that indicate bugs in Soteria itself

**Unacceptable exceptions:**
- User input errors (use `Result` types instead)
- Expected edge cases in analyzed code

### Error Handling Best Practices

- Use `Result` types for operations that can fail due to user input
- Use `Option` for values that may or may not be present
- Reserve exceptions for truly exceptional circumstances (bugs)

## Contributor License Agreement

Before we can accept your contribution, you must agree to our [Contributor License Agreement](./CLA.md).

By opening a pull request, you confirm that:
- You have read and accepted the terms of the CLA
- You have the right to submit the contribution
- Your contribution is compatible with the Apache-2.0 license

This ensures that we can safely incorporate your contribution into the project.

## Questions?

- Visit our [website](https://soteria-tools.com) for more information
- Join our [Zulip chat](https://soteria.zulipchat.com/) for questions and discussions
- Open an issue on GitHub for bug reports or feature requests
