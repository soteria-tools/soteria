# Contributing to Soteria

Thank you for your interest in contributing to Soteria! This document provides guidelines and instructions for contributing to the project.

Soteria is developed and maintained by [Soteria Tools Ltd](https://soteria-tools.com). The core team makes final decisions on project direction, but we value community input and aim to be transparent about our decision-making process.

## Table of Contents

- [Getting Started](#getting-started)
- [Installing from Source](#installing-from-source)
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
3. **Set up the development environment** (see [Installing from Source](#installing-from-source))
4. **Create a branch** for your changes:
   ```sh
   git checkout -b my-feature-branch
   ```

## Installing from Source

### Prerequisites

<!-- [versionsync: OCAML_VERSION=5.4.1] -->
| Dependency | Version | Notes |
|------------|---------|-------|
| OCaml | >= 5.4.1 | [ocaml.org/docs/installing-ocaml](https://ocaml.org/docs/installing-ocaml) |
| opam | >= 2.0 | Included with OCaml installation |
| Z3 | latest | [github.com/Z3Prover/z3](https://github.com/Z3Prover/z3) |
| Rustup | latest | Required for Soteria Rust; [rustup.rs](https://rustup.rs/) |
| Obol / Charon | see below | Required for Soteria Rust; [Installing Rust Frontends](#installing-rust-frontends) |

### Installing for Use

1. **Clone the repository:**
   ```sh
   git clone https://github.com/soteria-tools/soteria.git
   cd soteria
   ```

2. **Create a global opam switch and install:**
   ```sh
   make glob-switch && make install
   ```
   This creates a global opam switch called `soteria-install`, switches to it, and installs Soteria.

3. **Activate the switch:**
   ```sh
   eval $(opam env --switch=soteria-install)
   ```
   Add this line to your shell profile (`.bashrc`, `.zshrc`, etc.) to make it permanent.

### Installing for Development

1. **Create a local opam switch with all development dependencies:**

   ```sh
   make switch
   ```

   This installs all project dependencies, `ocamlformat`, `ocaml-lsp-server`, and `odig`. If you already have a switch you want to use:

   ```sh
   make ocaml-deps
   ```

2. **Build the project:**
   ```sh
   dune build
   ```

3. **Run tests:**
   ```sh
   dune test              # All tests (requires Rust frontends)
   dune test soteria      # Core library tests only
   dune test soteria-c    # Soteria C tests only
   dune build @doc        # Build documentation
   ```

### Installing Rust Frontends

To use Soteria Rust, you need a frontend to translate Rust code to an intermediate representation. We support two frontends: [Obol](https://github.com/soteria-tools/obol) and [Charon](https://github.com/AeneasVerif/charon).

> **Quick Setup:** Use the versionsync script to automatically install both frontends:
> ```sh
> ./scripts/versionsync.py pull all --init
> ```
> This will clone and build both Obol and Charon at the correct commits.

#### Installing Obol

[Obol](https://github.com/soteria-tools/obol) is the default frontend (recommended for most use cases).

**Using the versionsync script (recommended):**
```sh
./scripts/versionsync.py pull obol --init
```

**Manual installation:**
1. **Clone Obol at the correct commit:**
   <!-- [versionsync: OBOL_COMMIT_HASH=70610866c606acc9b4c3a460ac586d81459d1304] -->
   ```sh
   cd ..
   git clone https://github.com/soteria-tools/obol.git
   cd obol
   git checkout 70610866c606acc9b4c3a460ac586d81459d1304
   ```
   > **Note:** The required commit hash can always be found in [`scripts/versions.json`](scripts/versions.json) under `OBOL_COMMIT_HASH`.

2. **Build Obol:**
   ```sh
   make build
   ```

3. **Add Obol to your PATH:**
   ```sh
   export PATH="$PATH:$(pwd)/bin"
   ```
   Add this line to your shell profile (`.bashrc`, `.zshrc`, etc.) to make it permanent.

#### Installing Charon

[Charon](https://github.com/AeneasVerif/charon) is an alternative frontend. To use it, pass `--frontend charon` to `soteria-rust`.

**Using the versionsync script (recommended):**
```sh
./scripts/versionsync.py pull charon --init
```

**Manual installation:**
1. **Clone Charon at the correct commit:**
   <!-- [versionsync: CHARON_COMMIT_HASH=1ec8d4bb0116abf4486bac234ac01acf84e2a83a] -->
   ```sh
   cd ..
   git clone https://github.com/soteria-tools/charon.git
   cd charon
   git checkout 1ec8d4bb0116abf4486bac234ac01acf84e2a83a
   ```
   > **Note:** The required commit hash can always be found in [`scripts/versions.json`](scripts/versions.json) under `CHARON_COMMIT_HASH`.

2. **Build Charon:**
   ```sh
   make build-charon-rust
   ```

3. **Add Charon to your PATH:**
   ```sh
   export PATH="$PATH:$(pwd)/bin"
   ```
   Add this line to your shell profile (`.bashrc`, `.zshrc`, etc.) to make it permanent.

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
dune test soteria-c    # Soteria C tests only
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

To test against external suites, clone them first:

```sh
# Kani test suite
git clone https://github.com/model-checking/kani.git ../kani
soteria-rust/scripts/test.py kani

# Miri test suite
git clone https://github.com/rust-lang/miri.git ../miri
soteria-rust/scripts/test.py miri
```

## Upgrading versions

If you want to upgrade a version, say of OCaml, see `./scripts/versionsync.py list` to see current versions and `./scripts/versionsync.py set OCAML_VERSION X.Y.Z` to update it accross all its references in the repo.

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

### Naming conventions


**Identifiers**: We use OCaml standard naming practices. Every identifier should be in `snake_case`, except modules and constructor names that are snake case but start with a capital letter, such as `Module_name`.

**Types**: In OCaml, types live in a different namespace from other identifiers. Therefore, we avoid `name_t` for a type, and use `name`. For instance, for the pointer type, avoid `sptr_t` and use `sptr` instead.

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
