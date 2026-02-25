<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./assets/LOGO-SOTERIA-FULL-WHITE.png">
  <source media="(prefers-color-scheme: light)" srcset="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
  <img alt="Shows a black logo in light color mode and a white one in dark color mode." src="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
</picture>

Soteria is a library for writing efficient symbolic interpreters directly in OCaml.
The core library is just a small toolbox that we use for writing a set of analyses, currently for Rust and C.


[![Zulip Chat](https://img.shields.io/badge/join-zulip?logo=zulip&label=Zulip&labelColor=%2330363D&color=%232FBC4F)](https://soteria.zulipchat.com/)
[![CI](https://github.com/soteria-tools/soteria/actions/workflows/ci.yml/badge.svg)](https://github.com/soteria-tools/soteria/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg?labelColor=%2330363D)](https://opensource.org/licenses/Apache-2.0)

**[Website](https://soteria-tools.com)** | **[API Documentation](https://soteria-tools.github.io/soteria/api/main/soteria/index.html)**

## Table of Contents

- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installing from Source](#installing-from-source)
  - [Building from Source for Development](#building-from-source-for-development)
  - [Installing Rust Frontends](#installing-rust-frontends)
- [Using Soteria as a Library](#using-soteria-as-a-library)
- [Soteria Rust](#soteria-rust)
- [Soteria-C](#soteria-c)
- [Contributing](#contributing)
- [License](#license)

## Getting Started

### Prerequisites

Before using Soteria, ensure you have the following installed:

| Dependency | Version | Installation |
|------------|---------|--------------|
| OCaml | >= 5.4.0 | [ocaml.org/docs/installing-ocaml](https://ocaml.org/docs/installing-ocaml) |
| opam | >= 2.0 | Included with OCaml installation |
| Z3 | latest | [github.com/Z3Prover/z3](https://github.com/Z3Prover/z3) |

For **Soteria Rust**, you will also need:

| Dependency | Version | Installation |
|------------|---------|--------------|
| Rust | nightly | [rustup.rs](https://rustup.rs/) |
| Obol | see below | [Installing Obol](#installing-obol) |
| Charon | see below | [Installing Charon](#installing-charon) |

> **Note:** Both frontends are required to run the full test suite.

### Installing from Source

<details>
<summary>For users who want to install and use Soteria</summary>

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
   Add this line to your shell profile (`.bashrc`, `.zshrc`, etc.) to make it permanent, or run `opam switch soteria-install` to switch to it in your current shell.
</details>

### Building from Source for Development

<details>
<summary>For developers who want to contribute to Soteria:</summary>

1. **Clone the repository:**
   ```sh
   git clone https://github.com/soteria-tools/soteria.git
   cd soteria
   ```

2. **Create a local opam switch and install dependencies:**
   ```sh
   make switch
   ```
   This creates an isolated OCaml environment with all required dependencies.

   Alternatively, if you already have a switch you want to use:
   ```sh
   make ocaml-deps
   ```

3. **Build the project:**
   ```sh
   dune build
   ```

4. **Verify the installation by running the test suite:**
   ```sh
   dune test
   ```

   > **Note:** Running `dune test` requires both Rust frontends (Obol and Charon) to be installed. To run only the core Soteria or Soteria-C tests without Rust frontends:
   > ```sh
   > dune test soteria      # Core library tests only
   > dune test soteria-c    # Soteria-C tests only
   > ```
</details>

### Installing Rust Frontends

<details>
<summary>To use Soteria Rust, you need to install the supported frontends</summary>
To use Soteria Rust, you need a frontend to translate Rust code to an intermediate representation. 

We support two frontends: [Obol](https://github.com/soteria-tools/obol) and [Charon](https://github.com/AeneasVerif/charon).

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
   <!-- [versionsync: OBOL_COMMIT_HASH=bd47db0c3a7092cccac25b9432c9db82a3c18e38] -->
   ```sh
   cd ..
   git clone https://github.com/soteria-tools/obol.git
   cd obol
   git checkout bd47db0c3a7092cccac25b9432c9db82a3c18e38
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
   <!-- [versionsync: CHARON_COMMIT_HASH=79077103ea9d0493bc72315462a1c84fcdf014e0] -->
   ```sh
   cd ..
   git clone https://github.com/soteria-tools/charon.git
   cd charon
   git checkout 79077103ea9d0493bc72315462a1c84fcdf014e0
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
</details>

## Using Soteria as a Library

Soteria can be used as a library to build your own symbolic execution engines. The [API documentation](https://soteria-tools.github.io/soteria/api/main/soteria/index.html) provides a complete reference, and includes a tutorial on how to get started building your own analysis tools.

## Soteria Rust

Soteria Rust is a Kani-like symbolic execution engine for Rust. It is in heavy development.

### Usage

Run on a standalone Rust file, symbolically executing the `main` function:
```sh
soteria-rust exec <file.rs>
```

Run in Kani mode to execute any function with the `#[kani::proof]` attribute:
```sh
soteria-rust exec --kani <file.rs>
```

Run all tests in a crate:
```sh
soteria-rust exec <crate-dir>
```

Use `--help` with any command for a full list of options:
```sh
soteria-rust exec --help
```

### Testing Against External Suites

To test Soteria Rust against external test suites:

```sh
# Test against the Kani test suite
git clone https://github.com/model-checking/kani.git ../kani
soteria-rust/scripts/test.py kani

# Test against the Miri test suite
git clone https://github.com/rust-lang/miri.git ../miri
soteria-rust/scripts/test.py miri
```

### Limitations

Soteria Rust supports a large subset of Rust, but some features are not yet supported:
- Concurrency
- Inline assembly
- SIMD intrinsics

## Soteria-C

Soteria-C is an automatic bug finder for C programs. It is in heavy development.

### Usage

Run on a standalone C file:
```sh
soteria-c exec-main <file.c>
```

Run with a compilation database:
```sh
soteria-c capture-db compile_commands.json
```

Use `--help` for a full list of options:
```sh
soteria-c --help
```

## Contributing

We welcome contributions from the community! Soteria is open source and will remain open source.

Soteria is developed and maintained by [Soteria Tools Ltd](https://soteria-tools.com). The core team makes final decisions on project direction, but we value community input and aim to be transparent about our decision-making process.

- **Chat with us:** Join our [Zulip chat](https://soteria.zulipchat.com/) to ask questions or discuss ideas
- **Submit a PR:** Read our [contribution guidelines](./CONTRIBUTING.md) first
- **License agreement:** Review the [Contributor License Agreement](./CLA.md) before contributing

## License

Soteria and derived tools in this repository are under Apache-2.0 license, copyright Soteria Tools Ltd 2026.

The Soteria logo is a trademark of the Soteria Tools Ltd.
