<div align="center">
    <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./assets/LOGO-SOTERIA-FULL-WHITE.png">
    <source media="(prefers-color-scheme: light)" srcset="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
    <img alt="Shows a black logo in light color mode and a white one in dark color mode." src="./assets/LOGO-SOTERIA-FULL-COLOUR.png"
        style="width: 335px; height: 64.5; margin-bottom: 1.5rem;">
    </picture>
</div>

<br />

<div align="center">
    <a href="https://soteria.zulipchat.com/"><img src="https://img.shields.io/badge/join-zulip?logo=zulip&label=Zulip&labelColor=%2330363D&color=%232FBC4F" alt="Zulip Chat"></a>
    <a href="https://github.com/soteria-tools/soteria/actions/workflows/ci.yml"><img src="https://github.com/soteria-tools/soteria/actions/workflows/ci.yml/badge.svg" alt="CI"></a>
    <a href="https://soteria-tools.github.io/soteria/dev/bench/"><img src="https://img.shields.io/badge/benchmark-results-blue?label=Benchmarks&labelColor=%2330363D" alt="Benchmarks"></a>
    <a href="https://opensource.org/licenses/Apache-2.0"><img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg?labelColor=%2330363D" alt="License"></a>
</div>

<div align="center">
    <strong><a href="https://soteria-tools.com">Website</a></strong> | <strong><a href="https://soteria-tools.com/docs/library">API Documentation</a></strong>
</div>

<br />

Soteria is an OCaml library for writing efficient symbolic interpreters directly in OCaml.

The core library provides a set of batteries-included abstractions for writing one's own symbolic interpreter. Currently, two symbolic interpreters have been written: Soteria Rust and Soteria C. Our current main focus is Soteria Rust.

## Table of Contents

- [Getting Started](#getting-started)
- [Soteria Rust](#soteria-rust)
- [Soteria C](#soteria-c)
- [Contributing](#contributing)
- [Acknowledgements](#acknowledgements)
- [License](#license)

## Getting Started

### Install Soteria Rust

For users wishing to user Soteria Rust for writing and running symbolic tests,
we strongly recommend installing Soteria Rust through `cargo` on **macOS with M-series chips** and **Linux x86_64**:

```sh
cargo install soteria
cargo soteria setup # Installs pre-built binaries
# setup can be run again at every nightly release of Soteria Rust (every day).
```

For other architectures, please follow the [manual installation instructions](./CONTRIBUTING.md#installing-from-source) in the contributing guide.

### Install the Soteria Library

Soteria can be used as an OCaml library to build your own symbolic execution engines.
The [API documentation](https://soteria-tools.com/docs/library) provides a complete reference,
and includes a tutorial on how to get started building your own analysis tools.

Pin it with opam:

```sh
opam pin add soteria git+https://github.com/soteria-tools/soteria.git#<commit>
```

## Soteria Rust

Soteria Rust is a symbolic execution engine for Rust. It is in heavy development.

### Usage when installed from cargo

Using `cargo soteria`, the tests inside of the current crate can be run using:
```sh
cargo soteria
```

If the crate contains Kani harnesses, the Kani compatibility layer can be enabled
with:
```sh
cargo soteria --kani
```

The full help for `cargo soteria` can be obtained with:
```sh
cargo soteria --help
```

### Usage when installed from source

Installing from source install a `soteria-rust` binary, which can be used as follows.

Run on a standalone Rust file, symbolically executing the `main` function, or any
function with the `#[soteria::test]` attribute:
```sh
soteria-rust exec <file.rs>
```

Run all tests in a crate:
```sh
soteria-rust exec <crate-dir>
```

Run in Kani mode to execute any function with the `#[kani::proof]` attribute,
with compatibility with the Kani api (e.g. `kani::any`):
```sh
soteria-rust exec --kani <file.rs>
```

Use `--help` with any command for a full list of options:
```sh
soteria-rust exec --help
```

### Limitations

Soteria Rust supports a large subset of Rust, but some features are not yet supported:
- Concurrency
- Inline assembly
- SIMD intrinsics

## Soteria C

Soteria C is an automatic bug finder for C programs. It is in heavy development.

### Installatation

Binaries for Linux x86 and MacOS Arm are available in the latest release on this GitHub repository.
For other architectures, please follow the [manual installation instructions](./CONTRIBUTING.md#installing-from-source).

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

### AI Policy

We welcome contributions assisted by AI, as long as they follow our [AI Policy](./AI_POLICY.md).

## Acknowledgements

Soteria relies on many excellent open source libraries and tools, and we are deeply grateful to their authors and contributors.

We would especially like to thank:
- The [Charon](https://github.com/AeneasVerif/charon) team for providing the MIR frontend that powers Soteria Rust
- The [Cerberus](https://github.com/rems-project/cerberus) team for providing the C frontend that powers Soteria C

## License

Soteria and derived tools in this repository are under Apache-2.0 license, copyright Soteria Tools Ltd 2026.

The Soteria logo is a trademark of the Soteria Tools Ltd.
