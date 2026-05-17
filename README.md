<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./assets/LOGO-SOTERIA-FULL-WHITE.png">
  <source media="(prefers-color-scheme: light)" srcset="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
  <img alt="Shows a black logo in light color mode and a white one in dark color mode." src="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
</picture>

Soteria is an OCaml library for writing efficient symbolic interpreters directly in OCaml.
The core library provides a set of batteries-included abstractions for writing one's own symbolic interpreter. Currently, two symbolic interpreters have been written: Soteria-Rust and Soteria-C. Our current main focus is Soteria-Rust.


[![Zulip Chat](https://img.shields.io/badge/join-zulip?logo=zulip&label=Zulip&labelColor=%2330363D&color=%232FBC4F)](https://soteria.zulipchat.com/)
[![CI](https://github.com/soteria-tools/soteria/actions/workflows/ci.yml/badge.svg)](https://github.com/soteria-tools/soteria/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg?labelColor=%2330363D)](https://opensource.org/licenses/Apache-2.0)

**[Website](https://soteria-tools.com)** | **[API Documentation](https://soteria-tools.github.io/soteria/api/main/soteria/index.html)**

## Table of Contents

- [Getting Started](#getting-started)
- [Soteria Rust](#soteria-rust)
- [Soteria-C](#soteria-c)
- [Contributing](#contributing)
- [Acknowledgements](#acknowledgements)
- [License](#license)

## Getting Started

### Install the Soteria Library

Soteria can be used as a library to build your own symbolic execution engines. The [API documentation](https://soteria-tools.github.io/soteria/api/main/soteria/index.html) provides a complete reference, and includes a tutorial on how to get started building your own analysis tools.

Pin it with opam:

```sh
opam pin add soteria git+https://github.com/soteria-tools/soteria.git#<commit>
```

### Install Soteria-Rust

Soteria-Rust can be installed with `cargo` on **macOS with M-series chips** and **Linux x86_64**:

```sh
cargo install soteria --git https://github.com/soteria-tools/cargo-soteria.git
```

For other architectures, please follow the [manual installation instructions](./CONTRIBUTING.md#installing-from-source) in the contributing guide.

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

## Acknowledgements

Soteria relies on many excellent open source libraries and tools, and we are deeply grateful to their authors and contributors.

We would especially like to thank:
- The [Charon](https://github.com/AeneasVerif/charon) team for providing the MIR frontend that powers Soteria-Rust
- The [Cerberus](https://github.com/rems-project/cerberus) team for providing the C frontend that powers Soteria-C

## License

Soteria and derived tools in this repository are under Apache-2.0 license, copyright Soteria Tools Ltd 2026.

The Soteria logo is a trademark of the Soteria Tools Ltd.
