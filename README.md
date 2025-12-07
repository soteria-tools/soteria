<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./assets/LOGO-SOTERIA-FULL-WHITE.png">
  <source media="(prefers-color-scheme: light)" srcset="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
  <img alt="Shows a black logo in light color mode and a white one in dark color mode." src="./assets/LOGO-SOTERIA-FULL-COLOUR.png">
</picture>

Soteria is a library for writing efficient symbolic interpreters directly in OCaml.
The core library is just a small toolbox that we use for writing a set of analyses, currently for C and Rust.

# Build instructions

To build the current repository, you first need to [install OCaml](https://ocaml.org/docs/installing-ocaml).
We then advise to create a local switch for development, and build the project:
```sh
opam switch create . --deps-only -y
dune build @all
```

If you want to run the build version of the code, you must also [install Z3](https://github.com/Z3Prover/z3).

You can make sure that everything is working properly by running the test suite:
```sh
dune test
```

# Soteria-c (working name)

Soteria-C is an automatic in-IDE bug finder for C programs. It is in heavy development.

It can currently be tested on standalone files by opening this project in VSCode.
To do so, open the `Run and Debug` tab of the sidebar, select the "Launch (Local)" configuration and click the "play" button.

# Soteria Rust

Soteria-Rust is a Kani-like symbolic execution engine for Rust. It is in heavy development.

It can run standalone files, symbolically executing the `main` function, or any function with the attribute `#[kani::proof]`, if in Kani mode (`--kani`).
```sh
soteria-rust rustc <file>
```

It can also run all tests in a crate:
```sh
soteria-rust cargo <crate dir>
```

You may add `--help` to either of these commands to see all available options.

## Frontend

To use Soteria Rust you must have a frontend installed; we support [Obol](https://github.com/soteria-tools/obol) (recommended) and [Charon](https://github.com/AeneasVerif/charon).

To use [Obol](https://github.com/soteria-tools/obol), **you must add the `--obol` flag to `soteria-rust` commands**, and have the `obol` command on your path. To do so:
1. clone Obol
2. run `make build`
3. add `obol/bin` to your path (e.g. `export PATH=$PATH:/path/to/obol/bin`)


To use [Charon](https://github.com/AeneasVerif/charon), you must have the `charon` command on your path. To do so:
1. clone Charon
2. run `make build-charon-rust`
3. add `charon/bin` to your path (e.g. `export PATH=$PATH:/path/to/charon/bin`)

## Testing

To test Soteria Rust on the Kani test suite, clone [Kani](https://github.com/model-checking/kani) next to `soteria`, and run `soteria-rust/scripts/test.py kani`.

You can also test Soteria Rust on the Miri test suite: clone [Miri](https://github.com/rust-lang/miri) next to `soteria` and run `soteria-rust/scripts/test.py miri`.

## Limitations

Soteria-rust supports a large subset of Rust, but is still in development. Some currently unsupported features include:
- Concurrency
- Inline assembly
- SIMD intrinsics
- Trait objects (`dyn Trait`)

Currently, and unlike Soteria-C, Soteria-Rust has neither IDE integration or compositionality support (all tests must instead start from an entry point). We are actively working on the latter!

# License

Soteria and derived tools in this repository are under Apache-2.0 license, copyright Soteria Team 2025.

The Soteria logo is a trademark of the Soteria Tools Ltd.

# Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md), and make sure you have read the [Contributor License Agreement](./CLA.md)
