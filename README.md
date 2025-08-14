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

# Rusteria (soteria-rust)

Soteria-Rust is a Kani-like symbolic execution engine for Rust. It is in heavy development.

It can run standalone files, symbolically executing the `main` function, or any function with the attribute `#[kani::proof]`. You must have [Charon](https://github.com/AeneasVerif/charon) built and on your path -- to do so, clone Charon and run `make build-dev-charon-rust` and add `charon/bin` to your path. Once this is done, simply run:
```sh
soteria-rust exec-main <file>
```

To test Rusteria on the Kani test suite, clone [Kani](https://github.com/model-checking/kani) next to `soteria`, and run `soteria-rust/scripts/kani.sh`. Run `soteria-rust/scripts/kani.sh --help` to see all available arguments.

You can also test Rusteria on the Miri test suite: clone [Miri](https://github.com/rust-lang/miri) next to `soteria` and run `soteria-rust/scripts/miri.sh`.

# License

Soteria and derived tools in this repository are under Apache-2.0 license, copyright Soteria Team 2025.

The Soteria logo is a trademark of the Soteria Team.