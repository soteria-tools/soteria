<picture>
  <source media="(prefers-color-scheme: dark)" srcset="https://github.com/user-attachments/assets/d9351dcf-f008-40f8-a667-5f23fd13741a">
  <source media="(prefers-color-scheme: light)" srcset="https://github.com/user-attachments/assets/68071d32-eb40-4fe2-9ca0-d46f41c95271">
  <img alt="Shows a black logo in light color mode and a white one in dark color mode." src="[https://user-images.githubusercontent.com/25423296/163456779-a8556205-d0a5-45e2-ac17-42d089e3c3f8.png](https://github.com/user-attachments/assets/68071d32-eb40-4fe2-9ca0-d46f41c95271)">
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
