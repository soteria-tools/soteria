# Soteria

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

# bfa-c (working name)

BFA-C is an automatic in-IDE bug finder for C programs. It is in heavy development.

It can currently be tested on standalone files by opening this project in VSCode.
To do so, open the `Run and Debug` tab of the sidebar, select the "Launch (Local)" configuration and click the "play" button.
