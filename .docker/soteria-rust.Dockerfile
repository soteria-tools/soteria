FROM soteria-rust-deps:latest

USER opam

WORKDIR /soteria

RUN opam exec -- dune build -p bfa-rust
RUN opam exec -- dune test bfa_rust
RUN opam install ./bfa-rust.opam -y