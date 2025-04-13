FROM soteria-c-deps:latest

USER opam
WORKDIR /soteria

RUN opam exec -- dune build -p bfa-c
RUN opam exec -- dune test bfa_c
RUN opam install ./bfa-c.opam -y
