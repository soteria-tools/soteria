FROM all-deps:latest

WORKDIR /soteria

COPY --chown=opam:opam . .
RUN opam exec -- dune build @all
RUN opam exec -- dune test
RUN opam exec -- dune build @fmt
