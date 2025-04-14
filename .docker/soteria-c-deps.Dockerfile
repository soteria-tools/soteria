FROM soteria-deps:latest

USER root

WORKDIR /tmp

RUN curl -fsSL https://deb.nodesource.com/setup_23.x | bash
RUN apt-get install -y nodejs

USER opam

WORKDIR /soteria

COPY --chown=opam:opam bfa-c.opam ./bfa-vscode.opam /soteria/

RUN opam install ./bfa.opam ./bfa-c.opam ./bfa-vscode.opam --with-test --with-doc --deps-only -y

COPY --chown=opam:opam . /soteria/
RUN opam install ./bfa.opam -y