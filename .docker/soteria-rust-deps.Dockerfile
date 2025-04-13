FROM soteria-deps:latest

USER opam

RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/home/opam/.cargo/bin:${PATH}"

WORKDIR /charon
RUN git init && \
  git remote add origin https://github.com/N1ark/charon.git && \
  git fetch --depth=1 origin 24cce6151f166ec540ef3daa24112d0e3ace4aad && \
  git checkout 24cce6151f166ec540ef3daa24112d0e3ace4aad

RUN make build-dev-charon-rust
ENV PATH="/charon/bin:${PATH}"

WORKDIR /soteria

COPY --chown=opam:opam ./bfa-rust.opam /soteria/
RUN opam install ./bfa.opam ./bfa-rust.opam --with-test --with-doc --deps-only -y

COPY --chown=opam:opam . /soteria/
RUN opam install ./bfa.opam -y