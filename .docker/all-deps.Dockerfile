# Use a minimal Alpine-based OCaml image
FROM ocaml/opam:debian-12-ocaml-5.3-flambda

WORKDIR /tmp

# Setup Node.js repository and install apt dependencies
RUN (curl -fsSL https://deb.nodesource.com/setup_23.x | sudo bash) && \
  sudo apt-get update && \
  sudo apt-get install -y --no-install-recommends \
  libgmp-dev \
  m4 \
  pkg-config \
  unzip \
  ca-certificates \
  nodejs && \
  sudo rm -rf /var/lib/apt/lists/*

# Switch to the default opam user
USER opam

# Install Z3 solver from GitHub release
RUN curl -L https://github.com/Z3Prover/z3/releases/download/z3-4.14.1/z3-4.14.1-x64-glibc-2.35.zip -o z3-4.14.1-x64-glibc-2.35.zip && \
  unzip z3-4.14.1-x64-glibc-2.35.zip -d z3 && \
  sudo cp z3/z3-4.14.1-x64-glibc-2.35/bin/z3 /usr/bin/ && \
  sudo chmod +x /usr/bin/z3 && \
  rm -rf z3 z3-4.14.1-x64-glibc-2.35.zip

# Install yq to read yaml files
RUN curl -L https://github.com/mikefarah/yq/releases/download/v4.45.1/yq_linux_amd64 -o yq && \
  sudo cp yq /usr/bin/yq && \
  sudo chmod +x /usr/bin/yq && \
  rm -f yq && \
  yq --version

# Install the specific Rust version required for Charon
WORKDIR /charon
RUN git init && \
  git remote add origin https://github.com/N1ark/charon.git && \
  git fetch --depth=1 origin 24cce6151f166ec540ef3daa24112d0e3ace4aad && \
  git checkout 24cce6151f166ec540ef3daa24112d0e3ace4aad

RUN (curl https://sh.rustup.rs -sSf | \
  bash -s -- -y \
  --default-toolchain "$(yq '.toolchain.channel' rust-toolchain -p toml -oy)" \
  --component "$(yq '.toolchain.components | join(",")' rust-toolchain -p toml -oy)" \
  --profile minimal \
  --no-update-default-toolchain)


ENV PATH="/home/opam/.cargo/bin:${PATH}"
RUN rustup --version

# Build Charon
RUN make build-dev-charon-rust && \
  sudo mv /charon/bin/* /usr/bin/ && \
  sudo chmod +x /usr/bin/charon && \
  cd /charon/charon && \
  cargo clean -Z gc && \
  cd / \
  && sudo rm -rf /charon

WORKDIR /soteria

RUN charon --help

# Copy only the opam file to install all dependencies (better for caching)
COPY --chown=opam:opam bfa.opam bfa-c.opam bfa-rust.opam bfa-vscode.opam /soteria/
RUN opam install ./bfa.opam ./bfa-c.opam ./bfa-rust.opam ./bfa-vscode.opam --with-test --with-doc --deps-only -y && \
  opam clean -a -c -s --logs




