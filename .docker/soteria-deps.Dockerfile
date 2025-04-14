# Use a minimal Alpine-based OCaml image
FROM ocaml/opam:debian-12-ocaml-5.3-flambda

# Set working directory
WORKDIR /soteria

# Install only necessary system dependencies
RUN sudo apt-get update && \
  sudo apt-get install -y --no-install-recommends \
  libgmp-dev \
  m4 \
  pkg-config \
  wget \
  unzip \
  ca-certificates && \
  sudo rm -rf /var/lib/apt/lists/*

# Switch to the default opam user
USER opam

# Install Z3 solver from GitHub release
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-4.14.1/z3-4.14.1-arm64-glibc-2.34.zip && \
  unzip z3-4.14.1-arm64-glibc-2.34.zip -d z3 && \
  sudo cp z3/z3-4.14.1-arm64-glibc-2.34/bin/z3 /usr/bin/ && \
  sudo chmod +x /usr/bin/z3 && \
  rm -rf z3 z3-4.14.1-arm64-glibc-2.34.zip

# Copy only the opam file to install all dependencies (better for caching)
COPY --chown=opam:opam bfa.opam /soteria/
RUN opam install ./bfa.opam --with-test --with-doc --deps-only -y && \
  opam clean -a -c -s --logs
