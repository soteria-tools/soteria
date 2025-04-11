# Use a minimal Alpine-based OCaml image
FROM ocaml/opam:debian-ocaml-5.3-flambda

# Set working directory
WORKDIR /app

# Install only necessary system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
  m4 \
  libgmp-dev \
  pkg-config

# Switch to the default opam user
USER opam

# Install Z3 solver from GitHub release
RUN sudo apt-get install -y wget unzip && \
  wget https://github.com/Z3Prover/z3/releases/download/z3-4.14.1/z3-4.14.1-arm64-glibc-2.34.zip && \
  unzip z3-4.14.1-arm64-glibc-2.34.zip -d z3 && \
  sudo cp z3/z3-4.14.1-arm64-glibc-2.34/bin/z3 /usr/bin/ && \
  sudo chmod +x /usr/bin/z3 && \
  rm -rf z3 z3-4.14.1-arm64-glibc-2.34.zip

# Initialize opam environment and install dependencies
RUN opam update

# Copy only the opam file to install all dependencies (better for caching)
COPY --chown=opam:opam bfa.opam bfa-c.opam /app/
RUN opam install ./bfa.opam ./bfa-c.opam --deps-only -y

# Copy the rest of the project files
COPY --chown=opam:opam . /app


RUN opam install ./bfa.opam ./bfa-c.opam -y

# Set the default command
CMD ["opam", "exec", "--", "bfa-c", "lsp"]

