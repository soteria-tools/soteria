#!/bin/bash
set -euo pipefail

# Script to install dependencies for soteria-rust
# Usage: ./install.sh [charon] [obol]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Extract charon hash from soteria-rust.opam
# Looks for: ["charon.~dev" "git+https://github.com/soteria-tools/charon#<hash>"]
extract_charon_hash() {
    grep -o 'charon\.~dev.*charon#[a-f0-9]*' "$PROJECT_ROOT/soteria-rust.opam" | grep -o '[a-f0-9]\{40\}'
}

# Extract obol hash from .github/workflows/ci.yml
# Looks for: OBOL_COMMIT_HASH: <hash>
extract_obol_hash() {
    grep 'OBOL_COMMIT_HASH:' "$PROJECT_ROOT/.github/workflows/ci.yml" | grep -o '[a-f0-9]\{40\}'
}

CHARON_HASH="$(extract_charon_hash)"
OBOL_HASH="$(extract_obol_hash)"

CHARON_REPO="https://github.com/soteria-tools/charon.git"
OBOL_REPO="https://github.com/soteria-tools/obol.git"

echo "Detected charon hash: $CHARON_HASH"
echo "Detected obol hash: $OBOL_HASH"

# Pin charon and name_matcher_parser
pin_opam_deps() {
    echo "Pinning charon and name_matcher_parser..."
    opam pin add -y name_matcher_parser "git+https://github.com/soteria-tools/charon#${CHARON_HASH}"
    opam pin add -y charon "git+https://github.com/soteria-tools/charon#${CHARON_HASH}"
}

install_charon() {
    echo "Installing charon..."
    local charon_dir="$PROJECT_ROOT/../charon"
    
    if [ ! -d "$charon_dir/.git" ]; then
        echo "Cloning charon repository..."
        mkdir -p "$charon_dir"
        cd "$charon_dir"
        git init
        git remote add origin "$CHARON_REPO"
    else
        echo "Charon repository already exists."
        cd "$charon_dir"
    fi
    
    echo "Fetching and checking out $CHARON_HASH..."
    git fetch --depth=1 origin "$CHARON_HASH"
    git checkout "$CHARON_HASH"
    
    echo "Building charon-rust..."
    make build-charon-rust
    
    echo "Charon installed successfully."
}

install_obol() {
    echo "Installing obol..."
    local obol_dir="$PROJECT_ROOT/../obol"
    
    if [ ! -d "$obol_dir/.git" ]; then
        echo "Cloning obol repository..."
        mkdir -p "$obol_dir"
        cd "$obol_dir"
        git init
        git remote add origin "$OBOL_REPO"
    else
        echo "Obol repository already exists."
        cd "$obol_dir"
    fi
    
    echo "Fetching and checking out $OBOL_HASH..."
    git fetch --depth=1 origin "$OBOL_HASH"
    git checkout "$OBOL_HASH"
    
    echo "Building obol..."
    make build
    
    echo "Obol installed successfully."
}

# Main
pin_opam_deps

for arg in "$@"; do
    case "$arg" in
        charon)
            install_charon
            ;;
        obol)
            install_obol
            ;;
        *)
            echo "Unknown argument: $arg"
            echo "Usage: $0 [charon] [obol]"
            exit 1
            ;;
    esac
done
