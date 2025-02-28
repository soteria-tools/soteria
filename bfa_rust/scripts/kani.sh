#!/usr/bin/env bash
# Script to run all Kani tests using BFA Rust.
#
# Requirement: the https://github.com/model-checking/kani repository must be cloned
#              as a sibling directory to the bfa-ocaml repository.
#
# Behaviour: runs tests, one by one. Stops on the first failure.
#
# Usage: ./kani.sh
#

# Tests are in kani/tests/<category...>/<test>.rs
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
KANI_LIB_PATH=$(realpath $SCRIPT_DIR/../kani_lib)
KANI_PATH=$(realpath $SCRIPT_DIR/../../../kani)

touch "$SCRIPT_DIR/kani.log"
LOG_FILE=$(realpath $SCRIPT_DIR/kani.log)

# Formatting =^^=
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
RESET='\033[0m'

# Clean log file
echo -n > $LOG_FILE

# Build Rusteria
eval $(opam env)
dune build

# Build our Kani library
export KANI_LIB_PATH=$KANI_LIB_PATH
(cd $KANI_LIB_PATH/kani && charon --only-cargo --lib --input ./src/)

# Run all tests
for test in $(find $KANI_PATH/tests/kani -name '*.rs' | sort); do
    echo -e "${CYAN}-${RESET} Running $test ..."
    start=$(date +%s)
    dune exec --no-build -- bfa-rust exec-main $test 2>&1 >> $LOG_FILE
    if [ $? -eq 0 ]; then
        end=$(date +%s)
        echo -e "  ${GREEN}Test passed in $((end-start)) seconds${RESET}"
    else
        echo -e "  ${RED}Test failed${RESET}"
        exit 1
    fi
done
