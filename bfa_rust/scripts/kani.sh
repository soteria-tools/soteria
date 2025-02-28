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

# Silence warnings
export RUSTFLAGS="-Awarnings"

# Handle arguments:
# -f: filter tests
FILTER=""
while [[ $# -gt 0 ]]; do
  case $1 in
      -f|--filter)
            FILTER=$2
            shift
            shift
            ;;
        -*|--*)
            echo "Unknown option $1"
            exit 1
            ;;
    esac
done

# If on MacOS, use coreutils:
shopt -s expand_aliases
alias date="gdate"
alias realpath="grealpath"

# Run all tests
for test in $(find $KANI_PATH/tests/kani -name '*.rs' | grep $FILTER | sort); do
    test_rel_name=$(realpath --relative-to=$KANI_PATH $test)
    expect_failure=$(echo $test_rel_name | grep "_fail.rs")
    echo -e "${CYAN}⭑${RESET} Running $test_rel_name ..."
    echo "Running $test" >> $LOG_FILE
    start=$(($(date +%s%N)/1000000))
    # redirect both stdout and stderr to the log file
    dune exec --no-build -- bfa-rust exec-main $test >> $LOG_FILE 2>>$LOG_FILE
    result=$?
    end=$(($(date +%s%N)/1000000))

    if [ $result -eq 0 ] && [ -z $expect_failure ]; then
        echo -e "  ${GREEN}Test passed${RESET} in $((end-start))ms"
    elif [ $result -eq 1 ] && [ -n $expect_failure ]; then
        echo -e "  ${GREEN}Test failed (expected)${RESET} in $((end-start))ms"
    elif [ $result -eq 0 ] && [ -n $expect_failure ]; then
        echo -e "  ${RED}Test passed (expected failure)${RESET}"
        exit 1
    else
        echo -e "  ${RED}Test failed${RESET}"
        exit 1
    fi

    echo -e "\n" >> $LOG_FILE
done
