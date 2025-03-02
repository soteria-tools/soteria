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
PURPLE='\033[0;35m'
RED='\033[0;31m'
ORANGE='\033[0;33m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
BLUE='\033[0;34m'
RESET='\033[0m'

function rainbow() {
    n=$(($1 % 7))
    if [ $n -eq 0 ]; then
        echo -e "\033[38;5;197m"
    elif [ $n -eq 1 ]; then
        echo -e "\033[38;5;208m"
    elif [ $n -eq 2 ]; then
        echo -e "\033[38;5;220m"
    elif [ $n -eq 3 ]; then
        echo -e "\033[38;5;70m"
    elif [ $n -eq 4 ]; then
        echo -e "\033[38;5;74m"
    elif [ $n -eq 5 ]; then
        echo -e "\033[38;5;33m"
    elif [ $n -eq 6 ]; then
        echo -e "\033[38;5;127m"
    fi
}

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
# -f --filter: filter tests
# -e --exclude: exclude tests
# --no-compile: do not compile the tests
# -v: verbose
CMD="bfa-rust exec-main"
TESTS=$(find $KANI_PATH/tests/kani -name '*.rs' | sort)
while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--filter)
            TESTS=$(echo $TESTS | xargs -n1 | grep $2)
            shift
            shift
            ;;
        -e|--exclude)
            TESTS=$(echo $TESTS | xargs -n1 | grep -v $2)
            shift
            shift
            ;;
      --no-compile)
            CMD="$CMD --no-compile"
            shift
            ;;
      -v)
            CMD="$CMD -v"
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
step=0
for test in $TESTS; do
    test_rel_name=$(realpath --relative-to=$KANI_PATH $test)
    expect_failure=$(echo $test_rel_name | grep "_fail.rs")
    echo -en "$(rainbow step)|${RESET} Running $test_rel_name ..."
    echo "Running $test" >> $LOG_FILE
    start=$(($(date +%s%N)/1000000))
    # redirect both stdout and stderr to the log file
    dune exec --no-build -- $CMD $test >> $LOG_FILE 2>>$LOG_FILE
    result=$?
    end=$(($(date +%s%N)/1000000))

    if [ $result -eq 0 ] && [ -z $expect_failure ]; then
        echo -e " ${GREEN}passed${RESET} in $((end-start))ms"
    elif [ $result -eq 1 ] && [ -n $expect_failure ]; then
        echo -e " ${GREEN}failed (expected)${RESET} in $((end-start))ms"
    elif [ $result -eq 0 ] && [ -n $expect_failure ]; then
        echo -e " ${RED}passed (expected failure)${RESET}"
        exit 1
    elif [ $result -eq 1 ] && [ -z $expect_failure ]; then
        echo -e " ${RED}failed${RESET}"
        exit 1
    else
        echo -e " ${PURPLE}crashed: status code $result${RESET}"
        exit 1
    fi

    echo -e "\n" >> $LOG_FILE
    step=$((step+1))
done
