#!/usr/bin/env bash
# Script to run all Kani tests using Soteria Rust.
#
# Requirement: the https://github.com/model-checking/kani repository must be cloned
#              as a sibling directory to the soteria repository.
#
# Behaviour: runs tests, one by one. Stops on the first failure.
#
# Usage: ./kani.sh
#

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
source $SCRIPT_DIR/common.sh

# Tests are in kani/tests/<category...>/<test>.rs
KANI_PATH=$(realpath $SCRIPT_DIR/../../../kani)

# Handle arguments:
CMD="soteria-rust exec-main --ignore-leaks --kani"
TAG=""
STOP_ON_FAIL=true
STORE_PASSES=false
TESTS=$(find $KANI_PATH/tests/kani -name '*.rs' | sort)
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--directory)
            TESTS=$(find $KANI_PATH/tests/$2 -name '*.rs' | sort)
            shift
            shift
            ;;
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
        --clean)
            CMD="$CMD --clean"
            shift
            ;;
        --smt)
            CMD="$CMD --dump-smt $SMT_FILE"
            shift
            ;;
        --ok)
            STOP_ON_FAIL=false
            shift
            ;;
        --store-passes)
            STORE_PASSES=true
            shift
            ;;
        --html)
            CMD="$CMD --html"
            shift
            ;;
        --tag)
            TAG="-$2"
            shift
            shift
            ;;
        -v)
            CMD="$CMD -v"
            shift
            ;;
        --help)
            echo -e "${BOLD}Options:$RESET"
            echo -e "  $CYAN-d, --directory <path>$RESET     Specify the subdirectory to search for tests, default is kani"
            echo -e "  $CYAN-f, --filter <pattern>$RESET     Filter by a given pattern"
            echo -e "  $CYAN-e, --exclude <pattern>$RESET    Exclude by a given pattern"
            echo -e "  $CYAN--no-compile$RESET               Do not re-compile files"
            echo -e "  $CYAN--clean$RESET                    Clean the build directory"
            echo -e "  $CYAN--smt$RESET                      Dump SMT queries to a file"
            echo -e "  $CYAN--store-passes$RESET             Store passed tests in a file"
            echo -e "  $CYAN--ok$RESET                       Ignore errors"
            echo -e "  $CYAN-v$RESET                         Verbose logging"
            exit 1
            ;;
      -*|--*)
            echo "Unknown option $1"
            exit 1
            ;;
      *)
            echo "Unhandled arg $1"
            exit 1
            ;;
    esac
done

# Clean log file
touch "$SCRIPT_DIR/kani$TAG.log"
LOG_FILE=$SCRIPT_DIR/kani$TAG.log
echo -n > $LOG_FILE
if $STORE_PASSES; then
    echo -n > $PASS_FILE
fi

# Run all tests
build_rusteria
step=0
passed=0
failed=0
script_start=$(($(date +%s%N)/1000000))
for test in $TESTS; do
    test_rel_name=$(realpath --relative-to=$KANI_PATH $test)
    expect_failure=$(grep -c "kani-verify-fail" "$test")
    if (( $expect_failure > 1 )); then
        expect_failure=1
    fi

    echo -en "$(rainbow step)|${RESET} Running $test_rel_name ..."
    echo "Running $test" >> $LOG_FILE
    start=$(($(date +%s%N)/1000000))
    # redirect both stdout and stderr to the log file
    dune exec --no-build -- $CMD $test >> $LOG_FILE 2>>$LOG_FILE
    result=$?
    end=$(($(date +%s%N)/1000000))
    if [ $result -eq $expect_failure ]; then
        if [ $result -eq 0 ]; then
            echo -ne " ${GREEN}passed"
        else
            echo -ne " ${GREEN}failed (expected)"
        fi
        passed=$((passed+1))
        if $STORE_PASSES; then
            echo $test >> $PASS_FILE
        fi
    else
        if [ $result -eq 0 ] && [ $expect_failure -eq 1 ]; then
            echo -ne " ${RED}passed (expected failure)"
            failed=$((failed+1))
        elif [ $result -eq 1 ] && [ $expect_failure -eq 0 ]; then
            echo -ne " ${RED}failed"
            failed=$((failed+1))
        else
            if [ $result -eq 3 ]; then
                echo -ne " ${PURPLE}crashed due to Charon"
            else
                echo -ne " ${ORANGE}crashed: status code $result"
            fi
        fi
        if $STOP_ON_FAIL; then
            exit 1
        fi
    fi

    echo -e "${RESET} in $((end-start))ms"
    echo -e "\n" >> $LOG_FILE
    step=$((step+1))
done

script_end=$(($(date +%s%N)/1000000))
script_duration=$((script_end-script_start))

# | Passed N/N tests in N ms.    Have a nice day 〜
echo -e "$(rainbow step)|${RESET} \
${BOLD}Result: \
${GREEN}$passed${RESET}\
${BOLD}/\
${RED}$failed${RESET}\
${BOLD}/$step tests in ${CYAN}$script_duration ms${RESET}    \
${PURPLE}Have a nice day 〜${RESET}"
