<?php

function needs_argument($argument)
{
}

function divide()
{
    return 1 / 0;
}

$array = [];
$scalar = 1;

try {
    divide();
} catch (DivisionByZeroError) {
    echo "division";
} finally {
    echo ":finally;";
}

try {
    "not numeric" + 1;
} catch (TypeError) {
    echo "operand";
} finally {
    echo ":finally;";
}

try {
    needs_argument();
} catch (ArgumentCountError) {
    echo "argument";
} finally {
    echo ":finally;";
}

try {
    $array[[]];
} catch (TypeError) {
    echo "offset";
} finally {
    echo ":finally;";
}

try {
    $scalar->property = 1;
} catch (Error) {
    echo "property";
} finally {
    echo ":finally;";
}

try {
    while (true) {
        1 / 0;
    }
} catch (DivisionByZeroError) {
    echo "loop";
} finally {
    echo ":finally;";
}
