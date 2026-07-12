<?php

function iterable_once() {
    echo "once;";
    return [2 => "two", "name" => "value", 5 => "five"];
}

$source = iterable_once();
foreach ($source as $key => $value) {
    echo $key, "=", $value, ";";
    if ($key === 2) {
        $source["name"] = "changed";
        $source[] = "new";
    }
}
echo "|", $source["name"], ":", $source[6], ";";

$copy = [1, 2];
foreach ($copy as $value) {
    $value = 9;
}
echo $copy[0], ":", $copy[1], ":", $value, ";";

$keys = [];
$values = [];
foreach (["a" => 10, "b" => 20] as $keys[] => $values[]) {
}
echo $keys[0], ":", $values[0], ":", $keys[1], ":", $values[1], ";";

foreach ([1, 2, 3] as $outer) {
    foreach ([10, 20] as $inner) {
        if ($outer === 1) {
            continue 2;
        }
        echo $outer, ":", $inner, ";";
        if ($outer === 2) {
            break 2;
        }
    }
}

$referent = 20;
$referenced = [10, 0, 30];
$referenced[1] =& $referent;
foreach ($referenced as $key => $value) {
    if ($key === 0) {
        $referent = 21;
    }
    echo $value, ";";
}

function find_value() {
    foreach ([3, 4, 5] as $value) {
        if ($value === 4) {
            return $value;
        }
    }
    return 0;
}

echo find_value(), ";";

try {
    foreach ([1, 2] as $value) {
        if ($value === 2) {
            throw new RuntimeException("stop");
        }
    }
} catch (RuntimeException $exception) {
    echo "caught";
}

