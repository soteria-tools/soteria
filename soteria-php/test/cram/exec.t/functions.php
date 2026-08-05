<?php
$outside = 7;
$result = add(2, 3, $side_effect = 9);
echo $result, ":", $outside, ":", $side_effect, "\n";

function add($left, $right) {
    $outside = 99;
    if ($left > 0) {
        return $left + $right;
    }
    return;
}
