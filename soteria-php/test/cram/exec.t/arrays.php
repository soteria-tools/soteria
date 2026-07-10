<?php
function key_zero() {
    echo "k";
    return 0;
}

function value_one() {
    echo "v";
    return 1;
}

$array = ["00" => "string", "0" => "integer"];
$array[key_zero()] = value_one();
$array[] = "append";

$nested = [];
$nested["item"][] = 7;
$copy = $nested;
$copy["item"][0] = 8;

$negative = [];
$negative[-5] = "negative";
$negative[] = "next";

$reserved = [];
$reserved[] = ($reserved[] = 4);

$union = ["left" => 1, 0 => 2] + [0 => 9, "right" => 3];
$expected = ["left" => 1, 0 => 2, "right" => 3];

echo ":", $array["00"], ":", $array[0], ":", $array[1];
echo ":", $nested["item"][0], ":", $copy["item"][0];
echo ":", $negative[-4], ":", $union["right"];
echo ":", $reserved[0], ":", $reserved[1];
echo ":", $union === $expected, "\n";
