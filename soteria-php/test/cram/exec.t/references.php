<?php
$value = 1;
$alias =& $value;
$alias = 2;

$other = 3;
$alias =& $other;
$alias = 4;
unset($alias);
$alias = 5;

$original = [1];
$before = $original;
$element =& $original[0];
$element = 2;
$after = $original;
$after[0] = 3;

$array = [];
$array[] =& $other;
$array_copy = $array;
$array_copy[0] = 6;
unset($array[0]);
$other = 7;
$array[] = 8;

echo $value, ":", $alias, ":", $other;
echo ":", $before[0], ":", $original[0], ":", $after[0];
echo ":", $array_copy[0], ":", $array_copy === [7], ":", $array[1], "\n";
