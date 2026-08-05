<?php

$source = [1, 2];
foreach ($source as $key => &$value) {
    echo $key, '=', $value, ';';
    if ($key === 0) {
        $source[] = 3;
    }
}

$copy = $source;
$value = 9;
echo $source[2], ':', $copy[2], ':';

unset($value);
$value = 7;
echo $source[2], ':';

foreach ([5, 6] as &$temporary) {
}
$temporary = 8;
echo $temporary, ':';

$removed = [1, 2, 3];
foreach ($removed as $key => &$item) {
    echo $key, '=', $item, ';';
    if ($key === 0) {
        unset($removed[1]);
    }
}

echo ':';
$reinserted = [1, 2];
$once = true;
foreach ($reinserted as $key => &$item) {
    echo $key, '=', $item, ';';
    if ($once) {
        unset($reinserted[$key]);
        $reinserted[$key] = 7;
        $once = false;
    }
}
