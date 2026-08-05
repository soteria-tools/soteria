<?php
class Box
{
    public $value = 1;
    public $items = ["first"];
    public $empty;
}

function set_value($box)
{
    $box->value = 5;
}

$first = new Box();
$alias = $first;
$other = new box();

if ($first === $alias) {
    echo "same";
}
if ($first !== $other) {
    echo ":different";
}

$alias->value = 2;
echo ":", $first->value, ":", $other->value;

$reference =& $first->value;
$reference = 3;
echo ":", $alias->value;

unset($alias->value);
$first->value = 4;
echo ":", $alias->value, ":", $reference;

$first->items[] = "second";
echo ":", $first->items[1], ":", $other->items[0];

set_value($alias);
echo ":", $first->value, "\n";
