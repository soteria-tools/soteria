<?php

class NestedValue
{
    public $value = 1;
}

class CloneBox
{
    public $value = 1;
    public $items = [1];
    public $nested;
    public $reference = 0;

    public function __clone()
    {
        echo "clone;";
        $this->value = $this->value + 10;
    }
}

class PlainBox
{
    public $value = 1;
}

class PrivateClone
{
    private function __clone()
    {
    }

    public function copy()
    {
        return clone $this;
    }
}

class ThrowingClone
{
    public $nested;

    public function __clone()
    {
        $this->nested->value = 9;
        throw new RuntimeException("clone failed");
    }
}

$shared = 4;
$original = new CloneBox();
$original->nested = new NestedValue();
$original->reference =& $shared;
$copy = clone $original;
$copy->items[0] = 2;
$copy->nested->value = 3;
$copy->reference = 7;
echo $original !== $copy, ":", $original->value, ":", $copy->value, ":";
echo $original->items[0], ":", $copy->items[0], ":";
echo $original->nested->value, ":", $original->reference, ":", $shared, ";";

$temporary = 5;
$unreferenced = new PlainBox();
$unreferenced->value =& $temporary;
unset($temporary);
$separate = clone $unreferenced;
$separate->value = 6;
echo $unreferenced->value, ":", $separate->value, ";";

$private = new PrivateClone();
try {
    clone $private;
} catch (Error $error) {
    echo "private;";
}
echo get_class($private->copy()), ";";

$throwing = new ThrowingClone();
$throwing->nested = new NestedValue();
try {
    clone $throwing;
} catch (RuntimeException $error) {
    echo "throw:", $throwing->nested->value, ";";
}

try {
    clone 1;
} catch (Error $error) {
    echo "non-object;";
}
try {
    clone new RuntimeException("failure");
} catch (Error $error) {
    echo "uncloneable\n";
}
