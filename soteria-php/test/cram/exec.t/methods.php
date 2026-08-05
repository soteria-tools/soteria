<?php
class Counter
{
    public $value = 1;

    public function __construct($value)
    {
        echo "initial=", $this->value;
        $this->value = $value;
    }

    public function set($value)
    {
        echo ":method";
        $this->value = $value;
        return $this->value;
    }

    public function recurse($depth)
    {
        if ($depth === 0) {
            return $this->value;
        }
        return $this->recurse($depth - 1);
    }

    public function fail()
    {
        throw new RuntimeException("method");
    }
}

class Throwing
{
    public function __construct()
    {
        echo ":constructing";
        throw new RuntimeException("boom");
    }
}

function receiver($object)
{
    echo ":receiver";
    return $object;
}

function argument()
{
    echo ":argument";
    return 7;
}

$counter = new Counter(3);
$alias = $counter;
echo ":result=", receiver($alias)->SeT(argument());
echo ":value=", $counter->value;
echo ":recursive=", $counter->recurse(3);

try {
    $counter->missing(argument());
} catch (Error $error) {
    echo ":missing";
}

try {
    null->missing(argument());
} catch (Error $error) {
    echo ":null";
}

try {
    $counter->fail();
} catch (RuntimeException $exception) {
    echo ":method-throw";
}

try {
    new Throwing();
} catch (RuntimeException $exception) {
    echo ":caught";
}

echo "\n";
