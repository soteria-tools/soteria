<?php

class MagicCallBase
{
    public static $kind = "base";
    public $value = 0;

    private static function hidden()
    {
        return -1;
    }

    public function ordinary()
    {
        return -1;
    }

    public static function __callStatic($name, $arguments)
    {
        echo "static:", static::$kind, ":", $name, ";";
        return $arguments[0] + $arguments[1];
    }

    public function __invoke($value)
    {
        echo "invoke;";
        $this->value = $value;
        return $value + 1;
    }
}

class MagicCallChild extends MagicCallBase
{
    public static $kind = "child";
}

class NotInvokable
{
}

function sideEffect()
{
    echo "effect;";
    return 3;
}

echo MagicCallChild::missing(sideEffect(), 2), ":";
echo MagicCallChild::hidden(1, 2), ":";

$object = new MagicCallChild();
echo $object(sideEffect()), ":", $object->value, ":";

try {
    MagicCallChild::ordinary(sideEffect());
} catch (Error $error) {
    echo "non-static:";
}

$notInvokable = new NotInvokable();
try {
    $notInvokable(sideEffect());
} catch (Error $error) {
    echo "not-callable\n";
}
