<?php

class MagicBase
{
    private $inheritedPrivate = 1;
    private static $privateStatic = 1;
    protected $inheritedProtected = 2;
    public static $staticValue = 3;

    private function hiddenMethod()
    {
        return 1;
    }
}

class MagicBox extends MagicBase
{
    private $secret = "secret";
    private $values = [];
    public $nullable = null;

    public function __get($name)
    {
        echo "get:", $name, ";";
        if ($name === "secret") {
            return $this->secret;
        }
        if ($name === "throwing") {
            throw new RuntimeException("magic read");
        }
        if ($name === "nested") {
            return ["x" => 1];
        }
        return $this->values[$name];
    }

    public function __set($name, $value)
    {
        echo "set:", $name, "=", $value, ";";
        $this->values[$name] = $value;
    }

    public function __isset($name)
    {
        echo "isset:", $name, ";";
        return $name === "stored" || $name === "secret" || $name === "nested";
    }

    public function __unset($name)
    {
        echo "unset:", $name, ";";
        unset($this->values[$name]);
    }

    public function __call($name, $arguments)
    {
        echo "call:", $name, ";";
        return $arguments[0] + $arguments[1];
    }

    public function __toString()
    {
        echo "string;";
        return "box";
    }
}

class BadString
{
    public function __toString()
    {
        return [];
    }
}

class NoString
{
}

function sideEffect()
{
    echo "effect;";
    return 7;
}

$box = new MagicBox();
echo $box->secret, ":";
$box->stored = sideEffect();
echo $box->stored, ":";
echo isset($box->stored), ":", isset($box->missing), ":";
echo isset($box->nullable), ":", isset($box->missing, $box->secret), ":";
echo isset($box->nested["x"]), ":", isset($box->missing["x"]), ":";
echo isset(MagicBase::$privateStatic), ":";
unset($box->stored);
echo $box->missingMethod(sideEffect(), 2), ":";
echo $box->hiddenMethod(3, 2), ":";
echo (string) $box, ":", "value=" . $box, ":";
try {
    echo $box->throwing;
} catch (RuntimeException $error) {
    echo "magic-throw:";
}
try {
    echo (string) new BadString();
} catch (TypeError $error) {
    echo "bad-string:";
}
try {
    echo (string) new NoString();
} catch (Error $error) {
    echo "no-string:";
}

echo get_class($box), ":";
echo is_a($box, "MagicBase"), is_a($box, "Stringable"),
    is_a("MagicBox", "MagicBase"),
    is_a("MagicBox", "MagicBase", true), ":";
echo property_exists($box, "secret"),
    property_exists($box, "inheritedPrivate"),
    property_exists($box, "inheritedProtected"),
    property_exists($box, "staticValue"),
    property_exists($box, "stored"), ":";
echo method_exists($box, "hiddenMethod"),
    method_exists($box, "__toString"),
    method_exists($box, "missingMethod"), ":";
$getClass = get_class(...);
echo $getClass($box), ":";
try {
    get_class(1);
} catch (TypeError $error) {
    echo "get-class-type:";
}
try {
    is_a($box);
} catch (ArgumentCountError $error) {
    echo "is-a-arity:";
}
try {
    property_exists($box, []);
} catch (TypeError $error) {
    echo "property-exists-type:";
}

$array = ["x" => 1];
echo isset($array["x"]), isset($array["y"]), isset($unknown), "\n";
