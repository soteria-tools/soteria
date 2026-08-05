<?php
class Vault
{
    private $privateValue = 1;
    private $privateItems = [];
    protected $protectedValue = 2;
    public $publicValue = 3;

    public function exercise()
    {
        echo $this->privateValue, $this->protectedValue, $this->publicValue;
        $private =& $this->privateValue;
        $private = 4;
        $this->protectedValue = 5;
        unset($this->publicValue);
        $this->publicValue = 6;
        echo ":", $this->privateMethod(), $this->protectedMethod();
        inspectFromFunction($this);
    }

    private function privateMethod()
    {
        return $this->privateValue;
    }

    protected function protectedMethod()
    {
        return $this->protectedValue;
    }
}

function inspectFromFunction($vault)
{
    try {
        echo $vault->privateValue;
    } catch (Error $error) {
        echo ":function-private-read";
    }
}

class Stranger
{
    public function exercise($vault)
    {
        try {
            echo $vault->privateValue;
        } catch (Error $error) {
            echo ":unrelated-private-read";
        }
        try {
            $vault->protectedValue = sideEffect();
        } catch (Error $error) {
            echo ":unrelated-protected-write";
        }
        try {
            $reference =& $vault->privateValue;
        } catch (Error $error) {
            echo ":unrelated-private-reference";
        }
        try {
            unset($vault->protectedValue);
        } catch (Error $error) {
            echo ":unrelated-protected-unset";
        }
        try {
            $vault->privateMethod(sideEffect());
        } catch (Error $error) {
            echo ":unrelated-private-method";
        }
        try {
            $vault->protectedMethod();
        } catch (Error $error) {
            echo ":unrelated-protected-method";
        }
    }
}

class Closed
{
    private function __construct()
    {
        echo ":constructed";
    }
}

function sideEffect()
{
    echo ":side-effect";
    return 9;
}

$vault = new Vault();
$vault->exercise();
echo ":", $vault->publicValue;
(new Stranger())->exercise($vault);

try {
    echo $vault->privateValue;
} catch (Error $error) {
    echo ":global-private-read";
}
try {
    $vault->protectedValue = sideEffect();
} catch (Error $error) {
    echo ":global-protected-write";
}
try {
    $vault->privateItems[] = sideEffect();
} catch (Error $error) {
    echo ":global-private-nested-write";
}
try {
    $vault->privateMethod(sideEffect());
} catch (Error $error) {
    echo ":global-private-method";
}
try {
    new Closed(sideEffect());
} catch (Error $error) {
    echo ":private-constructor";
}

echo "\n";
