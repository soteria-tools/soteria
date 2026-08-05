<?php
interface Named
{
    public function name($prefix);
}

interface Detailed extends Named
{
    public function detail();
}

trait LeftFeature
{
    public $traitValue = "trait";

    public function source()
    {
        return "left";
    }

    public function onlyLeft()
    {
        return "selected";
    }

    protected function hidden()
    {
        return $this->traitValue;
    }
}

trait RightFeature
{
    public function source()
    {
        return "right";
    }
}

class Base
{
    private $value = "base";
    protected $shared = 1;

    public function __construct($shared)
    {
        $this->shared = $shared;
    }

    public function describe()
    {
        return "base:" . $this->shared;
    }

    public function baseValue()
    {
        return $this->value;
    }
}

class Child extends Base implements Detailed
{
    use LeftFeature, RightFeature {
        LeftFeature::source insteadof RightFeature;
        onlyLeft as selectedSource;
        RightFeature::source as rightSource;
        LeftFeature::hidden as public revealed;
    }

    private $value = "child";

    public function name($prefix)
    {
        return $prefix . $this->value;
    }

    public function detail()
    {
        return parent::describe() . ":child";
    }

    public function setShared($value)
    {
        $this->shared = $value;
    }

    public function childValue()
    {
        return $this->value;
    }
}

class InheritedConstructor extends Base
{
}

class ExceptionBase extends RuntimeException
{
}

class WrappedException extends ExceptionBase
{
    public function __construct($message)
    {
        parent::__construct($message);
    }
}

$child = new Child(3);
echo $child->detail(), ":", $child->name("name="), ":";
echo $child->source(), ":", $child->selectedSource(), ":";
echo $child->rightSource(), ":", $child->revealed(), ":";
echo $child->baseValue(), ":", $child->childValue(), ":";
$child->setShared(5);
echo $child->describe(), ":";
echo (new InheritedConstructor(7))->describe(), ":";

try {
    throw new WrappedException("wrapped");
} catch (RuntimeException $exception) {
    echo "caught";
}

echo "\n";
