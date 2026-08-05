<?php

function add_one($value) {
    return $value + 1;
}

class Counter {
    public static $value = 1;
    public $offset = 10;

    public static function add($value) {
        self::$value = self::$value + $value;
        return self::$value;
    }

    public function combine($value) {
        return $this->offset + $value;
    }
}

class BaseCounter {
    public static $number = 1;

    public static function bump() {
        static::$number = static::$number + 1;
        return static::$number;
    }
}

class ChildCounter extends BaseCounter {
    public static $number = 20;
}

$function = add_one(...);
$static = Counter::add(...);
$counter = new Counter();
$method = $counter->combine(...);
echo $function === $function, ':', $function === add_one(...), ':';
echo $function(2), ':', $static(3), ':', Counter::$value, ':', $method(5), ':';
echo ChildCounter::bump(), ':', BaseCounter::$number, ':';
$alias =& Counter::$value;
$alias = 6;
echo Counter::$value, ':';

$value = 2;
$byValue = function () use ($value) {
    $value = $value + 1;
    return $value;
};
$value = 10;
echo $byValue(), ':', $byValue(), ':', $value, ':';

$reference = 1;
$byReference = function () use (&$reference) {
    $reference = $reference + 1;
};
$byReference();
echo $reference;
