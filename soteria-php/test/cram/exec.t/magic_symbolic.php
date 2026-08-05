<?php

class MagicStore
{
    private $values = [];

    public function __get($name)
    {
        return $this->values[$name];
    }

    public function __set($name, $value)
    {
        $this->values[$name] = $value;
    }
}

$store = new MagicStore();
$store->value = 0;
$condition = Soteria\symbolic_bool();
if ($condition) {
    $store->value = 1;
    Soteria\assert($store->value === 1);
} else {
    Soteria\assert($store->value === 0);
    $store->value = 2;
    Soteria\assert($store->value === 2);
}
