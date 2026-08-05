<?php

class Container
{
}

$array = [];
$array[1.25];
$false_value = false;
$false_value[0] = 1;
$object = new Container();
$object->dynamic = 2;
echo $false_value[0], ":", $object->dynamic;
