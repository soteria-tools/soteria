<?php

require_once dirname(__DIR__) . "/functions.php";

spl_autoload_register(function ($class) {
    $files = [
        "Demo\\Greeter" => dirname(__DIR__) . "/src/Greeter.php",
        "Demo\\BaseGreeter" => dirname(__DIR__) . "/src/BaseGreeter.php",
        "Package\\PackageThing" => __DIR__ . "/demo/package/src/PackageThing.php",
        "LegacyBox" => dirname(__DIR__) . "/legacy/LegacyBox.php",
    ];
    if (isset($files[$class])) {
        require_once $files[$class];
    }
});
