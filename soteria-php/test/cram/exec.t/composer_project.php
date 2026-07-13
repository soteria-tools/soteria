<?php

require_once __DIR__ . "/composer-project/vendor/autoload.php";

use Demo\Greeter;
use Package\PackageThing;

#[Soteria\Test]
function composer_entry()
{
    $greeter = new Greeter();
    $box = new LegacyBox();
    $package = new PackageThing();
    echo composer_helper(), ":", $greeter->greet(), ":", $box->value(), ":", $package->value();
}

class ComposerCases
{
    #[Soteria\Test]
    public static function static_entry()
    {
        echo "static";
    }

    #[Soteria\Test]
    public function instance_entry()
    {
        echo "instance";
    }
}
