<?php
Soteria\expect_fail();
$number = Soteria\symbolic_int();
Soteria\assert($number !== 7);
