<?php
$condition = Soteria\symbolic_bool();
$number = Soteria\symbolic_int();
$fraction = Soteria\symbolic_float();
Soteria\assume($condition);
Soteria\assume($number === 42);
Soteria\assume($fraction === 1.5);
Soteria\assert(false);
