<?php

require_once __DIR__ . "/includes/math.php";
require_once __DIR__ . "/includes/math.php";
include __DIR__ . "/includes/message.php";

use Included\Message;

$message = new Message();
echo $message->text(), ":", double_value(6);
