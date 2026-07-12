  $ php program.php > php.out
  $ ../../../bin/soteria_php.exe exec program.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  ok

  $ php operators.php > php.out
  $ ../../../bin/soteria_php.exe exec operators.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  7,3,10,2.5,3.5,1,12

  $ php arrays.php > php.out
  $ ../../../bin/soteria_php.exe exec arrays.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  kv:string:1:append:7:8:next:3:4:4:1

  $ php foreach.php > php.out
  $ ../../../bin/soteria_php.exe exec foreach.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  once;2=two;name=value;5=five;|changed:new;1:2:9;a:10:b:20;2:10;10;21;30;4;caught

  $ php foreach_reference.php > php.out
  $ ../../../bin/soteria_php.exe exec foreach_reference.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  0=1;1=2;2=3;9:9:9:8:0=1;2=3;:0=1;1=2;0=7;

  $ php -d display_errors=0 foreach_invalid.php > php.out 2> /dev/null
  $ ../../../bin/soteria_php.exe exec foreach_invalid.php --runtime-events ignore > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  done

  $ php references.php > php.out
  $ ../../../bin/soteria_php.exe exec references.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  2:5:7:1:3:3:7:1:8

  $ php objects.php > php.out
  $ ../../../bin/soteria_php.exe exec objects.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  same:different:2:1:3:4:3:second:first:5

  $ php methods.php > php.out
  $ ../../../bin/soteria_php.exe exec methods.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  initial=1:result=:receiver:argument:method7:value=7:recursive=7:missing:null:method-throw:constructing:caught

  $ php visibility.php > php.out
  $ ../../../bin/soteria_php.exe exec visibility.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  123:45:function-private-read:6:unrelated-private-read:side-effect:unrelated-protected-write:unrelated-private-reference:unrelated-protected-unset:unrelated-private-method:unrelated-protected-method:global-private-read:side-effect:global-protected-write:side-effect:global-private-nested-write:global-private-method:private-constructor

  $ php exceptions.php > php.out
  $ ../../../bin/soteria_php.exe exec exceptions.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  ififdone:caught:same:finally7:9:error

  $ php catchable_errors.php > php.out
  $ ../../../bin/soteria_php.exe exec catchable_errors.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  division:finally;operand:finally;argument:finally;offset:finally;property:finally;loop:finally;

  $ ../../../bin/soteria_php.exe exec runtime_events.php --runtime-events ignore

  $ php -d display_errors=0 deprecations.php > php.out 2> /dev/null
  $ ../../../bin/soteria_php.exe exec deprecations.php --runtime-events ignore > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  1:2

  $ ../../../bin/soteria_php.exe exec runtime_events.php --runtime-events report
  warning: Undefined variable $undefined
      --> runtime_events.php:3:10
    3 |  $value = $undefined;
      |           ^^^^^^^^^^ Triggering operation

  $ ../../../bin/soteria_php.exe exec runtime_events.php
  Entry point: runtime_events.php
  error: Undefined variable $undefined
      --> runtime_events.php:3:10
    3 |  $value = $undefined;
      |           ^^^^^^^^^^ Triggering operation
  [1]

  $ php functions.php > php.out
  $ ../../../bin/soteria_php.exe exec functions.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  5:7:9

  $ ../../../bin/soteria_php.exe exec symbolic.php
  symbolic ok

  $ ../../../bin/soteria_php.exe exec counterexample.php
  Entry point: counterexample.php
  error: Failed assertion
      --> counterexample.php:8:1
    8 |  Soteria\assert(false);
      |  ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
  Counterexample:
  $input0 = true
  $input1 = 42
  $input2 = 1.5
  [1]

  $ ../../../bin/soteria_php.exe exec expect_fail.php

  $ ../../../bin/soteria_php.exe exec expect_fail_missing.php
  error: Expected failure in entry point expect_fail_missing.php, but none was found
  [1]

  $ ../../../bin/soteria_php.exe exec --step-fuel=5 expect_fail_incomplete.php
  Step fuel exhausted
  [3]

  $ ../../../bin/soteria_php.exe exec failure.php
  Entry point: failure.php
  error: Failed assertion
      --> failure.php:2:1
    2 |  Soteria\assert(false);
      |  ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
  [1]

  $ ../../../bin/soteria_php.exe exec function_failure.php
  Entry point: function_failure.php
  error: Failed assertion
      --> function_failure.php:3:5
    3 |      Soteria\assert(false);
      |      ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
      .  
    6 |  fail_here();
      |  ----------- 1: Call to fail_here
  [1]

  $ ../../../bin/soteria_php.exe exec function_entry.php --function selected_entry
  Entry point: selected_entry
  error: Failed assertion
      --> function_entry.php:6:5
    2 |    Soteria\assert(false);
    3 | /  function selected_entry() {
    4 | |      $number = Soteria\symbolic_int();
    5 | |      Soteria\assume($number === 9);
    6 | |      Soteria\assert(false);
      | |      ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
    7 | |  }
      | \--' 1: Call to selected_entry
    8 |    Soteria\assert(false);
  Counterexample:
  $input0 = 9
  [1]

  $ ../../../bin/soteria_php.exe exec functions.php --function add
  Entry point error: function add has 2 parameter(s); function entry points must have no parameters
  [2]

  $ ../../../bin/soteria_php.exe exec functions.php --function missing
  Entry point error: function missing was not found
  [2]

  $ ../../../bin/soteria_php.exe exec uncaught_exception.php
  Entry point: uncaught_exception.php
  error: Uncaught RuntimeException: boom
      --> uncaught_exception.php:3:1
    3 |  throw new RuntimeException("boom");
      |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Triggering operation
  [1]

  $ ../../../bin/soteria_php.exe exec --step-fuel=5 loop.php
  Step fuel exhausted
  [3]

  $ ../../../bin/soteria_php.exe exec --branching-fuel=1 branches.php
  Branching fuel exhausted
  [3]
