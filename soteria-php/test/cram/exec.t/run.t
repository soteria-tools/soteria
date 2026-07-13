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

  $ php cloning.php > php.out
  $ ../../../bin/soteria_php.exe exec cloning.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  clone;1:1:11:1:2:3:7:7;5:6;private;PrivateClone;throw:9;non-object;uncloneable

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

  $ php inheritance.php > php.out
  $ ../../../bin/soteria_php.exe exec inheritance.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  base:3:child:name=child:left:selected:right:trait:base:child:base:5:base:7:caught

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

  $ php multi_file.php > php.out
  $ ../../../bin/soteria_php.exe exec multi_file.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  files:12

  $ php composer_run.php > php.out
  $ ../../../bin/soteria_php.exe exec composer_run.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  composer:autoload:23:package

  $ ../../../bin/soteria_php.exe discover composer_project.php
  composer_entry
  ComposerCases::static_entry
  ComposerCases::instance_entry

  $ ../../../bin/soteria_php.exe exec composer_project.php --function ComposerCases::static_entry
  static

  $ ../../../bin/soteria_php.exe exec composer_project.php --function ComposerCases::instance_entry
  instance

  $ php static_closures.php > php.out
  $ ../../../bin/soteria_php.exe exec static_closures.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  1::3:4:4:15:21:1:6:3:3:10:2

  $ php magic_objects.php > php.out
  $ ../../../bin/soteria_php.exe exec magic_objects.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  get:secret;secret:effect;set:stored=7;get:stored;7:isset:stored;1:isset:missing;::isset:missing;:isset:nested;get:nested;1:isset:missing;::unset:stored;effect;call:missingMethod;9:call:hiddenMethod;5:string;box:string;value=box:get:throwing;magic-throw:bad-string:no-string:MagicBox:111:111:11:MagicBox:get-class-type:is-a-arity:property-exists-type:1

  $ php magic_calls.php > php.out
  $ ../../../bin/soteria_php.exe exec magic_calls.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  effect;static:child:missing;5:static:child:hidden;3:effect;invoke;4:3:non-static:not-callable

  $ ../../../bin/soteria_php.exe exec magic_symbolic.php

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
