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

  $ php references.php > php.out
  $ ../../../bin/soteria_php.exe exec references.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  2:5:7:1:3:3:7:1:8

  $ php exceptions.php > php.out
  $ ../../../bin/soteria_php.exe exec exceptions.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  ififdone:caught:same:finally7:9:error

  $ php functions.php > php.out
  $ ../../../bin/soteria_php.exe exec functions.php > soteria.out
  $ cmp php.out soteria.out
  $ cat soteria.out
  5:7:9

  $ ../../../bin/soteria_php.exe exec symbolic.php
  symbolic ok

  $ ../../../bin/soteria_php.exe exec failure.php
  error: Failed assertion
      --> failure.php:2:1
    2 |  Soteria\assert(false);
      |  ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
  [1]

  $ ../../../bin/soteria_php.exe exec function_failure.php
  error: Failed assertion
      --> function_failure.php:3:5
    3 |      Soteria\assert(false);
      |      ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
      .  
    6 |  fail_here();
      |  ----------- 1: Call to fail_here
  [1]

  $ ../../../bin/soteria_php.exe exec uncaught_exception.php
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
