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

  $ ../../../bin/soteria_php.exe exec symbolic.php
  symbolic ok

  $ ../../../bin/soteria_php.exe exec failure.php
  error: Failed assertion
      --> failure.php:2:1
    2 |  Soteria\assert(false);
      |  ^^^^^^^^^^^^^^^^^^^^^ Triggering operation
  [1]

  $ ../../../bin/soteria_php.exe exec --step-fuel=5 loop.php
  Step fuel exhausted
  [3]

  $ ../../../bin/soteria_php.exe exec --branching-fuel=1 branches.php
  Branching fuel exhausted
  [3]
