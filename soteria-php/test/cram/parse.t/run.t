  $ ../../../bin/soteria_php.exe parse valid.php > first.json
  $ ../../../bin/soteria_php.exe parse valid.php > second.json
  $ cmp first.json second.json
  $ php summarize.php first.json
  schema=16 php=8.4.19 source=valid.php
  echo 2:1-2:42 [6,47)
    null=-
    bool=true
    bool=false
    int="42"
    float="3.5"
    string="hello"
  expression 3:1-3:8 [48,55)

  $ ../../../bin/soteria_php.exe parse unsupported.php
  Frontend error: unsupported.php:2:1: unsupported statement (Stmt_For)
  [2]

  $ ../../../bin/soteria_php.exe parse unsupported_function.php
  Frontend error: unsupported_function.php:2:20: unsupported function parameter (Param)
  [2]

  $ ../../../bin/soteria_php.exe parse late_include.php
  Frontend error: late_include.php:4:1: unsupported include after executable top-level code
  [2]

  $ ../../../bin/soteria_php.exe parse dynamic_include.php
  Frontend error: dynamic_include.php:4:9: unsupported dynamic include path (Expr_Variable)
  [2]

  $ ../../../bin/soteria_php.exe parse executable_include.php
  Frontend error: valid.php:2:1: unsupported executable code in an included or autoloaded file
  [2]

  $ ../../../bin/soteria_php.exe parse foreach_reference.php > /dev/null

  $ ../../../bin/soteria_php.exe parse malformed.php
  Frontend error: malformed.php:2:6: parse error: Syntax error, unexpected ';'
  [2]
