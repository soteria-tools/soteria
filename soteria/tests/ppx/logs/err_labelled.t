  $ ../test.sh err_labelled.ml
  File "err_labelled.ml", line 3, characters 18-19:
  3 | let () = [%l.info f ~foo:1 "x"]
                        ^
  Error: %l.info does not support labelled or optional arguments (found ~foo)
  [1]
