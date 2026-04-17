  $ ../test.sh err_function_payload.ml
  File "err_function_payload.ml", line 3, characters 19-29:
  3 | let () = [%l.debug fun x -> x]
                         ^^^^^^^^^^
  Error: %l.debug expects a string literal format as first argument
  [1]
