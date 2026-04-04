Reject extra key in ignore attribute
  $ ../standalone.exe -impl ignore_extra_key.ml -o out.ml
  File "ignore_extra_key.ml", line 5, characters 6-58:
  5 |       [@sym_state.ignore { empty = None; field = my_int }]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@deriving sym_state] unexpected field 'field' in [@sym_state.ignore], only 'field' expected
  [1]
