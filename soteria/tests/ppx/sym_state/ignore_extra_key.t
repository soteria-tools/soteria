Reject extra key in ignore attribute
  $ ../test.sh ignore_extra_key.ml
  File "ignore_extra_key.ml", line 5, characters 6-58:
  5 |       [@sym_state.ignore { empty = None; field = my_int }]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@deriving sym_state] unexpected field 'field' in [@sym_state.ignore], expected one of empty, is_empty, pp
  [1]
