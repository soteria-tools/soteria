Reject extra key in ignore attribute
  $ ../test.sh ignore_missing_empty.ml
  File "ignore_missing_empty.ml", line 3, characters 40-56:
  3 | type t = { my_int : Excl_int.t option [@sym_state.ignore] }
                                              ^^^^^^^^^^^^^^^^
  Error: :: expected
  [1]
