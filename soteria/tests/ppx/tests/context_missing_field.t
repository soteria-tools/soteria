Reject context to unknown field
  $ ./test.sh context_missing_field.ml
  File "context_missing_field.ml", lines 5-6, characters 2-53:
  5 | ..my_super_int : Excl_int_in_int.t option;
  6 |       [@sym_state.context { field = does_not_exist }]
  Error: [@deriving sym_state] sym_state.context references non-existent field "does_not_exist", expected one of 
  "my_int"
  [1]
