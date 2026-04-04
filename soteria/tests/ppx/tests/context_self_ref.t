Reject self-referential context
  $ ../standalone.exe -impl context_self_ref.ml -o out.ml
  File "context_self_ref.ml", lines 5-6, characters 2-51:
  5 | ..my_super_int : Excl_int_in_int.t option;
  6 |       [@sym_state.context { field = my_super_int }]
  Error: [@deriving sym_state] sym_state.context.field cannot reference itself
  [1]
