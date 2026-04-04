Reject context reference to ignored field
  $ ../test.sh context_on_ignored_ref.ml
  File "context_on_ignored_ref.ml", lines 5-6, characters 2-50:
  5 | ..my_super_int : Excl_int_in_int.t option;
  6 |       [@sym_state.context { field = ignored_int }]
  Error: [@deriving sym_state] sym_state.context.field cannot reference an ignored field
  [1]
