Reject sum constructor carrying <Module>.t option instead of <Module>.t
  $ ../test.sh invalid_ctor_arg.ml
  File "invalid_ctor_arg.ml", line 3, characters 16-33:
  3 | type t = Bad of Excl_int.t option [@@deriving sym_state { symex = Symex }]
                      ^^^^^^^^^^^^^^^^^
  Error: [@deriving sym_state] sum constructors must carry a single <Module>.t argument
  [1]
