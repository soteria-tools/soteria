open Prelude

type t = {
  ignored_int : Excl_int.t option [@sym_state.ignore { empty = None }];
  my_super_int : Excl_int_in_int.t option;
      [@sym_state.context { field = ignored_int }]
}
[@@deriving sym_state { symex = Symex }]
