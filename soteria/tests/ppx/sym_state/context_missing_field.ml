open Prelude

type t = {
  my_int : Excl_int.t option;
  my_super_int : Excl_int_in_int.t option;
      [@sym_state.context { field = does_not_exist }]
}
[@@deriving sym_state { symex = Symex }]
