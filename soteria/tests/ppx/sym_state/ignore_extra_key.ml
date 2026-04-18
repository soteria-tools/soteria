open Prelude

type t = {
  my_int : Excl_int.t option;
      [@sym_state.ignore { empty = None; field = my_int }]
}
[@@deriving sym_state { symex = Symex }]
