open Prelude

type t = {
  my_int : Excl_int.t option;
      [@sym_state.ignore
        {
          empty = None;
          pp = Format.pp_print_option Excl_int.pp;
          is_empty = Option.is_none;
        }]
}
[@@deriving sym_state { symex = Symex }]
