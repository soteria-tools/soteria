open Prelude

type t = {
  my_int_1 : Excl_int.t option;
      [@sym_state.ignore
        {
          empty = None;
          pp = Format.pp_print_option Excl_int.pp;
          is_empty = Option.is_none;
        }]
  my_int_2 : Excl_int.t option;
      [@sym_state.ignore
        {
          empty = None;
          is_empty = Option.is_none;
          pp = Format.pp_print_option Excl_int.pp;
        }]
  my_int_3 : Excl_int.t option;
      [@sym_state.ignore
        {
          pp = Format.pp_print_option Excl_int.pp;
          is_empty = Option.is_none;
          empty = None;
        }]
  my_int_4 : Excl_int.t option;
      [@sym_state.ignore
        {
          is_empty = Option.is_none;
          empty = None;
          pp = Format.pp_print_option Excl_int.pp;
        }]
}
[@@deriving sym_state { symex = Symex }]
