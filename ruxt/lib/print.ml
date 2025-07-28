open Soteria_rust_lib
open Charon

let print_ret ret =
  Fmt.pr "Ret: %a\n" (Charon_util.pp_rust_val Heap.Sptr.pp) ret

let print_args args =
  Fmt.pr "ARGS:\n";
  List.iter (Fmt.pr "%a\n" (Charon_util.pp_rust_val Heap.Sptr.pp)) args

let print_pcs pcs =
  Fmt.pr "PCS:\n";
  List.iter (Fmt.pr "%a\n" (Typed.pp Typed.T.pp_sbool)) pcs

let print_vars vars =
  Fmt.pr "VARS:\n";
  List.iter (Fmt.pr "%a\n" (Typed.pp Typed.T.pp_cval)) (List.map fst vars)

let print_state state =
  Fmt.pr "Heap: %a\n" (Heap.pp_pretty ~ignore_freed:false) state

let name_str crate =
  PrintTypes.name_to_string @@ PrintUllbcAst.Crate.crate_to_fmt_env crate

let print_fundef crate (fundef : UllbcAst.fun_decl) =
  Fmt.pr "\nFunction: %s\n" (name_str crate fundef.item_meta.name)
