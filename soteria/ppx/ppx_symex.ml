open Ppxlib

let if_sat_ext ext =
  let open Expander.If_sat in
  Extension.declare_with_path_arg
    (Extension_name.to_string ext)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> expand ~ext expr)

let log_ext ext =
  let open Logs in
  Extension.declare_with_path_arg
    (Extension_name.to_string ext)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> expand ~ext expr)

(* Register [if%sat] *)
let () =
  let extensions =
    List.map if_sat_ext Expander.If_sat.Extension_name.[ Sat; Sat1; Sure ]
  in
  Driver.register_transformation "if_sat" ~extensions

(* Register [0s], [1s] etc. *)
let () =
  let open Expander.Sym_constants in
  let kind = Context_free.Rule.Constant_kind.Integer in
  let rule = Context_free.Rule.constant kind suffix rewriter in
  Driver.register_transformation ~rules:[ rule ] "sym_constants"

(* Register [@@deriving reversible] *)
let () = Reversible.register ()

(* Register [%l.debug], [%l.info], ... *)
let () =
  let extensions =
    List.map log_ext
      Logs.Extension_name.[ Debug; Info; Warn; Error; Trace; Smt ]
  in
  Driver.register_transformation "logs" ~extensions

(* Register [@@deriving sym_state] *)
let () = Sym_state.register ()
