open Ppxlib

let ext ext =
  let open Expander.If_sat in
  Extension.declare_with_path_arg
    (Extension_name.to_string ext)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> expand ~ext expr)

(* Registring [if%sat] *)
let () =
  let open Expander.If_sat in
  let register extension =
    Driver.register_transformation
      (Extension_name.to_string extension)
      ~extensions:[ ext extension ]
  in
  List.iter register [ Sat; Sat1 ]

(* Register [0s], [1s] etc. *)
let () =
  let open Expander.Sym_constants in
  let kind = Context_free.Rule.Constant_kind.Integer in
  let rule = Context_free.Rule.constant kind suffix rewriter in
  Driver.register_transformation ~rules:[ rule ] "sym_constants"
