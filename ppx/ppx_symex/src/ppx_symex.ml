open Ppxlib

let ext ext =
  Extension.declare_with_path_arg
    (Ppx_symex_expander.Extension_name.to_string ext)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> Ppx_symex_expander.expand ~ext expr)

let () =
  Driver.register_transformation
    (Ppx_symex_expander.Extension_name.to_string Sat)
    ~extensions:[ ext Sat ]
