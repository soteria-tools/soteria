(* TODO: handle qualifiers! *)
let get_param_tys ~prog fid =
  List.find_map
    (fun (id, (_, _, decl)) ->
      if Cerb_frontend.Symbol.equal_sym id fid then
        match decl with
        | Cerb_frontend.AilSyntax.Decl_function
            (_proto, _ret_qual, ptys, _is_variadic, _is_inline, _is_noreturn) ->
            Some (List.map (fun (_, ty, _) -> ty) ptys)
        | _ -> None
      else None)
    prog.Cerb_frontend.AilSyntax.declarations
