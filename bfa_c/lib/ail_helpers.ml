(*** More standard interface to [Symbol], making it nicer for 
   using with Stdlib functors *)
module Symbol_std = struct
  open Cerb_frontend.Symbol

  type t = Cerb_frontend.Symbol.sym

  let equal = equal_sym
  let compare = compare_sym
  let hash = Hashtbl.hash
  let pp = Fmt_ail.pp_sym
end

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

let find_fun_sym ~prog to_find =
  List.find_map
    (fun ((id, _) as decl) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then Some decl else None)
    prog.Cerb_frontend.AilSyntax.function_definitions

let find_fun_name ~prog to_find =
  List.find_map
    (fun ((id, _) as decl) ->
      let name = Cerb_frontend.Pp_symbol.to_string id in
      if String.starts_with ~prefix:to_find name then Some decl else None)
    prog.Cerb_frontend.AilSyntax.function_definitions

let find_fun_loc ~prog to_find =
  List.find_map
    (fun (id, (loc, _, _, _, _)) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then Some loc else None)
    prog.Cerb_frontend.AilSyntax.function_definitions
