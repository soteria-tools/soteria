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

let sym_is_id sym id =
  let open Cerb_frontend.Symbol in
  match sym with Symbol (_digest, _i, SD_Id id') -> id = id' | _ -> false

let rec resolve_sym ~(prog : Ail_tys.linked_program) sym =
  match Pmap.lookup sym prog.symmap with
  | None -> sym
  | Some sym' ->
      if Symbol_std.equal sym sym' then sym else resolve_sym ~prog sym'

(* TODO: handle qualifiers! *)
let get_param_tys ~(prog : Ail_tys.linked_program) fid =
  let fid = resolve_sym ~prog fid in
  List.find_map
    (fun (id, (_, _, decl)) ->
      if Cerb_frontend.Symbol.equal_sym id fid then
        match decl with
        | Cerb_frontend.AilSyntax.Decl_function
            (_proto, _ret_qual, ptys, _is_variadic, _is_inline, _is_noreturn) ->
            Some (List.map (fun (_, ty, _) -> ty) ptys)
        | _ -> None
      else None)
    prog.sigma.declarations

let find_fun_name ~(prog : Ail_tys.linked_program) to_find =
  List.find_map
    (fun ((sym, _) as decl) ->
      if sym_is_id sym to_find then Some decl else None)
    prog.sigma.function_definitions

let find_fun_loc ~(prog : Ail_tys.linked_program) to_find =
  List.find_map
    (fun (id, (loc, _, _, _, _)) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then Some loc else None)
    prog.sigma.function_definitions

let find_fun_def ~(prog : Ail_tys.linked_program)
    (to_find : Cerb_frontend.Symbol.sym) =
  let to_find = resolve_sym ~prog to_find in
  List.find_opt
    (fun (id, _) -> Cerb_frontend.Symbol.equal_sym id to_find)
    prog.sigma.function_definitions

let find_obj_def ~(prog : Ail_tys.linked_program)
    (to_find : Cerb_frontend.Symbol.sym) =
  let to_find = resolve_sym ~prog to_find in
  List.find_map
    (fun (id, e) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then Some e else None)
    prog.sigma.object_definitions

let find_obj_decl ~(prog : Ail_tys.linked_program)
    (to_find : Cerb_frontend.Symbol.sym) =
  let to_find = resolve_sym ~prog to_find in
  List.find_map
    (fun (id, (_, _, decl)) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then
        match decl with
        | Cerb_frontend.AilSyntax.Decl_object (sto, al, quals, ty) ->
            Some (sto, al, quals, ty)
        | _ -> None
      else None)
    prog.sigma.declarations
