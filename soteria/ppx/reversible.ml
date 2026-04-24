open Ppxlib
open Util.Syntaxes
open Util.LocCtx

module Names = struct
  let ppx = "reversible"
  let ignore_attr = "soteria.reversible.ignore"
end

let mk_ignore_attr ctx =
  Attribute.declare Names.ignore_attr ctx Ast_pattern.(pstr nil) ()

let ignore_label_decl_attr = mk_ignore_attr Attribute.Context.label_declaration
let ignore_core_type_attr = mk_ignore_attr Attribute.Context.core_type

let module_of_core_type_exn (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Ldot (path, "t"); _ }, []) -> path
  | _ ->
      Location.raise_errorf ~loc:ct.ptyp_loc
        "[@@deriving %s] expects fields/components to have type <Module>.t"
        Names.ppx

let module_call mod_path fn args =
  let id = pexp_ident_dot mod_path fn in
  pexp_apply id (List.map (fun arg -> (Nolabel, arg)) args)

let seq_of_exprs ~loc = function
  | [] -> [%expr ()]
  | hd :: tl ->
      List.fold_left
        (fun acc expr ->
          [%expr
            [%e acc];
            [%e expr]])
        hd tl

let map_fields f =
  List.map (fun ((_, _, loc, _) as field) -> with_loc loc (fun _ -> f field))

let mk_field name ty loc ignored =
  let mod_path = module_of_core_type_exn ty in
  let is_ignored = Option.is_some ignored in
  (name, mod_path, loc, is_ignored)

let init_of_field (_, mod_path, _, _) = module_call mod_path "init" [ eunit () ]

let mk_record_impl labels =
  let fields =
    labels
    |> List.map (fun ld ->
        mk_field ld.pld_name.txt ld.pld_type ld.pld_loc
          (Attribute.get ignore_label_decl_attr ld))
  in
  let arg_pattern = pvar "state" in
  let init =
    fields
    |> map_fields (fun ((name, _, _, _) as f) -> (lident name, init_of_field f))
    |> Fun.flip pexp_record None
  in
  let access_field (name, _, _, _) = pexp_field (evar "state") (lident name) in
  (fields, init, arg_pattern, access_field)

let mk_tuple_impl tys =
  let fields =
    tys
    |> List.mapi (fun i ty ->
        mk_field (Printf.sprintf "x%d" i) ty ty.ptyp_loc
          (Attribute.get ignore_core_type_attr ty))
  in
  let arg_pattern =
    fields
    |> map_fields (fun (name, _, _, ignore) ->
        if ignore then ppat_any () else pvar name)
    |> ppat_tuple
  in
  let init = fields |> map_fields init_of_field |> pexp_tuple in
  let access_field (name, _, _, _) = evar name in
  (fields, init, arg_pattern, access_field)

let make_impl ~loc (td : type_declaration) =
  let@ loc = with_loc loc in
  if td.ptype_name.txt <> "t" then
    Location.raise_errorf ~loc:td.ptype_name.loc
      "[@@deriving %s] only supports type named 't'" Names.ppx;
  let fields, init, arg, access =
    match (td.ptype_kind, td.ptype_manifest) with
    | Ptype_record labels, _ -> mk_record_impl labels
    | Ptype_abstract, Some { ptyp_desc = Ptyp_tuple tys; _ } ->
        mk_tuple_impl tys
    | _ ->
        Location.raise_errorf ~loc:td.ptype_loc
          "[@@deriving %s] only supports records and tuples" Names.ppx
  in
  let call fn args =
    fields
    |> List.filter (fun (_, _, _, ignore) -> not ignore)
    |> map_fields (fun ((_, mod_path, _, _) as field) ->
        module_call mod_path fn (access field :: args))
    |> seq_of_exprs ~loc
  in
  let all_ignored = List.for_all (fun (_, _, _, ignored) -> ignored) fields in
  let opt_arg p = if all_ignored then ppat_any () else p in
  [
    [%stri let init () = [%e init]];
    [%stri let save [%p opt_arg arg] = [%e call "save" []]];
    [%stri
      let backtrack_n [%p opt_arg arg] [%p opt_arg [%pat? n]] =
        [%e call "backtrack_n" [ [%expr n] ]]];
    [%stri let reset [%p opt_arg arg] = [%e call "reset" []]];
  ]

let str_type_decl ~loc ~path:_ (_rec, tds) =
  match tds with
  | [ td ] -> make_impl ~loc td
  | _ ->
      Location.raise_errorf ~loc
        "[@@deriving %s] expects exactly one type declaration" Names.ppx

let register () =
  let open Ppxlib in
  let str = Deriving.Generator.make_noarg str_type_decl in
  Deriving.add Names.ppx ~str_type_decl:str |> Deriving.ignore
