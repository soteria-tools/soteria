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
  let id = pexp_ident (wloc (Ldot (mod_path, fn))) in
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

let mk_record_impl ~loc labels =
  let fields =
    labels
    |> List.map (fun ld ->
        mk_field ld.pld_name.txt ld.pld_type ld.pld_loc
          (Attribute.get ignore_label_decl_attr ld))
  in
  if List.for_all (fun (_, _, _, i) -> i) fields then
    Location.raise_errorf ~loc
      "[@@deriving %s] cannot be used on a record where all fields are ignored"
      Names.ppx;
  let arg_pattern () = [%pat? state] in
  let mk_init () =
    fields
    |> map_fields (fun ((name, _, _, _) as f) ->
        (wloc (Lident name), init_of_field f))
    |> Fun.flip pexp_record None
  in
  let call_on_fields fn args =
    fields
    |> List.filter (fun (_, _, _, ignore) -> not ignore)
    |> map_fields (fun (name, mod_path, _, _) ->
        let state_field = pexp_field (evar "state") (wloc (Lident name)) in
        module_call mod_path fn (state_field :: args))
    |> seq_of_exprs ~loc
  in
  (mk_init, arg_pattern, call_on_fields)

let mk_tuple_impl ~loc tys =
  let fields =
    tys
    |> List.mapi (fun i ty ->
        mk_field (Printf.sprintf "x%d" i) ty ty.ptyp_loc
          (Attribute.get ignore_core_type_attr ty))
  in
  if List.for_all (fun (_, _, _, i) -> i) fields then
    Location.raise_errorf ~loc
      "[@@deriving %s] cannot be used on a record where all fields are ignored"
      Names.ppx;
  let arg_pattern () =
    fields
    |> map_fields (fun (name, _, _, ignore) ->
        if ignore then ppat_any () else pvar name)
    |> ppat_tuple
  in
  let mk_init () = fields |> map_fields init_of_field |> pexp_tuple in
  let call_on_fields fn args =
    fields
    |> List.filter (fun (_, _, _, ignore) -> not ignore)
    |> map_fields (fun (name, mod_path, _, _) ->
        module_call mod_path fn (evar name :: args))
    |> seq_of_exprs ~loc
  in
  (mk_init, arg_pattern, call_on_fields)

let make_impl ~loc (td : type_declaration) =
  let@ loc = with_loc loc in
  if td.ptype_name.txt <> "t" then
    Location.raise_errorf ~loc:td.ptype_name.loc
      "[@@deriving %s] only supports type named 't'" Names.ppx;
  let mk_init, arg_pat, call =
    match (td.ptype_kind, td.ptype_manifest) with
    | Ptype_record labels, _ -> mk_record_impl ~loc labels
    | Ptype_abstract, Some { ptyp_desc = Ptyp_tuple tys; _ } ->
        mk_tuple_impl ~loc tys
    | _ ->
        Location.raise_errorf ~loc:td.ptype_loc
          "[@@deriving %s] only supports records and tuples" Names.ppx
  in
  [
    [%stri let init () = [%e mk_init ()]];
    [%stri let save [%p arg_pat ()] = [%e call "save" []]];
    [%stri
      let backtrack_n [%p arg_pat ()] n = [%e call "backtrack_n" [ [%expr n] ]]];
    [%stri let reset [%p arg_pat ()] = [%e call "reset" []]];
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
