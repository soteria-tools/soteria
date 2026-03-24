open Ppxlib

module If_sat = struct
  module Branch_names = struct
    let then_name = "lname"
    let else_name = "rname"
    let branch_name = "name"

    let attribute_expr (attr : attribute) =
      match attr.attr_payload with
      | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> expr
      | _ ->
          Location.raise_errorf ~loc:attr.attr_name.loc
            "Invalid branch name attribute payload"
  end

  module Extension_name = struct
    type t = Sat | Sat1 | Sure

    let to_string = function Sat -> "sat" | Sat1 -> "sat1" | Sure -> "sure"
  end

  let get_attr ~name expr =
    List.find_opt (fun attr -> attr.attr_name.txt = name) expr.pexp_attributes

  let override_attr_opt ~name expr (new_attr : attribute option) =
    match new_attr with
    | None -> expr
    | Some new_attr -> (
        let existing_attr = get_attr ~name expr in
        match existing_attr with
        | Some _ ->
            Location.raise_errorf ~loc:new_attr.attr_name.loc
              "Branch name attribute specified multiple times"
        | None ->
            let new_attr =
              {
                new_attr with
                attr_name = { new_attr.attr_name with txt = name };
              }
            in
            { expr with pexp_attributes = new_attr :: expr.pexp_attributes })

  let associated_fn ~loc = function
    | Extension_name.Sat -> [%expr Symex_syntax.branch_on]
    | Sat1 -> [%expr Symex_syntax.branch_on_take_one]
    | Sure -> [%expr Symex_syntax.if_sure]

  let expand_if ~loc ~ext guard then_ else_ =
    let associated_fn = associated_fn ~loc ext in
    let lname =
      get_attr ~name:Branch_names.branch_name then_
      |> Option.fold ~some:Branch_names.attribute_expr
           ~none:[%expr Stdlib.String.cat "Left branch at " __LOC__]
    in
    let rname =
      get_attr ~name:Branch_names.branch_name else_
      |> Option.fold ~some:Branch_names.attribute_expr
           ~none:[%expr Stdlib.String.cat "Right branch at " __LOC__]
    in
    [%expr
      [%e associated_fn] [%e guard] ~left_branch_name:[%e lname]
        ~right_branch_name:[%e rname]
        ~then_:(fun () -> [%e then_])
        ~else_:(fun () -> [%e else_])]

  let expand ~ext expr =
    let loc = { expr.pexp_loc with loc_ghost = true } in
    let expansion =
      match expr with
      | [%expr if [%e? guard] then [%e? then_] else [%e? else_]] as whole_expr
        ->
          let then_ =
            get_attr ~name:Branch_names.then_name whole_expr
            |> override_attr_opt ~name:Branch_names.branch_name then_
          in
          let else_ =
            get_attr ~name:Branch_names.else_name whole_expr
            |> override_attr_opt ~name:Branch_names.branch_name else_
          in

          expand_if ~loc ~ext guard then_ else_
      | [%expr if [%e? _] then [%e? _]] ->
          Location.raise_errorf ~loc "'if%%%s' must include an else branch"
            (Extension_name.to_string ext)
      | _ -> Location.raise_errorf ~loc "%%sat can only be used with 'if'"
    in
    {
      expansion with
      pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
    }
end

module Sym_constants = struct
  let rewriter loc s =
    let i = int_of_string s in
    let ei = Ast_builder.Default.eint ~loc i in
    if i = 0 then [%expr Sym_int_syntax.zero ()]
    else if i = 1 then [%expr Sym_int_syntax.one ()]
    else [%expr Sym_int_syntax.mk_nonzero [%e ei]]

  let suffix = 's'
end

module Reversible = struct
  let has_ignore_attr attrs =
    List.exists
      (fun (attr : attribute) ->
        match attr.attr_name.txt with "reversible.ignore" -> true | _ -> false)
      attrs

  let strip_t = function Longident.Ldot (path, "t") -> Some path | _ -> None

  let module_of_core_type (ct : core_type) =
    match ct.ptyp_desc with
    | Ptyp_constr ({ txt; _ }, []) -> strip_t txt
    | _ -> None

  let module_of_core_type_exn (ct : core_type) =
    match module_of_core_type ct with
    | Some m -> m
    | None ->
        Location.raise_errorf ~loc:ct.ptyp_loc
          "[@@deriving reversible] expects fields/components to have type \
           <Module>.t"

  let module_call ~loc mod_path fn args =
    let open Ast_builder.Default in
    let id = pexp_ident ~loc { loc; txt = Longident.Ldot (mod_path, fn) } in
    List.fold_left
      (fun acc arg -> pexp_apply ~loc acc [ (Nolabel, arg) ])
      id args

  let module_call0 ~loc mod_path fn =
    module_call ~loc mod_path fn [ Ast_builder.Default.eunit ~loc ]

  let seq_of_exprs ~loc = function
    | [] -> [%expr ()]
    | hd :: tl ->
        List.fold_left
          (fun acc expr ->
            [%expr
              [%e acc];
              [%e expr]])
          hd tl

  let make_impl ~loc (td : type_declaration) =
    let open Ast_builder.Default in
    if td.ptype_name.txt <> "t" then
      Location.raise_errorf ~loc:td.ptype_name.loc
        "[@@deriving reversible] only supports type named 't'";
    let mk_record_impl labels =
      let fields =
        List.map
          (fun ld ->
            let mod_path = module_of_core_type_exn ld.pld_type in
            ( ld.pld_name.txt,
              mod_path,
              ld.pld_loc,
              has_ignore_attr ld.pld_attributes ))
          labels
      in
      let mk_init () =
        pexp_record ~loc
          (List.map
             (fun (name, mod_path, fld_loc, _) ->
               ( { loc = fld_loc; txt = Longident.Lident name },
                 module_call0 ~loc:fld_loc mod_path "init" ))
             fields)
          None
      in
      let call_on_fields fn args =
        fields
        |> List.filter (fun (_, _, _, ignore) -> not ignore)
        |> List.map (fun (name, mod_path, fld_loc, _) ->
            let state_field =
              pexp_field ~loc:fld_loc (evar ~loc "state")
                { loc = fld_loc; txt = Longident.Lident name }
            in
            module_call ~loc:fld_loc mod_path fn (state_field :: args))
        |> seq_of_exprs ~loc
      in
      (mk_init, call_on_fields)
    in
    let mk_tuple_impl tys =
      let fields =
        List.mapi
          (fun i ty ->
            let mod_path = module_of_core_type_exn ty in
            ( Printf.sprintf "x%d" i,
              mod_path,
              ty.ptyp_loc,
              has_ignore_attr ty.ptyp_attributes ))
          tys
      in
      let mk_init () =
        pexp_tuple ~loc
          (List.map
             (fun (_, mod_path, fld_loc, _) ->
               module_call0 ~loc:fld_loc mod_path "init")
             fields)
      in
      let call_on_fields fn args =
        let tuple_pats =
          List.map (fun (name, _, fld_loc, _) -> pvar ~loc:fld_loc name) fields
        in
        let body =
          fields
          |> List.filter (fun (_, _, _, ignore) -> not ignore)
          |> List.map (fun (name, mod_path, fld_loc, _) ->
              module_call ~loc:fld_loc mod_path fn
                (evar ~loc:fld_loc name :: args))
          |> seq_of_exprs ~loc
        in
        pexp_match ~loc (evar ~loc "state")
          [ case ~lhs:(ppat_tuple ~loc tuple_pats) ~guard:None ~rhs:body ]
      in
      (mk_init, call_on_fields)
    in
    let mk_init, call_on_fields =
      match td.ptype_kind with
      | Ptype_record labels -> mk_record_impl labels
      | Ptype_abstract -> (
          match td.ptype_manifest with
          | Some { ptyp_desc = Ptyp_tuple tys; _ } -> mk_tuple_impl tys
          | _ ->
              Location.raise_errorf ~loc:td.ptype_loc
                "[@@deriving reversible] only supports records and tuples")
      | _ ->
          Location.raise_errorf ~loc:td.ptype_loc
            "[@@deriving reversible] only supports records and tuples"
    in
    [
      [%stri let init () = [%e mk_init ()]];
      [%stri let save state = [%e call_on_fields "save" []]];
      [%stri
        let backtrack_n state n =
          [%e call_on_fields "backtrack_n" [ [%expr n] ]]];
      [%stri let reset state = [%e call_on_fields "reset" []]];
    ]

  let make_intf ~loc (_td : type_declaration) =
    [
      [%sigi: val init : unit -> t];
      [%sigi: val save : t -> unit];
      [%sigi: val backtrack_n : t -> int -> unit];
      [%sigi: val reset : t -> unit];
    ]

  let str_type_decl ~loc ~path:_ (_rec, tds) =
    match tds with
    | [ td ] -> make_impl ~loc td
    | _ ->
        Location.raise_errorf ~loc
          "[@@deriving reversible] expects exactly one type declaration"

  let sig_type_decl ~loc ~path:_ (_rec, tds) =
    match tds with
    | [ td ] -> make_intf ~loc td
    | _ ->
        Location.raise_errorf ~loc
          "[@@deriving reversible] expects exactly one type declaration"

  let register () =
    let open Ppxlib in
    Deriving.add "reversible"
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
    |> Deriving.ignore
end
