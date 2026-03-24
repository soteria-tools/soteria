open Ppxlib

module Sym_state_base = struct
  type field = {
    name : string;
    mod_path : Longident.t;
    loc : Location.t;
  }

  let lid ~loc txt = { loc; txt }

  let lident ~loc s = lid ~loc (Longident.Lident s)

  let strip_t = function Longident.Ldot (path, "t") -> Some path | _ -> None

  let module_of_core_type = function
    | {
        ptyp_desc =
          Ptyp_constr
            ({ txt = Longident.Lident "option"; _ }, [ { ptyp_desc; _ } ]);
        _;
      } -> (
        match ptyp_desc with
        | Ptyp_constr ({ txt; _ }, []) -> strip_t txt
        | _ -> None)
    | _ -> None

  let module_of_core_type_exn (ct : core_type) =
    match module_of_core_type ct with
    | Some m -> m
    | None ->
        Location.raise_errorf ~loc:ct.ptyp_loc
          "[@@deriving sym_state_base] expects record fields of type \
           <Module>.t option"

  let fields_of_td_exn (td : type_declaration) =
    if td.ptype_name.txt <> "t" then
      Location.raise_errorf ~loc:td.ptype_name.loc
        "[@@deriving sym_state_base] only supports type named 't'";
    match td.ptype_kind with
    | Ptype_record labels ->
        List.map
          (fun ld ->
            {
              name = ld.pld_name.txt;
              mod_path = module_of_core_type_exn ld.pld_type;
              loc = ld.pld_loc;
            })
          labels
    | _ ->
        Location.raise_errorf ~loc:td.ptype_loc
          "[@@deriving sym_state_base] only supports record types"

  let module_call ~loc mod_path fn args =
    let open Ast_builder.Default in
    let id = pexp_ident ~loc (lid ~loc (Longident.Ldot (mod_path, fn))) in
    List.fold_left (fun acc arg -> pexp_apply ~loc acc [ (Nolabel, arg) ]) id args

  let constr_name name = "Ser_" ^ name

  let serialized_ctor_decl ~loc (field : field) =
    let open Ast_builder.Default in
    let arg_ty =
      ptyp_constr ~loc (lid ~loc (Longident.Ldot (field.mod_path, "serialized")))
        []
    in
    constructor_declaration ~loc ~name:{ loc; txt = constr_name field.name }
      ~args:(Pcstr_tuple [ arg_ty ]) ~res:None

  let serialized_type_item ~loc fields =
    let open Ast_builder.Default in
    let td =
      type_declaration ~loc ~name:{ loc; txt = "serialized" }
        ~params:[]
        ~cstrs:[]
        ~kind:(Ptype_variant (List.map (serialized_ctor_decl ~loc) fields))
        ~private_:Public
        ~manifest:
          (Some
             (ptyp_constr ~loc
                (lid ~loc (Longident.Ldot (Longident.Lident "State_intf", "serialized")))
                []))
    in
    pstr_type ~loc Recursive [ td ]

  let pp_serialized_item ~loc fields =
    let open Ast_builder.Default in
    let cases =
      List.map
        (fun (field : field) ->
          let v = pvar ~loc "v" in
          let ctor = lident ~loc (constr_name field.name) in
          let lhs = ppat_construct ~loc ctor (Some v) in
          let rhs =
            [%expr
              Fmt.pf ft "@[<2>(%s@ %a)@]" [%e estring ~loc (constr_name field.name)]
                [%e
                  pexp_ident ~loc
                    (lid ~loc (Longident.Ldot (field.mod_path, "pp_serialized")))]
                v]
          in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri let pp_serialized ft s = [%e pexp_match ~loc [%expr s] cases]]

  let show_serialized_item ~loc =
    [%stri let show_serialized s = Format.asprintf "%a" pp_serialized s]

  let mk_none_pat ~loc = Ast_builder.Default.ppat_construct ~loc (lident ~loc "None") None
  let mk_none_expr ~loc = Ast_builder.Default.pexp_construct ~loc (lident ~loc "None") None
  let mk_some_expr ~loc e = Ast_builder.Default.pexp_construct ~loc (lident ~loc "Some") (Some e)

  let of_opt_item ~loc fields =
    let open Ast_builder.Default in
    let default_record =
      pexp_record ~loc
        (List.map (fun (f : field) -> (lident ~loc f.name, mk_none_expr ~loc)) fields)
        None
    in
    [%stri
      let of_opt = function
        | None -> [%e default_record]
        | Some v -> v]

  let to_opt_item ~loc fields =
    let open Ast_builder.Default in
    let all_none_pat =
      ppat_record ~loc
        (List.map (fun (f : field) -> (lident ~loc f.name, mk_none_pat ~loc)) fields)
        Closed
    in
    [%stri
      let to_opt = function
        | [%p all_none_pat] -> None
        | t -> Some t]

  let empty_item ~loc = [%stri let empty = None]

  let sm_item ~loc symex_module =
    let open Ast_builder.Default in
    let make_path =
      lid ~loc
        (Longident.Ldot
           ( Longident.Ldot
               ( Longident.Ldot (Longident.Lident "Soteria", "Sym_states"),
                 "State_monad" ),
             "Make" ))
    in
    let state_mod =
      pmod_structure ~loc
        [
          pstr_type ~loc Nonrecursive
            [
              type_declaration ~loc ~name:{ loc; txt = "t" } ~params:[]
                ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
                ~manifest:(Some [%type: t option]);
            ];
        ]
    in
    pstr_module ~loc
      (module_binding ~loc ~name:{ loc; txt = Some "SM" }
         ~expr:
           (pmod_apply ~loc
              (pmod_apply ~loc (pmod_ident ~loc make_path)
                 (pmod_ident ~loc (lid ~loc symex_module)))
              state_mod))

  let serialize_item ~loc fields =
    let open Ast_builder.Default in
    let per_field (f : field) =
      let field_expr =
        pexp_field ~loc [%expr st] (lident ~loc f.name)
      in
      let fold_expr =
        [%expr
          Option.fold ~none:[]
            ~some:[%e pexp_ident ~loc (lid ~loc (Longident.Ldot (f.mod_path, "serialize")))]
            [%e field_expr]]
      in
      [%expr
        List.map
          (fun v -> [%e pexp_construct ~loc (lident ~loc (constr_name f.name)) (Some [%expr v])])
          [%e fold_expr]]
    in
    let body =
      match List.map per_field fields with
      | [] -> [%expr []]
      | hd :: tl -> List.fold_left (fun acc e -> [%expr [%e acc] @ [%e e]]) hd tl
    in
    [%stri let serialize (st : t) : serialized list = [%e body]]

  let subst_serialized_item ~loc fields =
    let open Ast_builder.Default in
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident ~loc (constr_name f.name) in
          let lhs = ppat_construct ~loc ctor (Some (pvar ~loc "v")) in
          let rhs =
            [%expr
              [%e pexp_construct ~loc ctor
                     (Some (module_call ~loc f.mod_path "subst_serialized" [ [%expr subst_var]; [%expr v] ]))]]
          in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri
      let subst_serialized subst_var (serialized : serialized) : serialized =
        [%e pexp_match ~loc [%expr serialized] cases]]

  let iter_vars_serialized_item ~loc fields =
    let open Ast_builder.Default in
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident ~loc (constr_name f.name) in
          let lhs = ppat_construct ~loc ctor (Some (pvar ~loc "v")) in
          let rhs = module_call ~loc f.mod_path "iter_vars_serialized" [ [%expr v]; [%expr iter] ] in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri
      let iter_vars_serialized (serialized : serialized) iter =
        [%e pexp_match ~loc [%expr serialized] cases]]

  let with_field_item ~loc fields (target : field) =
    let open Ast_builder.Default in
    let fn_name = "with_" ^ target.name in
    let st_pat =
      ppat_record ~loc
        (List.map (fun (f : field) -> (lident ~loc f.name, pvar ~loc f.name)) fields)
        Closed
    in
    let set_state_record =
      pexp_record ~loc
        (List.map (fun (f : field) -> (lident ~loc f.name, evar ~loc f.name)) fields)
        None
    in
    let mapped_fix_expr =
      [%expr
        Soteria.Symex.Compo_res.map_missing res (fun fix ->
            List.map
              (fun v ->
                [%e
                  pexp_construct ~loc
                    (lident ~loc (constr_name target.name))
                    (Some [%expr v])])
              fix)]
    in
    let body =
      [%expr
        let open SM.Syntax in
        let* st_opt = SM.get_state () in
        let [%p st_pat] = of_opt st_opt in
        let*^ res, [%p pvar ~loc target.name] = f [%e evar ~loc target.name] in
        let+ () = SM.set_state (to_opt [%e set_state_record]) in
        [%e mapped_fix_expr]]
    in
    pstr_value ~loc Nonrecursive
      [
        value_binding ~loc ~pat:(pvar ~loc fn_name)
          ~expr:(pexp_fun ~loc Nolabel None (pvar ~loc "f") body);
      ]

  let make_impl ~loc ~symex_module (td : type_declaration) =
    let fields = fields_of_td_exn td in
    [ sm_item ~loc symex_module ]
    @ [ serialized_type_item ~loc fields; pp_serialized_item ~loc fields; show_serialized_item ~loc ]
    @ [ of_opt_item ~loc fields; to_opt_item ~loc fields; empty_item ~loc ]
    @ [ serialize_item ~loc fields; subst_serialized_item ~loc fields; iter_vars_serialized_item ~loc fields ]
    @ List.map (with_field_item ~loc fields) fields

  let make_intf ~loc ~symex_module:_ (_td : type_declaration) =
    [
      [%sigi: type serialized];
      [%sigi: val pp_serialized : Format.formatter -> serialized -> unit];
      [%sigi: val show_serialized : serialized -> string];
      [%sigi: val of_opt : t option -> t];
      [%sigi: val to_opt : t -> t option];
      [%sigi: val empty : t option];
      [%sigi: val serialize : t -> serialized list];
      [%sigi: val subst_serialized : (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized];
      [%sigi: val iter_vars_serialized : serialized -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit];
    ]

  let str_type_decl ~loc ~path:_ (_rec, tds) symex_module =
    let symex_module =
      match symex_module with
      | None -> Longident.Lident "Csymex"
      | Some ({ pexp_desc = Pexp_ident { txt; _ }; _ } : expression) -> txt
      | Some ({ pexp_desc = Pexp_construct ({ txt; _ }, None); _ } : expression)
        -> txt
      | Some expr ->
          Location.raise_errorf ~loc:expr.pexp_loc
            "[sym_state_base] expected { symex = <Module> }"
    in
    match tds with
    | [ td ] -> make_impl ~loc ~symex_module td
    | _ ->
        Location.raise_errorf ~loc
          "[@@deriving sym_state_base] expects exactly one type declaration"

  let sig_type_decl ~loc ~path:_ (_rec, tds) symex_module =
    let symex_module =
      match symex_module with
      | None -> Longident.Lident "Csymex"
      | Some ({ pexp_desc = Pexp_ident { txt; _ }; _ } : expression) -> txt
      | Some ({ pexp_desc = Pexp_construct ({ txt; _ }, None); _ } : expression)
        -> txt
      | Some expr ->
          Location.raise_errorf ~loc:expr.pexp_loc
            "[sym_state_base] expected { symex = <Module> }"
    in
    match tds with
    | [ td ] -> make_intf ~loc ~symex_module td
    | _ ->
        Location.raise_errorf ~loc
          "[@@deriving sym_state_base] expects exactly one type declaration"

  let register () =
    let symex_arg = Deriving.Args.arg "symex" Ast_pattern.__ in
    let str_args = Deriving.Args.(empty +> symex_arg) in
    let sig_args = Deriving.Args.(empty +> symex_arg) in
    let str = Deriving.Generator.make str_args str_type_decl in
    let sig_ = Deriving.Generator.make sig_args sig_type_decl in
    Deriving.add "sym_state_base" ~str_type_decl:str ~sig_type_decl:sig_
    |> Deriving.ignore;
    Deriving.add "soteria.sym_state_base" ~str_type_decl:str ~sig_type_decl:sig_
    |> Deriving.ignore
end
