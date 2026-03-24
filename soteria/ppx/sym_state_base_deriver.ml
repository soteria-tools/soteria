open Ppxlib
open Ast_builder.Default

module Sym_state_base = struct
  type field = { name : string; mod_path : Longident.t; loc : Location.t }

  let lid ~loc txt = { loc; txt }
  let lident ~loc s = lid ~loc (Longident.Lident s)
  let liddot ~loc base name = lid ~loc (Longident.Ldot (base, name))
  let exprdot ~loc base name = pexp_ident ~loc (liddot ~loc base name)
  let err ~loc msg = Location.raise_errorf ~loc "[@@deriving sym_state] %s" msg

  let module_of_core_type = function
    | {
        ptyp_desc =
          Ptyp_constr
            ({ txt = Longident.Lident "option"; _ }, [ { ptyp_desc; _ } ]);
        _;
      } -> (
        match ptyp_desc with
        | Ptyp_constr ({ txt = Ldot (path, "t"); _ }, []) -> Some path
        | _ -> None)
    | _ -> None

  let module_of_core_type_exn (ct : core_type) =
    match module_of_core_type ct with
    | Some m -> m
    | None ->
        err ~loc:ct.ptyp_loc "expects record fields of type <Module>.t option"

  let fields_of_td_exn (td : type_declaration) =
    if td.ptype_name.txt <> "t" then
      err ~loc:td.ptype_name.loc "only supports type named 't'";
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
    | _ -> err ~loc:td.ptype_loc "only supports record types"

  let constr_name name = "Ser_" ^ name
  let fn_with_name name = "with_" ^ name
  let fn_with_sym_name name = "with_" ^ name ^ "_sym"

  let record_pat ~loc fields =
    ppat_record ~loc
      (List.map
         (fun (f : field) -> (lident ~loc f.name, pvar ~loc f.name))
         fields)
      Closed

  let record_expr ~loc fields =
    pexp_record ~loc
      (List.map
         (fun (f : field) -> (lident ~loc f.name, evar ~loc f.name))
         fields)
      None

  let serialized_ctor_decl ~loc (field : field) =
    let arg_ty =
      ptyp_constr ~loc (liddot ~loc field.mod_path "serialized") []
    in
    constructor_declaration ~loc
      ~name:{ loc; txt = constr_name field.name }
      ~args:(Pcstr_tuple [ arg_ty ]) ~res:None

  let serialized_type_item ~loc fields =
    let td =
      type_declaration ~loc
        ~name:{ loc; txt = "serialized" }
        ~params:[] ~cstrs:[]
        ~kind:(Ptype_variant (List.map (serialized_ctor_decl ~loc) fields))
        ~private_:Public ~manifest:None
    in
    pstr_type ~loc Recursive [ td ]

  let pp_serialized_item ~loc fields =
    let cases =
      List.map
        (fun (field : field) ->
          let ctor = lident ~loc (constr_name field.name) in
          let lhs = ppat_construct ~loc ctor (Some [%pat? v]) in
          let rhs =
            [%expr
              Fmt.pf ft "(@[<2>%s@ %a@])"
                [%e estring ~loc (constr_name field.name)]
                [%e exprdot ~loc field.mod_path "pp_serialized"]
                v]
          in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri let pp_serialized ft s = [%e pexp_match ~loc [%expr s] cases]]

  let show_serialized_item ~loc =
    [%stri let show_serialized s = Format.asprintf "%a" pp_serialized s]

  let pp_item ~loc fields =
    let field_printer (f : field) =
      let field_expr = pexp_field ~loc [%expr x] (lident ~loc f.name) in
      [%expr
        Format.fprintf fmt "@[%s =@ " [%e estring ~loc f.name];
        (match [%e field_expr] with
        | None -> Format.pp_print_string fmt "empty"
        | Some v -> [%e exprdot ~loc f.mod_path "pp"] fmt v);
        Format.fprintf fmt "@]"]
    in
    let body =
      match List.map field_printer fields with
      | [] -> [%expr ()]
      | hd :: tl ->
          List.fold_left
            (fun acc expr ->
              [%expr
                [%e acc];
                Format.fprintf fmt ";@ ";
                [%e expr]])
            hd tl
    in
    [%stri
      let pp fmt x =
        Format.fprintf fmt "@[<2>{ ";
        [%e body];
        Format.fprintf fmt "@ }@]"]

  let show_item ~loc = [%stri let show x = Format.asprintf "%a" pp x]

  let of_opt_item ~loc fields =
    let default_record =
      pexp_record ~loc
        (List.map
           (fun (f : field) -> (lident ~loc f.name, [%expr None]))
           fields)
        None
    in
    [%stri let of_opt = function None -> [%e default_record] | Some v -> v]

  let to_opt_item ~loc fields =
    let all_none_pat =
      ppat_record ~loc
        (List.map
           (fun (f : field) -> (lident ~loc f.name, [%pat? None]))
           fields)
        Closed
    in
    [%stri let to_opt = function [%p all_none_pat] -> None | t -> Some t]

  let empty_item ~loc = [%stri let empty = None]

  let sm_item ~loc symex_module =
    [%stri
      module SM =
        Soteria.Sym_states.State_monad.Make
          ([%m
          pmod_ident ~loc (lid ~loc symex_module)])
          (struct
            type nonrec t = t option
          end)]

  let serialize_item ~loc fields =
    (*
     * let serialize (st : t) : serialized list =
     *   (List.map (fun v -> Ser_field1 v) (Module1.serialize st.field1))
     *   @ (List.map (fun v -> Ser_field2 v) (Module2.serialize st.field2))
     *)
    let per_field (f : field) =
      let field_expr = pexp_field ~loc [%expr st] (lident ~loc f.name) in
      [%expr
        List.map
          (fun v ->
            [%e
              pexp_construct ~loc
                (lident ~loc (constr_name f.name))
                (Some [%expr v])])
          (Option.fold ~none:[]
             ~some:[%e exprdot ~loc f.mod_path "serialize"]
             [%e field_expr])]
    in
    let body =
      match List.map per_field fields with
      | [] -> [%expr []]
      | hd :: tl ->
          List.fold_left (fun acc e -> [%expr [%e acc] @ [%e e]]) hd tl
    in
    [%stri let serialize (st : t) : serialized list = [%e body]]

  let subst_serialized_item ~loc fields =
    (*
     * let subst_serialized subst_var (serialized : serialized) : serialized =
     *   match serialized with
     *   | Ser_field1 v -> Ser_field1 (subst_serialized subst_var v)
     *   | Ser_field2 v -> Ser_field2 (subst_serialized subst_var v)
     *)
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident ~loc (constr_name f.name) in
          let lhs = ppat_construct ~loc ctor (Some [%pat? v]) in
          let subst_ser = exprdot ~loc f.mod_path "subst_serialized" in
          let rhs =
            pexp_construct ~loc ctor (Some [%expr [%e subst_ser] subst_var v])
          in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri
      let subst_serialized subst_var (serialized : serialized) : serialized =
        [%e pexp_match ~loc [%expr serialized] cases]]

  let iter_vars_serialized_item ~loc fields =
    (*
     * let iter_vars_serialized (serialized : serialized) iter =
     *   match serialized with
     *   | Ser_field1 v -> iter_vars_serialized v iter
     *   | Ser_field2 v -> iter_vars_serialized v iter
     *)
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident ~loc (constr_name f.name) in
          let lhs = ppat_construct ~loc ctor (Some [%pat? v]) in
          let iter_vars = exprdot ~loc f.mod_path "iter_vars_serialized" in
          let rhs = [%expr [%e iter_vars] v iter] in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri
      let iter_vars_serialized (serialized : serialized) iter =
        [%e pexp_match ~loc [%expr serialized] cases]]

  let with_field_item ~loc fields (target : field) =
    (*
     * let with_field1 f =
     *   let open SM.Syntax in
     *   let* st_opt = SM.get_state () in
     *   let { field1; etc } = of_opt st_opt in
     *   let*^ res, field1 = f field1 in
     *   let+ () = SM.set_state (to_opt { field1; etc }) in
     *   Soteria.Symex.Compo_res.map_missing res
     *     (List.map (fun v -> Ser_field1 v))
     *)
    let st_pat = record_pat ~loc fields in
    let set_state_record = record_expr ~loc fields in
    let body =
      [%expr
        let open SM.Syntax in
        let* st_opt = SM.get_state () in
        let [%p st_pat] = of_opt st_opt in
        let*^ res, [%p pvar ~loc target.name] = f [%e evar ~loc target.name] in
        let+ () = SM.set_state (to_opt [%e set_state_record]) in
        Soteria.Symex.Compo_res.map_missing res
          (List.map (fun v ->
               [%e
                 pexp_construct ~loc
                   (lident ~loc (constr_name target.name))
                   (Some [%expr v])]))]
    in
    pstr_value ~loc Nonrecursive
      [
        value_binding ~loc
          ~pat:(pvar ~loc (fn_with_name target.name))
          ~expr:(pexp_fun ~loc Nolabel None (pvar ~loc "f") body);
      ]

  let with_field_sym_item ~loc fields (target : field) =
    (*
     * let with_field1_sym f =
     *   let open SM.Syntax in
     *   let* st_opt = SM.get_state () in
     *   let { field1; etc } = of_opt st_opt in
     *   let*^ res, field1 = f field1 in
     *   let+ () = SM.set_state (to_opt { field1; etc }) in
     *   res
     *)
    let st_pat = record_pat ~loc fields in
    let set_state_record = record_expr ~loc fields in
    let body =
      [%expr
        let open SM.Syntax in
        let* st_opt = SM.get_state () in
        let [%p st_pat] = of_opt st_opt in
        let*^ res, [%p pvar ~loc target.name] = f [%e evar ~loc target.name] in
        let+ () = SM.set_state (to_opt [%e set_state_record]) in
        res]
    in
    pstr_value ~loc Nonrecursive
      [
        value_binding ~loc
          ~pat:(pvar ~loc (fn_with_sym_name target.name))
          ~expr:(pexp_fun ~loc Nolabel None (pvar ~loc "f") body);
      ]

  let produce_item ~loc fields =
    (*
     * let produce (serialized : serialized) : unit SM.t =
     *   match serialized with
     *   | Ser_field1 v -> with_field1_sym (produce v)
     *   | Ser_field2 v -> with_field2_sym (produce v)
     *)
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident ~loc (constr_name f.name) in
          let lhs = ppat_construct ~loc ctor (Some [%pat? v]) in
          let produce = exprdot ~loc f.mod_path "produce" in
          let rhs =
            [%expr [%e evar ~loc (fn_with_sym_name f.name)] ([%e produce] v)]
          in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri
      let produce (serialized : serialized) : unit SM.t =
        [%e pexp_match ~loc [%expr serialized] cases]]

  let make_impl ~loc ~symex_module (td : type_declaration) =
    let fields = fields_of_td_exn td in
    [ sm_item ~loc symex_module ]
    @ [ pp_item ~loc fields; show_item ~loc ]
    @ [
        serialized_type_item ~loc fields;
        pp_serialized_item ~loc fields;
        show_serialized_item ~loc;
      ]
    @ [ of_opt_item ~loc fields; to_opt_item ~loc fields; empty_item ~loc ]
    @ [
        serialize_item ~loc fields;
        subst_serialized_item ~loc fields;
        iter_vars_serialized_item ~loc fields;
      ]
    @ List.map (with_field_item ~loc fields) fields
    @ List.map (with_field_sym_item ~loc fields) fields
    @ [ produce_item ~loc fields ]

  let make_intf ~loc ~symex_module:_ (_td : type_declaration) =
    [
      [%sigi: val pp : Format.formatter -> t -> unit];
      [%sigi: val show : t -> string];
      [%sigi: type serialized];
      [%sigi: val pp_serialized : Format.formatter -> serialized -> unit];
      [%sigi: val show_serialized : serialized -> string];
      [%sigi: val of_opt : t option -> t];
      [%sigi: val to_opt : t -> t option];
      [%sigi: val empty : t option];
      [%sigi: val serialize : t -> serialized list];
      [%sigi:
        val subst_serialized :
          (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized];
      [%sigi:
        val iter_vars_serialized :
          serialized -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit];
      [%sigi: val produce : serialized -> unit SM.t];
    ]

  let module_expr_as_longindent ~loc expr =
    match expr with
    | Some { pexp_desc = Pexp_construct ({ txt; _ }, None); _ } -> txt
    | Some expr -> err ~loc:expr.pexp_loc "expected { symex = <Module> }"
    | None ->
        err ~loc
          "requires a 'symex' argument specifying the symbolic execution \
           module to use"

  let str_type_decl ~loc ~path:_ (_rec, tds) symex_module =
    let symex_module = module_expr_as_longindent ~loc symex_module in
    match tds with
    | [ td ] -> make_impl ~loc ~symex_module td
    | _ -> err ~loc "expects exactly one type declaration"

  let sig_type_decl ~loc ~path:_ (_rec, tds) symex_module =
    let symex_module = module_expr_as_longindent ~loc symex_module in
    match tds with
    | [ td ] -> make_intf ~loc ~symex_module td
    | _ -> err ~loc "expects exactly one type declaration"

  let register () =
    let symex_arg = Deriving.Args.arg "symex" Ast_pattern.__ in
    let str_args = Deriving.Args.(empty +> symex_arg) in
    let sig_args = Deriving.Args.(empty +> symex_arg) in
    let str = Deriving.Generator.make str_args str_type_decl in
    let sig_ = Deriving.Generator.make sig_args sig_type_decl in
    Deriving.add "sym_state" ~str_type_decl:str ~sig_type_decl:sig_
    |> Deriving.ignore
end
