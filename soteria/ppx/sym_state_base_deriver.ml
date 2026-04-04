open Ppxlib
open Ast_builder.Default
open Util.Syntaxes
open Util.LocCtx

module Sym_state_base = struct
  type context_cfg = { field : string }
  type field_kind = Managed of Longident.t | Ignored of expression

  type field = {
    name : string;
    kind : field_kind;
    context : context_cfg option;
    loc : Location.t;
  }

  let err ?loc msg =
    let loc = match loc with Some l -> l | None -> get_loc () in
    Location.raise_errorf ~loc "[@@deriving sym_state] %s" msg

  let is_managed (f : field) =
    match f.kind with Managed _ -> true | Ignored _ -> false

  let is_ignored (f : field) =
    match f.kind with Managed _ -> false | Ignored _ -> true

  let as_managed (f : field) =
    match f.kind with
    | Managed m -> m
    | Ignored _ -> err "internal: expected managed field"

  let as_ignored (f : field) =
    match f.kind with
    | Managed _ -> err "internal: expected ignored field"
    | Ignored e -> e

  let managed_fields = List.filter is_managed
  let ignored_fields = List.filter is_ignored
  let syn_name name = "Ser_" ^ name
  let lift_fix_name name = "lift_" ^ name ^ "_fixes"
  let fn_with_name name = "with_" ^ name
  let fn_with_sym_name name = "with_" ^ name ^ "_sym"
  let ignore_attr = "sym_state.ignore"
  let context_attr = "sym_state.context"
  let lident s = wloc (Longident.Lident s)
  let liddot base name = wloc (Longident.Ldot (base, name))

  let liddots base path =
    wloc
    @@ List.fold_left (fun acc name -> Longident.Ldot (acc, name)) base path

  let pexp_ident_dot base name = pexp_ident (liddot base name)
  let pexp_ident_dots base name = pexp_ident (liddots base name)

  let symex_ty symex_module path args =
    ptyp_constr (liddots symex_module path) args

  (** For a field Foo, creates pattern [Ser_foo(v)] *)
  let ppat_field field =
    let loc = get_loc () in
    ppat_construct (lident (syn_name field.name)) (Some [%pat? v])

  (** For a field Foo and expression e, creates expression [Ser_foo(e)] *)
  let constr_field field expr =
    pexp_construct (lident (syn_name field.name)) (Some expr)

  let module_expr_as_longindent ~err_msg expr =
    match expr with
    | Some { pexp_desc = Pexp_construct ({ txt; _ }, None); _ } -> txt
    | _ -> err err_msg

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
    let@ () = with_loc ct.ptyp_loc in
    match module_of_core_type ct with
    | Some m -> m
    | None -> err "expects record fields of type <Module>.t option"

  let ignored_empty_of_attr_exn (attr : attribute) =
    let@ () = with_loc attr.attr_loc in
    let bad () = err "expects [@sym_state.ignore { empty = <expr> }]" in
    match attr.attr_payload with
    | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> (
        match expr.pexp_desc with
        | Pexp_record (fields, None) -> (
            match
              List.find_opt
                (fun ({ txt; _ }, _) -> txt = Longident.Lident "empty")
                fields
            with
            | Some (_, e) -> e
            | None -> bad ())
        | _ -> bad ())
    | _ -> bad ()

  let find_record_field_expr name fields =
    fields
    |> List.find_map (fun ({ txt; _ }, e) ->
        match txt with
        | Longident.Lident n when String.equal n name -> Some e
        | _ -> None)

  let ident_name_exn ~err_msg = function
    | Some
        {
          pexp_desc =
            ( Pexp_ident { txt = Lident s; _ }
            | Pexp_construct ({ txt = Lident s; _ }, None) );
          _;
        } ->
        s
    | _ -> err err_msg

  let context_cfg_of_attr_exn (attr : attribute) =
    let@ () = with_loc attr.attr_loc in
    let err_msg = Fmt.str "expected [@%s { field = <field> }]" context_attr in
    let err () = err err_msg in
    match attr.attr_payload with
    | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> (
        match expr.pexp_desc with
        | Pexp_record (fields, None) ->
            let field =
              find_record_field_expr "field" fields |> ident_name_exn ~err_msg
            in
            { field }
        | _ -> err ())
    | _ -> err ()

  let validate_context_attr (fields : field list) =
    fields
    |> List.iter @@ fun f ->
       let@ () = with_loc f.loc in
       match (f.kind, f.context) with
       | Ignored _, Some _ ->
           err (context_attr ^ " cannot be applied to ignored fields")
       | Managed _, Some { field; _ } ->
           let ctx_field =
             match List.find_opt (fun f -> f.name = field) fields with
             | Some f -> f
             | None ->
                 let valid_fields = List.map (fun f -> f.name) fields in
                 Fmt.kstr (err ?loc:None)
                   "%s references non-existent field '%s', expected one of %a"
                   context_attr field
                   Fmt.(list ~sep:Fmt.comma Fmt.string)
                   valid_fields
           in
           if ctx_field.name = f.name then
             err (context_attr ^ " field cannot reference itself");
           if is_ignored ctx_field then
             err (context_attr ^ " field cannot reference an ignored field")
       | _, None -> ()

  let check_no_extra_attrs (ld : label_declaration) =
    let allowed = [ ignore_attr; context_attr ] in
    ld.pld_attributes
    |> List.iter @@ fun (attr : attribute) ->
       let@ () = with_loc attr.attr_loc in
       if not (List.exists (String.equal attr.attr_name.txt) allowed) then
         let rec pp_attrs ?(acc = "") = function
           | [] -> acc
           | [ a ] -> acc ^ "and [@" ^ a ^ "]"
           | a :: rest -> pp_attrs ~acc:(acc ^ "[@" ^ a ^ "], ") rest
         in
         err ("only supports attributes " ^ pp_attrs allowed)

  let context_cfg_of_label (ld : label_declaration) =
    ld.pld_attributes
    |> List.find_opt (fun (attr : attribute) ->
        String.equal attr.attr_name.txt context_attr)
    |> Option.map context_cfg_of_attr_exn

  let ignored_empty_expr (ld : label_declaration) =
    ld.pld_attributes
    |> List.find_opt (fun (attr : attribute) ->
        String.equal attr.attr_name.txt ignore_attr)
    |> Option.map ignored_empty_of_attr_exn

  let fields_of_td_exn (td : type_declaration) =
    let@ () = with_loc td.ptype_loc in
    if td.ptype_name.txt <> "t" then
      err ~loc:td.ptype_name.loc "only supports type named 't'";
    match td.ptype_kind with
    | Ptype_record labels ->
        List.map
          (fun ld ->
            check_no_extra_attrs ld;
            let kind =
              match ignored_empty_expr ld with
              | Some e -> Ignored e
              | None -> Managed (module_of_core_type_exn ld.pld_type)
            in
            {
              name = ld.pld_name.txt;
              kind;
              context = context_cfg_of_label ld;
              loc = ld.pld_loc;
            })
          labels
    | _ -> err "only supports record types"

  let syn_type_item fields =
    let syn_ctor_decl (field : field) =
      let mod_path = as_managed field in
      let arg_ty = symex_ty mod_path [ "syn" ] [] in
      constructor_declaration ~name:(syn_name field.name)
        ~args:(Pcstr_tuple [ arg_ty ]) ~res:None
    in
    let fields = managed_fields fields in
    let td =
      type_declaration ~name:"syn" ~params:[] ~cstrs:[]
        ~kind:(Ptype_variant (List.map syn_ctor_decl fields))
        ~private_:Public ~manifest:None
    in
    pstr_type Recursive [ td ]

  let pp_syn_item ~loc fields =
    let cases =
      List.map
        (fun (field : field) ->
          let mod_path = as_managed field in
          let lhs = ppat_field field in
          let rhs =
            [%expr
              Fmt.pf ft "(@[<2>%s@ %a@])"
                [%e estring (syn_name field.name)]
                [%e pexp_ident_dot mod_path "pp_syn"]
                v]
          in
          case ~lhs ~guard:None ~rhs)
        (managed_fields fields)
    in
    [%stri let pp_syn ft s = [%e pexp_match [%expr s] cases]]

  let show_syn_item ~loc =
    [%stri let show_syn s = Format.asprintf "%a" pp_syn s]

  let pp_item ~loc fields =
    let field_printer (f : field) =
      let field_expr = pexp_field [%expr x] (lident f.name) in
      match f.kind with
      | Managed mod_path ->
          [%expr
            Format.fprintf fmt "@[%s =@ " [%e estring f.name];
            (match [%e field_expr] with
            | None -> Format.pp_print_string fmt "empty"
            | Some v -> [%e pexp_ident_dot mod_path "pp"] fmt v);
            Format.fprintf fmt "@]"]
      | Ignored _ ->
          [%expr Format.fprintf fmt "@[%s =@ <ignored>@]" [%e estring f.name]]
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
      pexp_record
        (List.map
           (fun (f : field) ->
             let empty =
               match f.kind with Managed _ -> [%expr None] | Ignored e -> e
             in
             (lident f.name, empty))
           fields)
        None
    in
    [%stri let of_opt = function None -> [%e default_record] | Some v -> v]

  let to_opt_item ~loc fields =
    (*
     * let to_opt = function
     *   | { field1 = None; field2 = None; ... } -> None
     *   | t -> Some t
     *
     * IF NO IGNORED FIELDS, otherwise
     * let to_opt = function
     *   | { field1 = None; field2 = None; ... } when <ignored_field1> = <empty1> && ... -> None
     *   | t -> Some t
     *)
    let all_none_pat =
      ppat_record
        (List.map
           (fun (f : field) ->
             let p =
               match f.kind with
               | Managed _ -> [%pat? None]
               | Ignored _ -> ppat_var (wloc f.name)
             in
             (lident f.name, p))
           fields)
        Closed
    in
    match ignored_fields fields with
    | [] ->
        [%stri let to_opt = function [%p all_none_pat] -> None | t -> Some t]
    | hd :: tl ->
        let is_emp f = [%expr [%e evar f.name] = [%e as_ignored f]] in
        let all_ignored_are_emp =
          List.fold_left
            (fun acc f -> [%expr [%e acc] && [%e is_emp f]])
            (is_emp hd) tl
        in
        [%stri
          let to_opt = function
            | [%p all_none_pat] when [%e all_ignored_are_emp] -> None
            | t -> Some t]

  let empty_item ~loc = [%stri let empty = None]

  let sm_item ~loc symex_module =
    let symex_module = pmod_ident (wloc symex_module) in
    [%stri
      module SM =
        Soteria.Sym_states.State_monad.Make
          ([%m
          symex_module])
          (struct
            type nonrec t = t option
          end)]

  let to_syn_item ~loc fields =
    let fields = managed_fields fields in
    (*
     * let to_syn (st : t) : syn list =
     *   (List.map (fun v -> Ser_field1 v)
     *     (Option.fold ~none:[] ~some:Module1.to_syn st.field1))
     *   @ (List.map (fun v -> Ser_field2 v)
     *     (Option.fold ~none:[] ~some:Module2.to_syn st.field2))
     *)
    let per_field (f : field) =
      let field_expr = pexp_field [%expr st] (lident f.name) in
      [%expr
        List.map
          (fun v -> [%e constr_field f [%expr v]])
          (Option.fold ~none:[]
             ~some:[%e pexp_ident_dot (as_managed f) "to_syn"]
             [%e field_expr])]
    in
    let body =
      match List.map per_field fields with
      | [] -> [%expr []]
      | hd :: tl ->
          List.fold_left (fun acc e -> [%expr [%e acc] @ [%e e]]) hd tl
    in
    [%stri let to_syn (st : t) : syn list = [%e body]]

  let ins_outs_item ~loc fields =
    let fields = managed_fields fields in
    (*
     * let ins_outs_item = function
     *   | Ser_field1 v -> Module1.ins_outs v
     *   | Ser_field2 v -> Module2.ins_outs v
     *)
    let cases =
      List.map
        (fun (f : field) ->
          let ins_outs = pexp_ident_dot (as_managed f) "ins_outs" in
          let lhs = ppat_field f in
          let rhs = [%expr [%e ins_outs] v] in
          case ~lhs ~guard:None ~rhs)
        fields
    in
    [%stri let ins_outs (syn : syn) = [%e pexp_match [%expr syn] cases]]

  let lift_syn_fix_item (target : field) =
    (*
     * ONLY MANAGED FIELDS:
     * let lift_field1_fixes = List.map (fun v -> Ser_field1 v)
     *)
    let loc = target.loc in
    [%stri
      let [%p pvar (lift_fix_name target.name)] =
        List.map (fun v -> [%e constr_field target [%expr v]])]

  let with_field_sym_item fields (target : field) =
    (*
     * DEFAULT:
     * let with_field1_sym f =
     *   let open SM.Syntax in
     *   let* st_opt = SM.get_state () in
     *   let st = of_opt st_opt in
     *   let { field1; _ } = st in
     *   let*^ res, field1 = f field1 in
     *   let+ () = SM.set_state (to_opt { st with field1 }) in
     *   res
     *
     * IF CONTEXT:
     * ...
     * let*^ (res, field1), ctx_field =
     *  CtxField.SM.run_with_state ~state:st.ctx_field (f field1)
     * in
     * let+ () = SM.set_state (to_opt { st with field1; ctx_field }) in
     * ...
     *
     * IF IGNORED:
     * ...
     * let**^ res, field1 = f field1 in
     * let+ () = SM.set_state (to_opt st) in
     * Soteria.Symex.Compo_res.Ok res
     *)
    let loc = target.loc in
    let updated_fields =
      match target.context with
      | None -> [ target.name ]
      | Some { field; _ } -> [ target.name; field ]
    in
    let open_pat = List.compare_lengths updated_fields fields <> 0 in
    let st_pat =
      ppat_record
        (List.map (fun l -> (lident l, pvar l)) updated_fields)
        (if open_pat then Open else Closed)
    in
    let bind_expr =
      match target.context with
      | None -> [%expr f [%e evar target.name]]
      | Some { field } ->
          let field = List.find (fun f -> f.name = field) fields in
          let field_symex = as_managed field in
          let ctx_run =
            pexp_ident_dots field_symex [ "SM"; "run_with_state" ]
          in
          let field = evar field.name in
          [%expr [%e ctx_run] ~state:[%e field] (f [%e evar target.name])]
    in
    let bind_pat =
      match target.context with
      | None -> [%pat? res, [%p pvar target.name]]
      | Some { field; _ } ->
          [%pat? (res, [%p pvar target.name]), [%p pvar field]]
    in
    let updated =
      pexp_record
        (List.map (fun l -> (lident l, evar l)) updated_fields)
        (if open_pat then Some [%expr st] else None)
    in
    let call_and_assign =
      match target.kind with
      | Managed _ ->
          [%expr
            let*^ [%p bind_pat] = [%e bind_expr] in
            let+ () = SM.set_state (to_opt [%e updated]) in
            res]
      | Ignored _ ->
          [%expr
            let**^ [%p bind_pat] = [%e bind_expr] in
            let+ () = SM.set_state (to_opt [%e updated]) in
            Soteria.Symex.Compo_res.Ok res]
    in
    [%stri
      let [%p pvar (fn_with_sym_name target.name)] =
       fun f ->
        let open SM.Syntax in
        let* st_opt = SM.get_state () in
        let st = of_opt st_opt in
        let [%p st_pat] = st in
        [%e call_and_assign]]

  let with_field_item (target : field) =
    (*
     * ONLY MANAGED FIELDS:
     * let with_field1 f =
     *   SM.Result.map_missing (with_field1_sym f) lift_field1_fixes
     *)
    let loc = target.loc in
    let with_sym = evar (fn_with_sym_name target.name) in
    let lift_fixes = evar (lift_fix_name target.name) in
    [%stri
      let [%p pvar (fn_with_name target.name)] =
       fun f -> SM.Result.map_missing ([%e with_sym] f) [%e lift_fixes]]

  let mk_cons_prod_item ~loc ~kind fields target =
    (*
     * Helper for produce_item/consume_item. Given a field, an option wrap
     * expression, generates:
     *
     * let+ field1 = <lift_expr> (Module1.<produce/consume> v st.field1) in
     * to_opt { st with field1 }
     *
     * OR, if context field:
     * let+ (field1, ctx_field) =
     *   <lift_expr>
     *   @@ CtxField.<Producer/Consumer>.run_with_state ~state:st.ctx_field
     *   @@ Module1.<produce/consume> v st.field1
     * in
     * to_opt { st with field1; ctx_field }
     *
     * where <lift_expr> is either identity (for produce) or a fixes-lifting
     * function (for consume)
     *)
    let fn_name, module_name =
      match kind with
      | `Produce -> ("produce", "Producer")
      | `Consume -> ("consume", "Consumer")
    in
    let fn_expr = pexp_ident_dot (as_managed target) fn_name in
    let field = pexp_field [%expr st] (lident target.name) in
    let expr = [%expr [%e fn_expr] v [%e field]] in
    let expr =
      match target.context with
      | None -> expr
      | Some { field = ctx_field } ->
          let ctx_field = List.find (fun f -> f.name = ctx_field) fields in
          let field_symex = as_managed ctx_field in
          let ctx_run_with =
            pexp_ident_dots field_symex [ "SM"; module_name; "run_with_state" ]
          in
          let ctx_field = pexp_field [%expr st] (lident ctx_field.name) in
          [%expr [%e ctx_run_with] ~state:[%e ctx_field] [%e expr]]
    in
    let expr =
      match kind with
      | `Produce -> expr
      | `Consume ->
          let lift_fixes = evar (lift_fix_name target.name) in
          [%expr
            let+? fixes = [%e expr] in
            [%e lift_fixes] fixes]
    in
    let updated_fields =
      match target.context with
      | None -> [ target.name ]
      | Some { field } -> [ target.name; field ]
    in
    let assign_pat = ppat_tuple (List.map pvar updated_fields) in
    let updated =
      pexp_record
        (List.map (fun l -> (lident l, evar l)) updated_fields)
        (if List.compare_lengths fields updated_fields = 0 then None
         else Some [%expr st])
    in
    [%expr
      let+ [%p assign_pat] = [%e expr] in
      to_opt [%e updated]]

  let produce_item ~loc fields =
    (*
     * let produce (syn : syn) (st : t option) =
     *   let open SM.Symex.Producer.Syntax in
     *   let st = of_opt st in
     *   match syn with
     *   | Ser_field1 v ->
     *     let+ field1 = Module1.produce v st.field1 in
     *     to_opt { st with field1 }
     *   | Ser_field2 v -> ...
     *
     * IF CONTEXT FIELD:
     *   | Ser_field1 v ->
     *     let+ (field1, ctx_field) =
     *       CtxField.Producer.run_with_state ~state:st.ctx_field
     *         (Module1.produce v st.field1)
     *     in
     *     to_opt { st with field1; ctx_field }
     *)
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident (syn_name f.name) in
          let lhs = ppat_construct ctor (Some [%pat? v]) in
          let rhs = mk_cons_prod_item ~loc ~kind:`Produce fields f in
          case ~lhs ~guard:None ~rhs)
        (managed_fields fields)
    in
    [%stri
      let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
        let open SM.Symex.Producer.Syntax in
        let st = of_opt st in
        [%e pexp_match [%expr syn] cases]]

  let consume_item ~loc fields =
    (*
     * let consume (syn : syn) (st : t option) =
     *   let open SM.Symex.Consumer.Syntax in
     *   let st = of_opt st in
     *   match syn with
     *   | Ser_field1 v ->
     *       let+ field1 =
     *         let+? fixes = Module1.consume v st.field1 in
     *         lift_field1_fixes fixes
     *       in
     *       to_opt { st with field1 }
     *  | Ser_field2 v -> ...
     *
     * IF CONTEXT FIELD:
     *   | Ser_field1 v ->
     *     let+ (field1, ctx_field) =
     *       let+? fixes =
     *         CtxField.Consumer.run_with_state ~state:st.ctx_field
     *           (Module1.consume v st.field1)
     *       in
     *       lift_field1_fixes fixes
     *     in
     *     to_opt { st with field1; ctx_field }
     *)
    let cases =
      List.map
        (fun (f : field) ->
          let ctor = lident (syn_name f.name) in
          let lhs = ppat_construct ctor (Some [%pat? v]) in
          let rhs = mk_cons_prod_item ~loc ~kind:`Consume fields f in
          case ~lhs ~guard:None ~rhs)
        (managed_fields fields)
    in
    [%stri
      let consume (syn : syn) (st : t option) :
          (t option, syn list) SM.Symex.Consumer.t =
        let open SM.Symex.Consumer.Syntax in
        let st = of_opt st in
        [%e pexp_match [%expr syn] cases]]

  let make_impl ~loc ~symex_module (td : type_declaration) =
    let@ () = with_loc loc in
    let fields = fields_of_td_exn td in
    validate_context_attr fields;
    [
      sm_item ~loc symex_module;
      pp_item ~loc fields;
      show_item ~loc;
      syn_type_item fields;
      pp_syn_item ~loc fields;
      show_syn_item ~loc;
      of_opt_item ~loc fields;
      to_opt_item ~loc fields;
      empty_item ~loc;
      to_syn_item ~loc fields;
      ins_outs_item ~loc fields;
    ]
    @ List.map lift_syn_fix_item (managed_fields fields)
    @ List.map (with_field_sym_item fields) fields
    @ List.map with_field_item (managed_fields fields)
    @ [ produce_item ~loc fields; consume_item ~loc fields ]

  let str_type_decl ~loc ~path:_ (_rec, tds) symex_module =
    let symex_module =
      module_expr_as_longindent ~err_msg:"expected { symex = <Module> }"
        symex_module
    in
    match tds with
    | [ td ] -> make_impl ~loc ~symex_module td
    | _ -> err "expects exactly one type declaration"

  let register () =
    let symex_arg = Deriving.Args.arg "symex" Ast_pattern.__ in
    let str_args = Deriving.Args.(empty +> symex_arg) in
    let str = Deriving.Generator.make str_args str_type_decl in
    Deriving.add "sym_state" ~str_type_decl:str |> Deriving.ignore
end
