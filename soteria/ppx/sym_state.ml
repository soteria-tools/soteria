open Ppxlib
open Ast_builder.Default
open Util.Syntaxes
open Util.LocCtx

module Helpers = struct
  let rec pp_longident fmt = function
    | Lident s -> Format.pp_print_string fmt s
    | Ldot (base, name) -> Format.fprintf fmt "%a.%s" pp_longident base name
    | Lapply (f, arg) ->
        Format.fprintf fmt "%a(%a)" pp_longident f pp_longident arg

  let lident s = wloc (Lident s)
  let liddot base name = wloc (Ldot (base, name))

  let liddots base path =
    wloc @@ List.fold_left (fun acc name -> Ldot (acc, name)) base path

  let pexp_ident_dot base name = pexp_ident (liddot base name)
  let pexp_ident_dots base name = pexp_ident (liddots base name)

  let ptyp_constr_dot symex_module path args =
    ptyp_constr (liddot symex_module path) args

  let ptyp_constr_dots symex_module path args =
    ptyp_constr (liddots symex_module path) args

  let record_of_names ?base names =
    pexp_record (List.map (fun n -> (lident n, evar n)) names) base

  (** Given an attribute and a list of fields, returns the bindings to those
      fields in the order they are provided, along with the list of remaining
      bindings. Returns Err if the input attribute does not have a struct
      associated. *)
  let find_expr_fields expr fields =
    let rec find_field field rest = function
      | [] -> (None, rest)
      | ({ txt; _ }, v) :: tl when txt = Lident field -> (Some v, rest @ tl)
      | binding :: tl -> find_field field (rest @ [ binding ]) tl
    in
    let rec find_fields acc rest = function
      | [] -> (List.rev acc, rest)
      | field :: tl ->
          let found, rest = find_field field [] rest in
          find_fields (found :: acc) rest tl
    in
    match expr.pexp_desc with
    | Pexp_record (bindings, None) -> Ok (find_fields [] bindings fields)
    | _ -> Error ()
end

open Helpers

module Names = struct
  let syn name = "Ser_" ^ name
  let lift_fixes name = "lift_" ^ name ^ "_fixes"
  let with_ name = "with_" ^ name
  let with_sym name = "with_" ^ name ^ "_sym"
  let ppx = "sym_state"
  let ignore_attr = ppx ^ ".ignore"
  let context_attr = ppx ^ ".context"
end

let err ?loc msg =
  let loc = match loc with Some l -> l | None -> get_loc () in
  Location.raise_errorf ~loc "[@@deriving %s] %s" Names.ppx msg

type context_attr = { field : string; ctx_sym_state : Longident.t }

type ignored_field = {
  empty : expression;
  is_empty : expression option;
  pp : expression option;
}

type managed_field = { sym_state : Longident.t; context : context_attr option }
type field_kind = Managed of managed_field | Ignored of ignored_field
type field = { name : string; kind : field_kind; loc : Location.t }

let is_managed (f : field) =
  match f.kind with Managed _ -> true | Ignored _ -> false

let is_ignored (f : field) =
  match f.kind with Managed _ -> false | Ignored _ -> true

let managed_fields =
  List.filter_map (fun f ->
      match f.kind with Managed m -> Some (f, m) | _ -> None)

let ignored_fields =
  List.filter_map (fun f ->
      match f.kind with Ignored i -> Some (f, i) | _ -> None)

module Attributes = struct
  let validate_attr_field ~name ~loc expr fields =
    match find_expr_fields expr fields with
    | Ok (found, []) ->
        assert (List.compare_lengths found fields = 0);
        found
    | Ok (_, extra) ->
        Fmt.kstr (err ~loc)
          "unexpected field(s) '%a' in [@%s], expected one of %s"
          Fmt.(list ~sep:(Fmt.any ", ") pp_longident)
          (List.map (fun (f, _) -> f.txt) extra)
          name
          (String.concat "; " fields)
    | Error () ->
        Fmt.kstr (err ~loc) "expects [@%s { %s }]" name
          (String.concat "; " fields)

  module Ignore = struct
    let name = Names.ignore_attr

    let attr =
      Attribute.declare_with_attr_loc name Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun ~attr_loc expr -> (attr_loc, expr))

    let find_opt ld =
      Attribute.get attr ld
      |> Option.map @@ fun (attr_loc, expr) ->
         let@ loc = with_loc attr_loc in
         match
           validate_attr_field ~name ~loc expr [ "empty"; "is_empty"; "pp" ]
         with
         | [ None; _is_empty; _pp ] ->
             Fmt.kstr (err ~loc) "expects [@%s { empty = <expr>; ... }]" name
         | [ Some empty; is_empty; pp ] -> { empty; is_empty; pp }
         | _ -> failwith "Impossible"
  end

  module Context = struct
    let name = Names.context_attr

    let attr =
      Attribute.declare_with_attr_loc name Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun ~attr_loc expr -> (attr_loc, expr))

    let find_opt ld =
      Attribute.get attr ld
      |> Option.map @@ fun (attr_loc, expr) ->
         match validate_attr_field ~name ~loc:attr_loc expr [ "field" ] with
         | [ Some { pexp_desc = Pexp_ident { txt = Lident field; _ }; _ } ] ->
             { field; ctx_sym_state = Lident "TEMP_PRE_VALIDATION" }
         | _ ->
             Fmt.kstr (err ~loc:attr_loc) "expects [@%s { field = <field> }]"
               name

    let validate (fields : field list) =
      fields
      |> List.map @@ fun f ->
         let@ _ = with_loc f.loc in
         match f.kind with
         | Managed
             ({ context = Some { field; ctx_sym_state = _ }; _ } as
              managed_field) -> (
             let ctx_field =
               match List.find_opt (fun f -> f.name = field) fields with
               | Some f -> f
               | None ->
                   let valid_fields =
                     fields
                     |> List.filter_map (fun cf ->
                         if f.name = cf.name then None else Some cf.name)
                   in
                   Fmt.kstr (err ?loc:None)
                     "%s references non-existent field %a, expected one of %a"
                     name
                     Fmt.(quote string)
                     field
                     Fmt.(list ~sep:(Fmt.any ", ") Fmt.(quote string))
                     valid_fields
             in
             if ctx_field.name = f.name then
               Fmt.kstr (err ~loc:f.loc) "%s.field cannot reference itself" name;
             match ctx_field.kind with
             | Ignored _ ->
                 Fmt.kstr (err ~loc:f.loc)
                   "%s.field cannot reference an ignored field" name
             | Managed { sym_state = ctx_sym_state; _ } ->
                 (* update context's sym_state *)
                 let context = Some { field; ctx_sym_state } in
                 { f with kind = Managed { managed_field with context } })
         | _ -> f
  end

  let check_no_extra_attrs (ld : label_declaration) =
    let allowed = [ Names.ignore_attr; Names.context_attr ] in
    ld.pld_attributes
    |> List.iter @@ fun (attr : attribute) ->
       if not (List.exists (String.equal attr.attr_name.txt) allowed) then
         let rec pp_attrs ?(acc = "") = function
           | [] -> acc
           | [ a ] -> acc ^ "and [@" ^ a ^ "]"
           | a :: rest -> pp_attrs ~acc:(acc ^ "[@" ^ a ^ "], ") rest
         in
         Fmt.kstr (err ~loc:attr.attr_loc) "only supports attributes %s, got %s"
           (pp_attrs allowed) attr.attr_name.txt

  let validate fs = Context.validate fs
end

let parse_mod_t_option (ct : core_type) =
  let@ _ = with_loc ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc; _ } ]) -> (
      match ptyp_desc with
      | Ptyp_constr ({ txt = Ldot (path, "t"); _ }, []) -> path
      | _ -> err "expects record fields of type <Module>.t option")
  | _ -> err "expects record fields of type <Module>.t option"

let mk_field ld =
  Attributes.check_no_extra_attrs ld;
  let kind =
    match Attributes.Ignore.find_opt ld with
    | Some ignored -> Ignored ignored
    | None ->
        let sym_state = parse_mod_t_option ld.pld_type in
        let context = Attributes.Context.find_opt ld in
        Managed { sym_state; context }
  in
  { name = ld.pld_name.txt; kind; loc = ld.pld_loc }

let fields_of_td_exn (td : type_declaration) =
  let@ _ = with_loc td.ptype_loc in
  if td.ptype_name.txt <> "t" then
    err ~loc:td.ptype_name.loc "only supports type named 't'";
  let labels =
    match td.ptype_kind with
    | Ptype_record labels -> labels
    | _ -> err "only supports record types"
  in
  labels |> List.map mk_field |> Attributes.validate

(** Folds over fields, applying f to each field and joining with join, with
    empty as the base case. *)
let fold_fields ~empty ~f ~join fields =
  match fields with
  | [] -> empty
  | hd :: tl -> List.fold_left (fun acc field -> join acc (f field)) (f hd) tl

(** For a field Foo, creates pattern [Ser_foo(v)] *)
let ppat_field field =
  let loc = get_loc () in
  ppat_construct (lident (Names.syn field.name)) (Some [%pat? v])

(** For a field Foo and expression e, creates expression [Ser_foo(e)] *)
let constr_field field expr =
  pexp_construct (lident (Names.syn field.name)) (Some expr)

let match_on_syn fields f e =
  let loc = get_loc () in
  let cases =
    List.map
      (fun (field, as_managed) ->
        let lhs = ppat_field field in
        let rhs = f field as_managed in
        case ~lhs ~guard:None ~rhs)
      (managed_fields fields)
  in
  (* we add an irrefutable case at the end, so that the pattern match is still
     valid if there are no managed fields. *)
  let irrefutable =
    case ~lhs:[%pat? _] ~guard:None ~rhs:(pexp_unreachable ())
  in
  pexp_match e (cases @ [ irrefutable ])

let syn_type_item (syn_ty : longident option) fields =
  let syn_ctor_decl (field, { sym_state; _ }) =
    let arg_ty = ptyp_constr_dot sym_state "syn" [] in
    constructor_declaration ~name:(Names.syn field.name)
      ~args:(Pcstr_tuple [ arg_ty ]) ~res:None
  in
  let fields = managed_fields fields in
  let manifest : core_type option =
    match syn_ty with
    | Some ty -> Some (ptyp_constr (wloc ty) [])
    | None -> None
  in
  let td =
    type_declaration ~name:"syn" ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant (List.map syn_ctor_decl fields))
      ~private_:Public ~manifest
  in
  pstr_type Recursive [ td ]

let pp_syn_item ~loc fields =
  let case field { sym_state; _ } =
    [%expr
      Fmt.pf ft "(@[<2>%s@ %a@])"
        [%e estring (Names.syn field.name)]
        [%e pexp_ident_dot sym_state "pp_syn"]
        v]
  in
  if not (List.exists is_managed fields) then [%stri let pp_syn _ _ = ()]
  else [%stri let pp_syn ft (s : syn) = [%e match_on_syn fields case [%expr s]]]

let show_syn_item ~loc = [%stri let show_syn s = Format.asprintf "%a" pp_syn s]

let pp_item ~loc fields =
  let f (f : field) =
    match f.kind with
    | Managed { sym_state; _ } ->
        [%expr
          Format.fprintf fmt "@[%s =@ " [%e estring f.name];
          (match [%e pexp_field [%expr x] (lident f.name)] with
          | None -> Format.pp_print_string fmt "empty"
          | Some v -> [%e pexp_ident_dot sym_state "pp"] fmt v);
          Format.fprintf fmt "@]"]
    | Ignored { pp = Some pp; _ } ->
        [%expr
          Format.fprintf fmt "@[%s =@ " [%e estring f.name];
          [%e pp] fmt [%e pexp_field [%expr x] (lident f.name)];
          Format.fprintf fmt "@]"]
    | Ignored { pp = None; _ } ->
        [%expr Format.fprintf fmt "@[%s =@ <ignored>@]" [%e estring f.name]]
  in
  let body =
    fold_fields fields ~empty:[%expr ()] ~f ~join:(fun acc expr ->
        [%expr
          [%e acc];
          Format.fprintf fmt ";@ ";
          [%e expr]])
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
             match f.kind with
             | Managed _ -> [%expr None]
             | Ignored e -> e.empty
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
      let is_emp (f, i) =
        match i.is_empty with
        | Some is_empty -> [%expr [%e is_empty] [%e evar f.name]]
        | None -> [%expr [%e evar f.name] = [%e i.empty]]
      in
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
  (*
   * let to_syn (st : t) : syn list =
   *   (List.map (fun v -> Ser_field1 v)
   *     (Option.fold ~none:[] ~some:Module1.to_syn st.field1))
   *   @ (List.map (fun v -> Ser_field2 v)
   *     (Option.fold ~none:[] ~some:Module2.to_syn st.field2))
   *)
  let f (f, m) =
    [%expr
      List.map
        (fun v -> [%e constr_field f [%expr v]])
        (Option.fold ~none:[]
           ~some:[%e pexp_ident_dot m.sym_state "to_syn"]
           [%e pexp_field [%expr st] (lident f.name)])]
  in
  let body =
    fold_fields ~empty:[%expr []] ~f
      ~join:(fun acc e -> [%expr [%e acc] @ [%e e]])
      (managed_fields fields)
  in
  if not (List.exists is_managed fields) then
    [%stri let to_syn (_ : t) : syn list = []]
  else [%stri let to_syn (st : t) : syn list = [%e body]]

let ins_outs_item ~loc fields =
  (*
   * let ins_outs_item = function
   *   | Ser_field1 v -> Module1.ins_outs v
   *   | Ser_field2 v -> Module2.ins_outs v
   *)
  let case _ { sym_state; _ } =
    [%expr [%e pexp_ident_dot sym_state "ins_outs"] v]
  in
  [%stri let ins_outs (syn : syn) = [%e match_on_syn fields case [%expr syn]]]

let lift_syn_fix_item (target, _) =
  (*
   * ONLY MANAGED FIELDS:
   * let lift_field1_fixes = List.map (fun v -> Ser_field1 v)
   *)
  let loc = target.loc in
  [%stri
    let [%p pvar (Names.lift_fixes target.name)] =
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
  let@ loc = with_loc target.loc in
  let context =
    match target.kind with
    | Managed { context = Some context; _ } -> Some context
    | _ -> None
  in
  let updated_fields =
    match context with
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
    match target.kind with
    | Managed { context = Some { field; ctx_sym_state }; _ } ->
        let ctx_run =
          pexp_ident_dots ctx_sym_state [ "SM"; "run_with_state" ]
        in
        [%expr [%e ctx_run] ~state:[%e evar field] (f [%e evar target.name])]
    | _ -> [%expr f [%e evar target.name]]
  in
  let bind_pat =
    match context with
    | None -> [%pat? res, [%p pvar target.name]]
    | Some { field; _ } -> [%pat? (res, [%p pvar target.name]), [%p pvar field]]
  in
  let updated =
    record_of_names updated_fields
      ?base:(if open_pat then Some [%expr st] else None)
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
    let [%p pvar (Names.with_sym target.name)] =
     fun f ->
      let open SM.Syntax in
      let* st_opt = SM.get_state () in
      let st = of_opt st_opt in
      let [%p st_pat] = st in
      [%e call_and_assign]]

let with_field_item (target, _) =
  (*
   * ONLY MANAGED FIELDS:
   * let with_field1 f =
   *   SM.Result.map_missing (with_field1_sym f) lift_field1_fixes
   *)
  let@ loc = with_loc target.loc in
  let with_sym = evar (Names.with_sym target.name) in
  let lift_fixes = evar (Names.lift_fixes target.name) in
  [%stri
    let [%p pvar (Names.with_ target.name)] =
     fun f -> SM.Result.map_missing ([%e with_sym] f) [%e lift_fixes]]

let mk_cons_prod_item ~loc ~kind fields target managed_field =
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
  let fn_expr = pexp_ident_dot managed_field.sym_state fn_name in
  let field = pexp_field [%expr st] (lident target.name) in
  let expr = [%expr [%e fn_expr] v [%e field]] in
  let expr =
    match target.kind with
    | Managed { context = Some { field; ctx_sym_state }; _ } ->
        let ctx_run_with =
          pexp_ident_dots ctx_sym_state [ "SM"; module_name; "run_with_state" ]
        in
        let ctx_field = pexp_field [%expr st] (lident field) in
        [%expr [%e ctx_run_with] ~state:[%e ctx_field] [%e expr]]
    | _ -> expr
  in
  let expr =
    match kind with
    | `Produce -> expr
    | `Consume ->
        let lift_fixes = evar (Names.lift_fixes target.name) in
        [%expr
          let+? fixes = [%e expr] in
          [%e lift_fixes] fixes]
  in
  let updated_fields =
    match managed_field.context with
    | None -> [ target.name ]
    | Some { field; _ } -> [ target.name; field ]
  in
  let assign_pat = ppat_tuple (List.map pvar updated_fields) in
  let is_open = List.compare_lengths updated_fields fields <> 0 in
  let updated =
    record_of_names updated_fields
      ?base:(if is_open then Some [%expr st] else None)
  in
  [%expr
    let+ [%p assign_pat] = [%e expr] in
    to_opt [%e updated]]

let mk_cons_prod_match ~loc ~kind fields =
  match_on_syn fields (mk_cons_prod_item ~loc ~kind fields) [%expr syn]

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
  if not (List.exists is_managed fields) then
    [%stri let produce (syn : syn) () = match syn with _ -> .]
  else
    [%stri
      let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
        let open SM.Symex.Producer.Syntax in
        let st = of_opt st in
        [%e mk_cons_prod_match ~loc ~kind:`Produce fields]]

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
  if not (List.exists is_managed fields) then
    [%stri let consume (syn : syn) () = match syn with _ -> .]
  else
    [%stri
      let consume (syn : syn) (st : t option) :
          (t option, syn list) SM.Symex.Consumer.t =
        let open SM.Symex.Consumer.Syntax in
        let st = of_opt st in
        [%e mk_cons_prod_match ~loc ~kind:`Consume fields]]

let make_impl ~loc ~symex_module ~syn_ty (td : type_declaration) =
  let@ loc = with_loc loc in
  let fields = fields_of_td_exn td in
  [
    sm_item ~loc symex_module;
    pp_item ~loc fields;
    show_item ~loc;
    syn_type_item syn_ty fields;
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

let str_type_decl ~loc ~path:_ (_rec, tds) symex_module syn_ty =
  let@ _ = with_loc loc in
  let symex_module =
    match symex_module with
    | Some { pexp_desc = Pexp_construct ({ txt; _ }, None); _ } -> txt
    | _ -> err "expected { symex = <Module> }"
  in
  let syn_ty =
    match syn_ty with
    | Some { pexp_desc = Pexp_ident { txt; _ }; _ } -> Some txt
    | None -> None
    | _ -> err "expected { syn_ty = <ty> }"
  in
  match tds with
  | [ td ] -> make_impl ~loc ~symex_module ~syn_ty td
  | _ -> err "expects exactly one type declaration"

let register () =
  let symex_arg = Deriving.Args.arg "symex" Ast_pattern.__ in
  let syn_ty_arg = Deriving.Args.arg "syn" Ast_pattern.__ in
  let str_args = Deriving.Args.(empty +> symex_arg +> syn_ty_arg) in
  let str = Deriving.Generator.make str_args str_type_decl in
  Deriving.add Names.ppx ~str_type_decl:str |> Deriving.ignore
