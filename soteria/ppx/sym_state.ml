open Ppxlib
open Ast_builder.Default
open Util.Syntaxes
open Util.LocCtx

module Names = struct
  let syn name = "Ser_" ^ name
  let lift_fixes name = "lift_" ^ name ^ "_fixes"
  let with_ name = "with_" ^ name
  let with_sym name = "with_" ^ name ^ "_sym"
  let ppx = "sym_state"
  let ignore_attr = "soteria." ^ ppx ^ ".ignore"
  let context_attr = "soteria." ^ ppx ^ ".context"
end

module Config = struct
  type _ Effect.t +=
    | Get_syn_ty : core_type option Effect.t
    | Get_symex_module : Longident.t Effect.t
    | Get_inside_soteria : bool Effect.t

  let get_syn_ty () = Effect.perform Get_syn_ty
  let get_symex_module () = Effect.perform Get_symex_module
  let get_inside_soteria () = Effect.perform Get_inside_soteria

  let with_config ~(syn_ty : core_type option) ~(symex_module : Longident.t)
      ~(inside_soteria : bool) f =
    let open Effect.Deep in
    try f () with
    | effect Get_syn_ty, k -> continue k syn_ty
    | effect Get_symex_module, k -> continue k symex_module
    | effect Get_inside_soteria, k -> continue k inside_soteria
end

let record_of_names ?base names =
  pexp_record (List.map (fun n -> (lident n, evar n)) names) base

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
  open Util.Attributes

  module Ignore = struct
    let name = Names.ignore_attr
    let attr = declare_record ~name (must "empty" ** may "is_empty" ** may "pp")

    let find_opt ld =
      Attribute.get attr ld
      |> Option.map @@ fun (empty, (is_empty, pp)) -> { empty; is_empty; pp }
  end

  module Context = struct
    let name = Names.context_attr
    let attr = declare_record ~name (must "field")

    let find_opt ld =
      Attribute.get attr ld
      |> Option.map @@ function
         | { pexp_desc = Pexp_ident { txt = Lident field; _ }; _ } ->
             { field; ctx_sym_state = Lident "TEMP_PRE_VALIDATION" }
         | _ ->
             Fmt.kstr (err ?loc:None) "expects [@%s { field = <field> }]" name

    let validate_field fields f { field; ctx_sym_state = _ } managed_field =
      let@ _ = with_loc f.loc in
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
              "%s references non-existent field %a, expected one of %a" name
              Fmt.(quote string)
              field
              Fmt.(list ~sep:(Fmt.any ", ") Fmt.(quote string))
              valid_fields
      in
      if ctx_field.name = f.name then
        Fmt.kstr (err ~loc:f.loc) "%s.field cannot reference itself" name;
      match ctx_field.kind with
      | Ignored _ ->
          Fmt.kstr (err ~loc:f.loc) "%s.field cannot reference an ignored field"
            name
      | Managed { sym_state = ctx_sym_state; _ } ->
          (* update context's sym_state *)
          let context = Some { field; ctx_sym_state } in
          { f with kind = Managed { managed_field with context } }

    let validate (fields : field list) =
      fields
      |> List.map @@ fun f ->
         match f.kind with
         | Managed ({ context = Some context; _ } as managed_field) ->
             validate_field fields f context managed_field
         | Managed { context = None; _ } | Ignored _ -> f
  end

  let check_no_extra_attrs (ld : label_declaration) =
    Attribute.check_unused#label_declaration ld

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

let parse_mod_t (ct : core_type) =
  let@ _ = with_loc ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Ldot (path, "t"); _ }, []) -> path
  | _ -> err "sum constructors must carry a single <Module>.t argument"

(** A sum constructor [Foo of <Module>.t] becomes a managed field named after
    the constructor. Context and ignored attributes are record-only. *)
let mk_variant_field (cd : constructor_declaration) =
  let@ loc = with_loc cd.pcd_loc in
  let sym_state =
    match cd.pcd_args with
    | Pcstr_tuple [ ct ] -> parse_mod_t ct
    | _ -> err "sum constructors must carry a single <Module>.t argument"
  in
  { name = cd.pcd_name.txt; kind = Managed { sym_state; context = None }; loc }

(** A record models a product memory model (all fields owned together); a
    variant models a sum memory model (exactly one variant active at a time). *)
type shape = Record of field list | Variant of field list

let fields_of_td_exn (td : type_declaration) =
  let@ _ = with_loc td.ptype_loc in
  if td.ptype_name.txt <> "t" then
    err ~loc:td.ptype_name.loc "only supports type named 't'";
  match td.ptype_kind with
  | Ptype_record labels ->
      Record (labels |> List.map mk_field |> Attributes.validate)
  | Ptype_variant ctors -> Variant (List.map mk_variant_field ctors)
  | _ -> err "only supports record or variant types"

(** Folds over fields, applying f to each field and joining with join, with
    empty as the base case. *)
let fold_fields ~empty ~f ~join fields =
  match fields with
  | [] -> empty
  | hd :: tl -> List.fold_left (fun acc field -> join acc (f field)) (f hd) tl

(** The module the syn constructors live in, when a parameterised manifest puts
    them in another module. *)
let syn_ctor_prefix (manifest : core_type option) : Longident.t option =
  match manifest with
  | Some { ptyp_desc = Ptyp_constr ({ txt = Ldot (m, _); _ }, _ :: _); _ } ->
      Some m
  | _ -> None

(** The longident of a field Foo's syn constructor, qualified if needed. *)
let syn_ctor_lident name =
  match syn_ctor_prefix @@ Config.get_syn_ty () with
  | None -> lident (Names.syn name)
  | Some prefix -> wloc (Ldot (prefix, Names.syn name))

(** For a field Foo, creates pattern [Ser_foo(v)] *)
let ppat_field field =
  let loc = get_loc () in
  ppat_construct (syn_ctor_lident field.name) (Some [%pat? v])

(** For a field Foo and expression e, creates expression [Ser_foo(e)] *)
let constr_field field expr =
  pexp_construct (syn_ctor_lident field.name) (Some expr)

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

let syn_type_item fields =
  let syn_ctor_decl (field, { sym_state; _ }) =
    let arg_ty = ptyp_constr_dot sym_state "syn" [] in
    constructor_declaration ~name:(Names.syn field.name)
      ~args:(Pcstr_tuple [ arg_ty ]) ~res:None
  in
  let fields = managed_fields fields in
  let manifest = Config.get_syn_ty () in
  let kind =
    match manifest with
    | Some { ptyp_desc = Ptyp_constr (_, _ :: _); _ } ->
        (* A parameterised manifest (e.g. [I.syn freeable_syn]) rebinds a type
           constructor that has type parameters, but [syn] has none;
           re-declaring its representation would be an arity mismatch. We alias
           it transparently instead — the constructors come from the
           manifest. *)
        Ptype_abstract
    | _ -> Ptype_variant (List.map syn_ctor_decl fields)
  in
  let td =
    type_declaration ~name:"syn" ~params:[] ~cstrs:[] ~kind ~private_:Public
      ~manifest
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

let sm_item ~loc =
  (* When the PPX is used inside Soteria itself, [Soteria.Sym_states] is not in
     scope; the modules must be referred to directly (e.g. [Sym_states]). *)
  let make =
    pmod_ident
      (if Config.get_inside_soteria () then liddots' [ "State_monad"; "Make" ]
       else liddots' [ "Soteria"; "Sym_states"; "State_monad"; "Make" ])
  in
  let symex_module = pmod_ident (wloc (Config.get_symex_module ())) in
  [%stri
    module SM =
      [%m make] ([%m symex_module])
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
   * Soteria.Soteria_std.Compo_res.Ok res
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
          let* () = SM.set_state (to_opt [%e updated]) in
          SM.Result.ok res]
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
   *   SM.Result.map_missing lift_field1_fixes (with_field1_sym f)
   *)
  let@ loc = with_loc target.loc in
  let with_sym = evar (Names.with_sym target.name) in
  let lift_fixes = evar (Names.lift_fixes target.name) in
  [%stri
    let [%p pvar (Names.with_ target.name)] =
     fun f -> SM.Result.map_missing [%e lift_fixes] ([%e with_sym] f)]

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

(** For a variant Foo, pattern [Foo v] over the (unwrapped) state. *)
let ppat_variant field arg = ppat_construct (lident field.name) (Some arg)

let pp_variant_item ~loc fields =
  let mk_case (field, { sym_state; _ }) =
    let lhs = ppat_variant field [%pat? v] in
    let rhs =
      [%expr
        Format.fprintf fmt "@[<2>%s@ " [%e estring field.name];
        [%e pexp_ident_dot sym_state "pp"] fmt v;
        Format.fprintf fmt "@]"]
    in
    case ~lhs ~guard:None ~rhs
  in
  [%stri
    let pp fmt x =
      [%e pexp_match [%expr x] (List.map mk_case (managed_fields fields))]]

let to_syn_variant_item ~loc fields =
  let mk_case (field, { sym_state; _ }) =
    let lhs = ppat_variant field [%pat? x] in
    let rhs =
      [%expr
        List.map
          (fun v -> [%e constr_field field [%expr v]])
          ([%e pexp_ident_dot sym_state "to_syn"] x)]
    in
    case ~lhs ~guard:None ~rhs
  in
  [%stri
    let to_syn (st : t) : syn list =
      [%e pexp_match [%expr st] (List.map mk_case (managed_fields fields))]]

let mk_variant_dispatch ~loc ~kind fields =
  (*
   * The variant stores a bare [Module.t], but the inner produce/consume work on
   * [Module.t option] (its own empty state), so we wrap/unwrap around the call:
   *
   * match syn with
   * | Ser_foo v ->
   *     let* inner =
   *       match st with
   *       | None -> return None
   *       | Some (Foo x) -> return (Some x)
   *       | Some (Bar _) -> vanish ()
   *     in
   *     let+ inner' = Module.<produce/consume> v inner in
   *     (match inner' with None -> None | Some y -> Some (Foo y))
   *)
  let fn_name =
    match kind with `Produce -> "produce" | `Consume -> "consume"
  in
  let pure e =
    match kind with
    | `Produce -> [%expr return [%e e]]
    | `Consume -> [%expr ok [%e e]]
  in
  let incompatible =
    match kind with
    | `Produce -> [%expr vanish ()]
    | `Consume -> [%expr lfail (SM.Symex.Value.of_bool false)]
  in
  let mk_case field { sym_state; _ } =
    let other_cases =
      managed_fields fields
      |> List.filter_map (fun (other, _) ->
          if other.name = field.name then None
          else
            Some
              (case
                 ~lhs:[%pat? Some [%p ppat_variant other [%pat? _]]]
                 ~guard:None ~rhs:incompatible))
    in
    let st_match =
      pexp_match [%expr st]
        (case ~lhs:[%pat? None] ~guard:None ~rhs:(pure [%expr None])
        :: case
             ~lhs:[%pat? Some [%p ppat_variant field [%pat? x]]]
             ~guard:None
             ~rhs:(pure [%expr Some x])
        :: other_cases)
    in
    let call = [%expr [%e pexp_ident_dot sym_state fn_name] v inner] in
    let call =
      match kind with
      | `Produce -> call
      | `Consume ->
          let lift_fixes = evar (Names.lift_fixes field.name) in
          [%expr
            let+? fixes = [%e call] in
            [%e lift_fixes] fixes]
    in
    [%expr
      let* inner = [%e st_match] in
      let+ inner' = [%e call] in
      match inner' with
      | None -> None
      | Some y -> Some [%e pexp_construct (lident field.name) (Some [%expr y])]]
  in
  match_on_syn fields mk_case [%expr syn]

let produce_variant_item ~loc fields =
  [%stri
    let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
      let open SM.Symex.Producer in
      let open SM.Symex.Producer.Syntax in
      [%e mk_variant_dispatch ~loc ~kind:`Produce fields]]

let consume_variant_item ~loc fields =
  [%stri
    let consume (syn : syn) (st : t option) :
        (t option, syn list) SM.Symex.Consumer.t =
      let open SM.Symex.Consumer in
      let open SM.Symex.Consumer.Syntax in
      [%e mk_variant_dispatch ~loc ~kind:`Consume fields]]

let make_record_impl ~loc fields =
  [
    sm_item ~loc;
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

let make_variant_impl ~loc fields =
  [
    sm_item ~loc;
    pp_variant_item ~loc fields;
    show_item ~loc;
    syn_type_item fields;
    pp_syn_item ~loc fields;
    show_syn_item ~loc;
    empty_item ~loc;
    to_syn_variant_item ~loc fields;
    ins_outs_item ~loc fields;
  ]
  @ List.map lift_syn_fix_item (managed_fields fields)
  @ [ produce_variant_item ~loc fields; consume_variant_item ~loc fields ]

let make_impl ~loc (td : type_declaration) =
  let@ loc = with_loc loc in
  match fields_of_td_exn td with
  | Record fields -> make_record_impl ~loc fields
  | Variant fields -> make_variant_impl ~loc fields

(* Convert a deriver payload expressions from the [syn] argument into a core
   type. *)
let rec core_type_of_expr (e : expression) : core_type =
  let@ _ = with_loc e.pexp_loc in
  match e.pexp_desc with
  | Pexp_ident lid -> ptyp_constr lid []
  | Pexp_apply (params, (_ :: _ as constrs)) ->
      let constr_lid = function
        | _, { pexp_desc = Pexp_ident lid; _ } -> lid
        | _ -> err "syn expects a type, e.g. `Foo.t` or `I.syn freeable_syn`"
      in
      let init_params =
        match params.pexp_desc with
        | Pexp_tuple es -> List.map core_type_of_expr es
        | _ -> [ core_type_of_expr params ]
      in
      let first, rest =
        match List.map constr_lid constrs with
        | c :: cs -> (c, cs)
        | [] -> assert false
      in
      List.fold_left
        (fun acc c -> ptyp_constr c [ acc ])
        (ptyp_constr first init_params)
        rest
  | _ -> err "syn expects a type, e.g. `Foo.t` or `I.syn freeable_syn`"

let str_type_decl ~loc ~path:_ (_rec, tds) symex_module syn_ty inside_soteria =
  let@ _ = with_loc loc in
  let symex_module =
    match symex_module with
    | Some { pexp_desc = Pexp_construct ({ txt; _ }, None); _ } -> txt
    | _ -> err "expected { symex = <Module> }"
  in
  let syn_ty = Option.map core_type_of_expr syn_ty in
  let@ () = Config.with_config ~syn_ty ~symex_module ~inside_soteria in
  match tds with
  | [ td ] -> make_impl ~loc td
  | _ -> err "expects exactly one type declaration"

let register () =
  let symex_arg = Deriving.Args.arg "symex" Ast_pattern.__ in
  let syn_ty_arg = Deriving.Args.arg "syn" Ast_pattern.__ in
  let inside_soteria_arg = Deriving.Args.flag "inside_soteria" in
  let str_args =
    Deriving.Args.(empty +> symex_arg +> syn_ty_arg +> inside_soteria_arg)
  in
  let str = Deriving.Generator.make str_args str_type_decl in
  Deriving.add Names.ppx ~str_type_decl:str |> Deriving.ignore
