open Ppxlib
open Ast_builder.Default

module Names = struct
  let ppx = "abstr"

  (* A deriver/attribute name written by the user may be bare, [abstr.]-prefixed
     or [soteria.abstr.]-prefixed. *)
  let candidates base =
    [ base; ppx ^ "." ^ base; "soteria." ^ ppx ^ "." ^ base ]
end

let err ~loc fmt = Location.raise_errorf ~loc ("[@@deriving abstr] " ^^ fmt)

(* {2 Small AST helpers} *)

let lid ~loc txt = { txt; loc }
let qual_lid base parts = List.fold_left (fun acc p -> Ldot (acc, p)) base parts

(** [qual ~loc base ["A"; "f"]] is the expression [base.A.f]. *)
let qual ~loc base parts = pexp_ident ~loc (lid ~loc (qual_lid base parts))

(** [tcon ~loc base ["A"; "t"] args] is the type [args base.A.t]. *)
let tcon ~loc base parts args =
  ptyp_constr ~loc (lid ~loc (qual_lid base parts)) args

let app ~loc f args = pexp_apply ~loc f (List.map (fun a -> (Nolabel, a)) args)
let field_of ~loc record name = pexp_field ~loc record (lid ~loc (Lident name))

let with_open ~loc modlid body =
  pexp_open ~loc
    {
      popen_override = Fresh;
      popen_expr = pmod_ident ~loc (lid ~loc modlid);
      popen_loc = loc;
      popen_attributes = [];
    }
    body

let rec longident_eq a b =
  match (a, b) with
  | Lident a, Lident b -> String.equal a b
  | Ldot (a, x), Ldot (b, y) -> String.equal x y && longident_eq a b
  | Lapply (a, x), Lapply (b, y) -> longident_eq a x && longident_eq b y
  | _ -> false

(* {2 Configuration parsed from the deriver payload} *)

type config = {
  value : longident;  (** symbolic value module, e.g. [Typed] *)
  symex : longident;  (** monad module, e.g. [Rustsymex] *)
  gen_sem_eq : bool;
  gen_simplify : bool;
}

let default_config =
  {
    value = Lident "Typed";
    symex = Lident "Rustsymex";
    gen_sem_eq = false;
    gen_simplify = false;
  }

let module_of_expr ~loc = function
  | { pexp_desc = Pexp_construct ({ txt; _ }, None); _ } -> txt
  | { pexp_loc; _ } -> err ~loc:pexp_loc "expected a module name"

let bool_of_expr ~loc = function
  | { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ } -> true
  | { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ } ->
      false
  | { pexp_loc; _ } -> err ~loc:pexp_loc "expected a boolean"

let parse_config ~loc = function
  | None -> default_config
  | Some { pexp_desc = Pexp_record (fields, None); _ } ->
      List.fold_left
        (fun cfg (key, v) ->
          match key.txt with
          | Lident "value" -> { cfg with value = module_of_expr ~loc v }
          | Lident "symex" -> { cfg with symex = module_of_expr ~loc v }
          | Lident "sem_eq" -> { cfg with gen_sem_eq = bool_of_expr ~loc v }
          | Lident "simplify" -> { cfg with gen_simplify = bool_of_expr ~loc v }
          | _ ->
              err ~loc:key.loc
                "unknown key, expected one of: value, symex, sem_eq, simplify")
        default_config fields
  | Some { pexp_loc; _ } ->
      err ~loc:pexp_loc "expected a record payload { key = value; ... }"

(* {2 Fields} *)

type fkind =
  | Fmod of longident  (** field of type [M.t] *)
  | Ftyped of { witness : string option }  (** field of type [_ value.t] *)
  | Fconcrete  (** field marked [@concrete] *)

type ffield = {
  fname : string;
  floc : location;
  fctype : core_type;
  fkind : fkind;
  ovr : (string * expression) list;  (** per-function overrides *)
  fpp : expression option;  (** [@pp] printer for concrete fields *)
}

let is_concrete f = match f.fkind with Fconcrete -> true | _ -> false
let override f name = List.assoc_opt name f.ovr

(** Witness name for a [_ value.t] field: phantom [sint] -> [t_int]. The phantom
    may be qualified (e.g. [Typed.T.sint]); only its last component matters. *)
let witness_of_phantom = function
  | { ptyp_desc = Ptyp_constr ({ txt; _ }, []); _ } ->
      let name = Longident.last_exn txt in
      let base =
        if String.length name > 0 && name.[0] = 's' then
          String.sub name 1 (String.length name - 1)
        else name
      in
      Some ("t_" ^ base)
  | _ -> None

let attr_payload_expr attr =
  match attr.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
  | _ ->
      err ~loc:attr.attr_loc "attribute [@%s ...] expects an expression payload"
        attr.attr_name.txt

let find_attr names attrs =
  List.find_opt (fun a -> List.mem a.attr_name.txt names) attrs

let mk_field ~cfg (ld : label_declaration) =
  let fname = ld.pld_name.txt in
  let floc = ld.pld_loc in
  let fctype = ld.pld_type in
  let overrides_names =
    [
      "fresh"; "to_syn"; "subst"; "learn_eq"; "exprs_syn"; "sem_eq"; "simplify";
    ]
  in
  let ovr =
    List.filter_map
      (fun base ->
        find_attr (Names.candidates base) ld.pld_attributes
        |> Option.map (fun a -> (base, attr_payload_expr a)))
      overrides_names
  in
  let fpp =
    find_attr (Names.candidates "pp") ld.pld_attributes
    |> Option.map attr_payload_expr
  in
  let concrete =
    Option.is_some (find_attr (Names.candidates "concrete") ld.pld_attributes)
  in
  let fkind =
    if concrete then Fconcrete
    else
      match fctype.ptyp_desc with
      | Ptyp_constr ({ txt = Ldot (m, "t"); _ }, []) -> Fmod m
      | Ptyp_constr ({ txt = Ldot (m, "t"); _ }, [ arg ])
        when longident_eq m cfg.value ->
          Ftyped { witness = witness_of_phantom arg }
      | _ ->
          err ~loc:floc
            "field %S must have type <Module>.t or <phantom> %s.t, or be \
             annotated [@concrete]"
            fname
            (String.concat "." (Longident.flatten_exn cfg.value))
  in
  { fname; floc; fctype; fkind; ovr; fpp }

(* {2 Type declarations: abstr_raw, t, syn} *)

let non_concrete fields = List.filter (fun f -> not (is_concrete f)) fields

let abstr_raw_decl ~loc ~derivers fields =
  let params =
    non_concrete fields
    |> List.map (fun f -> (ptyp_var ~loc f.fname, (NoVariance, NoInjectivity)))
  in
  let labels =
    List.map
      (fun f ->
        let type_ = if is_concrete f then f.fctype else ptyp_var ~loc f.fname in
        label_declaration ~loc ~name:(lid ~loc f.fname) ~mutable_:Immutable
          ~type_)
      fields
  in
  let td =
    type_declaration ~loc ~name:(lid ~loc "abstr_raw") ~params ~cstrs:[]
      ~kind:(Ptype_record labels) ~private_:Public ~manifest:None
  in
  pstr_type ~loc Recursive [ { td with ptype_attributes = derivers } ]

let alias_decl ~loc name args =
  let manifest = Some (ptyp_constr ~loc (lid ~loc (Lident "abstr_raw")) args) in
  let td =
    type_declaration ~loc ~name:(lid ~loc name) ~params:[] ~cstrs:[]
      ~kind:Ptype_abstract ~private_:Public ~manifest
  in
  pstr_type ~loc Recursive [ td ]

let t_decl ~loc fields =
  alias_decl ~loc "t" (List.map (fun f -> f.fctype) (non_concrete fields))

let syn_decl ~loc ~cfg fields =
  let args =
    non_concrete fields
    |> List.map (fun f ->
        match f.fkind with
        | Fmod m -> tcon ~loc m [ "syn" ] []
        | Ftyped _ -> tcon ~loc cfg.value [ "Expr"; "t" ] []
        | Fconcrete -> assert false)
  in
  alias_decl ~loc "syn" args

(* {2 Printers} *)

(** Printer expression for a concrete field: the [@pp] override if given, else a
    standard-library printer for a few built-in types, else a placeholder. *)
let concrete_printer ~loc f =
  match f.fpp with
  | Some e -> e
  | None -> (
      match f.fctype.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
          [%expr Format.pp_print_bool]
      | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
          [%expr Format.pp_print_int]
      | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
          [%expr Format.pp_print_string]
      | Ptyp_constr ({ txt = Lident "char"; _ }, []) ->
          [%expr Format.pp_print_char]
      | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
          [%expr Format.pp_print_float]
      | _ -> [%expr fun fmt _ -> Format.pp_print_string fmt "<opaque>"])

(** Manual [pp]/[pp_syn], mirroring the style of the [sym_state] deriver, so the
    generated code does not depend on [ppx_deriving]. *)
let pp_item ~loc ~cfg ~syn fields =
  let printer f =
    match f.fkind with
    | Fmod m -> qual ~loc m [ (if syn then "pp_syn" else "pp") ]
    | Ftyped _ ->
        if syn then qual ~loc cfg.value [ "Expr"; "pp" ]
        else qual ~loc cfg.value [ "ppa" ]
    | Fconcrete -> concrete_printer ~loc f
  in
  let field_stmt f =
    let access = field_of ~loc (evar ~loc "x") f.fname in
    [%expr
      Format.fprintf fmt "@[%s =@ " [%e estring ~loc f.fname];
      [%e printer f] fmt [%e access];
      Format.fprintf fmt "@]"]
  in
  let body =
    match fields with
    | [] -> [%expr ()]
    | hd :: tl ->
        List.fold_left
          (fun acc f ->
            [%expr
              [%e acc];
              Format.fprintf fmt ";@ ";
              [%e field_stmt f]])
          (field_stmt hd) tl
  in
  let ty = if syn then [%type: syn] else [%type: t] in
  [%stri
    let [%p pvar ~loc (if syn then "pp_syn" else "pp")] =
     fun fmt (x : [%t ty]) ->
      Format.fprintf fmt "@[<2>{ ";
      [%e body];
      Format.fprintf fmt "@ }@]"]

let pp_items ~loc ~cfg fields =
  [
    pp_item ~loc ~cfg ~syn:false fields;
    [%stri let show x = Format.asprintf "%a" pp x];
    pp_item ~loc ~cfg ~syn:true fields;
    [%stri let show_syn x = Format.asprintf "%a" pp_syn x];
  ]

(* {2 Functions} *)

(** [fresh ()] returns a fresh value, monadically. *)
let fresh_item ~loc ~cfg fields =
  let contribution f =
    match override f "fresh" with
    | Some e -> Some (app ~loc e [ eunit ~loc ])
    | None -> (
        match f.fkind with
        | Fmod m -> Some (app ~loc (qual ~loc m [ "fresh" ]) [ eunit ~loc ])
        | Ftyped { witness = Some w } ->
            Some
              (app ~loc
                 (qual ~loc cfg.symex [ "nondet" ])
                 [ qual ~loc cfg.value [ w ] ])
        | Ftyped { witness = None } | Fconcrete -> None)
  in
  let binds = List.map (fun f -> (f, contribution f)) fields in
  if List.exists (fun (_, c) -> Option.is_none c) binds then
    [%stri let fresh () = failwith "[@@deriving abstr]: no fresh for this type"]
  else
    let record =
      pexp_record ~loc
        (List.map
           (fun f -> (lid ~loc (Lident f.fname), evar ~loc f.fname))
           fields)
        None
    in
    let final = app ~loc (qual ~loc cfg.symex [ "return" ]) [ record ] in
    let body =
      List.fold_right
        (fun (f, c) acc ->
          let e = Option.get c in
          [%expr
            let* [%p pvar ~loc f.fname] = [%e e] in
            [%e acc]])
        binds final
    in
    [%stri
      let fresh () = [%e with_open ~loc (qual_lid cfg.symex [ "Syntax" ]) body]]

let to_syn_item ~loc ~cfg fields =
  let entry f =
    let access = field_of ~loc (evar ~loc "x") f.fname in
    let e =
      match override f "to_syn" with
      | Some e -> app ~loc e [ access ]
      | None -> (
          match f.fkind with
          | Fmod m -> app ~loc (qual ~loc m [ "to_syn" ]) [ access ]
          | Ftyped _ ->
              app ~loc (qual ~loc cfg.value [ "Expr"; "of_value" ]) [ access ]
          | Fconcrete -> access)
    in
    (lid ~loc (Lident f.fname), e)
  in
  let record = pexp_record ~loc (List.map entry fields) None in
  [%stri let to_syn (x : t) : syn = [%e record]]

let subst_item ~loc ~cfg fields =
  let entry f =
    let access = field_of ~loc (evar ~loc "x") f.fname in
    let e =
      match override f "subst" with
      | Some e -> app ~loc e [ evar ~loc "sub"; access ]
      | None -> (
          match f.fkind with
          | Fmod m ->
              app ~loc (qual ~loc m [ "subst" ]) [ evar ~loc "sub"; access ]
          | Ftyped _ ->
              app ~loc
                (qual ~loc cfg.value [ "Expr"; "subst" ])
                [ evar ~loc "sub"; access ]
          | Fconcrete -> access)
    in
    (lid ~loc (Lident f.fname), e)
  in
  let record = pexp_record ~loc (List.map entry fields) None in
  [%stri let subst sub (x : syn) : t = [%e record]]

let learn_eq_item ~loc ~cfg fields =
  let entry f =
    let s = field_of ~loc (evar ~loc "s") f.fname in
    let st = field_of ~loc (evar ~loc "st") f.fname in
    match override f "learn_eq" with
    | Some e -> app ~loc e [ s; st ]
    | None -> (
        match f.fkind with
        | Fmod m -> app ~loc (qual ~loc m [ "learn_eq" ]) [ s; st ]
        | Ftyped _ ->
            app ~loc (qual ~loc cfg.symex [ "Consumer"; "learn_eq" ]) [ s; st ]
        | Fconcrete ->
            let ok =
              app ~loc (qual ~loc cfg.symex [ "Consumer"; "ok" ]) [ eunit ~loc ]
            in
            let lfail =
              app ~loc
                (qual ~loc cfg.symex [ "Consumer"; "lfail" ])
                [
                  app ~loc (qual ~loc cfg.value [ "of_bool" ]) [ [%expr false] ];
                ]
            in
            [%expr if [%e s] = [%e st] then [%e ok] else [%e lfail]])
  in
  let final =
    app ~loc (qual ~loc cfg.symex [ "Consumer"; "ok" ]) [ eunit ~loc ]
  in
  let body =
    List.fold_right
      (fun f acc ->
        [%expr
          let* () = [%e entry f] in
          [%e acc]])
      fields final
  in
  [%stri
    let learn_eq (s : syn) (st : t) =
      [%e with_open ~loc (qual_lid cfg.symex [ "Consumer"; "Syntax" ]) body]]

let exprs_syn_item ~loc ~cfg fields =
  let entries =
    List.filter_map
      (fun f ->
        let s = field_of ~loc (evar ~loc "s") f.fname in
        match override f "exprs_syn" with
        | Some e -> Some (app ~loc e [ s ])
        | None -> (
            match f.fkind with
            | Fmod m -> Some (app ~loc (qual ~loc m [ "exprs_syn" ]) [ s ])
            | Ftyped _ -> Some [%expr [ [%e s] ]]
            | Fconcrete -> None))
      fields
  in
  let body =
    match entries with
    | [] -> [%expr []]
    | hd :: tl -> List.fold_left (fun acc e -> [%expr [%e acc] @ [%e e]]) hd tl
  in
  [%stri let exprs_syn (s : syn) = [%e body]]

let sem_eq_item ~loc ~cfg fields =
  let entry f =
    let a = field_of ~loc (evar ~loc "a") f.fname in
    let b = field_of ~loc (evar ~loc "b") f.fname in
    match override f "sem_eq" with
    | Some e -> app ~loc e [ a; b ]
    | None -> (
        match f.fkind with
        | Fmod m -> app ~loc (qual ~loc m [ "sem_eq" ]) [ a; b ]
        | Ftyped _ ->
            app ~loc (qual ~loc cfg.value [ "sem_eq_untyped" ]) [ a; b ]
        | Fconcrete ->
            app ~loc
              (qual ~loc cfg.value [ "of_bool" ])
              [ [%expr [%e a] = [%e b]] ])
  in
  let body =
    match fields with
    | [] -> app ~loc (qual ~loc cfg.value [ "of_bool" ]) [ [%expr true] ]
    | hd :: tl ->
        List.fold_left
          (fun acc f ->
            app ~loc (qual ~loc cfg.value [ "Bool"; "and_" ]) [ acc; entry f ])
          (entry hd) tl
  in
  [%stri let sem_eq (a : t) (b : t) = [%e body]]

let simplify_item ~loc ~cfg fields =
  let contribution f =
    let access = field_of ~loc (evar ~loc "x") f.fname in
    match override f "simplify" with
    | Some e -> app ~loc e [ access ]
    | None -> (
        match f.fkind with
        | Fmod m -> app ~loc (qual ~loc m [ "simplify" ]) [ access ]
        | Ftyped _ -> app ~loc (qual ~loc cfg.symex [ "simplify" ]) [ access ]
        | Fconcrete -> app ~loc (qual ~loc cfg.symex [ "return" ]) [ access ])
  in
  let record =
    pexp_record ~loc
      (List.map
         (fun f -> (lid ~loc (Lident f.fname), evar ~loc f.fname))
         fields)
      None
  in
  let final = app ~loc (qual ~loc cfg.symex [ "return" ]) [ record ] in
  let body =
    List.fold_right
      (fun f acc ->
        [%expr
          let* [%p pvar ~loc f.fname] = [%e contribution f] in
          [%e acc]])
      fields final
  in
  [%stri
    let simplify (x : t) =
      [%e with_open ~loc (qual_lid cfg.symex [ "Syntax" ]) body]]

(* {2 Driving the expansion} *)

(** [@@deriving] attribute for [abstr_raw] carrying any derivers the user listed
    besides [abstr] (e.g. [eq]). [pp]/[show] are generated manually, so [show]
    is not added here and the generated code does not depend on [ppx_deriving].
*)
let derivers_attr ~loc leftover =
  match leftover with
  | [] -> []
  | _ ->
      let payload =
        match leftover with [ e ] -> e | es -> pexp_tuple ~loc es
      in
      [
        {
          attr_name = lid ~loc "deriving";
          attr_payload = PStr [ pstr_eval ~loc payload [] ];
          attr_loc = loc;
        };
      ]

let expand ~loc ~cfg ~leftover (td : type_declaration) =
  if td.ptype_name.txt <> "t" then
    err ~loc:td.ptype_name.loc "only supports a type named 't'";
  let fields =
    match td.ptype_kind with
    | Ptype_record labels -> List.map (mk_field ~cfg) labels
    | _ -> err ~loc "only supports record types"
  in
  let base =
    [
      abstr_raw_decl ~loc ~derivers:(derivers_attr ~loc leftover) fields;
      t_decl ~loc fields;
      syn_decl ~loc ~cfg fields;
    ]
    @ pp_items ~loc ~cfg fields
    @ [
        fresh_item ~loc ~cfg fields;
        to_syn_item ~loc ~cfg fields;
        subst_item ~loc ~cfg fields;
        learn_eq_item ~loc ~cfg fields;
        exprs_syn_item ~loc ~cfg fields;
      ]
  in
  let opt = [] in
  let opt =
    if cfg.gen_sem_eq then sem_eq_item ~loc ~cfg fields :: opt else opt
  in
  let opt =
    if cfg.gen_simplify then simplify_item ~loc ~cfg fields :: opt else opt
  in
  base @ opt

(** Splits a type's attributes: returns
    [Some (abstr_args, leftover_derivers, other_attrs)] if
    [@@deriving abstr ...] is present, else [None]. *)
let split_abstr (td : type_declaration) =
  let is_deriving a = String.equal a.attr_name.txt "deriving" in
  let deriving_attr = List.find_opt is_deriving td.ptype_attributes in
  let other_attrs =
    List.filter (fun a -> not (is_deriving a)) td.ptype_attributes
  in
  match deriving_attr with
  | None -> None
  | Some attr -> (
      let elements =
        match attr.attr_payload with
        | PStr
            [
              { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple es; _ }, _); _ };
            ] ->
            es
        | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> [ e ]
        | _ -> []
      in
      let head e =
        match e.pexp_desc with
        | Pexp_ident { txt; _ } -> Some (txt, None)
        | Pexp_apply
            ({ pexp_desc = Pexp_ident { txt; _ }; _ }, [ (Nolabel, arg) ]) ->
            Some (txt, Some arg)
        | _ -> None
      in
      let is_abstr e =
        match head e with
        | Some (Lident "abstr", _) -> true
        | Some (Ldot (Lident "soteria", "abstr"), _) -> true
        | _ -> false
      in
      match List.find_opt is_abstr elements with
      | None -> None
      | Some abstr_e ->
          let args =
            match head abstr_e with Some (_, a) -> a | None -> None
          in
          let leftover = List.filter (fun e -> not (is_abstr e)) elements in
          Some (args, leftover, other_attrs))

let transform_item (item : structure_item) =
  match item.pstr_desc with
  | Pstr_type (_, [ td ]) -> (
      match split_abstr td with
      | None -> [ item ]
      | Some (args, leftover, _other) ->
          let loc = td.ptype_loc in
          let cfg = parse_config ~loc args in
          expand ~loc ~cfg ~leftover td)
  | Pstr_type (_, tds) ->
      List.iter
        (fun td ->
          if Option.is_some (split_abstr td) then
            err ~loc:td.ptype_loc
              "expects exactly one type declaration named 't'")
        tds;
      [ item ]
  | _ -> [ item ]

(* Rewrites [@@deriving abstr] type declarations at every nesting level
   (top-level and inside nested modules). *)
let mapper =
  object
    inherit Ast_traverse.map as super

    method! structure str =
      let str = super#structure str in
      List.concat_map transform_item str
  end

let transform (str : structure) = mapper#structure str

let register () =
  (* The actual expansion is done by the [preprocess_impl] pass below, which
     rewrites the [type t] declaration into [abstr_raw]/[t]/[syn] and the
     generated functions. We still register [abstr] as a (no-op) deriver so that
     ppxlib's up-front "deriver exists" check passes: by the time the deriving
     pass runs, the preprocessing has already stripped the [abstr] attribute, so
     this generator never actually fires. *)
  let args =
    Deriving.Args.(
      empty
      +> arg "sem_eq" __
      +> arg "simplify" __
      +> arg "value" __
      +> arg "symex" __)
  in
  let noop ~loc:_ ~path:_ _ _ _ _ _ = [] in
  Deriving.add Names.ppx ~str_type_decl:(Deriving.Generator.make args noop)
  |> Deriving.ignore;
  Driver.register_transformation (Names.ppx ^ "_rewrite")
    ~preprocess_impl:transform
