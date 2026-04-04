open Ppxlib
open Ast_builder.Default

let err ~loc msg = Location.raise_errorf ~loc "[@@deriving diagnostic] %s" msg

let find_attr name attrs =
  List.find_opt (fun (a : attribute) -> String.equal a.attr_name.txt name) attrs

let parse_attr_expr name attrs =
  match find_attr name attrs with
  | None -> None
  | Some
      ({ attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ } :
        attribute) ->
      Some e
  | Some attr ->
      err ~loc:attr.attr_loc
        (Fmt.str "attribute [@%s] expects expression payload" name)

let parse_string_expr ~loc name = function
  | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> s
  | _ ->
      err ~loc (Fmt.str "attribute [@%s] expects string literal payload" name)

let default_kind_of_name (name : string) : string =
  let b = Buffer.create (String.length name * 2) in
  let is_upper c =
    let x = Char.code c in
    x >= Char.code 'A' && x <= Char.code 'Z'
  in
  let is_lower c =
    let x = Char.code c in
    x >= Char.code 'a' && x <= Char.code 'z'
  in
  let is_alpha c = is_upper c || is_lower c in
  let rec loop i prev_is_upper prev_is_alpha =
    if i >= String.length name then ()
    else
      let c = String.get name i in
      let curr_is_upper = is_upper c in
      let curr_is_alpha = is_alpha c in
      let next_is_lower =
        if i + 1 < String.length name then is_lower (String.get name (i + 1))
        else false
      in
      let needs_sep =
        i > 0
        && curr_is_upper
        && ((prev_is_alpha && not prev_is_upper) || (prev_is_upper && next_is_lower))
      in
      if needs_sep then Buffer.add_char b '_';
      Buffer.add_char b (Char.uppercase_ascii c);
      loop (i + 1) curr_is_upper curr_is_alpha
  in
  loop 0 false false;
  Buffer.contents b

let printer_attr_of_type (ty : core_type) : expression option =
  let find_attr name attrs =
    List.find_opt (fun (a : attribute) -> String.equal a.attr_name.txt name) attrs
  in
  match find_attr "printer" ty.ptyp_attributes with
  | Some
      ({
         attr_payload =
           PStr [ { pstr_desc = Pstr_eval (printer_expr, _); _ } ];
         _;
       } : attribute) ->
      Some printer_expr
  | Some attr ->
      err ~loc:attr.attr_loc
        "attribute [@printer] expects payload [@printer <pp_function>]"
  | None -> None

type format_spec = String_fmt of string | Printer_fn of expression

let parse_format_spec attrs =
  match parse_attr_expr "diag.format" attrs with
  | None -> None
  | Some { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } ->
      Some (String_fmt s)
  | Some e -> Some (Printer_fn e)

let format_arg_count fmt =
  let rec loop i acc =
    if i + 1 >= String.length fmt then acc
    else if fmt.[i] = '%' then
      match fmt.[i + 1] with
      | '%' -> loop (i + 2) acc
      | 'a' -> loop (i + 2) (acc + 1)
      | _ -> loop (i + 2) acc
    else loop (i + 1) acc
  in
  loop 0 0

let pp_list_sep ~loc = [%expr fun ft () -> Format.pp_print_string ft "; "]

let rec pp_of_type ~loc (ty : core_type) : expression =
  match printer_attr_of_type ty with
  | Some pp -> pp
  | None ->
  let pp_tuple tys =
    let names = List.mapi (fun i _ -> "x" ^ string_of_int i) tys in
    let tuple_pat =
      ppat_tuple ~loc (List.map (fun n -> ppat_var ~loc { loc; txt = n }) names)
    in
    let rec print_elems i =
      if i >= List.length tys then [%expr ()]
      else
        let pp_i = pp_of_type ~loc (List.nth tys i) in
        let x_i = evar ~loc (List.nth names i) in
        let rest = print_elems (i + 1) in
        if i = 0 then
          [%expr
            [%e pp_i] ft [%e x_i];
            [%e rest]]
        else
          [%expr
            Format.pp_print_string ft ", ";
            [%e pp_i] ft [%e x_i];
            [%e rest]]
    in
    [%expr
      fun ft [%p tuple_pat] ->
        Format.pp_print_string ft "(";
        [%e print_elems 0];
        Format.pp_print_string ft ")"]
  in
  match ty.ptyp_desc with
  | Ptyp_any -> [%expr fun ft _ -> Format.pp_print_string ft "_"]
  | Ptyp_var n -> evar ~loc ("pp_" ^ n)
  | Ptyp_tuple tys -> pp_tuple tys
  | Ptyp_alias (t, _) -> pp_of_type ~loc t
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
      [%expr Format.pp_print_string]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr Format.pp_print_int]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr Format.pp_print_bool]
  | Ptyp_constr ({ txt = Lident "char"; _ }, []) -> [%expr Format.pp_print_char]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
      [%expr Format.pp_print_float]
  | Ptyp_constr ({ txt = Lident "int32"; _ }, []) -> [%expr Int32.pp]
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) -> [%expr Int64.pp]
  | Ptyp_constr ({ txt = Lident "nativeint"; _ }, []) -> [%expr Nativeint.pp]
  | Ptyp_constr ({ txt = Lident "bytes"; _ }, []) ->
      [%expr fun ft b -> Format.pp_print_string ft (Bytes.to_string b)]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> [%expr fun _ _ -> ()]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ t ]) ->
      let pp_t = pp_of_type ~loc t in
      [%expr Format.pp_print_option [%e pp_t]]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ t ]) ->
      let pp_t = pp_of_type ~loc t in
      [%expr Format.pp_print_list ~pp_sep:[%e pp_list_sep ~loc] [%e pp_t]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ t ]) ->
      let pp_t = pp_of_type ~loc t in
      [%expr
        fun ft arr ->
          Format.pp_print_list ~pp_sep:[%e pp_list_sep ~loc] [%e pp_t] ft
            (Array.to_list arr)]
  | Ptyp_constr ({ txt = Lident "seq"; _ }, [ t ]) ->
      let pp_t = pp_of_type ~loc t in
      [%expr
        fun ft s ->
          Format.pp_print_list ~pp_sep:[%e pp_list_sep ~loc] [%e pp_t] ft
            (List.of_seq s)]
  | Ptyp_constr ({ txt = Lident "result"; _ }, [ ok; er ]) ->
      let pp_ok = pp_of_type ~loc ok in
      let pp_er = pp_of_type ~loc er in
      [%expr Format.pp_print_result ~ok:[%e pp_ok] ~error:[%e pp_er]]
  | Ptyp_constr ({ txt = Lident name; _ }, params) ->
      let pp_name =
        if String.equal name "t" then evar ~loc "pp"
        else evar ~loc ("pp_" ^ name)
      in
      List.fold_left
        (fun acc p -> [%expr [%e acc] [%e pp_of_type ~loc p]])
        pp_name params
  | Ptyp_constr ({ txt = Ldot (path, "t"); _ }, params) ->
      let pp_mod = pexp_ident ~loc { loc; txt = Ldot (path, "pp") } in
      List.fold_left
        (fun acc p -> [%expr [%e acc] [%e pp_of_type ~loc p]])
        pp_mod params
  | Ptyp_constr ({ txt = Ldot (path, name); _ }, params) ->
      let pp_mod = pexp_ident ~loc { loc; txt = Ldot (path, "pp_" ^ name) } in
      List.fold_left
        (fun acc p -> [%expr [%e acc] [%e pp_of_type ~loc p]])
        pp_mod params
  | Ptyp_variant (rows, _, _) ->
      let row_cases =
        List.map
          (fun row ->
            match row.prf_desc with
            | Rtag (label, true, []) ->
                case
                  ~lhs:(ppat_variant ~loc label.txt None)
                  ~guard:None
                  ~rhs:
                    [%expr
                      Format.pp_print_string ft [%e estring ~loc label.txt]]
            | Rtag (_, true, _ :: _) ->
                err ~loc:row.prf_loc
                  "constant polymorphic variant constructors cannot have \
                   payloads"
            | Rtag (label, false, tys) ->
                let names = List.mapi (fun i _ -> "v" ^ string_of_int i) tys in
                let pat_arg =
                  match names with
                  | [ n ] -> Some (ppat_var ~loc { loc; txt = n })
                  | _ ->
                      Some
                        (ppat_tuple ~loc
                           (List.map
                              (fun n -> ppat_var ~loc { loc; txt = n })
                              names))
                in
                let rhs =
                  List.fold_left2
                    (fun acc ty n ->
                      let pp = pp_of_type ~loc ty in
                      [%expr
                        [%e acc];
                        Format.pp_print_string ft " ";
                        [%e pp] ft [%e evar ~loc n]])
                    [%expr
                      Format.pp_print_string ft [%e estring ~loc label.txt]]
                    tys names
                in
                case ~lhs:(ppat_variant ~loc label.txt pat_arg) ~guard:None ~rhs
            | Rinherit _ ->
                err ~loc:row.prf_loc
                  "polymorphic variant inheritance not supported for guessed \
                   printers")
          rows
      in
      [%expr fun ft v -> [%e pexp_match ~loc [%expr v] row_cases]]
  | _ ->
      err ~loc:ty.ptyp_loc
        "unsupported constructor argument type for guessed printers"

type ctor_info = {
  name : string;
  attrs : attributes;
  tys : core_type list;
  build_pat : Location.t -> pattern;
  vars : string list;
}

let ctor_info_of_cd (cd : constructor_declaration) : ctor_info =
  match cd.pcd_args with
  | Pcstr_tuple tys ->
      let vars = List.mapi (fun i _ -> "v" ^ string_of_int i) tys in
      let build_pat loc =
        let pat_arg =
          match vars with
          | [] -> None
          | [ v ] -> Some (ppat_var ~loc { loc; txt = v })
          | _ ->
              Some
                (ppat_tuple ~loc
                   (List.map (fun v -> ppat_var ~loc { loc; txt = v }) vars))
        in
        ppat_construct ~loc { loc; txt = Lident cd.pcd_name.txt } pat_arg
      in
      {
        name = cd.pcd_name.txt;
        attrs = cd.pcd_attributes;
        tys;
        build_pat;
        vars;
      }
  | Pcstr_record _ ->
      err ~loc:cd.pcd_loc
        "record constructors are not supported by deriving diagnostic"

let ctor_info_of_rtag (row : row_field) : ctor_info =
  match row.prf_desc with
  | Rtag (label, true, []) ->
      let build_pat loc = ppat_variant ~loc label.txt None in
      {
        name = label.txt;
        attrs = row.prf_attributes;
        tys = [];
        build_pat;
        vars = [];
      }
  | Rtag (_, true, _ :: _) ->
      err ~loc:row.prf_loc
        "constant polymorphic variant constructors cannot have payloads"
  | Rtag (label, false, tys) ->
      let vars = List.mapi (fun i _ -> "v" ^ string_of_int i) tys in
      let build_pat loc =
        let pat_arg =
          match vars with
          | [ v ] -> Some (ppat_var ~loc { loc; txt = v })
          | _ ->
              Some
                (ppat_tuple ~loc
                   (List.map (fun v -> ppat_var ~loc { loc; txt = v }) vars))
        in
        ppat_variant ~loc label.txt pat_arg
      in
      { name = label.txt; attrs = row.prf_attributes; tys; build_pat; vars }
  | Rinherit _ ->
      err ~loc:row.prf_loc
        "polymorphic variant inheritance is not supported by deriving \
         diagnostic"

let mk_pp_case ~loc (ci : ctor_info) =
  let fmt_spec = parse_format_spec ci.attrs in
  let pat = ci.build_pat loc in
  let rhs =
    match fmt_spec with
    | Some (Printer_fn pp_fn) ->
        let arg_expr =
          match ci.vars with
          | [] -> [%expr ()]
          | [ v ] -> evar ~loc v
          | _ -> pexp_tuple ~loc (List.map (evar ~loc) ci.vars)
        in
        [%expr [%e pp_fn] ft [%e arg_expr]]
    | Some (String_fmt fmt) ->
        let placeholders = format_arg_count fmt in
        let mk_format_call printers vars =
          match vars with
          | [] -> [%expr Format.pp_print_string ft [%e estring ~loc fmt]]
          | _ ->
              List.fold_left2
                (fun acc pp v -> [%expr [%e acc] [%e pp] [%e evar ~loc v]])
                [%expr Fmt.pf ft [%e estring ~loc fmt]] printers vars
        in
        if placeholders = List.length ci.vars then
          let printers = List.map (pp_of_type ~loc) ci.tys in
          mk_format_call printers ci.vars
        else (
          match (ci.tys, ci.vars) with
          | [ { ptyp_desc = Ptyp_tuple tys; _ } ], [ tuple_v ]
            when placeholders = List.length tys ->
              let tuple_vars = List.mapi (fun i _ -> "t" ^ string_of_int i) tys in
              let tuple_pat =
                ppat_tuple ~loc
                  (List.map (fun n -> ppat_var ~loc { loc; txt = n }) tuple_vars)
              in
              let printers = List.map (pp_of_type ~loc) tys in
              let body = mk_format_call printers tuple_vars in
              [%expr
                let [%p tuple_pat] = [%e evar ~loc tuple_v] in
                [%e body]]
          | _ ->
              err ~loc
                (Fmt.str
                   "constructor %s has %d args but [@diag.format] has %d %%a \
                    placeholders"
                   ci.name (List.length ci.vars) placeholders))
    | None -> [%expr Format.pp_print_string ft [%e estring ~loc ci.name]]
  in
  case ~lhs:pat ~guard:None ~rhs

let mk_kind_case ~loc (ci : ctor_info) =
  let kind =
    parse_attr_expr "diag.kind" ci.attrs
    |> Option.map (parse_string_expr ~loc "diag.kind")
    |> Option.value ~default:(default_kind_of_name ci.name)
  in
  case ~lhs:(ci.build_pat loc) ~guard:None ~rhs:(estring ~loc kind)

let severity_path_of_expr ~loc = function
  | { pexp_desc = Pexp_construct ({ txt; _ }, None); _ } -> (
      match txt with
      | Lident _ | Ldot _ -> txt
      | _ ->
          err ~loc
            "attribute [@diag.severity] expects constructor-like identifier")
  | { pexp_desc = Pexp_ident { txt; _ }; _ } -> (
      match txt with
      | Lident _ | Ldot _ -> txt
      | _ ->
          err ~loc
            "attribute [@diag.severity] expects constructor-like identifier")
  | _ ->
      err ~loc "attribute [@diag.severity] expects constructor-like identifier"

let mk_severity_case ~loc (ci : ctor_info) =
  let sev_path =
    parse_attr_expr "diag.severity" ci.attrs
    |> Option.map (severity_path_of_expr ~loc)
    |> Option.value ~default:(Lident "Error")
  in
  let sev_expr =
    match sev_path with
    | Lident n ->
        pexp_construct ~loc
          {
            loc;
            txt =
              Ldot (Ldot (Ldot (Lident "Soteria", "Terminal"), "Diagnostic"), n);
          }
          None
    | Ldot (_, _) -> pexp_construct ~loc { loc; txt = sev_path } None
    | _ -> err ~loc "unsupported severity path"
  in
  case ~lhs:(ci.build_pat loc) ~guard:None ~rhs:sev_expr

let generate ~loc ~td_name ctor_infos =
  let is_recursive_pp =
    List.exists
      (fun ci ->
        List.exists
          (fun ty ->
            match ty.ptyp_desc with
            | Ptyp_constr ({ txt = Lident n; _ }, _) -> String.equal n td_name
            | _ -> false)
          ci.tys)
      ctor_infos
  in
  let pp_cases = List.map (mk_pp_case ~loc) ctor_infos in
  let kind_cases = List.map (mk_kind_case ~loc) ctor_infos in
  let severity_cases = List.map (mk_severity_case ~loc) ctor_infos in
  let pp_binding =
    if is_recursive_pp then
      [%stri
        let rec pp ft
            (e : [%t ptyp_constr ~loc { loc; txt = Lident td_name } []]) =
          [%e pexp_match ~loc [%expr e] pp_cases]]
    else
      [%stri
        let pp ft (e : [%t ptyp_constr ~loc { loc; txt = Lident td_name } []]) =
          [%e pexp_match ~loc [%expr e] pp_cases]]
  in
  [
    pp_binding;
    [%stri
      let kind_string
          (e : [%t ptyp_constr ~loc { loc; txt = Lident td_name } []]) =
        [%e pexp_match ~loc [%expr e] kind_cases]];
    [%stri
      let severity (e : [%t ptyp_constr ~loc { loc; txt = Lident td_name } []])
          =
        [%e pexp_match ~loc [%expr e] severity_cases]];
  ]

let str_type_decl ~loc ~path:_ (_rec, tds) =
  let loc = { loc with loc_ghost = true } in
  match tds with
  | [ td ] -> (
      match td.ptype_kind with
      | Ptype_variant ctors ->
          let infos = List.map ctor_info_of_cd ctors in
          generate ~loc ~td_name:td.ptype_name.txt infos
      | Ptype_abstract -> (
          match td.ptype_manifest with
          | Some { ptyp_desc = Ptyp_variant (rows, _, _); _ } ->
              let infos = List.map ctor_info_of_rtag rows in
              generate ~loc ~td_name:td.ptype_name.txt infos
          | _ -> err ~loc "only variants and polymorphic variants are supported"
          )
      | _ -> err ~loc "only variants and polymorphic variants are supported")
  | _ -> err ~loc "expects exactly one type declaration"

let register () =
  let str = Deriving.Generator.make Deriving.Args.empty str_type_decl in
  Deriving.add "diagnostic" ~str_type_decl:str |> Deriving.ignore
