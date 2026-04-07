open Ppxlib

module Syntaxes = struct
  let ( let@ ) = ( @@ )
end

module LocCtx = struct
  open Ast_builder.Default

  type _ Effect.t += Get_loc : location Effect.t

  let with_loc (loc : location) f =
    let open Effect.Deep in
    try f loc with effect Get_loc, k -> continue k loc

  let get_loc () = Effect.perform Get_loc
  let wloc x = { loc = get_loc (); txt = x }

  (* Override anything we need *)
  let constructor_declaration ~name ~args =
    constructor_declaration ~loc:(get_loc ()) ~name:(wloc name) ~args

  let estring x = estring ~loc:(get_loc ()) x
  let eunit () = eunit ~loc:(get_loc ())
  let evar x = evar ~loc:(get_loc ()) x
  let pexp_apply x y = pexp_apply ~loc:(get_loc ()) x y
  let pexp_construct x y = pexp_construct ~loc:(get_loc ()) x y
  let pexp_field x y = pexp_field ~loc:(get_loc ()) x y
  let pexp_ident x = pexp_ident ~loc:(get_loc ()) x
  let pexp_match x y = pexp_match ~loc:(get_loc ()) x y
  let pexp_record x y = pexp_record ~loc:(get_loc ()) x y
  let pexp_tuple x = pexp_tuple ~loc:(get_loc ()) x
  let pexp_unreachable () = pexp_unreachable ~loc:(get_loc ())
  let pmod_ident x = pmod_ident ~loc:(get_loc ()) x
  let ppat_any () = ppat_any ~loc:(get_loc ())
  let ppat_construct x y = ppat_construct ~loc:(get_loc ()) x y
  let ppat_record x y = ppat_record ~loc:(get_loc ()) x y

  let ppat_tuple pats =
    match pats with
    | [] -> ppat_any ()
    | [ pat ] -> pat
    | _ -> ppat_tuple ~loc:(get_loc ()) pats

  let ppat_var x = ppat_var ~loc:(get_loc ()) x
  let pstr_type x y = pstr_type ~loc:(get_loc ()) x y
  let ptyp_constr x y = ptyp_constr ~loc:(get_loc ()) x y
  let pvar x = pvar ~loc:(get_loc ()) x

  let type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest =
    type_declaration ~loc:(get_loc ()) ~name:(wloc name) ~params ~cstrs ~kind
      ~private_ ~manifest
end

module Printers = struct
  let rec pp_longident fmt = function
    | Lident s -> Format.pp_print_string fmt s
    | Ldot (base, name) -> Format.fprintf fmt "%a.%s" pp_longident base name
    | Lapply (f, arg) ->
        Format.fprintf fmt "%a(%a)" pp_longident f pp_longident arg
end

module Attributes : sig
  type 'a parser

  val must : string -> expression parser
  val may : string -> expression option parser
  val pair : 'a parser -> 'b parser -> ('a * 'b) parser
  val ( ** ) : 'a parser -> 'b parser -> ('a * 'b) parser

  val declare_record :
    name:string -> 'a parser -> (label_declaration, 'a) Attribute.t
end = struct
  (** Given a list of fields, returns their bindings in order and all extra
      bindings. *)
  let find_expr_fields bindings fields =
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
    find_fields [] bindings fields

  type 'a parser = {
    fields : string list;
    parse :
      name:string ->
      loc:Location.t ->
      expression option list ->
      'a * expression option list;
  }

  let must field =
    {
      fields = [ field ];
      parse =
        (fun ~name ~loc -> function
          | Some v :: tl -> (v, tl)
          | None :: _ ->
              Location.raise_errorf ~loc "expects [@%s { %s = <expr>; ... }]"
                name field
          | [] -> failwith "Impossible");
    }

  let may field =
    {
      fields = [ field ];
      parse =
        (fun ~name:_ ~loc:_ -> function
          | v :: tl -> (v, tl) | [] -> failwith "Impossible");
    }

  let pair lhs rhs =
    {
      fields = lhs.fields @ rhs.fields;
      parse =
        (fun ~name ~loc values ->
          let lhs_parsed, values = lhs.parse ~name ~loc values in
          let rhs_parsed, values = rhs.parse ~name ~loc values in
          ((lhs_parsed, rhs_parsed), values));
    }

  let ( ** ) = pair

  let validate_attr_field ~name parser ~attr_loc:loc bindings =
    let fields = parser.fields in
    match find_expr_fields bindings fields with
    | found, [] ->
        assert (List.compare_lengths found fields = 0);
        let parsed, remaining = parser.parse ~name ~loc found in
        assert (remaining = []);
        parsed
    | _, extra ->
        Location.raise_errorf ~loc
          "unexpected field(s) '%a' in [@%s], expected one of %s"
          Fmt.(list ~sep:(Fmt.any ", ") Printers.pp_longident)
          (List.map (fun (f, _) -> f.txt) extra)
          name
          (String.concat "; " fields)

  let declare_record ~name parser =
    Attribute.declare_with_attr_loc name Attribute.Context.label_declaration
      Ast_pattern.(single_expr_payload (pexp_record __ drop))
      (validate_attr_field ~name parser)
end
