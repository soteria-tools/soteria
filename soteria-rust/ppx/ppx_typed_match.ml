open Ppxlib

(** [match%ty x with ...] matches on the {e runtime} type of the typed value [x]
    (via [Typed.get_ty]) and, in each branch, rebinds [x] to the value cast at
    the static phantom type implied by the matched runtime-type constructor.

    For example:
    {[
    match%ty x with
    | TBitVector _ -> f x (* x : [< T.sint ] t *)
    | TExtension FullPtr -> g x (* x : [< T.sptr_f ] t *)
    | _ -> h x (* x unchanged *)
    ]}
    desugars to:
    {[
    match Typed.get_ty x with
    | TBitVector _ ->
        let x = (Typed.cast x : Typed.([< T.sint ] t)) in
        f x
    | TExtension FullPtr ->
        let x = (Typed.cast x : Typed.([< T.sptr_f ] t)) in
        g x
    | _ -> h x
    ]}

    The casts are unchecked: correctness rests on the table below matching how
    values are typed. Defining it once here removes the per-site [Typed.cast]
    boilerplate and the risk of casting to the wrong type.

    Several scrutinees are also supported; only the {e first} is the [get_ty]
    subject, the rest are matched as-is:
    {[
    match%ty (v, to_ty) with
    | TBitVector _, TLiteral _ -> f v (* v : [< T.sint ] t; to_ty untouched *)
    | _, _ -> g v
    ]} *)

(** The phantom-type table: runtime-type constructor -> phantom tag key. The
    keys index into [Typed.T]; ["seq"] is special-cased (it is the applied type
    [T.any T.sseq]). *)
let key_of_ty_ctor = function
  | "TBool" -> Some "sbool"
  | "TFloat" -> Some "sfloat"
  | "TLoc" -> Some "sloc"
  | "TPointer" -> Some "sptr"
  | "TBitVector" -> Some "sint"
  | "TSeq" -> Some "seq"
  | _ -> None

(** Same as {!key_of_ty_ctor} but for extension constructors. *)
let key_of_ext_ctor = function
  | "TFullPtr" -> Some "sptr_f"
  | "TThinPtr" -> Some "sptr_t"
  | "TTuple" -> Some "tuple"
  | "TEnum" -> Some "enum"
  | "TUnion" -> Some "union"
  | "TPolyType" -> Some "poly"
  | _ -> None

(* The module providing [get_ty], [cast] and the phantom tags [T.*]. *)
let typed = "Typed"

let lid_of_path ~loc = function
  | [] -> invalid_arg "lid_of_path"
  | hd :: tl ->
      {
        txt =
          List.fold_left (fun acc s -> Longident.Ldot (acc, s)) (Lident hd) tl;
        loc;
      }

(* [ [< <tag for key> ] Typed.t ] *)
let phantom_ty ~loc key =
  let open Ast_builder.Default in
  let tag s = ptyp_constr ~loc (lid_of_path ~loc [ typed; "T"; s ]) [] in
  let tag_ct =
    match key with
    | "seq" ->
        ptyp_constr ~loc (lid_of_path ~loc [ typed; "T"; "sseq" ]) [ tag "any" ]
    | s -> tag s
  in
  let row =
    { prf_desc = Rinherit tag_ct; prf_loc = loc; prf_attributes = [] }
  in
  let variant = ptyp_variant ~loc [ row ] Closed (Some []) in
  ptyp_constr ~loc (lid_of_path ~loc [ typed; "t" ]) [ variant ]

let combine ~loc a b =
  match (a, b) with
  | None, None -> None
  | Some x, Some y when String.equal x y -> Some x
  | _ ->
      Location.raise_errorf ~loc
        "match%%ty: this or-pattern mixes branches with different runtime \
         types; split them into separate cases"

(* Classifies a [ty] pattern to the phantom tag key it refines the scrutinee to,
   or [None] when no single static type is implied (wildcards). Raises on
   unknown constructors and on or-patterns whose branches refine to different
   types. *)
let rec classify pat =
  match pat.ppat_desc with
  | Ppat_constraint (p, _) | Ppat_alias (p, _) -> classify p
  | Ppat_any | Ppat_var _ -> None
  | Ppat_or (p1, p2) -> combine ~loc:pat.ppat_loc (classify p1) (classify p2)
  | Ppat_construct (lid, arg) -> (
      let loc = pat.ppat_loc in
      match Longident.last_exn lid.txt with
      | "TExtension" -> (
          match arg with
          | Some (_, p) -> Some (classify_ext p)
          | None ->
              Location.raise_errorf ~loc
                "match%%ty: [TExtension] must be applied to an extension \
                 constructor")
      | ctor -> (
          match key_of_ty_ctor ctor with
          | Some key -> Some key
          | None ->
              Location.raise_errorf ~loc
                "match%%ty: unknown runtime-type constructor [%s]" ctor))
  | _ -> None

and classify_ext pat =
  match pat.ppat_desc with
  | Ppat_constraint (p, _) | Ppat_alias (p, _) -> classify_ext p
  | Ppat_construct (lid, _) -> (
      let ctor = Longident.last_exn lid.txt in
      match key_of_ext_ctor ctor with
      | Some key -> key
      | None ->
          Location.raise_errorf ~loc:pat.ppat_loc
            "match%%ty: unknown extension constructor [%s]" ctor)
  | _ ->
      Location.raise_errorf ~loc:pat.ppat_loc
        "match%%ty: [TExtension] expects an extension constructor"

(* Classifies a whole case pattern. In [tuple] mode (several scrutinees) only
   the first component is the [get_ty] subject; the rest are left untouched. *)
let rec classify_case ~tuple pat =
  if not tuple then classify pat
  else
    match pat.ppat_desc with
    | Ppat_constraint (p, _) | Ppat_alias (p, _) -> classify_case ~tuple p
    | Ppat_or (p1, p2) ->
        combine ~loc:pat.ppat_loc (classify_case ~tuple p1)
          (classify_case ~tuple p2)
    | Ppat_tuple (p0 :: _) -> classify p0
    | _ -> None

let rewrite_case ~var ~tuple case =
  match classify_case ~tuple case.pc_lhs with
  | None -> case
  | Some key ->
      let loc = case.pc_lhs.ppat_loc in
      let ct = phantom_ty ~loc key in
      let v = Ast_builder.Default.evar ~loc var in
      let p = Ast_builder.Default.pvar ~loc var in
      let pc_rhs =
        [%expr
          let [%p p] =
            (Typed.cast [%e v] : [%t ct])
              [@@warning "-unused-var"]
          in
          [%e case.pc_rhs]]
      in
      { case with pc_rhs }

let expand ~loc expr =
  match expr.pexp_desc with
  | Pexp_match (scrut, cases) ->
      (* With several scrutinees only the first is the [get_ty] subject. *)
      let tuple, first =
        match scrut.pexp_desc with
        | Pexp_tuple (e0 :: _) -> (true, e0)
        | _ -> (false, scrut)
      in
      let var =
        match first.pexp_desc with
        | Pexp_ident { txt = Lident v; _ } -> v
        | _ ->
            Location.raise_errorf ~loc:first.pexp_loc
              "match%%ty: the%s scrutinee must be a simple identifier (bind it \
               with [let] first)"
              (if tuple then " first" else "")
      in
      let cases = List.map (rewrite_case ~var ~tuple) cases in
      let scrut =
        match scrut.pexp_desc with
        | Pexp_tuple (e0 :: rest) ->
            {
              scrut with
              pexp_desc = Pexp_tuple ([%expr Typed.get_ty [%e e0]] :: rest);
            }
        | _ -> [%expr Typed.get_ty [%e scrut]]
      in
      { expr with pexp_desc = Pexp_match (scrut, cases) }
  | _ -> Location.raise_errorf ~loc "%%ty can only be used on a [match]"

let ext =
  Extension.declare "ty" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ expr -> expand ~loc expr)

let () = Driver.register_transformation "typed_match" ~extensions:[ ext ]
