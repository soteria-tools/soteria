open Charon

let size_of_int_ty : Values.int_ty -> int = function
  | I128 -> 16
  | I64 -> 8
  | I32 -> 4
  | I16 -> 2
  | I8 -> 1
  | Isize -> Crate.pointer_size ()

let size_of_uint_ty : Values.u_int_ty -> int = function
  | U128 -> 16
  | U64 -> 8
  | U32 -> 4
  | U16 -> 2
  | U8 -> 1
  | Usize -> Crate.pointer_size ()

let size_of_literal_ty : Types.literal_type -> int = function
  | TInt int_ty -> size_of_int_ty int_ty
  | TUInt uint_ty -> size_of_uint_ty uint_ty
  | TBool -> 1
  | TChar -> 4
  | TFloat F16 -> 2
  | TFloat F32 -> 4
  | TFloat F64 -> 8
  | TFloat F128 -> 16

let[@inline] is_signed : Types.literal_type -> bool = function
  | TInt _ -> true
  | _ -> false

let z_of_scalar : Values.scalar_value -> Z.t = function
  | SignedScalar (ty, v) when Z.lt v Z.zero ->
      let bits = size_of_int_ty ty * 8 in
      Z.(v land pred (one lsl bits))
  | UnsignedScalar (_, v) | SignedScalar (_, v) -> v

let z_of_literal : Values.literal -> Z.t = function
  | VScalar s -> z_of_scalar s
  | VBool b -> if b then Z.one else Z.zero
  | VChar c -> Z.of_int @@ Uchar.to_int c
  | VFloat _ | VByteStr _ | VStr _ -> failwith "z_of_literal: not an int"

let type_of_operand : Expressions.operand -> Types.ty = function
  | Constant c -> c.ty
  | Copy p | Move p -> p.ty

let lit_to_string = PrintValues.literal_type_to_string

let ty_as_float : Types.ty -> Values.float_type = function
  | TLiteral (TFloat f) -> f
  | _ -> failwith "ty_as_float: not a float type"

let pp_literal_ty = Fmt.of_to_string PrintValues.literal_type_to_string

let rec pp_ty fmt : Types.ty -> unit = function
  | TAdt { id = TAdtId id; generics } ->
      let adt = Crate.get_adt_raw id in
      let name, generics =
        match List.rev adt.item_meta.name with
        | PeInstantiated generics :: rest ->
            (List.rev rest, generics.binder_value)
        | _ ->
            let args =
              if (Config.get ()).polymorphic then generics
              else TypesUtils.empty_generic_args
            in
            (adt.item_meta.name, args)
      in
      Fmt.pf fmt "%a%a" Crate.pp_name name Crate.pp_generic_args generics
  | TAdt { id = TTuple; generics = { types = tys; _ } } ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_ty) tys
  | TAdt { id = TBuiltin TBox; generics = { types = [ ty ]; _ } } ->
      Fmt.pf fmt "Box<%a>" pp_ty ty
  | TAdt { id = TBuiltin TBox; _ } -> Fmt.string fmt "Box<?>"
  | TArray (ty, CgValue (VScalar len)) ->
      Fmt.pf fmt "[%a; %a]" pp_ty ty Z.pp_print (z_of_scalar len)
  | TArray (ty, _) -> Fmt.pf fmt "[%a; ?]" pp_ty ty
  | TSlice ty -> Fmt.pf fmt "[%a]" pp_ty ty
  | TAdt { id = TBuiltin TStr; _ } -> Fmt.string fmt "str"
  | TLiteral lit -> pp_literal_ty fmt lit
  | TNever -> Fmt.string fmt "!"
  | TRef (_, ty, RMut) -> Fmt.pf fmt "&mut %a" pp_ty ty
  | TRef (_, ty, RShared) -> Fmt.pf fmt "&%a" pp_ty ty
  | TRawPtr (ty, RMut) -> Fmt.pf fmt "*mut %a" pp_ty ty
  | TRawPtr (ty, RShared) -> Fmt.pf fmt "*const %a" pp_ty ty
  | TFnPtr { binder_value = { inputs; output; is_unsafe }; _ } ->
      Fmt.pf fmt "%sfn (%a) -> %a"
        (if is_unsafe then "unsafe " else "")
        Fmt.(list ~sep:(any ", ") pp_ty)
        inputs pp_ty output
  | TDynTrait _ -> Fmt.string fmt "dyn <trait>"
  | TTraitType (tref, name) ->
      Fmt.pf fmt "Trait<%a>::%s" Crate.pp_trait_ref tref name
  | TFnDef { binder_value = { kind = FunId (FRegular fid); _ }; _ } ->
      let f = Crate.get_fun fid in
      Fmt.pf fmt "fn %a" Crate.pp_name f.item_meta.name
  | TPtrMetadata ty -> Fmt.pf fmt "meta(%a)" pp_ty ty
  | TFnDef _ -> Fmt.string fmt "fn ?"
  | TVar var -> Fmt.string fmt (PrintTypes.type_db_var_to_pretty_string var)
  | TError err -> Fmt.pf fmt "Error(%s)" err

let lit_of_int_ty : Types.integer_type -> Types.literal_type = function
  | Signed ity -> TInt ity
  | Unsigned uty -> TUInt uty

let lit_of_scalar : Values.scalar_value -> Types.literal_type = function
  | SignedScalar (ity, _) -> TInt ity
  | UnsignedScalar (uty, _) -> TUInt uty

let lit_ty_of_lit : Values.literal -> Types.literal_type = function
  | VScalar s -> lit_of_scalar s
  | VBool _ -> TBool
  | VChar _ -> TChar
  | VFloat { float_ty; _ } -> TFloat float_ty
  | VStr _ | VByteStr _ -> failwith "lit_ty_of_lit: not a literal type"

let z_of_const_generic : Types.const_generic -> Z.t = function
  | CgValue (VScalar s) -> z_of_scalar s
  | cg ->
      Fmt.failwith "int_of_const_generic: unhandled const: %a"
        Types.pp_const_generic cg

let int_of_const_generic (c : Types.const_generic) : int =
  Z.to_int (z_of_const_generic c)

let field_tys = List.map (fun (f : Types.field) -> f.field_ty)

let empty_span_data : Meta.span_data =
  {
    beg_loc = { line = 0; col = 0 };
    end_loc = { line = 0; col = 0 };
    file = { name = Virtual ""; contents = None; crate_name = "" };
  }

let empty_span : Meta.span =
  { data = empty_span_data; generated_from_span = None }

let fields_of_tys : Types.ty list -> Types.field list =
  List.map (fun field_ty : Types.field ->
      {
        span = empty_span;
        attr_info =
          { attributes = []; inline = None; rename = None; public = true };
        field_name = None;
        field_ty;
      })

let mk_array_ty ty len : Types.ty =
  TArray (ty, CgValue (VScalar (UnsignedScalar (Usize, len))))

(** The type [*const ()] *)
let unit_ptr = Types.TRawPtr (TypesUtils.mk_unit_ty, RShared)

let unit_ref = Types.TRef (RErased, TypesUtils.mk_unit_ty, RShared)

let decl_has_attr (decl : 'a GAst.gfun_decl) attr =
  List.exists
    (function Meta.AttrUnknown { path; _ } -> path = attr | _ -> false)
    decl.item_meta.attr_info.attributes

let meta_get_attr (meta : Types.item_meta) attr =
  List.find_map
    (function
      | Meta.AttrUnknown { path; args } when path = attr -> args | _ -> None)
    meta.attr_info.attributes

let decl_get_attr (decl : 'a GAst.gfun_decl) attr =
  meta_get_attr decl.item_meta attr

let get_pointee : Types.ty -> Types.ty = function
  | TRef (_, ty, _)
  | TRawPtr (ty, _)
  | TAdt { id = TBuiltin TBox; generics = { types = [ ty ]; _ } } ->
      ty
  | _ -> failwith "Non-pointer type given to get_pointee"

let float_precision : Values.float_type -> Svalue.FloatPrecision.t = function
  | F16 -> F16
  | F32 -> F32
  | F64 -> F64
  | F128 -> F128

let ty_as_adt (ty : Types.ty) : Types.type_decl_ref =
  match ty with
  | TAdt tref -> tref
  | _ -> invalid_arg "ty_as_adt: not an ADT type"

(** Whether the given type is monomorphic, i.e. contains no type variables.
    {b This is a conservative estimate}: [struct Foo<T> {}] is considered
    polymorphic despite the generics being unused. *)
let ty_is_monomorphic ty =
  let exception FoundGeneric in
  let ty_visitor =
    object (_)
      inherit [_] Types.iter_ty
      method! visit_TVar _ _ = raise FoundGeneric
    end
  in
  try
    ty_visitor#visit_ty () ty;
    true
  with FoundGeneric -> false

let pp_span_data ft ({ file; beg_loc; end_loc } : Meta.span_data) =
  let clean_filename name =
    let parts = String.split_on_char '/' name in
    if List.compare_length_with parts 3 <= 0 then name
    else
      let last_3 = List.rev (List.take 3 (List.rev parts)) in
      "../" ^ String.concat "/" last_3
  in
  let pp_filename ft ({ name; _ } : Meta.file) =
    match name with
    | Local name -> Fmt.string ft (clean_filename name)
    | Virtual name -> Fmt.pf ft "%s (virtual)" (clean_filename name)
    | NotReal name -> Fmt.pf ft "%s (synthetic)" (clean_filename name)
  in
  let pp_range ft ((start, stop) : Meta.loc * Meta.loc) =
    if start.line = stop.line then
      Fmt.pf ft "%d:%d-%d" start.line start.col stop.col
    else Fmt.pf ft "%d:%d-%d:%d" start.line start.col stop.line stop.col
  in
  Fmt.pf ft "%a:%a" pp_filename file
    (Soteria.Terminal.Printers.pp_unstable ~name:"range" pp_range)
    (beg_loc, end_loc)
