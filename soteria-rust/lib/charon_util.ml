open Charon

let z_of_scalar : Values.scalar_value -> Z.t = function
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
  | TAdt { id = TAdtId id; _ } ->
      let adt = Crate.get_adt id in
      let name, generics =
        match List.rev adt.item_meta.name with
        | PeMonomorphized generics :: rest -> (List.rev rest, generics)
        | _ -> (adt.item_meta.name, TypesUtils.empty_generic_args)
      in
      Fmt.pf fmt "%a%a" Crate.pp_name name Crate.pp_generic_args generics
  | TAdt { id = TTuple; generics = { types = tys; _ } } ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_ty) tys
  | TAdt { id = TBuiltin TBox; generics = { types = [ ty ]; _ } } ->
      Fmt.pf fmt "Box<%a>" pp_ty ty
  | TAdt { id = TBuiltin TBox; _ } -> Fmt.string fmt "Box<?>"
  | TAdt
      {
        id = TBuiltin TArray;
        generics =
          { types = [ ty ]; const_generics = [ CgValue (VScalar len) ]; _ };
      } ->
      Fmt.pf fmt "[%a; %a]" pp_ty ty Z.pp_print (z_of_scalar len)
  | TAdt { id = TBuiltin TArray; _ } -> Fmt.string fmt "[?; ?]"
  | TAdt { id = TBuiltin TSlice; generics = { types = [ ty ]; _ } } ->
      Fmt.pf fmt "[%a]" pp_ty ty
  | TAdt { id = TBuiltin TSlice; _ } -> Fmt.string fmt "[?]"
  | TAdt { id = TBuiltin TStr; _ } -> Fmt.string fmt "str"
  | TLiteral lit -> pp_literal_ty fmt lit
  | TNever -> Fmt.string fmt "!"
  | TRef (_, ty, RMut) -> Fmt.pf fmt "&mut %a" pp_ty ty
  | TRef (_, ty, RShared) -> Fmt.pf fmt "&%a" pp_ty ty
  | TRawPtr (ty, RMut) -> Fmt.pf fmt "*mut %a" pp_ty ty
  | TRawPtr (ty, RShared) -> Fmt.pf fmt "*const %a" pp_ty ty
  | TFnPtr { binder_value = ins, out; _ } ->
      Fmt.pf fmt "fn (%a) -> %a" Fmt.(list ~sep:(any ", ") pp_ty) ins pp_ty out
  | TDynTrait _ -> Fmt.string fmt "dyn <trait>"
  | TTraitType (_, name) -> Fmt.pf fmt "Trait<?>::%s" name
  | TFnDef { binder_value = { kind = FunId (FRegular fid); _ }; _ } ->
      let f = Crate.get_fun fid in
      Fmt.pf fmt "fn %a" Crate.pp_name f.item_meta.name
  | TPtrMetadata ty -> Fmt.pf fmt "meta(%a)" pp_ty ty
  | TFnDef _ -> Fmt.string fmt "fn ?"
  | TVar _ -> Fmt.string fmt "T?"
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

let empty_span : Meta.span =
  {
    data =
      {
        beg_loc = { line = 0; col = 0 };
        end_loc = { line = 0; col = 0 };
        file = { name = Virtual ""; contents = None; crate_name = "" };
      };
    generated_from_span = None;
  }

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
  TAdt
    {
      id = TBuiltin TArray;
      generics =
        {
          types = [ ty ];
          const_generics = [ CgValue (VScalar (UnsignedScalar (Usize, len))) ];
          regions = [];
          trait_refs = [];
        };
    }

let decl_has_attr (decl : 'a GAst.gfun_decl) attr =
  List.exists
    (function Meta.AttrUnknown { path; _ } -> path = attr | _ -> false)
    decl.item_meta.attr_info.attributes

let decl_get_attr (decl : 'a GAst.gfun_decl) attr =
  List.find_map
    (function
      | Meta.AttrUnknown { path; args } when path = attr -> args | _ -> None)
    decl.item_meta.attr_info.attributes

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

let pp_span ft ({ data = { file; beg_loc; end_loc }; _ } : Meta.span) =
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
  in
  if beg_loc.line = end_loc.line then
    Fmt.pf ft "%a:%d:%d-%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.col
  else
    Fmt.pf ft "%a:%d:%d-%d:%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.line end_loc.col
