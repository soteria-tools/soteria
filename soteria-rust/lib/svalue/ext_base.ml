open Charon
open Common.Charon_util
module Sv = Soteria.Bv_values.Svalue

(** {2 Type definitions} *)

module Unop = struct
  type ptr_part = PtrInner | PtrSize | PtrAlign
  and ptr_meta = MetaLen | MetaVTable

  and t =
    | ThinPtrPart of ptr_part
    | FullPtrInner
    | FullPtrMeta
    | PtrMetaAs of ptr_meta
    | Field of int
    | VariantField of (Types.variant_id[@hash Types.VariantId.to_int]) * int
    | IsVariant of (Types.variant_id[@hash Types.VariantId.to_int])
    (* TODO: we could make [ArrayField] a binop, with the second operator a
       (possibly symbolic) value, to allow symbolically sized arrays! *)
    | ArrayField of int
  [@@deriving eq, ord, hash]

  let pp_ptr_part ft = function
    | PtrInner -> Fmt.string ft "ptr"
    | PtrSize -> Fmt.string ft "size"
    | PtrAlign -> Fmt.string ft "align"

  let show_ptr_part = Fmt.to_to_string pp_ptr_part

  let pp_ptr_meta ft = function
    | MetaLen -> Fmt.string ft "len"
    | MetaVTable -> Fmt.string ft "vtable"
end

(* the full values *)
type 'g sv =
  (('g, 'g ext_t, 'g ext_ty) Sv.t
  [@equal Sv.equal] [@compare Sv.compare] [@hash fun (v : _ Sv.t) -> v.tag])

and 'g svty = 'g ext_ty Sv.ty

(* pointers; for simplicity's sake, we hardcode the pointer type. We can improve
   this later, but we lack a clear usecase for parametric pointer types, and
   have a clear reason to want deeply-embedded pointers *)
and 'g ptr = {
  ptr : 'g sv;
  tag : Ptr_tag.t option;
  size : 'g sv;
  align : 'g sv;
}

(** The value inside a {!block}: either a scalar or an aggregate that can be
    split further. *)
and ('sc, 'ag) block_value =
  | Scalar of 'sc
  | Aggregate of 'ag * (Types.ty[@hash Hashtbl.hash])

(** A block of an encoded value: a value, along with the offset it is at and the
    size it spans. [ty] is [Some] iff the value is a whole aggregate of that
    type. *)
and ('sc, 'ag, 'ofs, 'sz) block = {
  value : ('sc, 'ag) block_value;
  offset : 'ofs;
  size : 'sz;
}

(* values *)
and 'g ext_ty =
  | TEnum of
      (Types.type_decl_ref
      [@printer Crate.pp_type_decl_ref] [@hash Hashtbl.hash])
      (** the type decl ref of an {b enum} *)
  | TUnion of
      (Types.type_decl_ref
      [@printer Crate.pp_type_decl_ref] [@hash Hashtbl.hash])
      (** the type decl ref of a {b union} *)
  | TTuple of 'g svty list  (** structs and tuples (ordered fields) *)
  | TArray of 'g svty * Z.t  (** arrays (all elements share the same type) *)
  | TThinPtr
  | TFullPtr
  | TPtrMeta
      (** the type of a pointer's metadata (unit, length or metadata). this is
          not exposed to the user! *)
  | TPolyType

and 'g ptr_meta = MetaLen of 'g sv | MetaVTable of 'g sv | MetaUnit

and 'g ext_t =
  | Ptr of 'g sv * 'g sv  (** pointer, with meta *)
  | PtrMeta of 'g ptr_meta
  | ThinPtr of 'g ptr
      (** thin pointer, without metadata but with extra info on the pointer *)
  | Enum of (Types.variant_id[@hash Charon.Types.VariantId.to_int]) * 'g sv list
      (** variant id * values *)
  | Tuple of 'g sv list  (** structs and tuples: ordered values *)
  | Array of 'g sv Iarray.t  (** arrays: ordered values, all of the same type *)
  | Union of ('g sv, 'g sv, 'g sv, 'g sv) block list
      (** list of blocks in the union *)
  | PolyVal of (Charon.Types.type_var_id[@hash Charon.Types.TypeVarId.to_int])
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)
  | Unop of Unop.t * 'g sv  (** unary operation *)
[@@deriving eq, ord, hash]

let pp_block_value pp_v pp_ag ft = function
  | Scalar v -> pp_v ft v
  | Aggregate (ag, ty) -> Fmt.pf ft "%a : %a" pp_ag ag Types.pp_ty ty

let pp_block pp_v pp_ag pp_ofs pp_sz ft { value; offset; size } =
  Fmt.pf ft "(%a: %a-%a)" pp_ofs offset
    (pp_block_value pp_v pp_ag)
    value pp_sz size

let pp_ptr_meta_kind ft = function
  | MetaUnit -> Fmt.string ft "unit"
  | MetaLen _ -> Fmt.pf ft "len"
  | MetaVTable _ -> Fmt.pf ft "vtable"

let rec pp_ext_ty ft : 'g ext_ty -> unit = function
  | TEnum ty -> Crate.pp_type_decl_ref ft ty
  | TUnion ty -> Crate.pp_type_decl_ref ft ty
  | TTuple tys -> Fmt.(brackets (list ~sep:semi pp_svty)) ft tys
  | TArray (ty, n) -> Fmt.pf ft "[%a; %a]" pp_svty ty Z.pp n
  | TThinPtr -> Fmt.string ft "TThinPtr"
  | TFullPtr -> Fmt.string ft "TFullPtr"
  | TPtrMeta -> Fmt.string ft "TPtrMeta"
  | TPolyType -> Fmt.string ft "TPolyType"

and pp_svty ft (ty : 'g svty) = Sv.pp_ty pp_ext_ty ft ty

(** {2 Types and Rust type conversions} *)

let float_precision :
    Values.float_type -> Soteria.Bv_values.Svalue.FloatPrecision.t = function
  | F16 -> F16
  | F32 -> F32
  | F64 -> F64
  | F128 -> F128

let rec ty_of_rust : Types.ty -> 'g svty = function
  | TLiteral (TFloat ft) -> TFloat (float_precision ft)
  | TLiteral lit -> TBitVector (8 * size_of_literal_ty lit)
  | TRef _ | TRawPtr _ | TFnPtr _ -> TExtension TFullPtr
  | TNever | TFnDef _ -> TExtension (TTuple [])
  | TVar _ -> TExtension TPolyType
  | TPattern (ty, _) -> ty_of_rust ty
  | TArray (ty, n) -> TExtension (TArray (ty_of_rust ty, z_of_constant_expr n))
  | TAdt (adt, _) -> (
      assert (tyref_is_substituted adt);
      match (Crate.get_adt adt).kind with
      | Struct fs ->
          TExtension
            (TTuple
               (List.map (fun (f : Types.field) -> ty_of_rust f.field_ty) fs))
      | Enum _ -> TExtension (TEnum adt)
      | Union _ -> TExtension (TUnion adt)
      | kind ->
          L.failwith "ty_of_rust unexpected adt kind %a" Types.pp_type_decl_kind
            kind)
  | (TError _ | TPtrMetadata _ | TTraitType _ | TDynTrait _ | TSlice _) as ty ->
      L.failwith "ty_of_rust unexpected type %a" pp_ty ty

let t_as_tuple (ty : 'g svty) =
  match ty with
  | TExtension (TTuple tys) -> tys
  | _ -> invalid_arg "t_as_tuple: not a tuple type"

let t_as_array (ty : 'g svty) =
  match ty with
  | TExtension (TArray (elem_ty, n)) -> (elem_ty, n)
  | _ -> invalid_arg "t_as_array: not an array type"

let array_length (ty : 'g svty) =
  match ty with
  | TExtension (TArray (_, n)) -> n
  | _ -> invalid_arg "array_length: not an array type"

let array_elem_ty (ty : 'g svty) =
  match ty with
  | TExtension (TArray (elem_ty, _)) -> elem_ty
  | _ -> invalid_arg "array_elem_ty: not an array type"

let t_as_enum (ty : 'g svty) =
  match ty with
  | TExtension (TEnum adt) -> adt
  | _ -> invalid_arg "t_as_enum: not an enum type"

let t_as_union (ty : 'g svty) =
  match ty with
  | TExtension (TUnion adt) -> adt
  | _ -> invalid_arg "t_as_union: not a union type"

let usize_bits () = 8 * size_of_uint_ty Usize

(** {2 Smart constructors}

    We want to have our smart constructors accessible from within the extension,
    so that the extension's [eval] and [mk] functions can use the same smart
    constructors as the rest of the engine. *)

type 'g build = ('g, 'g ext_t, 'g ext_ty) Sv.t_kind -> 'g svty -> 'g sv

(** {3 Thin pointers} *)

let mk_thin_ptr ~build:(( <| ) : _ build) ptr =
  Extension (ThinPtr ptr) <| TExtension TThinPtr

let thin_ptr_part ~build:(( <| ) : _ build) (part : Unop.ptr_part)
    (v : 'ghost sv) =
  match v.node.kind with
  | Extension (ThinPtr inner) -> (
      match part with
      | PtrInner -> inner.ptr
      | PtrSize -> inner.size
      | PtrAlign -> inner.align)
  | _ ->
      let ty : 'ghost svty =
        match part with
        | PtrInner -> TPointer (usize_bits ())
        | PtrSize | PtrAlign -> TBitVector (usize_bits ())
      in
      Extension (Unop (ThinPtrPart part, v)) <| ty

(** {3 Fat pointers} *)

let mk_full_ptr ~build:(( <| ) : _ build) ptr meta =
  Extension (Ptr (ptr, meta)) <| TExtension TFullPtr

let mk_unit_meta ~build:(( <| ) : _ build) () =
  Extension (PtrMeta MetaUnit) <| TExtension TPtrMeta

let mk_len_meta ~build:(( <| ) : _ build) len =
  Extension (PtrMeta (MetaLen len)) <| TExtension TPtrMeta

let mk_vtable_meta ~build:(( <| ) : _ build) vtable =
  Extension (PtrMeta (MetaVTable vtable)) <| TExtension TPtrMeta

let full_ptr_inner ~build:(( <| ) : _ build) (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Ptr (p, _)) -> p
  | _ -> Extension (Unop (FullPtrInner, v)) <| TExtension TThinPtr

let of_thin_ptr ~build v = mk_full_ptr ~build v (mk_unit_meta ~build ())

let full_ptr_meta_raw ~build:(( <| ) : _ build) (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Ptr (_, m)) -> m
  | _ -> Extension (Unop (FullPtrMeta, v)) <| TExtension TPtrMeta

let ptr_meta_as ~build:(( <| ) : _ build) (part : Unop.ptr_meta) (v : 'ghost sv)
    =
  match v.node.kind with
  | Extension (PtrMeta m) -> (
      match (part, m) with
      | MetaLen, MetaLen len -> len
      | MetaVTable, MetaVTable vtable -> vtable
      | _ ->
          L.failwith "ptr_meta_as: expected %a but got %a" Unop.pp_ptr_meta part
            pp_ptr_meta_kind m)
  | _ ->
      let ty : 'ghost svty =
        match part with
        | MetaLen -> TBitVector (usize_bits ())
        | MetaVTable -> TExtension TThinPtr
      in
      Extension (Unop (PtrMetaAs part, v)) <| ty

let full_ptr_meta ~build (part : Unop.ptr_meta) (v : 'ghost sv) =
  full_ptr_meta_raw ~build v |> ptr_meta_as ~build part

let full_ptr_set_inner ~build inner (v : 'ghost sv) =
  mk_full_ptr ~build inner (full_ptr_meta_raw ~build v)

(** {3 Tuples} *)

let mk_tuple ~build:(( <| ) : _ build) vs =
  let tys = List.map (fun (v : _ sv) -> v.node.ty) vs in
  Extension (Tuple vs) <| TExtension (TTuple tys)

let field_of ~build:(( <| ) : _ build) idx (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Tuple vs) -> List.nth vs idx
  | _ -> Extension (Unop (Field idx, v)) <| List.nth (t_as_tuple v.node.ty) idx

let as_tuple ~build (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Tuple vs) -> vs
  | _ -> List.mapi (fun i _ -> field_of ~build i v) (t_as_tuple v.node.ty)

let set_field ~build idx x (v : 'ghost sv) =
  mk_tuple ~build (List.set_nth idx x (as_tuple ~build v))

(** {3 Enums} *)

let mk_enum ~build:(( <| ) : _ build) adt var_id vs =
  Extension (Enum (var_id, vs)) <| TExtension (TEnum adt)

let field_of_variant ~build:(( <| ) : _ build) var idx (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Enum (v, vs)) ->
      assert (Types.equal_variant_id v var);
      List.nth vs idx
  | _ ->
      let adt = t_as_enum v.node.ty in
      let variant = Types.VariantId.nth (Crate.as_enum adt) var in
      let field : Types.field = List.nth variant.fields idx in
      Extension (Unop (VariantField (var, idx), v)) <| ty_of_rust field.field_ty

let as_enum_of_variant ~build var (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Enum (v, vs)) ->
      assert (Types.equal_variant_id v var);
      vs
  | _ ->
      let adt = t_as_enum v.node.ty in
      let variant = Types.VariantId.nth (Crate.as_enum adt) var in
      List.mapi (fun i _ -> field_of_variant ~build var i v) variant.fields

let set_field_of_variant ~build var idx x (v : 'ghost sv) =
  let vs = List.set_nth idx x (as_enum_of_variant ~build var v) in
  mk_enum ~build (t_as_enum v.node.ty) var vs

let is_variant ~build:(( <| ) : _ build) var (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Enum (cur_var, _)) ->
      Bool (Types.equal_variant_id var cur_var) <| TBool
  | _ -> Extension (Unop (IsVariant var, v)) <| TBool

(** {3 Arrays} *)

let mk_array_of_svty ~build:(( <| ) : _ build) elem_ty vs =
  Extension (Array vs)
  <| TExtension (TArray (elem_ty, Z.of_int (Iarray.length vs)))

let mk_array ~(build : _ build) elem_ty (vs : _ sv Iarray.t) =
  let elem_ty =
    if Iarray.length vs = 0 then ty_of_rust elem_ty
    else (Iarray.get vs 0).node.ty
  in
  mk_array_of_svty ~build elem_ty vs

let array_field_of ~build:(( <| ) : _ build) idx (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Array vs) -> Iarray.get vs idx
  | _ -> Extension (Unop (ArrayField idx, v)) <| array_elem_ty v.node.ty

let as_array ~build (v : 'ghost sv) =
  match v.node.kind with
  | Extension (Array vs) -> vs
  | _ ->
      let n = array_length v.node.ty in
      Iarray.init (Z.to_int n) (fun i -> array_field_of ~build i v)

let set_array_field ~build idx x (v : 'ghost sv) =
  mk_array_of_svty ~build (array_elem_ty v.node.ty)
    (Iarray.copy_and_set idx x (as_array ~build v))

(** {3 Unions and PolyVal (limited support)} *)

let mk_union ~build:(( <| ) : _ build) adt blocks =
  Extension (Union blocks) <| TExtension (TUnion adt)

let mk_poly ~build:(( <| ) : _ build) ty_id =
  Extension (PolyVal ty_id) <| TExtension TPolyType

(** {3 Operators} *)

let apply_unop ~build : Unop.t -> _ sv -> _ sv = function
  | ThinPtrPart part -> thin_ptr_part ~build part
  | FullPtrInner -> full_ptr_inner ~build
  | FullPtrMeta -> full_ptr_meta_raw ~build
  | PtrMetaAs part -> ptr_meta_as ~build part
  | Field i -> field_of ~build i
  | VariantField (var_id, i) -> field_of_variant ~build var_id i
  | IsVariant var_id -> is_variant ~build var_id
  | ArrayField i -> array_field_of ~build i
