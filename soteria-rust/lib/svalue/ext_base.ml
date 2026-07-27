open Charon
open Common.Charon_util
module Sv = Soteria.Bv_values.Svalue

(** {2 Type definitions} *)

(* the full values *)
type 'g sv =
  (('g, 'g ext_t, 'g ext_ty) Sv.t
  [@equal fun a b -> Sv.equal a b] [@compare fun a b -> Sv.compare a b])

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

(** A block of an encoded value: a value, along with the offset it is at and the
    size it spans. [ty] is [Some] iff the value is a whole (unencoded) aggregate
    of that (normalised) type. *)
and ('v, 'ofs, 'sz) block = {
  value : 'v;
  ty : Types.ty option;
  offset : 'ofs;
  size : 'sz;
}

(* values *)
and 'g ext_ty =
  | TEnum of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of an {b enum} *)
  | TUnion of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
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
  | Enum of Types.variant_id * 'g sv list  (** variant id * values *)
  | Tuple of 'g sv list  (** structs and tuples: ordered values *)
  | Array of 'g sv Iarray.t  (** arrays: ordered values, all of the same type *)
  | Union of ('g sv, 'g sv, 'g sv) block list
      (** list of blocks in the union *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)
[@@deriving eq, ord]

let pp_block pp_v pp_ofs pp_sz ft { value; ty; offset; size } =
  Fmt.pf ft "(%a: %a%a-%a)" pp_ofs offset pp_v value
    Fmt.(option (any " : " ++ Types.pp_ty))
    ty pp_sz size

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
  | TAdt { id = TTuple; generics = { types; _ } } ->
      TExtension (TTuple (List.map ty_of_rust types))
  | TAdt ({ id = TAdtId _; _ } as adt) -> (
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
  | ( TError _ | TPtrMetadata _ | TTraitType _ | TDynTrait _ | TSlice _
    | TAdt { id = TBuiltin _; _ } ) as ty ->
      L.failwith "ty_of_rust unexpected type %a" pp_ty ty

let t_as_tuple (ty : 'g svty) =
  match ty with
  | TExtension (TTuple tys) -> tys
  | _ -> invalid_arg "t_as_tuple: not a tuple type"

let t_as_array (ty : 'g svty) =
  match ty with
  | TExtension (TArray (elem_ty, n)) -> (elem_ty, n)
  | _ -> invalid_arg "t_as_array: not an array type"

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

(** {3 Fat pointers} *)

let mk_full_ptr ~build:(( <| ) : _ build) ptr meta =
  Extension (Ptr (ptr, meta)) <| TExtension TFullPtr

let mk_unit_meta ~build:(( <| ) : _ build) () =
  Extension (PtrMeta MetaUnit) <| TExtension TPtrMeta

let mk_len_meta ~build:(( <| ) : _ build) len =
  Extension (PtrMeta (MetaLen len)) <| TExtension TPtrMeta

let mk_vtable_meta ~build:(( <| ) : _ build) vtable =
  Extension (PtrMeta (MetaVTable vtable)) <| TExtension TPtrMeta

(** {3 Tuples} *)

let mk_tuple ~build:(( <| ) : _ build) vs =
  let tys = List.map (fun (v : _ sv) -> v.node.ty) vs in
  Extension (Tuple vs) <| TExtension (TTuple tys)

(** {3 Enums} *)

let mk_enum ~build:(( <| ) : _ build) adt var_id vs =
  Extension (Enum (var_id, vs)) <| TExtension (TEnum adt)

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

(** {3 Unions and PolyVal} (limited support)  *)

let mk_union ~build:(( <| ) : _ build) adt blocks =
  Extension (Union blocks) <| TExtension (TUnion adt)

let mk_poly ~build:(( <| ) : _ build) ty_id =
  Extension (PolyVal ty_id) <| TExtension TPolyType
