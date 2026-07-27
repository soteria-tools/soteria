open Charon
module Sv = Soteria.Bv_values.Svalue

(* the full values *)
type 'ghost sv =
  (('ghost, 'ghost ext_t, 'ghost ext_ty) Sv.t
  [@equal fun a b -> Sv.equal a b] [@compare fun a b -> Sv.compare a b])

and 'ghost svty = 'ghost ext_ty Sv.ty

(* pointers; for simplicity's sake, we hardcode the pointer type. We can improve
   this later, but we lack a clear usecase for parametric pointer types, and
   have a clear reason to want deeply-embedded pointers *)
and 'ghost ptr = {
  ptr : 'ghost sv;
  tag : Ptr_tag.t option;
  size : 'ghost sv;
  align : 'ghost sv;
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
and 'ghost ext_ty =
  | TEnum of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of an {b enum} *)
  | TUnion of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of a {b union} *)
  | TTuple of 'ghost svty list  (** structs and tuples (ordered fields) *)
  | TArray of 'ghost svty * Z.t
      (** arrays (all elements share the same type) *)
  | TThinPtr
  | TFullPtr
  | TPtrMeta
      (** the type of a pointer's metadata (unit, length or metadata). this is
          not exposed to the user! *)
  | TPolyType

and 'g ptr_meta = MetaLen of 'g sv | MetaVTable of 'g sv | MetaUnit

and 'ghost ext_t =
  | Ptr of 'ghost sv * 'ghost sv  (** pointer, with meta *)
  | PtrMeta of 'ghost ptr_meta
  | ThinPtr of 'ghost ptr
      (** thin pointer, without metadata but with extra info on the pointer *)
  | Enum of Types.variant_id * 'ghost sv list  (** variant id * values *)
  | Tuple of 'ghost sv list  (** structs and tuples: ordered values *)
  | Array of 'ghost sv Iarray.t
      (** arrays: ordered values, all of the same type *)
  | Union of ('ghost sv, 'ghost sv, 'ghost sv) block list
      (** list of blocks in the union *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)
[@@deriving eq, ord]

let pp_block pp_v pp_ofs pp_sz ft { value; ty; offset; size } =
  Fmt.pf ft "(%a: %a%a-%a)" pp_ofs offset pp_v value
    Fmt.(option (any " : " ++ Types.pp_ty))
    ty pp_sz size

let rec pp_ext_ty ft : 'ghost ext_ty -> unit = function
  | TEnum ty -> Crate.pp_type_decl_ref ft ty
  | TUnion ty -> Crate.pp_type_decl_ref ft ty
  | TTuple tys -> Fmt.(brackets (list ~sep:semi pp_svty)) ft tys
  | TArray (ty, n) -> Fmt.pf ft "[%a; %a]" pp_svty ty Z.pp n
  | TThinPtr -> Fmt.string ft "TThinPtr"
  | TFullPtr -> Fmt.string ft "TFullPtr"
  | TPtrMeta -> Fmt.string ft "TPtrMeta"
  | TPolyType -> Fmt.string ft "TPolyType"

and pp_svty ft (ty : 'ghost svty) = Sv.pp_ty pp_ext_ty ft ty
