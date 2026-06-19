open Charon
open Common.Charon_util
open Soteria.Bv_values.Svalue
open Ext.Rust_ext

(* [Make_transparent] exposes [t]/[ty] as the underlying untyped svalue, so the
   extension helpers below can be written without ghost-typing ceremony. The
   [typed.mli] re-seals [t]/[ty] as abstract for the rest of Soteria Rust. *)
module Self = Soteria.Bv_values.Typed.Make_transparent (Ext.Rust_ext)
include Self

(* The base, non-extended view, used to instantiate the solver/symex (which
   expect exactly [Typed_intf.S], i.e. the base [T.any]). *)
module Solver_value = Self

module T = struct
  include T

  type sptr_f = [ `FullPtr ]
  type sptr_t = [ `ThinPtr ]
  type tuple = [ `Tuple ]
  type enum = [ `Enum ]
  type union = [ `Union ]
  type poly = [ `Poly ]
  type ptr_meta = [ sint | sptr_t ]
  type any = [ sint | sfloat | sptr_f | tuple | enum | union | poly ]

  let pp_sptr_f = Fmt.nop
  let pp_sptr_t = Fmt.nop
  let pp_tuple = Fmt.nop
  let pp_enum = Fmt.nop
  let pp_union = Fmt.nop
  let pp_poly = Fmt.nop
  let pp_any = Fmt.nop
end

(** [CastError (value, expected, got)] *)
exception CastError of T.any t * T.any ty * T.any ty

exception TypedMigration of string

let () =
  Printexc.register_printer (function
    | CastError (v, expected, got) ->
        Some
          (Fmt.str "Cast error: expected %a, got %a for value %a" ppa_ty
             expected ppa_ty got ppa v)
    | TypedMigration msg -> Some (Fmt.str "TODO(typed migration): %s" msg)
    | _ -> None)

let cast_error v ty = raise (CastError (v, ty, v.node.ty))
let todo_migration msg = raise (TypedMigration msg)
let ( <| ) = Self.Svalue.( <| )

let float_precision :
    Values.float_type -> Soteria.Bv_values.Svalue.FloatPrecision.t = function
  | F16 -> F16
  | F32 -> F32
  | F64 -> F64
  | F128 -> F128

type ptr = {
  ptr : T.sptr t;
  size : T.sint t;
  align : T.nonzero t;
  tag : Ptr_tag.t option;
}

(* The raw pointer type; only used to materialise a fully-symbolic nondet
   pointer in [Value_codec] (see {!Ptr.of_raw}). *)
let t_ptr () = t_ptr (8 * size_of_uint_ty Usize)
let t_ptr_f () : 'a ty = TExtension TFullPtr
let t_ptr_t () : 'a ty = TExtension TThinPtr
let t_loc () = t_loc (8 * size_of_uint_ty Usize)
let t_usize () = t_int (8 * size_of_uint_ty Usize)

let t_lit : Types.literal_type -> [> T.sint ] ty = function
  | (TInt _ | TUInt _ | TBool | TChar) as ty -> t_int (size_of_literal_ty ty * 8)
  | TFloat _ -> failwith "t_lit: unexpected float literal type"

let t_float (ty : Types.float_type) : [< T.sfloat ] ty =
  t_float (float_precision ty)

let t_tuple tys : [> T.tuple ] ty = TExtension (TTuple tys)

let t_enum adt : [> T.enum ] ty =
  assert (Common.Charon_util.tyref_is_substituted adt);
  TExtension (TEnum adt)

let t_union adt : [> T.union ] ty =
  assert (Common.Charon_util.tyref_is_substituted adt);
  TExtension (TUnion adt)

let t_poly () : [> T.poly ] ty = TExtension TPolyType

let cast_checked ~ty v =
  match cast_checked v ty with Some v -> v | None -> cast_error v ty

let as_any x = (x : [< T.any ] t :> [> T.any ] t)
let cast_nonzero (x : [< T.sint ] t) : [> T.nonzero ] t = x

let cast_lit ty (v : 'a t) : [> T.sint ] t =
  let size = 8 * size_of_literal_ty ty in
  cast_checked ~ty:(t_int size) v

let cast_i uty = cast_lit (TUInt uty)
let cast_f fty v = cast_checked ~ty:(t_float fty) v

let cast_float v =
  match cast_float v with Some v -> v | None -> cast_error v (t_float F64)

let cast_ptr_f v = cast_checked ~ty:(t_ptr_f ()) v
let cast_ptr_t v = cast_checked ~ty:(t_ptr_t ()) v

let cast_tuple v =
  match get_ty v with
  | TExtension (TTuple _) -> v
  | _ -> cast_error v (t_tuple [])

(* The [adt] ref, when given, additionally checks the value is that precise
   enum/union; callers that only know the kind (e.g. the generic store
   navigation) may omit it. *)
let dummy_decl_ref =
  { Types.id = TTuple; generics = TypesUtils.empty_generic_args }

let cast_enum ?adt v =
  match (get_ty v, adt) with
  | TExtension (TEnum _), None -> v
  | TExtension (TEnum adt'), Some adt when Types.equal_type_decl_ref adt adt' ->
      v
  | _ -> cast_error v (t_enum (Option.value adt ~default:dummy_decl_ref))

let cast_union ?adt v =
  match (get_ty v, adt) with
  | TExtension (TUnion _), None -> v
  | TExtension (TUnion adt'), Some adt when Types.equal_type_decl_ref adt adt'
    ->
      v
  | _ -> cast_error v (t_union (Option.value adt ~default:dummy_decl_ref))

module BitVec = struct
  include BitVec

  let mk_lit ty = BitVec.mk_masked (size_of_literal_ty ty * 8)
  let mk_lit_nz ty = BitVec.mk_nz (size_of_literal_ty ty * 8)
  let mki_lit ty = BitVec.mki_masked (size_of_literal_ty ty * 8)
  let mki_lit_nz ty = BitVec.mki_nz (size_of_literal_ty ty * 8)
  let u8 = mk_lit (TUInt U8)
  let u8i = mki_lit (TUInt U8)
  let u8nz = mk_lit_nz (TUInt U8)
  let u8inz = mki_lit_nz (TUInt U8)
  let u16 = mk_lit (TUInt U16)
  let u16i = mki_lit (TUInt U16)
  let u16nz = mk_lit_nz (TUInt U16)
  let u16inz = mki_lit_nz (TUInt U16)
  let u32 = mk_lit (TUInt U32)
  let u32i = mki_lit (TUInt U32)
  let u32nz = mk_lit_nz (TUInt U32)
  let u32inz = mki_lit_nz (TUInt U32)
  let u64 = mk_lit (TUInt U64)
  let u64i = mki_lit (TUInt U64)
  let u64nz = mk_lit_nz (TUInt U64)
  let u64inz = mki_lit_nz (TUInt U64)
  let u128 = mk_lit (TUInt U128)
  let u128i = mki_lit (TUInt U128)
  let u128nz = mk_lit_nz (TUInt U128)
  let u128inz = mki_lit_nz (TUInt U128)
  let usize z = mk_lit (TUInt Usize) z
  let usizei z = mki_lit (TUInt Usize) z
  let usizenz z = mk_lit_nz (TUInt Usize) z
  let usizeinz z = mki_lit_nz (TUInt Usize) z

  let of_bool : T.sbool t -> [> T.sint ] t =
    of_bool (size_of_literal_ty TBool * 8)

  let of_scalar : Values.scalar_value -> [> T.sint ] t = function
    | UnsignedScalar (Usize, v) | SignedScalar (Isize, v) -> usize v
    | UnsignedScalar (U8, v) | SignedScalar (I8, v) -> u8 v
    | UnsignedScalar (U16, v) | SignedScalar (I16, v) -> u16 v
    | UnsignedScalar (U32, v) | SignedScalar (I32, v) -> u32 v
    | UnsignedScalar (U64, v) | SignedScalar (I64, v) -> u64 v
    | UnsignedScalar (U128, v) | SignedScalar (I128, v) -> u128 v

  let of_literal : Values.literal -> [> T.sint ] t = function
    | VScalar s -> of_scalar s
    | VChar c -> u32i (Uchar.to_int c)
    | VBool b -> of_bool (Bool.of_bool b)
    | l ->
        Fmt.failwith "Cannot convert non-scalar literal %s to bitvector"
          (Print.literal_to_string l)

  let of_constant_expr : Types.constant_expr -> [> T.sint ] t = function
    | { kind = CLiteral lit; _ } -> of_literal lit
    | c ->
        Fmt.failwith "Cannot convert non-value const expr %a to bitvector"
          Types.pp_constant_expr c

  let max ~signed l r = ite (gt ~signed l r) l r
  let min ~signed l r = ite (lt ~signed l r) l r
  let sure_is_zero v = Option.is_some_and Z.(equal zero) (to_z v)
end

module BV = BitVec

module Float = struct
  include Float

  let mk fty = mk (float_precision fty)
end

(* This module exposes pointers as the two standalone embedded values, thin
   pointers ([sptr_t]) and full/wide pointers ([sptr_f]). The fact that a thin
   pointer wraps a "raw" [sptr] (a bare location+offset) is an implementation
   detail: the raw [Self.Ptr] operations are used only here, and are never
   re-exposed, so [sptr] never leaks into the rest of the interpreter. *)
module Ptr = struct
  (* {1 Locations} *)

  let null_loc () = Self.Ptr.null_loc (8 * size_of_uint_ty Usize)
  let loc_of_int i = Self.Ptr.loc_of_int (8 * size_of_uint_ty Usize) i
  let is_null_loc loc = Self.Ptr.is_null_loc loc

  (* {1 Internal raw-pointer plumbing (never exposed)} *)

  let _get_ptr ptr =
    match kind ptr with
    | Extension (ThinPtr ptr) -> ptr
    | _ -> todo_migration "todo: ThinPtr.ptr getter"

  let _set_ptr ptr f =
    match kind ptr with
    | Extension (ThinPtr inner) -> Extension (ThinPtr (f inner)) <| ptr.node.ty
    | _ ->
        (* NOTE: i actually don't think we want a setter unop, we just want to
           rebuild the ptr from scratch *)
        todo_migration "ThinPtr.ptr setter"

  let _inner ptr = (_get_ptr ptr).ptr

  let of_raw ~ptr ~size ~align ~tag =
    Extension (ThinPtr { ptr; size; align; tag }) <| t_ptr_t ()

  let mk_ptr_t ~loc ~ofs ~size ~align ~tag =
    of_raw ~ptr:(Self.Ptr.mk loc ofs) ~size ~align ~tag

  let loc ptr = Self.Ptr.loc (_inner ptr)
  let ofs ptr = Self.Ptr.ofs (_inner ptr)
  let decompose ptr = Self.Ptr.decompose (_inner ptr)
  let is_null ptr = Self.Ptr.is_null (_inner ptr)
  let is_at_null_loc ptr = Self.Ptr.is_at_null_loc (_inner ptr)

  let add_ofs ptr o =
    _set_ptr ptr (fun inner ->
        { inner with ptr = Self.Ptr.add_ofs inner.ptr o })

  (* Sets the (absolute) offset of a thin pointer, keeping its location, size,
     alignment and tag. Rebuilds only the inner raw pointer. *)
  let set_ofs ptr o =
    _set_ptr ptr (fun inner ->
        { inner with ptr = Self.Ptr.mk (Self.Ptr.loc inner.ptr) o })

  let align_of ptr = (_get_ptr ptr).align
  let size_of ptr = (_get_ptr ptr).size
  let allocation_info ptr = (size_of ptr, align_of ptr)
  let tag_of ptr = (_get_ptr ptr).tag
  let with_tag ptr tag = _set_ptr ptr (fun inner -> { inner with tag })
  let has_provenance ptr = not (is_at_null_loc ptr)
  let have_same_provenance p1 p2 = sem_eq (loc p1) (loc p2)

  let in_bound ptr =
    let open Infix in
    BV.usizei 0 <=@ ofs ptr &&@ (ofs ptr <@ size_of ptr)

  (** For Miri: the allocation ID of this location, as a u64. *)
  let as_id ptr =
    (* the cast converts the location to a bitvector, which is safe because they
       have the same type, internally. *)
    let loc = cast (loc ptr) in
    let size = size_of_int loc in
    if size < 64 then BV.extend ~signed:false (64 - size) loc
    else (
      (* should basically always be the case but let's be cautious *)
      assert (size = 64);
      loc)

  (** The null pointer, which always decays to 0, and has no provenance.
      Equivalent to [of_address 0]. *)
  let null () =
    mk_ptr_t ~loc:(null_loc ()) ~ofs:(BV.usizei 0) ~size:(BV.usizei 0)
      ~align:(BV.usizeinz 1) ~tag:None

  (** Converts an address into a pointer, without provenance. *)
  let of_address ofs = add_ofs (null ()) ofs

  (* {1 Full/wide pointers ([sptr_f])} *)

  let mk_ptr_f ptr meta = Extension (Ptr (ptr, meta)) <| t_ptr_f ()

  (** The null full (wide) pointer: a {!null} thin pointer with no metadata. *)
  let null_f () = mk_ptr_f (null ()) None

  (** Like {!of_address}, but produces a full pointer with no metadata. *)
  let of_address_f addr = mk_ptr_f (of_address addr) None

  let meta_of ptr =
    match kind ptr with
    | Extension (Ptr (_, meta)) -> meta
    | _ -> todo_migration "PtrFull get meta"

  let ptr_of ptr =
    match kind ptr with
    | Extension (Ptr (ptr, _)) -> ptr
    | _ -> todo_migration "PtrFull get ptr"

  let split ptr = (ptr_of ptr, meta_of ptr)
end

module Adt = struct
  let mk_tuple vs = Extension (Tuple vs) <| t_tuple (List.map get_ty vs)
  let mk_enum adt discr vs = Extension (Enum (discr, vs)) <| t_enum adt
  let mk_union adt blocks = Extension (Union blocks) <| t_union adt
  let mk_poly ty_id = Extension (PolyVal ty_id) <| t_poly ()

  (* HACK: i have no idea what this really means or how to lift this for
     variables... *)
  let as_union v =
    match kind v with
    | Extension (Union blocks) -> blocks
    | _ -> todo_migration "as_union unop"

  (* HACK: i don't like this; it forces all fields to be resolved, which is
     often overkill. i think i want to get rid of this, and force clients to
     instead go through [index_of] *)
  let as_tuple v =
    match kind v with
    | Extension (Tuple vs) -> vs
    | _ -> todo_migration "as_tuple unop"

  let as_tuple1 v =
    match as_tuple v with
    | [ a ] -> a
    | vs ->
        L.failwith "Expected a 1-tuple, got %d fields in %a" (List.length vs)
          ppa v

  let as_tuple2 v =
    match as_tuple v with
    | [ a; b ] -> (a, b)
    | vs ->
        L.failwith "Expected a 2-tuple, got %d fields in %a" (List.length vs)
          ppa v

  let as_tuple3 v =
    match as_tuple v with
    | [ a; b; c ] -> (a, b, c)
    | vs ->
        L.failwith "Expected a 3-tuple, got %d fields in %a" (List.length vs)
          ppa v

  (* HACK: for a symbolic enum, this branches; this means we can't implement
     this at the value level. we might have to get rid of [as_enum], and instead
     expose e.g. in [Rustsymex] a util that does [branch_on] for all variants of
     an enum, and tries seeing which matches. *)
  let as_enum v =
    match kind v with
    | Extension (Enum (discr, vs)) -> (discr, vs)
    | _ -> todo_migration "discriminant_of + fields_of unops"

  let as_type_var v =
    match kind v with
    | Extension (PolyVal ty_id) -> ty_id
    | _ -> todo_migration "as_type_var unop"

  let discriminant_of v =
    match kind v with
    | Extension (Enum (discr, _)) -> discr
    | _ -> todo_migration "discriminant_of unop"

  let field_of idx v =
    match kind v with
    | Extension (Tuple vs) -> (
        try List.nth vs idx
        with Failure _ ->
          L.failwith "Tuple index %d out of bounds for value %a" idx ppa v)
    | _ -> todo_migration "field_of op"

  let field_of_variant _var_id idx v =
    (* TODO: assert the variant? *)
    match kind v with
    | Extension (Enum (_, vs)) -> (
        try List.nth vs idx
        with Failure _ ->
          L.failwith "Enum field index %d out of bounds for value %a" idx ppa v)
    | _ -> todo_migration "field_of_variant op"

  let set_field idx f v =
    match kind v with
    | Extension (Tuple vs) -> (
        try Extension (Tuple (List.set_nth idx f vs)) <| v.node.ty
        with Failure _ ->
          L.failwith "Tuple index %d out of bounds for value %a" idx ppa v)
    | _ -> todo_migration "set_field op"

  let set_field_of_variant _var_id idx f v =
    (* TODO: assert the variant *)
    match kind v with
    | Extension (Enum (discr, vs)) -> (
        try Extension (Enum (discr, List.set_nth idx f vs)) <| v.node.ty
        with Failure _ ->
          L.failwith "Enum field index %d out of bounds for value %a" idx ppa v)
    | _ -> todo_migration "set_field_of_variant op"

  let update_field idx f v = set_field idx (f (field_of idx v)) v

  let update_field_of_variant var idx f v =
    set_field_of_variant var idx (f (field_of_variant var idx v)) v

  module Checked = struct
    let mk_enum tref variant vs =
      let variant =
        Crate.as_enum tref
        |> List.find (fun (v : Types.variant) -> v.variant_name = variant)
      in
      assert (List.compare_lengths variant.fields vs = 0);
      let discr = BV.of_literal variant.discriminant in
      mk_enum tref discr vs
  end
end

module Syntax = struct
  module U8 = struct
    module Sym_int_syntax = struct
      let mk_nonzero = BitVec.u8inz
      let zero () = BitVec.u8 Z.zero
      let one () = BitVec.u8nz Z.one
    end
  end

  module U32 = struct
    module Sym_int_syntax = struct
      let mk_nonzero = BitVec.u32inz
      let zero () = BitVec.u32 Z.zero
      let one () = BitVec.u32nz Z.one
    end
  end

  module Usize = struct
    module Sym_int_syntax = struct
      let mk_nonzero = BitVec.usizeinz
      let zero () = BitVec.usize Z.zero
      let one () = BitVec.usizenz Z.one
    end
  end
end
