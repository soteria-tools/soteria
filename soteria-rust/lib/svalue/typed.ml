open Charon
open Common.Charon_util
open Soteria.Bv_values.Svalue
open Ext.Rust_ext
include Typed_core

module T = struct
  include T

  type sptr_f = [ `FullPtr ]
  type adt = [ `Adt ]

  type any =
    [ sint_ovf | sfloat | sbool | sptr | sloc | any sseq | sptr_f | adt ]

  let pp_sptr_f = Fmt.nop
  let pp_adt = Fmt.nop
  let pp_any = Fmt.nop
end

(** [CastError (value, expected, got)] *)
exception CastError of T.any t * T.any ty * T.any ty

let () =
  Printexc.register_printer (function
    | CastError (v, expected, got) ->
        Some
          (Fmt.str "Cast error: expected %a, got %a for value %a" ppa_ty
             expected ppa_ty got ppa v)
    | _ -> None)

let cast_error (v : [< T.any ] t) (ty : [< T.any ] ty) =
  raise
    (CastError
       ((v :> T.any t), (ty :> T.any ty), (type_type @@ get_ty v :> T.any ty)))

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

type meta = Thin | Len of T.sint t | VTable of T.sptr t
type full_ptr = ptr * meta

let t_ptr () = t_ptr (8 * size_of_uint_ty Usize)
let t_fptr () : 'a ty = type_type @@ TExtension FullPtr
let t_loc () = t_loc (8 * size_of_uint_ty Usize)
let t_usize () = t_int (8 * size_of_uint_ty Usize)

let t_lit : Types.literal_type -> [> T.sint | T.sfloat ] ty = function
  | (TInt _ | TUInt _ | TBool | TChar) as ty -> t_int (size_of_literal_ty ty * 8)
  | TFloat _ -> failwith "t_lit: unexpected float literal type"

let t_float (ty : Types.float_type) : [< T.sfloat ] ty =
  t_float (float_precision ty)

let cast_checked ~ty v =
  match cast_checked v ty with Some v -> v | None -> cast_error v ty

let as_any x = (x : [< T.any ] t :> [> T.any ] t)
let cast_nonzero (x : [< T.sint ] t) : [> T.nonzero ] t = cast x

let cast_lit ty (v : 'a t) : [> T.sint ] t =
  let size = 8 * size_of_literal_ty ty in
  cast (cast_checked ~ty:(t_int size) v)

let cast_i uty = cast_lit (TUInt uty)
let cast_f fty v = cast_checked ~ty:(t_float fty) v

let cast_float v =
  match cast_float v with Some v -> v | None -> cast_error v (t_float F64)

let meta_as_len meta =
  match meta with
  | Len len -> len
  | Thin | VTable _ -> failwith "meta_as_len: invalid length for slice/str"

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
end

module BV = BitVec

module Float = struct
  include Float

  let mk fty = mk (float_precision fty)
end

module Ptr = struct
  include Ptr

  let null_loc () = null_loc (8 * size_of_uint_ty Usize)
  let null () = null (8 * size_of_uint_ty Usize)
  let loc_of_int i = loc_of_int (8 * size_of_uint_ty Usize) i
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
