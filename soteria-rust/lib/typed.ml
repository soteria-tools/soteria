open Charon
include Soteria.Bv_values.Typed
module Lc = Layout_common

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

let t_ptr () = t_ptr (8 * Lc.size_of_uint_ty Usize)
let t_usize () = t_int (8 * Lc.size_of_uint_ty Usize)

let cast_checked ~ty v =
  match cast_checked v ty with Some v -> v | None -> cast_error v ty

let cast_checked2 v1 v2 =
  match cast_checked2 v1 v2 with
  | Some (v1, v2, ty) -> (v1, v2, ty)
  | None -> cast_error v1 (type_type @@ get_ty v2)

let cast_lit ty (v : 'a t) : [> T.sint ] t =
  let size = 8 * Lc.size_of_literal_ty ty in
  cast_checked ~ty:(t_int size) v

let cast_i uty = cast_lit (TUInt uty)
let cast_fp fp v = cast_checked ~ty:(t_float fp) v

let cast_f fty v =
  let fp = Charon_util.float_precision fty in
  cast_checked ~ty:(t_float fp) v

let cast_float v =
  match cast_float v with Some v -> v | None -> cast_error v (t_float F64)

(** DEPRECATED: it is unlikely you need this; the interpreter should be well
    typed *)
let cast_int (v : 'a t) : [> T.sint ] t * int =
  match cast_int v with
  | Some v -> (v, size_of_int v)
  | None -> cast_error v (t_int 0)

module BitVec = struct
  include BitVec

  let mk_lit ty = BitVec.mk_masked (Lc.size_of_literal_ty ty * 8)
  let mk_lit_nz ty = BitVec.mk_nz (Lc.size_of_literal_ty ty * 8)
  let mki_lit ty = BitVec.mki_masked (Lc.size_of_literal_ty ty * 8)
  let mki_lit_nz ty = BitVec.mki_nz (Lc.size_of_literal_ty ty * 8)
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
  let usize_of_const_generic cgen = usize (Charon_util.z_of_const_generic cgen)

  let of_bool : T.sbool t -> [> T.sint ] t =
    of_bool (Lc.size_of_literal_ty TBool * 8)

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
    | VBool b -> of_bool (bool b)
    | l ->
        Fmt.failwith "Cannot convert non-scalar literal %s to bitvector"
          (PrintValues.literal_to_string l)

  let bv_to_z ty z =
    let tag_size = 8 * Lc.size_of_literal_ty ty in
    let signed = Lc.is_signed ty in
    bv_to_z signed tag_size z
end

module Float = struct
  include Float

  let mk fty = mk (Charon_util.float_precision fty)
end

module Ptr = struct
  include Ptr

  let null_loc () = null_loc (8 * Lc.size_of_uint_ty Usize)
  let null () = null (8 * Lc.size_of_uint_ty Usize)
  let loc_of_int i = loc_of_int (8 * Lc.size_of_uint_ty Usize) i
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
