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
