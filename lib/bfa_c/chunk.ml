type t = U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128 | F32 | F64
[@@deriving eq]

type components =
  | Float of { bit_width : int }
  | Int of { bit_width : int; signed : bool }

let to_string = function
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | U128 -> "u128"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | I128 -> "i128"
  | F32 -> "f32"
  | F64 -> "f64"

let of_string = function
  | "u8" -> U8
  | "u16" -> U16
  | "u32" -> U32
  | "u64" -> U64
  | "u128" -> U128
  | "i8" -> I8
  | "i16" -> I16
  | "i32" -> I32
  | "i64" -> I64
  | "i128" -> I128
  | "f32" -> F32
  | "f64" -> F64
  | _ -> failwith "invalid chunk"

let pp = Fmt.of_to_string to_string

let bounds =
  let open Z in
  function
  | U8 -> Some (zero, (one lsl 8) - one)
  | U16 -> Some (zero, (one lsl 16) - one)
  | U32 -> Some (zero, (one lsl 32) - one)
  | U64 -> Some (zero, (one lsl 64) - one)
  | U128 -> Some (zero, (one lsl 128) - one)
  | I8 -> Some (neg (one lsl 7), (one lsl 7) - one)
  | I16 -> Some (neg (one lsl 15), (one lsl 15) - one)
  | I32 -> Some (neg (one lsl 31), (one lsl 31) - one)
  | I64 -> Some (neg (one lsl 63), (one lsl 63) - one)
  | I128 -> Some (neg (one lsl 127), (one lsl 127) - one)
  | F32 | F64 -> None

let constrs_of_chunk chunk =
  match bounds chunk with
  | None -> failwith "constrs_of_chunk: not an int chunk"
  | Some (lower, upper) ->
      let open Svalue.Infix in
      fun v -> [ (int_z lower) #<= v; v #<= (int_z upper) ]

let size = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 | F32 -> 4
  | U64 | I64 | F64 -> 8
  | U128 | I128 -> 16

let size_s c = Svalue.int (size c)

let align = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 | F32 -> 4
  | U64 | I64 | F64 -> 8
  | U128 | I128 -> 16

let of_int_type ~signed ~size =
  match (size, signed) with
  | 8, true -> Some I8
  | 8, false -> Some U8
  | 16, true -> Some I16
  | 16, false -> Some U16
  | 32, true -> Some I32
  | 32, false -> Some U32
  | 64, true -> Some I64
  | 64, false -> Some U64
  | 128, true -> Some I128
  | 128, false -> Some U128
  | _ -> None

let to_components chunk =
  match chunk with
  | U8 -> Int { bit_width = 8; signed = false }
  | U16 -> Int { bit_width = 16; signed = false }
  | U32 -> Int { bit_width = 32; signed = false }
  | U64 -> Int { bit_width = 64; signed = false }
  | U128 -> Int { bit_width = 128; signed = false }
  | I8 -> Int { bit_width = 8; signed = true }
  | I16 -> Int { bit_width = 16; signed = true }
  | I32 -> Int { bit_width = 32; signed = true }
  | I64 -> Int { bit_width = 64; signed = true }
  | I128 -> Int { bit_width = 128; signed = true }
  | F32 -> Float { bit_width = 32 }
  | F64 -> Float { bit_width = 64 }

let int_chunk_to_signed_and_size chunk =
  match chunk with
  | U8 -> (false, 1)
  | U16 -> (false, 2)
  | U32 -> (false, 4)
  | U64 -> (false, 8)
  | U128 -> (false, 16)
  | I8 -> (true, 1)
  | I16 -> (true, 2)
  | I32 -> (true, 4)
  | I64 -> (true, 8)
  | I128 -> (true, 16)
  | F32 | F64 -> failwith "int_chunk_to_signed_and_size: not an int chunk"

let is_int = function
  | U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128 -> true
  | F32 | F64 -> false
