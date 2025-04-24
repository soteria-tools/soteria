open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Rustsymex

module M (Heap : Heap_intf.S) = struct
  module Std = Std.M (Heap)
  module Kani = Kani.M (Heap)

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  type std_bool = Id | Neg
  type type_loc = GenArg | Input

  type std_fun =
    (* Kani *)
    | KaniAssert
    | KaniAssume
    | KaniNondet
    (* Std *)
    | Abs
    | AssertZeroValid
    | AssertInhabited
    | Assume
    | BlackBox
    | BoolNot
    | BoxIntoRaw
    | Checked of Expressions.binop
    | CopyNonOverlapping
    | Ctpop
    | Deref
    | DiscriminantValue
    | Eq of std_bool
    | ExactDiv
    | Index
    | IsNone
    | IsSome
    | IsValStaticallyKnown
    | IterNth
    | MinAlignOf of type_loc
    | MulAdd
    | Nop
    | OptUnwrap
    | PtrByteOp of Expressions.binop
    | PtrOp of Expressions.binop
    | PtrOffsetFrom
    | ResUnwrap
    | SizeOf
    | SizeOfVal
    | SliceLen
    | StrChars
    | StrLen
    | ToString
    | Transmute
    | Unchecked of Expressions.binop
    | Wrapping of Expressions.binop
    | WriteBytes
    | Zeroed

  let std_fun_map =
    [
      (* Kani *)
      ("kani::assert", KaniAssert);
      ("kani::assume", KaniAssume);
      ("kani::nondet", KaniNondet);
      (* Core *)
      ("alloc::boxed::{alloc::boxed::Box}::into_raw", BoxIntoRaw);
      ("alloc::string::{alloc::string::String}::len", StrLen);
      ("alloc::string::{alloc::string::ToString}::to_string", ToString);
      ("alloc::string::{core::ops::deref::Deref}::deref", Deref);
      ("core::array::{core::ops::index::Index}::index", Index);
      ("core::cmp::impls::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::cmp::impls::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::hint::black_box", BlackBox);
      ("core::intrinsics::add_with_overflow", Checked Add);
      ("core::intrinsics::arith_offset", PtrOp Add);
      ("core::intrinsics::assert_inhabited", AssertInhabited);
      ("core::intrinsics::assert_zero_valid", AssertZeroValid);
      ("core::intrinsics::assume", Assume);
      ("core::intrinsics::black_box", BlackBox);
      ("core::intrinsics::cold_path", Nop);
      ("core::intrinsics::copy_nonoverlapping", CopyNonOverlapping);
      ("core::intrinsics::ctpop", Ctpop);
      ("core::intrinsics::discriminant_value", DiscriminantValue);
      ("core::intrinsics::exact_div", ExactDiv);
      ("core::intrinsics::fabsf64", Abs);
      ("core::intrinsics::fabsf32", Abs);
      ("core::intrinsics::fmaf64", MulAdd);
      ("core::intrinsics::fmaf32", MulAdd);
      ("core::intrinsics::is_val_statically_known", IsValStaticallyKnown);
      ("core::intrinsics::likely", Nop);
      ("core::intrinsics::min_align_of", MinAlignOf GenArg);
      ("core::intrinsics::min_align_of_val", MinAlignOf Input);
      ("core::intrinsics::pref_align_of", MinAlignOf GenArg);
      ("core::intrinsics::ptr_offset_from", PtrOffsetFrom);
      ("core::intrinsics::size_of", SizeOf);
      ("core::intrinsics::size_of_val", SizeOfVal);
      ("core::intrinsics::transmute", Transmute);
      ("core::intrinsics::unchecked_add", Unchecked Add);
      ("core::intrinsics::unchecked_div", Unchecked Div);
      ("core::intrinsics::unchecked_mul", Unchecked Mul);
      ("core::intrinsics::unchecked_rem", Unchecked Rem);
      ("core::intrinsics::unchecked_sub", Unchecked Sub);
      ("core::intrinsics::wrapping_add", Wrapping Add);
      ("core::intrinsics::wrapping_div", Wrapping Div);
      ("core::intrinsics::wrapping_mul", Wrapping Mul);
      ("core::intrinsics::wrapping_rem", Wrapping Rem);
      ("core::intrinsics::wrapping_sub", Wrapping Sub);
      ("core::intrinsics::write_bytes::write_bytes", WriteBytes);
      ("core::mem::zeroed", Zeroed);
      ("core::num::{@N}::wrapping_add", Wrapping Add);
      ("core::num::{@N}::wrapping_div", Wrapping Div);
      ("core::num::{@N}::wrapping_mul", Wrapping Mul);
      ("core::num::{@N}::wrapping_rem", Wrapping Rem);
      ("core::num::{@N}::wrapping_sub", Wrapping Sub);
      ("core::option::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::option::{@T}::is_none", IsNone);
      ("core::option::{@T}::is_some", IsSome);
      ("core::option::{@T}::unwrap", OptUnwrap);
      ("core::ptr::const_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::byte_add", PtrByteOp Add);
      ("core::ptr::const_ptr::{@T}::byte_sub", PtrByteOp Sub);
      ("core::ptr::const_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::sub", PtrOp Sub);
      ("core::ptr::mut_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_add", PtrByteOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_sub", PtrByteOp Sub);
      ("core::ptr::mut_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::mut_ptr::{@T}::sub", PtrOp Sub);
      ("core::result::{@T}::unwrap", ResUnwrap);
      ("core::result::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::result::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::slice::index::{core::ops::index::Index}::index", Index);
      ("core::slice::{@T}::len", SliceLen);
      ("core::str::iter::{core::iter::traits::iterator::Iterator}::nth", IterNth);
      ("core::str::{str}::chars", StrChars);
      ("core::option::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::ops::bit::{core::ops::bit::Not}::not", BoolNot);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let std_fun_eval ~crate (f : UllbcAst.fun_decl) =
    let open Std in
    let open Kani in
    let opt_bind f opt = match opt with None -> f () | x -> x in
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config f.item_meta.name std_fun_map
    |> ( Option.map @@ function
         | Abs -> abs f.signature
         | AssertZeroValid -> assert_zero_is_valid f.signature
         | AssertInhabited -> assert_inhabited f.signature
         | Assume -> std_assume
         | BlackBox -> black_box f.signature
         | BoolNot -> bool_not
         | BoxIntoRaw -> box_into_raw f.signature
         | Checked op -> checked_op op f.signature
         | CopyNonOverlapping -> copy_nonoverlapping f.signature
         | Ctpop -> ctpop f.signature
         | Deref -> deref f.signature
         | DiscriminantValue -> discriminant_value f.signature
         | Eq b -> eq_values ~neg:(b = Neg) f.signature
         | ExactDiv -> exact_div f.signature
         | Index -> array_index_fn f.signature
         | IsNone -> is_none f.signature
         | IsSome -> is_some f.signature
         | IsValStaticallyKnown -> is_val_statically_known
         | IterNth -> iter_nth f.signature
         | KaniAssert -> assert_
         | KaniAssume -> assume
         | KaniNondet -> kani_nondet f.signature
         | MinAlignOf t -> min_align_of ~in_input:(t = Input) f.signature
         | MulAdd -> mul_add f.signature
         | Nop -> nop
         | OptUnwrap -> unwrap_opt
         | PtrByteOp op -> ptr_op ~byte:true op f.signature
         | PtrOp op -> ptr_op op f.signature
         | PtrOffsetFrom -> ptr_offset_from f.signature
         | ResUnwrap -> unwrap_res
         | SizeOf -> size_of f.signature
         | SizeOfVal -> size_of_val f.signature
         | SliceLen -> slice_len f.signature
         | StrChars -> str_chars f.signature
         | StrLen -> str_len f.signature
         | ToString -> to_string f.signature
         | Transmute -> transmute f.signature
         | Unchecked op -> unchecked_op op f.signature
         | Wrapping op -> wrapping_op op f.signature
         | WriteBytes -> write_bytes f.signature
         | Zeroed -> zeroed f.signature )
    |> opt_bind @@ fun () ->
       match f.item_meta.name with
       | PeIdent (("core" | "std"), _) :: PeIdent ("intrinsics", _) :: _ ->
           Option.some @@ fun ~crate ~args:_ ~state:_ ->
           let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
           Fmt.kstr not_impl "Unsupported intrinsic: %s"
             (PrintTypes.name_to_string ctx f.item_meta.name)
       | _ -> None

  let builtin_fun_eval ~crate:_ (f : Expressions.builtin_fun_id) generics =
    let open Std in
    match f with
    | ArrayRepeat -> array_repeat generics
    | ArrayToSliceMut -> array_slice ~mut:true generics
    | ArrayToSliceShared -> array_slice ~mut:false generics
    | Index idx -> array_index idx generics
    | BoxNew -> box_new generics
    | PtrFromParts _ -> from_raw_parts
end
