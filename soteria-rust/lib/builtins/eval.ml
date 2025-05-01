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
    | BoxIntoRaw
    | Checked of Expressions.binop
    | CompareBytes
    | CopyNonOverlapping
    | CopySign
    | Ctpop
    | DiscriminantValue
    | ExactDiv
    | Index
    | IsValStaticallyKnown
    | Likely
    | MinAlignOf of type_loc
    | MulAdd
    | Nop
    | PtrByteOp of Expressions.binop
    | PtrOp of Expressions.binop
    | PtrOffsetFrom
    | SizeOf
    | SizeOfVal
    | Transmute
    | Unchecked of Expressions.binop
    | VariantCount
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
      (* FIXME: get rid of these, as Charon improves *)
      ("alloc::boxed::{alloc::boxed::Box}::into_raw", BoxIntoRaw);
      ("core::array::{core::ops::index::Index}::index", Index);
      ("core::hint::black_box", BlackBox);
      ("core::mem::zeroed", Zeroed);
      ("core::slice::index::{core::ops::index::Index}::index", Index);
      (* FIXME: all core::ptr operations could be removed, however because we must enable
         ub_checks at runtime due to unchecked_op, this means ub checks also happen in
         the impl of core::ptr::..., and these checks are *SLOW* -- they do binary operations
         on the integer value of the pointer to ensure it is well aligned etc. *)
      ("core::ptr::const_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::byte_add", PtrByteOp Add);
      ("core::ptr::const_ptr::{@T}::byte_offset", PtrByteOp Add);
      ("core::ptr::const_ptr::{@T}::byte_sub", PtrByteOp Sub);
      ("core::ptr::const_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::sub", PtrOp Sub);
      ("core::ptr::mut_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_add", PtrByteOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_offset", PtrByteOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_sub", PtrByteOp Sub);
      ("core::ptr::mut_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::mut_ptr::{@T}::sub", PtrOp Sub);
      (* Intrinsics *)
      ("core::intrinsics::add_with_overflow", Checked Add);
      ("core::intrinsics::arith_offset", PtrOp Add);
      ("core::intrinsics::assert_inhabited", AssertInhabited);
      (* TODO: is the following correct? *)
      ("core::intrinsics::assert_mem_uninitialized_valid", Nop);
      ("core::intrinsics::assert_zero_valid", AssertZeroValid);
      ("core::intrinsics::assume", Assume);
      ("core::intrinsics::black_box", BlackBox);
      ("core::intrinsics::cold_path", Nop);
      ("core::intrinsics::compare_bytes", CompareBytes);
      ("core::intrinsics::copy_nonoverlapping", CopyNonOverlapping);
      ("core::intrinsics::copysignf32", CopySign);
      ("core::intrinsics::copysignf64", CopySign);
      ("core::intrinsics::ctpop", Ctpop);
      ("core::intrinsics::discriminant_value", DiscriminantValue);
      ("core::intrinsics::exact_div", ExactDiv);
      ("core::intrinsics::fabsf64", Abs);
      ("core::intrinsics::fabsf32", Abs);
      ("core::intrinsics::fmaf64", MulAdd);
      ("core::intrinsics::fmaf32", MulAdd);
      ("core::intrinsics::is_val_statically_known", IsValStaticallyKnown);
      ("core::intrinsics::likely", Likely);
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
      ("core::intrinsics::unlikely", Likely);
      ("core::intrinsics::variant_count", VariantCount);
      ("core::intrinsics::wrapping_add", Wrapping Add);
      ("core::intrinsics::wrapping_div", Wrapping Div);
      ("core::intrinsics::wrapping_mul", Wrapping Mul);
      ("core::intrinsics::wrapping_rem", Wrapping Rem);
      ("core::intrinsics::wrapping_sub", Wrapping Sub);
      ("core::intrinsics::write_bytes::write_bytes", WriteBytes);
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
         | BoxIntoRaw -> box_into_raw f.signature
         | Checked op -> checked_op op f.signature
         | CompareBytes -> compare_bytes
         | CopyNonOverlapping -> copy_nonoverlapping f.signature
         | CopySign -> copy_sign
         | Ctpop -> ctpop f.signature
         | DiscriminantValue -> discriminant_value f.signature
         | ExactDiv -> exact_div f.signature
         | Index -> array_index_fn f.signature
         | IsValStaticallyKnown -> is_val_statically_known
         | KaniAssert -> assert_
         | KaniAssume -> assume
         | KaniNondet -> kani_nondet f.signature
         | Likely -> likely
         | MinAlignOf t -> min_align_of ~in_input:(t = Input) f.signature
         | MulAdd -> mul_add f.signature
         | Nop -> nop
         | PtrByteOp op -> ptr_op ~byte:true op f.signature
         | PtrOp op -> ptr_op op f.signature
         | PtrOffsetFrom -> ptr_offset_from f.signature
         | SizeOf -> size_of f.signature
         | SizeOfVal -> size_of_val f.signature
         | Transmute -> transmute f.signature
         | Unchecked op -> unchecked_op op f.signature
         | VariantCount -> variant_count f.signature
         | Wrapping op -> wrapping_op op f.signature
         | WriteBytes -> write_bytes f.signature
         | Zeroed -> zeroed f.signature )
    |> opt_bind @@ fun () ->
       let is_intrinsic =
         (match f.item_meta.name with
         | PeIdent (("core" | "std"), _) :: PeIdent ("intrinsics", _) :: _ ->
             true
         | _ -> false)
         || List.mem
              Meta.(AttrUnknown { path = "rustc_intrinsic"; args = None })
              f.item_meta.attr_info.attributes
       in
       if is_intrinsic then
         Option.some @@ fun ~crate ~args:_ ~state:_ ->
         let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
         Fmt.kstr not_impl "Unsupported intrinsic: %s"
           (PrintTypes.name_to_string ctx f.item_meta.name)
       else None

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
