open Rustsymex

module M (State : State_intf.S) = struct
  let abort _ = not_impl "Unsupported intrinsic: abort"

  let add_with_overflow ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: add_with_overflow"

  let aggregate_raw_ptr ~p:_ ~d:_ ~m:_ ~data:_ ~meta:_ _ =
    not_impl "Unsupported intrinsic: aggregate_raw_ptr"

  let align_of ~t:_ _ = not_impl "Unsupported intrinsic: align_of"

  let align_of_val ~t:_ ~ptr:_ _ =
    not_impl "Unsupported intrinsic: align_of_val"

  let arith_offset ~t:_ ~dst:_ ~offset:_ _ =
    not_impl "Unsupported intrinsic: arith_offset"

  let assert_inhabited ~t:_ _ =
    not_impl "Unsupported intrinsic: assert_inhabited"

  let assert_mem_uninitialized_valid ~t:_ _ =
    not_impl "Unsupported intrinsic: assert_mem_uninitialized_valid"

  let assert_zero_valid ~t:_ _ =
    not_impl "Unsupported intrinsic: assert_zero_valid"

  let assume ~b:_ _ = not_impl "Unsupported intrinsic: assume"
  let bitreverse ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: bitreverse"
  let black_box ~t:_ ~dummy:_ _ = not_impl "Unsupported intrinsic: black_box"
  let breakpoint _ = not_impl "Unsupported intrinsic: breakpoint"
  let bswap ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: bswap"
  let caller_location _ = not_impl "Unsupported intrinsic: caller_location"

  let carrying_mul_add ~t:_ ~u:_ ~multiplier:_ ~multiplicand:_ ~addend:_
      ~carry:_ _ =
    not_impl "Unsupported intrinsic: carrying_mul_add"

  let catch_unwind ~_try_fn:_ ~_data:_ ~_catch_fn:_ _ =
    not_impl "Unsupported intrinsic: catch_unwind"

  let ceilf128 ~x:_ _ = not_impl "Unsupported intrinsic: ceilf128"
  let ceilf16 ~x:_ _ = not_impl "Unsupported intrinsic: ceilf16"
  let ceilf32 ~x:_ _ = not_impl "Unsupported intrinsic: ceilf32"
  let ceilf64 ~x:_ _ = not_impl "Unsupported intrinsic: ceilf64"
  let cold_path _ = not_impl "Unsupported intrinsic: cold_path"

  let compare_bytes ~left:_ ~right:_ ~bytes:_ _ =
    not_impl "Unsupported intrinsic: compare_bytes"

  let const_deallocate ~_ptr:_ ~_size:_ ~_align:_ _ =
    not_impl "Unsupported intrinsic: const_deallocate"

  let const_eval_select ~arg:_ ~f:_ ~g:_ ~ret:_ ~_arg:_ ~_called_in_const:_
      ~_called_at_rt:_ _ =
    not_impl "Unsupported intrinsic: const_eval_select"

  let contract_check_ensures ~c:_ ~t_ret:_ ~cond:_ ~ret:_ _ =
    not_impl "Unsupported intrinsic: contract_check_ensures"

  let contract_check_requires ~c:_ ~arg1:_ _ =
    not_impl "Unsupported intrinsic: contract_check_requires"

  let contract_checks _ = not_impl "Unsupported intrinsic: contract_checks"

  let copy ~t:_ ~src:_ ~dst:_ ~count:_ _ =
    not_impl "Unsupported intrinsic: copy"

  let copy_nonoverlapping ~t:_ ~src:_ ~dst:_ ~count:_ _ =
    not_impl "Unsupported intrinsic: copy_nonoverlapping"

  let copysignf128 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: copysignf128"
  let copysignf16 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: copysignf16"
  let copysignf32 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: copysignf32"
  let copysignf64 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: copysignf64"
  let cosf128 ~x:_ _ = not_impl "Unsupported intrinsic: cosf128"
  let cosf16 ~x:_ _ = not_impl "Unsupported intrinsic: cosf16"
  let cosf32 ~x:_ _ = not_impl "Unsupported intrinsic: cosf32"
  let cosf64 ~x:_ _ = not_impl "Unsupported intrinsic: cosf64"
  let ctlz ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: ctlz"
  let ctlz_nonzero ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: ctlz_nonzero"
  let ctpop ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: ctpop"
  let cttz ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: cttz"
  let cttz_nonzero ~t:_ ~x:_ _ = not_impl "Unsupported intrinsic: cttz_nonzero"

  let discriminant_value ~t:_ ~v:_ _ =
    not_impl "Unsupported intrinsic: discriminant_value"

  let disjoint_bitor ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: disjoint_bitor"

  let exact_div ~t:_ ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: exact_div"
  let exp2f128 ~x:_ _ = not_impl "Unsupported intrinsic: exp2f128"
  let exp2f16 ~x:_ _ = not_impl "Unsupported intrinsic: exp2f16"
  let exp2f32 ~x:_ _ = not_impl "Unsupported intrinsic: exp2f32"
  let exp2f64 ~x:_ _ = not_impl "Unsupported intrinsic: exp2f64"
  let expf128 ~x:_ _ = not_impl "Unsupported intrinsic: expf128"
  let expf16 ~x:_ _ = not_impl "Unsupported intrinsic: expf16"
  let expf32 ~x:_ _ = not_impl "Unsupported intrinsic: expf32"
  let expf64 ~x:_ _ = not_impl "Unsupported intrinsic: expf64"
  let fabsf128 ~x:_ _ = not_impl "Unsupported intrinsic: fabsf128"
  let fabsf16 ~x:_ _ = not_impl "Unsupported intrinsic: fabsf16"
  let fabsf32 ~x:_ _ = not_impl "Unsupported intrinsic: fabsf32"
  let fabsf64 ~x:_ _ = not_impl "Unsupported intrinsic: fabsf64"

  let fadd_algebraic ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: fadd_algebraic"

  let fadd_fast ~t:_ ~a:_ ~b:_ _ = not_impl "Unsupported intrinsic: fadd_fast"

  let fdiv_algebraic ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: fdiv_algebraic"

  let fdiv_fast ~t:_ ~a:_ ~b:_ _ = not_impl "Unsupported intrinsic: fdiv_fast"

  let float_to_int_unchecked ~float:_ ~int:_ ~value:_ _ =
    not_impl "Unsupported intrinsic: float_to_int_unchecked"

  let floorf128 ~x:_ _ = not_impl "Unsupported intrinsic: floorf128"
  let floorf16 ~x:_ _ = not_impl "Unsupported intrinsic: floorf16"
  let floorf32 ~x:_ _ = not_impl "Unsupported intrinsic: floorf32"
  let floorf64 ~x:_ _ = not_impl "Unsupported intrinsic: floorf64"
  let fmaf128 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmaf128"
  let fmaf16 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmaf16"
  let fmaf32 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmaf32"
  let fmaf64 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmaf64"

  let fmul_algebraic ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: fmul_algebraic"

  let fmul_fast ~t:_ ~a:_ ~b:_ _ = not_impl "Unsupported intrinsic: fmul_fast"

  let fmuladdf128 ~a:_ ~b:_ ~c:_ _ =
    not_impl "Unsupported intrinsic: fmuladdf128"

  let fmuladdf16 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmuladdf16"
  let fmuladdf32 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmuladdf32"
  let fmuladdf64 ~a:_ ~b:_ ~c:_ _ = not_impl "Unsupported intrinsic: fmuladdf64"
  let forget ~t:_ ~arg:_ _ = not_impl "Unsupported intrinsic: forget"

  let frem_algebraic ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: frem_algebraic"

  let frem_fast ~t:_ ~a:_ ~b:_ _ = not_impl "Unsupported intrinsic: frem_fast"

  let fsub_algebraic ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: fsub_algebraic"

  let fsub_fast ~t:_ ~a:_ ~b:_ _ = not_impl "Unsupported intrinsic: fsub_fast"

  let is_val_statically_known ~t:_ ~_arg:_ _ =
    not_impl "Unsupported intrinsic: is_val_statically_known"

  let likely ~b:_ _ = not_impl "Unsupported intrinsic: likely"
  let log10f128 ~x:_ _ = not_impl "Unsupported intrinsic: log10f128"
  let log10f16 ~x:_ _ = not_impl "Unsupported intrinsic: log10f16"
  let log10f32 ~x:_ _ = not_impl "Unsupported intrinsic: log10f32"
  let log10f64 ~x:_ _ = not_impl "Unsupported intrinsic: log10f64"
  let log2f128 ~x:_ _ = not_impl "Unsupported intrinsic: log2f128"
  let log2f16 ~x:_ _ = not_impl "Unsupported intrinsic: log2f16"
  let log2f32 ~x:_ _ = not_impl "Unsupported intrinsic: log2f32"
  let log2f64 ~x:_ _ = not_impl "Unsupported intrinsic: log2f64"
  let logf128 ~x:_ _ = not_impl "Unsupported intrinsic: logf128"
  let logf16 ~x:_ _ = not_impl "Unsupported intrinsic: logf16"
  let logf32 ~x:_ _ = not_impl "Unsupported intrinsic: logf32"
  let logf64 ~x:_ _ = not_impl "Unsupported intrinsic: logf64"
  let maximumf128 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maximumf128"
  let maximumf16 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maximumf16"
  let maximumf32 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maximumf32"
  let maximumf64 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maximumf64"
  let maxnumf128 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maxnumf128"
  let maxnumf16 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maxnumf16"
  let maxnumf32 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maxnumf32"
  let maxnumf64 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: maxnumf64"
  let minimumf128 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minimumf128"
  let minimumf16 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minimumf16"
  let minimumf32 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minimumf32"
  let minimumf64 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minimumf64"
  let minnumf128 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minnumf128"
  let minnumf16 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minnumf16"
  let minnumf32 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minnumf32"
  let minnumf64 ~x:_ ~y:_ _ = not_impl "Unsupported intrinsic: minnumf64"

  let mul_with_overflow ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: mul_with_overflow"

  let needs_drop ~t:_ _ = not_impl "Unsupported intrinsic: needs_drop"

  let nontemporal_store ~t:_ ~ptr:_ ~val_:_ _ =
    not_impl "Unsupported intrinsic: nontemporal_store"

  let offset ~ptr:_ ~delta:_ ~dst:_ ~offset:_ _ =
    not_impl "Unsupported intrinsic: offset"

  let powf128 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powf128"
  let powf16 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powf16"
  let powf32 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powf32"
  let powf64 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powf64"
  let powif128 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powif128"
  let powif16 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powif16"
  let powif32 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powif32"
  let powif64 ~a:_ ~x:_ _ = not_impl "Unsupported intrinsic: powif64"

  let prefetch_read_data ~t:_ ~data:_ ~locality:_ _ =
    not_impl "Unsupported intrinsic: prefetch_read_data"

  let prefetch_read_instruction ~t:_ ~data:_ ~locality:_ _ =
    not_impl "Unsupported intrinsic: prefetch_read_instruction"

  let prefetch_write_data ~t:_ ~data:_ ~locality:_ _ =
    not_impl "Unsupported intrinsic: prefetch_write_data"

  let prefetch_write_instruction ~t:_ ~data:_ ~locality:_ _ =
    not_impl "Unsupported intrinsic: prefetch_write_instruction"

  let ptr_guaranteed_cmp ~t:_ ~ptr:_ ~other:_ _ =
    not_impl "Unsupported intrinsic: ptr_guaranteed_cmp"

  let ptr_mask ~t:_ ~ptr:_ ~mask:_ _ =
    not_impl "Unsupported intrinsic: ptr_mask"

  let ptr_metadata ~p:_ ~m:_ ~ptr:_ _ =
    not_impl "Unsupported intrinsic: ptr_metadata"

  let ptr_offset_from ~t:_ ~ptr:_ ~base:_ _ =
    not_impl "Unsupported intrinsic: ptr_offset_from"

  let ptr_offset_from_unsigned ~t:_ ~ptr:_ ~base:_ _ =
    not_impl "Unsupported intrinsic: ptr_offset_from_unsigned"

  let raw_eq ~t:_ ~a:_ ~b:_ _ = not_impl "Unsupported intrinsic: raw_eq"

  let read_via_copy ~t:_ ~ptr:_ _ =
    not_impl "Unsupported intrinsic: read_via_copy"

  let rotate_left ~t:_ ~x:_ ~shift:_ _ =
    not_impl "Unsupported intrinsic: rotate_left"

  let rotate_right ~t:_ ~x:_ ~shift:_ _ =
    not_impl "Unsupported intrinsic: rotate_right"

  let round_ties_even_f128 ~x:_ _ =
    not_impl "Unsupported intrinsic: round_ties_even_f128"

  let round_ties_even_f16 ~x:_ _ =
    not_impl "Unsupported intrinsic: round_ties_even_f16"

  let round_ties_even_f32 ~x:_ _ =
    not_impl "Unsupported intrinsic: round_ties_even_f32"

  let round_ties_even_f64 ~x:_ _ =
    not_impl "Unsupported intrinsic: round_ties_even_f64"

  let roundf128 ~x:_ _ = not_impl "Unsupported intrinsic: roundf128"
  let roundf16 ~x:_ _ = not_impl "Unsupported intrinsic: roundf16"
  let roundf32 ~x:_ _ = not_impl "Unsupported intrinsic: roundf32"
  let roundf64 ~x:_ _ = not_impl "Unsupported intrinsic: roundf64"
  let rustc_peek ~t:_ ~arg:_ _ = not_impl "Unsupported intrinsic: rustc_peek"

  let saturating_add ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: saturating_add"

  let saturating_sub ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: saturating_sub"

  let select_unpredictable ~t:_ ~b:_ ~true_val:_ ~false_val:_ _ =
    not_impl "Unsupported intrinsic: select_unpredictable"

  let sinf128 ~x:_ _ = not_impl "Unsupported intrinsic: sinf128"
  let sinf16 ~x:_ _ = not_impl "Unsupported intrinsic: sinf16"
  let sinf32 ~x:_ _ = not_impl "Unsupported intrinsic: sinf32"
  let sinf64 ~x:_ _ = not_impl "Unsupported intrinsic: sinf64"
  let size_of ~t:_ _ = not_impl "Unsupported intrinsic: size_of"
  let size_of_val ~t:_ ~ptr:_ _ = not_impl "Unsupported intrinsic: size_of_val"

  let slice_get_unchecked ~itemptr:_ ~sliceptr:_ ~t:_ ~slice_ptr:_ ~index:_ _ =
    not_impl "Unsupported intrinsic: slice_get_unchecked"

  let sqrtf128 ~x:_ _ = not_impl "Unsupported intrinsic: sqrtf128"
  let sqrtf16 ~x:_ _ = not_impl "Unsupported intrinsic: sqrtf16"
  let sqrtf32 ~x:_ _ = not_impl "Unsupported intrinsic: sqrtf32"
  let sqrtf64 ~x:_ _ = not_impl "Unsupported intrinsic: sqrtf64"

  let sub_with_overflow ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: sub_with_overflow"

  let three_way_compare ~t:_ ~lhs:_ ~rhss:_ _ =
    not_impl "Unsupported intrinsic: three_way_compare"

  let transmute ~t_src:_ ~dst:_ ~src:_ _ =
    not_impl "Unsupported intrinsic: transmute"

  let transmute_unchecked ~t_src:_ ~dst:_ ~src:_ _ =
    not_impl "Unsupported intrinsic: transmute_unchecked"

  let truncf128 ~x:_ _ = not_impl "Unsupported intrinsic: truncf128"
  let truncf16 ~x:_ _ = not_impl "Unsupported intrinsic: truncf16"
  let truncf32 ~x:_ _ = not_impl "Unsupported intrinsic: truncf32"
  let truncf64 ~x:_ _ = not_impl "Unsupported intrinsic: truncf64"
  let type_id ~t:_ _ = not_impl "Unsupported intrinsic: type_id"
  let type_name ~t:_ _ = not_impl "Unsupported intrinsic: type_name"

  let typed_swap_nonoverlapping ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: typed_swap_nonoverlapping"

  let ub_checks _ = not_impl "Unsupported intrinsic: ub_checks"

  let unaligned_volatile_load ~t:_ ~src:_ _ =
    not_impl "Unsupported intrinsic: unaligned_volatile_load"

  let unaligned_volatile_store ~t:_ ~dst:_ ~val_:_ _ =
    not_impl "Unsupported intrinsic: unaligned_volatile_store"

  let unchecked_add ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_add"

  let unchecked_div ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_div"

  let unchecked_mul ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_mul"

  let unchecked_rem ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_rem"

  let unchecked_shl ~t:_ ~u:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_shl"

  let unchecked_shr ~t:_ ~u:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_shr"

  let unchecked_sub ~t:_ ~x:_ ~y:_ _ =
    not_impl "Unsupported intrinsic: unchecked_sub"

  let unlikely ~b:_ _ = not_impl "Unsupported intrinsic: unlikely"
  let unreachable _ = not_impl "Unsupported intrinsic: unreachable"
  let va_arg ~t:_ ~ap:_ _ = not_impl "Unsupported intrinsic: va_arg"
  let va_copy ~dest:_ ~src:_ _ = not_impl "Unsupported intrinsic: va_copy"
  let va_end ~ap:_ _ = not_impl "Unsupported intrinsic: va_end"
  let variant_count ~t:_ _ = not_impl "Unsupported intrinsic: variant_count"

  let volatile_copy_memory ~t:_ ~dst:_ ~src:_ ~count:_ _ =
    not_impl "Unsupported intrinsic: volatile_copy_memory"

  let volatile_copy_nonoverlapping_memory ~t:_ ~dst:_ ~src:_ ~count:_ _ =
    not_impl "Unsupported intrinsic: volatile_copy_nonoverlapping_memory"

  let volatile_load ~t:_ ~src:_ _ =
    not_impl "Unsupported intrinsic: volatile_load"

  let volatile_set_memory ~t:_ ~dst:_ ~val_:_ ~count:_ _ =
    not_impl "Unsupported intrinsic: volatile_set_memory"

  let volatile_store ~t:_ ~dst:_ ~val_:_ _ =
    not_impl "Unsupported intrinsic: volatile_store"

  let vtable_align ~ptr:_ _ = not_impl "Unsupported intrinsic: vtable_align"
  let vtable_size ~ptr:_ _ = not_impl "Unsupported intrinsic: vtable_size"

  let wrapping_add ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: wrapping_add"

  let wrapping_mul ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: wrapping_mul"

  let wrapping_sub ~t:_ ~a:_ ~b:_ _ =
    not_impl "Unsupported intrinsic: wrapping_sub"

  let write_bytes ~t:_ ~dst:_ ~val_:_ ~count:_ _ =
    not_impl "Unsupported intrinsic: write_bytes"

  let write_via_move ~t:_ ~ptr:_ ~value:_ _ =
    not_impl "Unsupported intrinsic: write_via_move"

  include Intrinsics_impl.M (State)

  let fun_map =
    [
      ("core::intrinsics::abort", "abort");
      ("core::intrinsics::add_with_overflow", "add_with_overflow");
      ("core::intrinsics::aggregate_raw_ptr", "aggregate_raw_ptr");
      ("core::intrinsics::align_of", "align_of");
      ("core::intrinsics::align_of_val", "align_of_val");
      ("core::intrinsics::arith_offset", "arith_offset");
      ("core::intrinsics::assert_inhabited", "assert_inhabited");
      ( "core::intrinsics::assert_mem_uninitialized_valid",
        "assert_mem_uninitialized_valid" );
      ("core::intrinsics::assert_zero_valid", "assert_zero_valid");
      ("core::intrinsics::assume", "assume");
      ("core::intrinsics::bitreverse", "bitreverse");
      ("core::intrinsics::black_box", "black_box");
      ("core::intrinsics::breakpoint", "breakpoint");
      ("core::intrinsics::bswap", "bswap");
      ("core::intrinsics::caller_location", "caller_location");
      ("core::intrinsics::carrying_mul_add", "carrying_mul_add");
      ("core::intrinsics::catch_unwind", "catch_unwind");
      ("core::intrinsics::ceilf128", "ceilf128");
      ("core::intrinsics::ceilf16", "ceilf16");
      ("core::intrinsics::ceilf32", "ceilf32");
      ("core::intrinsics::ceilf64", "ceilf64");
      ("core::intrinsics::cold_path", "cold_path");
      ("core::intrinsics::compare_bytes", "compare_bytes");
      ("core::intrinsics::const_deallocate", "const_deallocate");
      ("core::intrinsics::const_eval_select", "const_eval_select");
      ("core::intrinsics::contract_check_ensures", "contract_check_ensures");
      ("core::intrinsics::contract_check_requires", "contract_check_requires");
      ("core::intrinsics::contract_checks", "contract_checks");
      ("core::intrinsics::copy", "copy");
      ("core::intrinsics::copy_nonoverlapping", "copy_nonoverlapping");
      ("core::intrinsics::copysignf128", "copysignf128");
      ("core::intrinsics::copysignf16", "copysignf16");
      ("core::intrinsics::copysignf32", "copysignf32");
      ("core::intrinsics::copysignf64", "copysignf64");
      ("core::intrinsics::cosf128", "cosf128");
      ("core::intrinsics::cosf16", "cosf16");
      ("core::intrinsics::cosf32", "cosf32");
      ("core::intrinsics::cosf64", "cosf64");
      ("core::intrinsics::ctlz", "ctlz");
      ("core::intrinsics::ctlz_nonzero", "ctlz_nonzero");
      ("core::intrinsics::ctpop", "ctpop");
      ("core::intrinsics::cttz", "cttz");
      ("core::intrinsics::cttz_nonzero", "cttz_nonzero");
      ("core::intrinsics::discriminant_value", "discriminant_value");
      ("core::intrinsics::disjoint_bitor", "disjoint_bitor");
      ("core::intrinsics::exact_div", "exact_div");
      ("core::intrinsics::exp2f128", "exp2f128");
      ("core::intrinsics::exp2f16", "exp2f16");
      ("core::intrinsics::exp2f32", "exp2f32");
      ("core::intrinsics::exp2f64", "exp2f64");
      ("core::intrinsics::expf128", "expf128");
      ("core::intrinsics::expf16", "expf16");
      ("core::intrinsics::expf32", "expf32");
      ("core::intrinsics::expf64", "expf64");
      ("core::intrinsics::fabsf128", "fabsf128");
      ("core::intrinsics::fabsf16", "fabsf16");
      ("core::intrinsics::fabsf32", "fabsf32");
      ("core::intrinsics::fabsf64", "fabsf64");
      ("core::intrinsics::fadd_algebraic", "fadd_algebraic");
      ("core::intrinsics::fadd_fast", "fadd_fast");
      ("core::intrinsics::fdiv_algebraic", "fdiv_algebraic");
      ("core::intrinsics::fdiv_fast", "fdiv_fast");
      ("core::intrinsics::float_to_int_unchecked", "float_to_int_unchecked");
      ("core::intrinsics::floorf128", "floorf128");
      ("core::intrinsics::floorf16", "floorf16");
      ("core::intrinsics::floorf32", "floorf32");
      ("core::intrinsics::floorf64", "floorf64");
      ("core::intrinsics::fmaf128", "fmaf128");
      ("core::intrinsics::fmaf16", "fmaf16");
      ("core::intrinsics::fmaf32", "fmaf32");
      ("core::intrinsics::fmaf64", "fmaf64");
      ("core::intrinsics::fmul_algebraic", "fmul_algebraic");
      ("core::intrinsics::fmul_fast", "fmul_fast");
      ("core::intrinsics::fmuladdf128", "fmuladdf128");
      ("core::intrinsics::fmuladdf16", "fmuladdf16");
      ("core::intrinsics::fmuladdf32", "fmuladdf32");
      ("core::intrinsics::fmuladdf64", "fmuladdf64");
      ("core::intrinsics::forget", "forget");
      ("core::intrinsics::frem_algebraic", "frem_algebraic");
      ("core::intrinsics::frem_fast", "frem_fast");
      ("core::intrinsics::fsub_algebraic", "fsub_algebraic");
      ("core::intrinsics::fsub_fast", "fsub_fast");
      ("core::intrinsics::is_val_statically_known", "is_val_statically_known");
      ("core::intrinsics::likely", "likely");
      ("core::intrinsics::log10f128", "log10f128");
      ("core::intrinsics::log10f16", "log10f16");
      ("core::intrinsics::log10f32", "log10f32");
      ("core::intrinsics::log10f64", "log10f64");
      ("core::intrinsics::log2f128", "log2f128");
      ("core::intrinsics::log2f16", "log2f16");
      ("core::intrinsics::log2f32", "log2f32");
      ("core::intrinsics::log2f64", "log2f64");
      ("core::intrinsics::logf128", "logf128");
      ("core::intrinsics::logf16", "logf16");
      ("core::intrinsics::logf32", "logf32");
      ("core::intrinsics::logf64", "logf64");
      ("core::intrinsics::maximumf128", "maximumf128");
      ("core::intrinsics::maximumf16", "maximumf16");
      ("core::intrinsics::maximumf32", "maximumf32");
      ("core::intrinsics::maximumf64", "maximumf64");
      ("core::intrinsics::maxnumf128", "maxnumf128");
      ("core::intrinsics::maxnumf16", "maxnumf16");
      ("core::intrinsics::maxnumf32", "maxnumf32");
      ("core::intrinsics::maxnumf64", "maxnumf64");
      ("core::intrinsics::minimumf128", "minimumf128");
      ("core::intrinsics::minimumf16", "minimumf16");
      ("core::intrinsics::minimumf32", "minimumf32");
      ("core::intrinsics::minimumf64", "minimumf64");
      ("core::intrinsics::minnumf128", "minnumf128");
      ("core::intrinsics::minnumf16", "minnumf16");
      ("core::intrinsics::minnumf32", "minnumf32");
      ("core::intrinsics::minnumf64", "minnumf64");
      ("core::intrinsics::mul_with_overflow", "mul_with_overflow");
      ("core::intrinsics::needs_drop", "needs_drop");
      ("core::intrinsics::nontemporal_store", "nontemporal_store");
      ("core::intrinsics::offset", "offset");
      ("core::intrinsics::powf128", "powf128");
      ("core::intrinsics::powf16", "powf16");
      ("core::intrinsics::powf32", "powf32");
      ("core::intrinsics::powf64", "powf64");
      ("core::intrinsics::powif128", "powif128");
      ("core::intrinsics::powif16", "powif16");
      ("core::intrinsics::powif32", "powif32");
      ("core::intrinsics::powif64", "powif64");
      ("core::intrinsics::prefetch_read_data", "prefetch_read_data");
      ( "core::intrinsics::prefetch_read_instruction",
        "prefetch_read_instruction" );
      ("core::intrinsics::prefetch_write_data", "prefetch_write_data");
      ( "core::intrinsics::prefetch_write_instruction",
        "prefetch_write_instruction" );
      ("core::intrinsics::ptr_guaranteed_cmp", "ptr_guaranteed_cmp");
      ("core::intrinsics::ptr_mask", "ptr_mask");
      ("core::intrinsics::ptr_metadata", "ptr_metadata");
      ("core::intrinsics::ptr_offset_from", "ptr_offset_from");
      ("core::intrinsics::ptr_offset_from_unsigned", "ptr_offset_from_unsigned");
      ("core::intrinsics::raw_eq", "raw_eq");
      ("core::intrinsics::read_via_copy", "read_via_copy");
      ("core::intrinsics::rotate_left", "rotate_left");
      ("core::intrinsics::rotate_right", "rotate_right");
      ("core::intrinsics::round_ties_even_f128", "round_ties_even_f128");
      ("core::intrinsics::round_ties_even_f16", "round_ties_even_f16");
      ("core::intrinsics::round_ties_even_f32", "round_ties_even_f32");
      ("core::intrinsics::round_ties_even_f64", "round_ties_even_f64");
      ("core::intrinsics::roundf128", "roundf128");
      ("core::intrinsics::roundf16", "roundf16");
      ("core::intrinsics::roundf32", "roundf32");
      ("core::intrinsics::roundf64", "roundf64");
      ("core::intrinsics::rustc_peek", "rustc_peek");
      ("core::intrinsics::saturating_add", "saturating_add");
      ("core::intrinsics::saturating_sub", "saturating_sub");
      ("core::intrinsics::select_unpredictable", "select_unpredictable");
      ("core::intrinsics::sinf128", "sinf128");
      ("core::intrinsics::sinf16", "sinf16");
      ("core::intrinsics::sinf32", "sinf32");
      ("core::intrinsics::sinf64", "sinf64");
      ("core::intrinsics::size_of", "size_of");
      ("core::intrinsics::size_of_val", "size_of_val");
      ("core::intrinsics::slice_get_unchecked", "slice_get_unchecked");
      ("core::intrinsics::sqrtf128", "sqrtf128");
      ("core::intrinsics::sqrtf16", "sqrtf16");
      ("core::intrinsics::sqrtf32", "sqrtf32");
      ("core::intrinsics::sqrtf64", "sqrtf64");
      ("core::intrinsics::sub_with_overflow", "sub_with_overflow");
      ("core::intrinsics::three_way_compare", "three_way_compare");
      ("core::intrinsics::transmute", "transmute");
      ("core::intrinsics::transmute_unchecked", "transmute_unchecked");
      ("core::intrinsics::truncf128", "truncf128");
      ("core::intrinsics::truncf16", "truncf16");
      ("core::intrinsics::truncf32", "truncf32");
      ("core::intrinsics::truncf64", "truncf64");
      ("core::intrinsics::type_id", "type_id");
      ("core::intrinsics::type_name", "type_name");
      ( "core::intrinsics::typed_swap_nonoverlapping",
        "typed_swap_nonoverlapping" );
      ("core::intrinsics::ub_checks", "ub_checks");
      ("core::intrinsics::unaligned_volatile_load", "unaligned_volatile_load");
      ("core::intrinsics::unaligned_volatile_store", "unaligned_volatile_store");
      ("core::intrinsics::unchecked_add", "unchecked_add");
      ("core::intrinsics::unchecked_div", "unchecked_div");
      ("core::intrinsics::unchecked_mul", "unchecked_mul");
      ("core::intrinsics::unchecked_rem", "unchecked_rem");
      ("core::intrinsics::unchecked_shl", "unchecked_shl");
      ("core::intrinsics::unchecked_shr", "unchecked_shr");
      ("core::intrinsics::unchecked_sub", "unchecked_sub");
      ("core::intrinsics::unlikely", "unlikely");
      ("core::intrinsics::unreachable", "unreachable");
      ("core::intrinsics::va_arg", "va_arg");
      ("core::intrinsics::va_copy", "va_copy");
      ("core::intrinsics::va_end", "va_end");
      ("core::intrinsics::variant_count", "variant_count");
      ("core::intrinsics::volatile_copy_memory", "volatile_copy_memory");
      ( "core::intrinsics::volatile_copy_nonoverlapping_memory",
        "volatile_copy_nonoverlapping_memory" );
      ("core::intrinsics::volatile_load", "volatile_load");
      ("core::intrinsics::volatile_set_memory", "volatile_set_memory");
      ("core::intrinsics::volatile_store", "volatile_store");
      ("core::intrinsics::vtable_align", "vtable_align");
      ("core::intrinsics::vtable_size", "vtable_size");
      ("core::intrinsics::wrapping_add", "wrapping_add");
      ("core::intrinsics::wrapping_mul", "wrapping_mul");
      ("core::intrinsics::wrapping_sub", "wrapping_sub");
      ("core::intrinsics::write_bytes", "write_bytes");
      ("core::intrinsics::write_via_move", "write_via_move");
    ]

  let eval_fun name (generics : Charon.Types.generic_args) ~args =
    match (name, generics.types, args) with
    | "abort", [], [] -> abort
    | "add_with_overflow", [ t ], [ x; y ] -> add_with_overflow ~t ~x ~y
    | "aggregate_raw_ptr", [ p; d; m ], [ data; meta ] ->
        aggregate_raw_ptr ~p ~d ~m ~data ~meta
    | "align_of", [ t ], [] -> align_of ~t
    | "align_of_val", [ t ], [ ptr ] -> align_of_val ~t ~ptr
    | "arith_offset", [ t ], [ dst; offset ] -> arith_offset ~t ~dst ~offset
    | "assert_inhabited", [ t ], [] -> assert_inhabited ~t
    | "assert_mem_uninitialized_valid", [ t ], [] ->
        assert_mem_uninitialized_valid ~t
    | "assert_zero_valid", [ t ], [] -> assert_zero_valid ~t
    | "assume", [], [ b ] -> assume ~b
    | "bitreverse", [ t ], [ x ] -> bitreverse ~t ~x
    | "black_box", [ t ], [ dummy ] -> black_box ~t ~dummy
    | "breakpoint", [], [] -> breakpoint
    | "bswap", [ t ], [ x ] -> bswap ~t ~x
    | "caller_location", [], [] -> caller_location
    | "carrying_mul_add", [ t; u ], [ multiplier; multiplicand; addend; carry ]
      ->
        carrying_mul_add ~t ~u ~multiplier ~multiplicand ~addend ~carry
    | "catch_unwind", [], [ _try_fn; _data; _catch_fn ] ->
        catch_unwind ~_try_fn ~_data ~_catch_fn
    | "ceilf128", [], [ x ] -> ceilf128 ~x
    | "ceilf16", [], [ x ] -> ceilf16 ~x
    | "ceilf32", [], [ x ] -> ceilf32 ~x
    | "ceilf64", [], [ x ] -> ceilf64 ~x
    | "cold_path", [], [] -> cold_path
    | "compare_bytes", [], [ left; right; bytes ] ->
        compare_bytes ~left ~right ~bytes
    | "const_deallocate", [], [ _ptr; _size; _align ] ->
        const_deallocate ~_ptr ~_size ~_align
    | ( "const_eval_select",
        [ arg; f; g; ret ],
        [ _arg; _called_in_const; _called_at_rt ] ) ->
        const_eval_select ~arg ~f ~g ~ret ~_arg ~_called_in_const ~_called_at_rt
    | "contract_check_ensures", [ c; t_ret ], [ cond; ret ] ->
        contract_check_ensures ~c ~t_ret ~cond ~ret
    | "contract_check_requires", [ c ], [ arg1 ] ->
        contract_check_requires ~c ~arg1
    | "contract_checks", [], [] -> contract_checks
    | "copy", [ t ], [ src; dst; count ] -> copy ~t ~src ~dst ~count
    | "copy_nonoverlapping", [ t ], [ src; dst; count ] ->
        copy_nonoverlapping ~t ~src ~dst ~count
    | "copysignf128", [], [ x; y ] -> copysignf128 ~x ~y
    | "copysignf16", [], [ x; y ] -> copysignf16 ~x ~y
    | "copysignf32", [], [ x; y ] -> copysignf32 ~x ~y
    | "copysignf64", [], [ x; y ] -> copysignf64 ~x ~y
    | "cosf128", [], [ x ] -> cosf128 ~x
    | "cosf16", [], [ x ] -> cosf16 ~x
    | "cosf32", [], [ x ] -> cosf32 ~x
    | "cosf64", [], [ x ] -> cosf64 ~x
    | "ctlz", [ t ], [ x ] -> ctlz ~t ~x
    | "ctlz_nonzero", [ t ], [ x ] -> ctlz_nonzero ~t ~x
    | "ctpop", [ t ], [ x ] -> ctpop ~t ~x
    | "cttz", [ t ], [ x ] -> cttz ~t ~x
    | "cttz_nonzero", [ t ], [ x ] -> cttz_nonzero ~t ~x
    | "discriminant_value", [ t ], [ v ] -> discriminant_value ~t ~v
    | "disjoint_bitor", [ t ], [ a; b ] -> disjoint_bitor ~t ~a ~b
    | "exact_div", [ t ], [ x; y ] -> exact_div ~t ~x ~y
    | "exp2f128", [], [ x ] -> exp2f128 ~x
    | "exp2f16", [], [ x ] -> exp2f16 ~x
    | "exp2f32", [], [ x ] -> exp2f32 ~x
    | "exp2f64", [], [ x ] -> exp2f64 ~x
    | "expf128", [], [ x ] -> expf128 ~x
    | "expf16", [], [ x ] -> expf16 ~x
    | "expf32", [], [ x ] -> expf32 ~x
    | "expf64", [], [ x ] -> expf64 ~x
    | "fabsf128", [], [ x ] -> fabsf128 ~x
    | "fabsf16", [], [ x ] -> fabsf16 ~x
    | "fabsf32", [], [ x ] -> fabsf32 ~x
    | "fabsf64", [], [ x ] -> fabsf64 ~x
    | "fadd_algebraic", [ t ], [ a; b ] -> fadd_algebraic ~t ~a ~b
    | "fadd_fast", [ t ], [ a; b ] -> fadd_fast ~t ~a ~b
    | "fdiv_algebraic", [ t ], [ a; b ] -> fdiv_algebraic ~t ~a ~b
    | "fdiv_fast", [ t ], [ a; b ] -> fdiv_fast ~t ~a ~b
    | "float_to_int_unchecked", [ float; int ], [ value ] ->
        float_to_int_unchecked ~float ~int ~value
    | "floorf128", [], [ x ] -> floorf128 ~x
    | "floorf16", [], [ x ] -> floorf16 ~x
    | "floorf32", [], [ x ] -> floorf32 ~x
    | "floorf64", [], [ x ] -> floorf64 ~x
    | "fmaf128", [], [ a; b; c ] -> fmaf128 ~a ~b ~c
    | "fmaf16", [], [ a; b; c ] -> fmaf16 ~a ~b ~c
    | "fmaf32", [], [ a; b; c ] -> fmaf32 ~a ~b ~c
    | "fmaf64", [], [ a; b; c ] -> fmaf64 ~a ~b ~c
    | "fmul_algebraic", [ t ], [ a; b ] -> fmul_algebraic ~t ~a ~b
    | "fmul_fast", [ t ], [ a; b ] -> fmul_fast ~t ~a ~b
    | "fmuladdf128", [], [ a; b; c ] -> fmuladdf128 ~a ~b ~c
    | "fmuladdf16", [], [ a; b; c ] -> fmuladdf16 ~a ~b ~c
    | "fmuladdf32", [], [ a; b; c ] -> fmuladdf32 ~a ~b ~c
    | "fmuladdf64", [], [ a; b; c ] -> fmuladdf64 ~a ~b ~c
    | "forget", [ t ], [ arg ] -> forget ~t ~arg
    | "frem_algebraic", [ t ], [ a; b ] -> frem_algebraic ~t ~a ~b
    | "frem_fast", [ t ], [ a; b ] -> frem_fast ~t ~a ~b
    | "fsub_algebraic", [ t ], [ a; b ] -> fsub_algebraic ~t ~a ~b
    | "fsub_fast", [ t ], [ a; b ] -> fsub_fast ~t ~a ~b
    | "is_val_statically_known", [ t ], [ _arg ] ->
        is_val_statically_known ~t ~_arg
    | "likely", [], [ b ] -> likely ~b
    | "log10f128", [], [ x ] -> log10f128 ~x
    | "log10f16", [], [ x ] -> log10f16 ~x
    | "log10f32", [], [ x ] -> log10f32 ~x
    | "log10f64", [], [ x ] -> log10f64 ~x
    | "log2f128", [], [ x ] -> log2f128 ~x
    | "log2f16", [], [ x ] -> log2f16 ~x
    | "log2f32", [], [ x ] -> log2f32 ~x
    | "log2f64", [], [ x ] -> log2f64 ~x
    | "logf128", [], [ x ] -> logf128 ~x
    | "logf16", [], [ x ] -> logf16 ~x
    | "logf32", [], [ x ] -> logf32 ~x
    | "logf64", [], [ x ] -> logf64 ~x
    | "maximumf128", [], [ x; y ] -> maximumf128 ~x ~y
    | "maximumf16", [], [ x; y ] -> maximumf16 ~x ~y
    | "maximumf32", [], [ x; y ] -> maximumf32 ~x ~y
    | "maximumf64", [], [ x; y ] -> maximumf64 ~x ~y
    | "maxnumf128", [], [ x; y ] -> maxnumf128 ~x ~y
    | "maxnumf16", [], [ x; y ] -> maxnumf16 ~x ~y
    | "maxnumf32", [], [ x; y ] -> maxnumf32 ~x ~y
    | "maxnumf64", [], [ x; y ] -> maxnumf64 ~x ~y
    | "minimumf128", [], [ x; y ] -> minimumf128 ~x ~y
    | "minimumf16", [], [ x; y ] -> minimumf16 ~x ~y
    | "minimumf32", [], [ x; y ] -> minimumf32 ~x ~y
    | "minimumf64", [], [ x; y ] -> minimumf64 ~x ~y
    | "minnumf128", [], [ x; y ] -> minnumf128 ~x ~y
    | "minnumf16", [], [ x; y ] -> minnumf16 ~x ~y
    | "minnumf32", [], [ x; y ] -> minnumf32 ~x ~y
    | "minnumf64", [], [ x; y ] -> minnumf64 ~x ~y
    | "mul_with_overflow", [ t ], [ x; y ] -> mul_with_overflow ~t ~x ~y
    | "needs_drop", [ t ], [] -> needs_drop ~t
    | "nontemporal_store", [ t ], [ ptr; val_ ] ->
        nontemporal_store ~t ~ptr ~val_
    | "offset", [ ptr; delta ], [ dst; offset_ ] ->
        offset ~ptr ~delta ~dst ~offset:offset_
    | "powf128", [], [ a; x ] -> powf128 ~a ~x
    | "powf16", [], [ a; x ] -> powf16 ~a ~x
    | "powf32", [], [ a; x ] -> powf32 ~a ~x
    | "powf64", [], [ a; x ] -> powf64 ~a ~x
    | "powif128", [], [ a; x ] -> powif128 ~a ~x
    | "powif16", [], [ a; x ] -> powif16 ~a ~x
    | "powif32", [], [ a; x ] -> powif32 ~a ~x
    | "powif64", [], [ a; x ] -> powif64 ~a ~x
    | "prefetch_read_data", [ t ], [ data; locality ] ->
        prefetch_read_data ~t ~data ~locality
    | "prefetch_read_instruction", [ t ], [ data; locality ] ->
        prefetch_read_instruction ~t ~data ~locality
    | "prefetch_write_data", [ t ], [ data; locality ] ->
        prefetch_write_data ~t ~data ~locality
    | "prefetch_write_instruction", [ t ], [ data; locality ] ->
        prefetch_write_instruction ~t ~data ~locality
    | "ptr_guaranteed_cmp", [ t ], [ ptr; other ] ->
        ptr_guaranteed_cmp ~t ~ptr ~other
    | "ptr_mask", [ t ], [ ptr; mask ] -> ptr_mask ~t ~ptr ~mask
    | "ptr_metadata", [ p; m ], [ ptr ] -> ptr_metadata ~p ~m ~ptr
    | "ptr_offset_from", [ t ], [ ptr; base ] -> ptr_offset_from ~t ~ptr ~base
    | "ptr_offset_from_unsigned", [ t ], [ ptr; base ] ->
        ptr_offset_from_unsigned ~t ~ptr ~base
    | "raw_eq", [ t ], [ a; b ] -> raw_eq ~t ~a ~b
    | "read_via_copy", [ t ], [ ptr ] -> read_via_copy ~t ~ptr
    | "rotate_left", [ t ], [ x; shift ] -> rotate_left ~t ~x ~shift
    | "rotate_right", [ t ], [ x; shift ] -> rotate_right ~t ~x ~shift
    | "round_ties_even_f128", [], [ x ] -> round_ties_even_f128 ~x
    | "round_ties_even_f16", [], [ x ] -> round_ties_even_f16 ~x
    | "round_ties_even_f32", [], [ x ] -> round_ties_even_f32 ~x
    | "round_ties_even_f64", [], [ x ] -> round_ties_even_f64 ~x
    | "roundf128", [], [ x ] -> roundf128 ~x
    | "roundf16", [], [ x ] -> roundf16 ~x
    | "roundf32", [], [ x ] -> roundf32 ~x
    | "roundf64", [], [ x ] -> roundf64 ~x
    | "rustc_peek", [ t ], [ arg ] -> rustc_peek ~t ~arg
    | "saturating_add", [ t ], [ a; b ] -> saturating_add ~t ~a ~b
    | "saturating_sub", [ t ], [ a; b ] -> saturating_sub ~t ~a ~b
    | "select_unpredictable", [ t ], [ b; true_val; false_val ] ->
        select_unpredictable ~t ~b ~true_val ~false_val
    | "sinf128", [], [ x ] -> sinf128 ~x
    | "sinf16", [], [ x ] -> sinf16 ~x
    | "sinf32", [], [ x ] -> sinf32 ~x
    | "sinf64", [], [ x ] -> sinf64 ~x
    | "size_of", [ t ], [] -> size_of ~t
    | "size_of_val", [ t ], [ ptr ] -> size_of_val ~t ~ptr
    | "slice_get_unchecked", [ itemptr; sliceptr; t ], [ slice_ptr; index ] ->
        slice_get_unchecked ~itemptr ~sliceptr ~t ~slice_ptr ~index
    | "sqrtf128", [], [ x ] -> sqrtf128 ~x
    | "sqrtf16", [], [ x ] -> sqrtf16 ~x
    | "sqrtf32", [], [ x ] -> sqrtf32 ~x
    | "sqrtf64", [], [ x ] -> sqrtf64 ~x
    | "sub_with_overflow", [ t ], [ x; y ] -> sub_with_overflow ~t ~x ~y
    | "three_way_compare", [ t ], [ lhs; rhss ] ->
        three_way_compare ~t ~lhs ~rhss
    | "transmute", [ t_src; dst ], [ src ] -> transmute ~t_src ~dst ~src
    | "transmute_unchecked", [ t_src; dst ], [ src ] ->
        transmute_unchecked ~t_src ~dst ~src
    | "truncf128", [], [ x ] -> truncf128 ~x
    | "truncf16", [], [ x ] -> truncf16 ~x
    | "truncf32", [], [ x ] -> truncf32 ~x
    | "truncf64", [], [ x ] -> truncf64 ~x
    | "type_id", [ t ], [] -> type_id ~t
    | "type_name", [ t ], [] -> type_name ~t
    | "typed_swap_nonoverlapping", [ t ], [ x; y ] ->
        typed_swap_nonoverlapping ~t ~x ~y
    | "ub_checks", [], [] -> ub_checks
    | "unaligned_volatile_load", [ t ], [ src ] ->
        unaligned_volatile_load ~t ~src
    | "unaligned_volatile_store", [ t ], [ dst; val_ ] ->
        unaligned_volatile_store ~t ~dst ~val_
    | "unchecked_add", [ t ], [ x; y ] -> unchecked_add ~t ~x ~y
    | "unchecked_div", [ t ], [ x; y ] -> unchecked_div ~t ~x ~y
    | "unchecked_mul", [ t ], [ x; y ] -> unchecked_mul ~t ~x ~y
    | "unchecked_rem", [ t ], [ x; y ] -> unchecked_rem ~t ~x ~y
    | "unchecked_shl", [ t; u ], [ x; y ] -> unchecked_shl ~t ~u ~x ~y
    | "unchecked_shr", [ t; u ], [ x; y ] -> unchecked_shr ~t ~u ~x ~y
    | "unchecked_sub", [ t ], [ x; y ] -> unchecked_sub ~t ~x ~y
    | "unlikely", [], [ b ] -> unlikely ~b
    | "unreachable", [], [] -> unreachable
    | "va_arg", [ t ], [ ap ] -> va_arg ~t ~ap
    | "va_copy", [], [ dest; src ] -> va_copy ~dest ~src
    | "va_end", [], [ ap ] -> va_end ~ap
    | "variant_count", [ t ], [] -> variant_count ~t
    | "volatile_copy_memory", [ t ], [ dst; src; count ] ->
        volatile_copy_memory ~t ~dst ~src ~count
    | "volatile_copy_nonoverlapping_memory", [ t ], [ dst; src; count ] ->
        volatile_copy_nonoverlapping_memory ~t ~dst ~src ~count
    | "volatile_load", [ t ], [ src ] -> volatile_load ~t ~src
    | "volatile_set_memory", [ t ], [ dst; val_; count ] ->
        volatile_set_memory ~t ~dst ~val_ ~count
    | "volatile_store", [ t ], [ dst; val_ ] -> volatile_store ~t ~dst ~val_
    | "vtable_align", [], [ ptr ] -> vtable_align ~ptr
    | "vtable_size", [], [ ptr ] -> vtable_size ~ptr
    | "wrapping_add", [ t ], [ a; b ] -> wrapping_add ~t ~a ~b
    | "wrapping_mul", [ t ], [ a; b ] -> wrapping_mul ~t ~a ~b
    | "wrapping_sub", [ t ], [ a; b ] -> wrapping_sub ~t ~a ~b
    | "write_bytes", [ t ], [ dst; val_; count ] ->
        write_bytes ~t ~dst ~val_ ~count
    | "write_via_move", [ t ], [ ptr; value ] -> write_via_move ~t ~ptr ~value
    | name, _, _ ->
        fun _ ->
          Fmt.kstr not_impl
            "Intrinsic %s not found, or not called with the right arguments"
            name
end [@warning "-32"]
