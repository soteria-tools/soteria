(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

[@@@warning "-unused-value-declaration"]

module M (StateM : State.StateM.S) : Intrinsics_intf.M(StateM).Impl = struct
  open StateM

  let abort = not_impl "Unsupported intrinsic: abort"

  let add_with_overflow ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: add_with_overflow"

  let aggregate_raw_ptr ~p:_ ~d:_ ~m:_ ~data:_ ~meta:_ =
    not_impl "Unsupported intrinsic: aggregate_raw_ptr"

  let align_of ~t:_ = not_impl "Unsupported intrinsic: align_of"
  let align_of_val ~t:_ ~ptr:_ = not_impl "Unsupported intrinsic: align_of_val"

  let arith_offset ~t:_ ~dst:_ ~offset:_ =
    not_impl "Unsupported intrinsic: arith_offset"

  let assert_inhabited ~t:_ = not_impl "Unsupported intrinsic: assert_inhabited"

  let assert_mem_uninitialized_valid ~t:_ =
    not_impl "Unsupported intrinsic: assert_mem_uninitialized_valid"

  let assert_zero_valid ~t:_ =
    not_impl "Unsupported intrinsic: assert_zero_valid"

  let assume ~b:_ = not_impl "Unsupported intrinsic: assume"

  let atomic_and ~t:_ ~u:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_and"

  let atomic_cxchg ~t:_ ~ord_succ:_ ~ord_fail:_ ~dst:_ ~old:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_cxchg"

  let atomic_cxchgweak ~t:_ ~ord_succ:_ ~ord_fail:_ ~_dst:_ ~_old:_ ~_src:_ =
    not_impl "Unsupported intrinsic: atomic_cxchgweak"

  let atomic_fence ~ord:_ = not_impl "Unsupported intrinsic: atomic_fence"

  let atomic_load ~t:_ ~ord:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_load"

  let atomic_max ~t:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_max"

  let atomic_min ~t:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_min"

  let atomic_nand ~t:_ ~u:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_nand"

  let atomic_or ~t:_ ~u:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_or"

  let atomic_singlethreadfence ~ord:_ =
    not_impl "Unsupported intrinsic: atomic_singlethreadfence"

  let atomic_store ~t:_ ~ord:_ ~dst:_ ~val_:_ =
    not_impl "Unsupported intrinsic: atomic_store"

  let atomic_umax ~t:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_umax"

  let atomic_umin ~t:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_umin"

  let atomic_xadd ~t:_ ~u:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_xadd"

  let atomic_xchg ~t:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_xchg"

  let atomic_xor ~t:_ ~u:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_xor"

  let atomic_xsub ~t:_ ~u:_ ~ord:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: atomic_xsub"

  let autodiff ~t_f:_ ~g:_ ~t:_ ~r:_ ~f:_ ~df:_ ~args:_ =
    not_impl "Unsupported intrinsic: autodiff"

  let bitreverse ~t:_ ~x:_ = not_impl "Unsupported intrinsic: bitreverse"
  let black_box ~t:_ ~dummy:_ = not_impl "Unsupported intrinsic: black_box"
  let breakpoint = not_impl "Unsupported intrinsic: breakpoint"
  let bswap ~t:_ ~x:_ = not_impl "Unsupported intrinsic: bswap"
  let caller_location = not_impl "Unsupported intrinsic: caller_location"

  let carrying_mul_add ~t:_ ~u:_ ~multiplier:_ ~multiplicand:_ ~addend:_
      ~carry:_ =
    not_impl "Unsupported intrinsic: carrying_mul_add"

  let catch_unwind _ ~_try_fn:_ ~_data:_ ~_catch_fn:_ =
    not_impl "Unsupported intrinsic: catch_unwind"

  let ceilf128 ~x:_ = not_impl "Unsupported intrinsic: ceilf128"
  let ceilf16 ~x:_ = not_impl "Unsupported intrinsic: ceilf16"
  let ceilf32 ~x:_ = not_impl "Unsupported intrinsic: ceilf32"
  let ceilf64 ~x:_ = not_impl "Unsupported intrinsic: ceilf64"
  let cold_path = not_impl "Unsupported intrinsic: cold_path"

  let compare_bytes ~left:_ ~right:_ ~bytes:_ =
    not_impl "Unsupported intrinsic: compare_bytes"

  let const_deallocate ~_ptr:_ ~_size:_ ~_align:_ =
    not_impl "Unsupported intrinsic: const_deallocate"

  let const_eval_select ~arg:_ ~f:_ ~g:_ ~ret:_ ~_arg:_ ~_called_in_const:_
      ~_called_at_rt:_ =
    not_impl "Unsupported intrinsic: const_eval_select"

  let const_make_global ~ptr:_ =
    not_impl "Unsupported intrinsic: const_make_global"

  let contract_check_ensures ~c:_ ~t_ret:_ ~cond:_ ~ret:_ =
    not_impl "Unsupported intrinsic: contract_check_ensures"

  let contract_check_requires ~c:_ ~cond:_ =
    not_impl "Unsupported intrinsic: contract_check_requires"

  let copy ~t:_ ~src:_ ~dst:_ ~count:_ = not_impl "Unsupported intrinsic: copy"

  let copy_nonoverlapping ~t:_ ~src:_ ~dst:_ ~count:_ =
    not_impl "Unsupported intrinsic: copy_nonoverlapping"

  let copysignf128 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: copysignf128"
  let copysignf16 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: copysignf16"
  let copysignf32 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: copysignf32"
  let copysignf64 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: copysignf64"
  let cosf128 ~x:_ = not_impl "Unsupported intrinsic: cosf128"
  let cosf16 ~x:_ = not_impl "Unsupported intrinsic: cosf16"
  let cosf32 ~x:_ = not_impl "Unsupported intrinsic: cosf32"
  let cosf64 ~x:_ = not_impl "Unsupported intrinsic: cosf64"
  let ctlz ~t:_ ~x:_ = not_impl "Unsupported intrinsic: ctlz"
  let ctlz_nonzero ~t:_ ~x:_ = not_impl "Unsupported intrinsic: ctlz_nonzero"
  let ctpop ~t:_ ~x:_ = not_impl "Unsupported intrinsic: ctpop"
  let cttz ~t:_ ~x:_ = not_impl "Unsupported intrinsic: cttz"
  let cttz_nonzero ~t:_ ~x:_ = not_impl "Unsupported intrinsic: cttz_nonzero"

  let discriminant_value ~t:_ ~v:_ =
    not_impl "Unsupported intrinsic: discriminant_value"

  let disjoint_bitor ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: disjoint_bitor"

  let exact_div ~t:_ ~x:_ ~y:_ = not_impl "Unsupported intrinsic: exact_div"
  let exp2f128 ~x:_ = not_impl "Unsupported intrinsic: exp2f128"
  let exp2f16 ~x:_ = not_impl "Unsupported intrinsic: exp2f16"
  let exp2f32 ~x:_ = not_impl "Unsupported intrinsic: exp2f32"
  let exp2f64 ~x:_ = not_impl "Unsupported intrinsic: exp2f64"
  let expf128 ~x:_ = not_impl "Unsupported intrinsic: expf128"
  let expf16 ~x:_ = not_impl "Unsupported intrinsic: expf16"
  let expf32 ~x:_ = not_impl "Unsupported intrinsic: expf32"
  let expf64 ~x:_ = not_impl "Unsupported intrinsic: expf64"
  let fabsf128 ~x:_ = not_impl "Unsupported intrinsic: fabsf128"
  let fabsf16 ~x:_ = not_impl "Unsupported intrinsic: fabsf16"
  let fabsf32 ~x:_ = not_impl "Unsupported intrinsic: fabsf32"
  let fabsf64 ~x:_ = not_impl "Unsupported intrinsic: fabsf64"

  let fadd_algebraic ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: fadd_algebraic"

  let fadd_fast ~t:_ ~a:_ ~b:_ = not_impl "Unsupported intrinsic: fadd_fast"

  let fdiv_algebraic ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: fdiv_algebraic"

  let fdiv_fast ~t:_ ~a:_ ~b:_ = not_impl "Unsupported intrinsic: fdiv_fast"

  let float_to_int_unchecked ~float:_ ~int:_ ~value:_ =
    not_impl "Unsupported intrinsic: float_to_int_unchecked"

  let floorf128 ~x:_ = not_impl "Unsupported intrinsic: floorf128"
  let floorf16 ~x:_ = not_impl "Unsupported intrinsic: floorf16"
  let floorf32 ~x:_ = not_impl "Unsupported intrinsic: floorf32"
  let floorf64 ~x:_ = not_impl "Unsupported intrinsic: floorf64"
  let fmaf128 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmaf128"
  let fmaf16 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmaf16"
  let fmaf32 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmaf32"
  let fmaf64 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmaf64"

  let fmul_algebraic ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: fmul_algebraic"

  let fmul_fast ~t:_ ~a:_ ~b:_ = not_impl "Unsupported intrinsic: fmul_fast"
  let fmuladdf128 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmuladdf128"
  let fmuladdf16 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmuladdf16"
  let fmuladdf32 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmuladdf32"
  let fmuladdf64 ~a:_ ~b:_ ~c:_ = not_impl "Unsupported intrinsic: fmuladdf64"
  let forget ~t:_ ~arg:_ = not_impl "Unsupported intrinsic: forget"

  let frem_algebraic ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: frem_algebraic"

  let frem_fast ~t:_ ~a:_ ~b:_ = not_impl "Unsupported intrinsic: frem_fast"

  let fsub_algebraic ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: fsub_algebraic"

  let fsub_fast ~t:_ ~a:_ ~b:_ = not_impl "Unsupported intrinsic: fsub_fast"

  let is_val_statically_known ~t:_ ~_arg:_ =
    not_impl "Unsupported intrinsic: is_val_statically_known"

  let likely ~b:_ = not_impl "Unsupported intrinsic: likely"
  let log10f128 ~x:_ = not_impl "Unsupported intrinsic: log10f128"
  let log10f16 ~x:_ = not_impl "Unsupported intrinsic: log10f16"
  let log10f32 ~x:_ = not_impl "Unsupported intrinsic: log10f32"
  let log10f64 ~x:_ = not_impl "Unsupported intrinsic: log10f64"
  let log2f128 ~x:_ = not_impl "Unsupported intrinsic: log2f128"
  let log2f16 ~x:_ = not_impl "Unsupported intrinsic: log2f16"
  let log2f32 ~x:_ = not_impl "Unsupported intrinsic: log2f32"
  let log2f64 ~x:_ = not_impl "Unsupported intrinsic: log2f64"
  let logf128 ~x:_ = not_impl "Unsupported intrinsic: logf128"
  let logf16 ~x:_ = not_impl "Unsupported intrinsic: logf16"
  let logf32 ~x:_ = not_impl "Unsupported intrinsic: logf32"
  let logf64 ~x:_ = not_impl "Unsupported intrinsic: logf64"
  let maximumf128 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maximumf128"
  let maximumf16 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maximumf16"
  let maximumf32 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maximumf32"
  let maximumf64 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maximumf64"
  let maxnumf128 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maxnumf128"
  let maxnumf16 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maxnumf16"
  let maxnumf32 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maxnumf32"
  let maxnumf64 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: maxnumf64"
  let minimumf128 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minimumf128"
  let minimumf16 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minimumf16"
  let minimumf32 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minimumf32"
  let minimumf64 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minimumf64"
  let minnumf128 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minnumf128"
  let minnumf16 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minnumf16"
  let minnumf32 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minnumf32"
  let minnumf64 ~x:_ ~y:_ = not_impl "Unsupported intrinsic: minnumf64"

  let mul_with_overflow ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: mul_with_overflow"

  let needs_drop ~t:_ = not_impl "Unsupported intrinsic: needs_drop"

  let nontemporal_store ~t:_ ~ptr:_ ~val_:_ =
    not_impl "Unsupported intrinsic: nontemporal_store"

  let offset ~ptr:_ ~delta:_ ~dst:_ ~offset:_ =
    not_impl "Unsupported intrinsic: offset"

  let offset_of ~t:_ ~variant:_ ~field:_ =
    not_impl "Unsupported intrinsic: offset_of"

  let overflow_checks = not_impl "Unsupported intrinsic: overflow_checks"
  let powf128 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powf128"
  let powf16 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powf16"
  let powf32 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powf32"
  let powf64 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powf64"
  let powif128 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powif128"
  let powif16 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powif16"
  let powif32 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powif32"
  let powif64 ~a:_ ~x:_ = not_impl "Unsupported intrinsic: powif64"

  let prefetch_read_data ~t:_ ~locality:_ ~data:_ =
    not_impl "Unsupported intrinsic: prefetch_read_data"

  let prefetch_read_instruction ~t:_ ~locality:_ ~data:_ =
    not_impl "Unsupported intrinsic: prefetch_read_instruction"

  let prefetch_write_data ~t:_ ~locality:_ ~data:_ =
    not_impl "Unsupported intrinsic: prefetch_write_data"

  let prefetch_write_instruction ~t:_ ~locality:_ ~data:_ =
    not_impl "Unsupported intrinsic: prefetch_write_instruction"

  let ptr_guaranteed_cmp ~t:_ ~ptr:_ ~other:_ =
    not_impl "Unsupported intrinsic: ptr_guaranteed_cmp"

  let ptr_mask ~t:_ ~ptr:_ ~mask:_ = not_impl "Unsupported intrinsic: ptr_mask"

  let ptr_metadata ~p:_ ~m:_ ~ptr:_ =
    not_impl "Unsupported intrinsic: ptr_metadata"

  let ptr_offset_from ~t:_ ~ptr:_ ~base:_ =
    not_impl "Unsupported intrinsic: ptr_offset_from"

  let ptr_offset_from_unsigned ~t:_ ~ptr:_ ~base:_ =
    not_impl "Unsupported intrinsic: ptr_offset_from_unsigned"

  let raw_eq ~t:_ ~a:_ ~b:_ = not_impl "Unsupported intrinsic: raw_eq"

  let read_via_copy ~t:_ ~ptr:_ =
    not_impl "Unsupported intrinsic: read_via_copy"

  let rotate_left ~t:_ ~x:_ ~shift:_ =
    not_impl "Unsupported intrinsic: rotate_left"

  let rotate_right ~t:_ ~x:_ ~shift:_ =
    not_impl "Unsupported intrinsic: rotate_right"

  let round_ties_even_f128 ~x:_ =
    not_impl "Unsupported intrinsic: round_ties_even_f128"

  let round_ties_even_f16 ~x:_ =
    not_impl "Unsupported intrinsic: round_ties_even_f16"

  let round_ties_even_f32 ~x:_ =
    not_impl "Unsupported intrinsic: round_ties_even_f32"

  let round_ties_even_f64 ~x:_ =
    not_impl "Unsupported intrinsic: round_ties_even_f64"

  let roundf128 ~x:_ = not_impl "Unsupported intrinsic: roundf128"
  let roundf16 ~x:_ = not_impl "Unsupported intrinsic: roundf16"
  let roundf32 ~x:_ = not_impl "Unsupported intrinsic: roundf32"
  let roundf64 ~x:_ = not_impl "Unsupported intrinsic: roundf64"
  let rustc_peek ~t:_ ~arg:_ = not_impl "Unsupported intrinsic: rustc_peek"

  let saturating_add ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: saturating_add"

  let saturating_sub ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: saturating_sub"

  let select_unpredictable ~t:_ ~b:_ ~true_val:_ ~false_val:_ =
    not_impl "Unsupported intrinsic: select_unpredictable"

  let sinf128 ~x:_ = not_impl "Unsupported intrinsic: sinf128"
  let sinf16 ~x:_ = not_impl "Unsupported intrinsic: sinf16"
  let sinf32 ~x:_ = not_impl "Unsupported intrinsic: sinf32"
  let sinf64 ~x:_ = not_impl "Unsupported intrinsic: sinf64"
  let size_of ~t:_ = not_impl "Unsupported intrinsic: size_of"
  let size_of_val ~t:_ ~ptr:_ = not_impl "Unsupported intrinsic: size_of_val"

  let slice_get_unchecked ~itemptr:_ ~sliceptr:_ ~t:_ ~slice_ptr:_ ~index:_ =
    not_impl "Unsupported intrinsic: slice_get_unchecked"

  let sqrtf128 ~x:_ = not_impl "Unsupported intrinsic: sqrtf128"
  let sqrtf16 ~x:_ = not_impl "Unsupported intrinsic: sqrtf16"
  let sqrtf32 ~x:_ = not_impl "Unsupported intrinsic: sqrtf32"
  let sqrtf64 ~x:_ = not_impl "Unsupported intrinsic: sqrtf64"

  let sub_with_overflow ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: sub_with_overflow"

  let three_way_compare ~t:_ ~lhs:_ ~rhss:_ =
    not_impl "Unsupported intrinsic: three_way_compare"

  let transmute ~t_src:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: transmute"

  let transmute_unchecked ~t_src:_ ~dst:_ ~src:_ =
    not_impl "Unsupported intrinsic: transmute_unchecked"

  let truncf128 ~x:_ = not_impl "Unsupported intrinsic: truncf128"
  let truncf16 ~x:_ = not_impl "Unsupported intrinsic: truncf16"
  let truncf32 ~x:_ = not_impl "Unsupported intrinsic: truncf32"
  let truncf64 ~x:_ = not_impl "Unsupported intrinsic: truncf64"
  let type_id ~t:_ = not_impl "Unsupported intrinsic: type_id"
  let type_id_eq ~a:_ ~b:_ = not_impl "Unsupported intrinsic: type_id_eq"
  let type_name ~t:_ = not_impl "Unsupported intrinsic: type_name"

  let typed_swap_nonoverlapping ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: typed_swap_nonoverlapping"

  let ub_checks = not_impl "Unsupported intrinsic: ub_checks"

  let unaligned_volatile_load ~t:_ ~src:_ =
    not_impl "Unsupported intrinsic: unaligned_volatile_load"

  let unaligned_volatile_store ~t:_ ~dst:_ ~val_:_ =
    not_impl "Unsupported intrinsic: unaligned_volatile_store"

  let unchecked_add ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_add"

  let unchecked_div ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_div"

  let unchecked_funnel_shl ~t:_ ~a:_ ~b:_ ~shift:_ =
    not_impl "Unsupported intrinsic: unchecked_funnel_shl"

  let unchecked_funnel_shr ~t:_ ~a:_ ~b:_ ~shift:_ =
    not_impl "Unsupported intrinsic: unchecked_funnel_shr"

  let unchecked_mul ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_mul"

  let unchecked_rem ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_rem"

  let unchecked_shl ~t:_ ~u:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_shl"

  let unchecked_shr ~t:_ ~u:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_shr"

  let unchecked_sub ~t:_ ~x:_ ~y:_ =
    not_impl "Unsupported intrinsic: unchecked_sub"

  let unlikely ~b:_ = not_impl "Unsupported intrinsic: unlikely"
  let unreachable = not_impl "Unsupported intrinsic: unreachable"
  let va_arg ~t:_ ~ap:_ = not_impl "Unsupported intrinsic: va_arg"
  let va_copy ~dest:_ ~src:_ = not_impl "Unsupported intrinsic: va_copy"
  let va_end ~ap:_ = not_impl "Unsupported intrinsic: va_end"
  let variant_count ~t:_ = not_impl "Unsupported intrinsic: variant_count"

  let volatile_copy_memory ~t:_ ~dst:_ ~src:_ ~count:_ =
    not_impl "Unsupported intrinsic: volatile_copy_memory"

  let volatile_copy_nonoverlapping_memory ~t:_ ~dst:_ ~src:_ ~count:_ =
    not_impl "Unsupported intrinsic: volatile_copy_nonoverlapping_memory"

  let volatile_load ~t:_ ~src:_ =
    not_impl "Unsupported intrinsic: volatile_load"

  let volatile_set_memory ~t:_ ~dst:_ ~val_:_ ~count:_ =
    not_impl "Unsupported intrinsic: volatile_set_memory"

  let volatile_store ~t:_ ~dst:_ ~val_:_ =
    not_impl "Unsupported intrinsic: volatile_store"

  let vtable_align ~ptr:_ = not_impl "Unsupported intrinsic: vtable_align"
  let vtable_size ~ptr:_ = not_impl "Unsupported intrinsic: vtable_size"

  let wrapping_add ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: wrapping_add"

  let wrapping_mul ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: wrapping_mul"

  let wrapping_sub ~t:_ ~a:_ ~b:_ =
    not_impl "Unsupported intrinsic: wrapping_sub"

  let write_bytes ~t:_ ~dst:_ ~val_:_ ~count:_ =
    not_impl "Unsupported intrinsic: write_bytes"

  let write_via_move ~t:_ ~ptr:_ ~value:_ =
    not_impl "Unsupported intrinsic: write_via_move"
end
