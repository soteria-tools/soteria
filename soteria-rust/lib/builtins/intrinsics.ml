(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

[@@@warning "-unused-value-declaration"]

open Rustsymex
open Rust_val

module M (State : State_intf.S) = struct
  type rust_val = State.Sptr.t Rust_val.t

  let ( let+ ) x f () st =
    let open Rustsymex.Syntax in
    let++ y, (), st' = x () st in
    (f y, (), st')

  let[@inline] as_ptr (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Int v ->
        let v = Typed.cast_i Usize v in
        let ptr = State.Sptr.null_ptr_of v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v
  let abort () _state = not_impl "Unsupported intrinsic: abort"

  let add_with_overflow ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: add_with_overflow"

  let aggregate_raw_ptr ~p:_ ~d:_ ~m:_ ~data:_ ~meta:_ () _state =
    not_impl "Unsupported intrinsic: aggregate_raw_ptr"

  let align_of ~t:_ () _state = not_impl "Unsupported intrinsic: align_of"

  let align_of_val ~t:_ ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: align_of_val"

  let arith_offset ~t:_ ~dst:_ ~offset:_ () _state =
    not_impl "Unsupported intrinsic: arith_offset"

  let assert_inhabited ~t:_ () _state =
    not_impl "Unsupported intrinsic: assert_inhabited"

  let assert_mem_uninitialized_valid ~t:_ () _state =
    not_impl "Unsupported intrinsic: assert_mem_uninitialized_valid"

  let assert_zero_valid ~t:_ () _state =
    not_impl "Unsupported intrinsic: assert_zero_valid"

  let assume ~b:_ () _state = not_impl "Unsupported intrinsic: assume"

  let autodiff ~t_f:_ ~g:_ ~t:_ ~r:_ ~f:_ ~df:_ ~args:_ () _state =
    not_impl "Unsupported intrinsic: autodiff"

  let bitreverse ~t:_ ~x:_ () _state =
    not_impl "Unsupported intrinsic: bitreverse"

  let black_box ~t:_ ~dummy:_ () _state =
    not_impl "Unsupported intrinsic: black_box"

  let breakpoint () _state = not_impl "Unsupported intrinsic: breakpoint"
  let bswap ~t:_ ~x:_ () _state = not_impl "Unsupported intrinsic: bswap"

  let caller_location () _state =
    not_impl "Unsupported intrinsic: caller_location"

  let carrying_mul_add ~t:_ ~u:_ ~multiplier:_ ~multiplicand:_ ~addend:_
      ~carry:_ () _state =
    not_impl "Unsupported intrinsic: carrying_mul_add"

  let catch_unwind _ ~_try_fn:_ ~_data:_ ~_catch_fn:_ () _state =
    not_impl "Unsupported intrinsic: catch_unwind"

  let ceilf128 ~x:_ () _state = not_impl "Unsupported intrinsic: ceilf128"
  let ceilf16 ~x:_ () _state = not_impl "Unsupported intrinsic: ceilf16"
  let ceilf32 ~x:_ () _state = not_impl "Unsupported intrinsic: ceilf32"
  let ceilf64 ~x:_ () _state = not_impl "Unsupported intrinsic: ceilf64"
  let cold_path () _state = not_impl "Unsupported intrinsic: cold_path"

  let compare_bytes ~left:_ ~right:_ ~bytes:_ () _state =
    not_impl "Unsupported intrinsic: compare_bytes"

  let const_deallocate ~_ptr:_ ~_size:_ ~_align:_ () _state =
    not_impl "Unsupported intrinsic: const_deallocate"

  let const_eval_select ~arg:_ ~f:_ ~g:_ ~ret:_ ~_arg:_ ~_called_in_const:_
      ~_called_at_rt:_ () _state =
    not_impl "Unsupported intrinsic: const_eval_select"

  let const_make_global ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: const_make_global"

  let contract_check_ensures ~c:_ ~t_ret:_ ~cond:_ ~ret:_ () _state =
    not_impl "Unsupported intrinsic: contract_check_ensures"

  let contract_check_requires ~c:_ ~cond:_ () _state =
    not_impl "Unsupported intrinsic: contract_check_requires"

  let copy ~t:_ ~src:_ ~dst:_ ~count:_ () _state =
    not_impl "Unsupported intrinsic: copy"

  let copy_nonoverlapping ~t:_ ~src:_ ~dst:_ ~count:_ () _state =
    not_impl "Unsupported intrinsic: copy_nonoverlapping"

  let copysignf128 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: copysignf128"

  let copysignf16 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: copysignf16"

  let copysignf32 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: copysignf32"

  let copysignf64 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: copysignf64"

  let cosf128 ~x:_ () _state = not_impl "Unsupported intrinsic: cosf128"
  let cosf16 ~x:_ () _state = not_impl "Unsupported intrinsic: cosf16"
  let cosf32 ~x:_ () _state = not_impl "Unsupported intrinsic: cosf32"
  let cosf64 ~x:_ () _state = not_impl "Unsupported intrinsic: cosf64"
  let ctlz ~t:_ ~x:_ () _state = not_impl "Unsupported intrinsic: ctlz"

  let ctlz_nonzero ~t:_ ~x:_ () _state =
    not_impl "Unsupported intrinsic: ctlz_nonzero"

  let ctpop ~t:_ ~x:_ () _state = not_impl "Unsupported intrinsic: ctpop"
  let cttz ~t:_ ~x:_ () _state = not_impl "Unsupported intrinsic: cttz"

  let cttz_nonzero ~t:_ ~x:_ () _state =
    not_impl "Unsupported intrinsic: cttz_nonzero"

  let discriminant_value ~t:_ ~v:_ () _state =
    not_impl "Unsupported intrinsic: discriminant_value"

  let disjoint_bitor ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: disjoint_bitor"

  let exact_div ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: exact_div"

  let exp2f128 ~x:_ () _state = not_impl "Unsupported intrinsic: exp2f128"
  let exp2f16 ~x:_ () _state = not_impl "Unsupported intrinsic: exp2f16"
  let exp2f32 ~x:_ () _state = not_impl "Unsupported intrinsic: exp2f32"
  let exp2f64 ~x:_ () _state = not_impl "Unsupported intrinsic: exp2f64"
  let expf128 ~x:_ () _state = not_impl "Unsupported intrinsic: expf128"
  let expf16 ~x:_ () _state = not_impl "Unsupported intrinsic: expf16"
  let expf32 ~x:_ () _state = not_impl "Unsupported intrinsic: expf32"
  let expf64 ~x:_ () _state = not_impl "Unsupported intrinsic: expf64"
  let fabsf128 ~x:_ () _state = not_impl "Unsupported intrinsic: fabsf128"
  let fabsf16 ~x:_ () _state = not_impl "Unsupported intrinsic: fabsf16"
  let fabsf32 ~x:_ () _state = not_impl "Unsupported intrinsic: fabsf32"
  let fabsf64 ~x:_ () _state = not_impl "Unsupported intrinsic: fabsf64"

  let fadd_algebraic ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fadd_algebraic"

  let fadd_fast ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fadd_fast"

  let fdiv_algebraic ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fdiv_algebraic"

  let fdiv_fast ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fdiv_fast"

  let float_to_int_unchecked ~float:_ ~int:_ ~value:_ () _state =
    not_impl "Unsupported intrinsic: float_to_int_unchecked"

  let floorf128 ~x:_ () _state = not_impl "Unsupported intrinsic: floorf128"
  let floorf16 ~x:_ () _state = not_impl "Unsupported intrinsic: floorf16"
  let floorf32 ~x:_ () _state = not_impl "Unsupported intrinsic: floorf32"
  let floorf64 ~x:_ () _state = not_impl "Unsupported intrinsic: floorf64"

  let fmaf128 ~a:_ ~b:_ ~c:_ () _state =
    not_impl "Unsupported intrinsic: fmaf128"

  let fmaf16 ~a:_ ~b:_ ~c:_ () _state = not_impl "Unsupported intrinsic: fmaf16"
  let fmaf32 ~a:_ ~b:_ ~c:_ () _state = not_impl "Unsupported intrinsic: fmaf32"
  let fmaf64 ~a:_ ~b:_ ~c:_ () _state = not_impl "Unsupported intrinsic: fmaf64"

  let fmul_algebraic ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fmul_algebraic"

  let fmul_fast ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fmul_fast"

  let fmuladdf128 ~a:_ ~b:_ ~c:_ () _state =
    not_impl "Unsupported intrinsic: fmuladdf128"

  let fmuladdf16 ~a:_ ~b:_ ~c:_ () _state =
    not_impl "Unsupported intrinsic: fmuladdf16"

  let fmuladdf32 ~a:_ ~b:_ ~c:_ () _state =
    not_impl "Unsupported intrinsic: fmuladdf32"

  let fmuladdf64 ~a:_ ~b:_ ~c:_ () _state =
    not_impl "Unsupported intrinsic: fmuladdf64"

  let forget ~t:_ ~arg:_ () _state = not_impl "Unsupported intrinsic: forget"

  let frem_algebraic ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: frem_algebraic"

  let frem_fast ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: frem_fast"

  let fsub_algebraic ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fsub_algebraic"

  let fsub_fast ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: fsub_fast"

  let is_val_statically_known ~t:_ ~_arg:_ () _state =
    not_impl "Unsupported intrinsic: is_val_statically_known"

  let likely ~b:_ () _state = not_impl "Unsupported intrinsic: likely"
  let log10f128 ~x:_ () _state = not_impl "Unsupported intrinsic: log10f128"
  let log10f16 ~x:_ () _state = not_impl "Unsupported intrinsic: log10f16"
  let log10f32 ~x:_ () _state = not_impl "Unsupported intrinsic: log10f32"
  let log10f64 ~x:_ () _state = not_impl "Unsupported intrinsic: log10f64"
  let log2f128 ~x:_ () _state = not_impl "Unsupported intrinsic: log2f128"
  let log2f16 ~x:_ () _state = not_impl "Unsupported intrinsic: log2f16"
  let log2f32 ~x:_ () _state = not_impl "Unsupported intrinsic: log2f32"
  let log2f64 ~x:_ () _state = not_impl "Unsupported intrinsic: log2f64"
  let logf128 ~x:_ () _state = not_impl "Unsupported intrinsic: logf128"
  let logf16 ~x:_ () _state = not_impl "Unsupported intrinsic: logf16"
  let logf32 ~x:_ () _state = not_impl "Unsupported intrinsic: logf32"
  let logf64 ~x:_ () _state = not_impl "Unsupported intrinsic: logf64"

  let maximumf128 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maximumf128"

  let maximumf16 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maximumf16"

  let maximumf32 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maximumf32"

  let maximumf64 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maximumf64"

  let maxnumf128 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maxnumf128"

  let maxnumf16 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maxnumf16"

  let maxnumf32 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maxnumf32"

  let maxnumf64 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: maxnumf64"

  let minimumf128 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minimumf128"

  let minimumf16 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minimumf16"

  let minimumf32 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minimumf32"

  let minimumf64 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minimumf64"

  let minnumf128 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minnumf128"

  let minnumf16 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minnumf16"

  let minnumf32 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minnumf32"

  let minnumf64 ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: minnumf64"

  let mul_with_overflow ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: mul_with_overflow"

  let needs_drop ~t:_ () _state = not_impl "Unsupported intrinsic: needs_drop"

  let nontemporal_store ~t:_ ~ptr:_ ~val_:_ () _state =
    not_impl "Unsupported intrinsic: nontemporal_store"

  let offset ~ptr:_ ~delta:_ ~dst:_ ~offset:_ () _state =
    not_impl "Unsupported intrinsic: offset"

  let offset_of ~t:_ ~variant:_ ~field:_ () _state =
    not_impl "Unsupported intrinsic: offset_of"

  let overflow_checks () _state =
    not_impl "Unsupported intrinsic: overflow_checks"

  let powf128 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powf128"
  let powf16 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powf16"
  let powf32 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powf32"
  let powf64 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powf64"
  let powif128 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powif128"
  let powif16 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powif16"
  let powif32 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powif32"
  let powif64 ~a:_ ~x:_ () _state = not_impl "Unsupported intrinsic: powif64"

  let prefetch_read_data ~t:_ ~data:_ () _state =
    not_impl "Unsupported intrinsic: prefetch_read_data"

  let prefetch_read_instruction ~t:_ ~data:_ () _state =
    not_impl "Unsupported intrinsic: prefetch_read_instruction"

  let prefetch_write_data ~t:_ ~data:_ () _state =
    not_impl "Unsupported intrinsic: prefetch_write_data"

  let prefetch_write_instruction ~t:_ ~data:_ () _state =
    not_impl "Unsupported intrinsic: prefetch_write_instruction"

  let ptr_guaranteed_cmp ~t:_ ~ptr:_ ~other:_ () _state =
    not_impl "Unsupported intrinsic: ptr_guaranteed_cmp"

  let ptr_mask ~t:_ ~ptr:_ ~mask:_ () _state =
    not_impl "Unsupported intrinsic: ptr_mask"

  let ptr_metadata ~p:_ ~m:_ ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: ptr_metadata"

  let ptr_offset_from ~t:_ ~ptr:_ ~base:_ () _state =
    not_impl "Unsupported intrinsic: ptr_offset_from"

  let ptr_offset_from_unsigned ~t:_ ~ptr:_ ~base:_ () _state =
    not_impl "Unsupported intrinsic: ptr_offset_from_unsigned"

  let raw_eq ~t:_ ~a:_ ~b:_ () _state = not_impl "Unsupported intrinsic: raw_eq"

  let read_via_copy ~t:_ ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: read_via_copy"

  let rotate_left ~t:_ ~x:_ ~shift:_ () _state =
    not_impl "Unsupported intrinsic: rotate_left"

  let rotate_right ~t:_ ~x:_ ~shift:_ () _state =
    not_impl "Unsupported intrinsic: rotate_right"

  let round_ties_even_f128 ~x:_ () _state =
    not_impl "Unsupported intrinsic: round_ties_even_f128"

  let round_ties_even_f16 ~x:_ () _state =
    not_impl "Unsupported intrinsic: round_ties_even_f16"

  let round_ties_even_f32 ~x:_ () _state =
    not_impl "Unsupported intrinsic: round_ties_even_f32"

  let round_ties_even_f64 ~x:_ () _state =
    not_impl "Unsupported intrinsic: round_ties_even_f64"

  let roundf128 ~x:_ () _state = not_impl "Unsupported intrinsic: roundf128"
  let roundf16 ~x:_ () _state = not_impl "Unsupported intrinsic: roundf16"
  let roundf32 ~x:_ () _state = not_impl "Unsupported intrinsic: roundf32"
  let roundf64 ~x:_ () _state = not_impl "Unsupported intrinsic: roundf64"

  let rustc_peek ~t:_ ~arg:_ () _state =
    not_impl "Unsupported intrinsic: rustc_peek"

  let saturating_add ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: saturating_add"

  let saturating_sub ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: saturating_sub"

  let select_unpredictable ~t:_ ~b:_ ~true_val:_ ~false_val:_ () _state =
    not_impl "Unsupported intrinsic: select_unpredictable"

  let sinf128 ~x:_ () _state = not_impl "Unsupported intrinsic: sinf128"
  let sinf16 ~x:_ () _state = not_impl "Unsupported intrinsic: sinf16"
  let sinf32 ~x:_ () _state = not_impl "Unsupported intrinsic: sinf32"
  let sinf64 ~x:_ () _state = not_impl "Unsupported intrinsic: sinf64"
  let size_of ~t:_ () _state = not_impl "Unsupported intrinsic: size_of"

  let size_of_val ~t:_ ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: size_of_val"

  let slice_get_unchecked ~itemptr:_ ~sliceptr:_ ~t:_ ~slice_ptr:_ ~index:_ ()
      _state =
    not_impl "Unsupported intrinsic: slice_get_unchecked"

  let sqrtf128 ~x:_ () _state = not_impl "Unsupported intrinsic: sqrtf128"
  let sqrtf16 ~x:_ () _state = not_impl "Unsupported intrinsic: sqrtf16"
  let sqrtf32 ~x:_ () _state = not_impl "Unsupported intrinsic: sqrtf32"
  let sqrtf64 ~x:_ () _state = not_impl "Unsupported intrinsic: sqrtf64"

  let sub_with_overflow ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: sub_with_overflow"

  let three_way_compare ~t:_ ~lhs:_ ~rhss:_ () _state =
    not_impl "Unsupported intrinsic: three_way_compare"

  let transmute ~t_src:_ ~dst:_ ~src:_ () _state =
    not_impl "Unsupported intrinsic: transmute"

  let transmute_unchecked ~t_src:_ ~dst:_ ~src:_ () _state =
    not_impl "Unsupported intrinsic: transmute_unchecked"

  let truncf128 ~x:_ () _state = not_impl "Unsupported intrinsic: truncf128"
  let truncf16 ~x:_ () _state = not_impl "Unsupported intrinsic: truncf16"
  let truncf32 ~x:_ () _state = not_impl "Unsupported intrinsic: truncf32"
  let truncf64 ~x:_ () _state = not_impl "Unsupported intrinsic: truncf64"
  let type_id ~t:_ () _state = not_impl "Unsupported intrinsic: type_id"

  let type_id_eq ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: type_id_eq"

  let type_name ~t:_ () _state = not_impl "Unsupported intrinsic: type_name"

  let typed_swap_nonoverlapping ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: typed_swap_nonoverlapping"

  let ub_checks () _state = not_impl "Unsupported intrinsic: ub_checks"

  let unaligned_volatile_load ~t:_ ~src:_ () _state =
    not_impl "Unsupported intrinsic: unaligned_volatile_load"

  let unaligned_volatile_store ~t:_ ~dst:_ ~val_:_ () _state =
    not_impl "Unsupported intrinsic: unaligned_volatile_store"

  let unchecked_add ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_add"

  let unchecked_div ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_div"

  let unchecked_funnel_shl ~t:_ ~a:_ ~b:_ ~shift:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_funnel_shl"

  let unchecked_funnel_shr ~t:_ ~a:_ ~b:_ ~shift:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_funnel_shr"

  let unchecked_mul ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_mul"

  let unchecked_rem ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_rem"

  let unchecked_shl ~t:_ ~u:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_shl"

  let unchecked_shr ~t:_ ~u:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_shr"

  let unchecked_sub ~t:_ ~x:_ ~y:_ () _state =
    not_impl "Unsupported intrinsic: unchecked_sub"

  let unlikely ~b:_ () _state = not_impl "Unsupported intrinsic: unlikely"
  let unreachable () _state = not_impl "Unsupported intrinsic: unreachable"
  let va_arg ~t:_ ~ap:_ () _state = not_impl "Unsupported intrinsic: va_arg"

  let va_copy ~dest:_ ~src:_ () _state =
    not_impl "Unsupported intrinsic: va_copy"

  let va_end ~ap:_ () _state = not_impl "Unsupported intrinsic: va_end"

  let variant_count ~t:_ () _state =
    not_impl "Unsupported intrinsic: variant_count"

  let volatile_copy_memory ~t:_ ~dst:_ ~src:_ ~count:_ () _state =
    not_impl "Unsupported intrinsic: volatile_copy_memory"

  let volatile_copy_nonoverlapping_memory ~t:_ ~dst:_ ~src:_ ~count:_ () _state
      =
    not_impl "Unsupported intrinsic: volatile_copy_nonoverlapping_memory"

  let volatile_load ~t:_ ~src:_ () _state =
    not_impl "Unsupported intrinsic: volatile_load"

  let volatile_set_memory ~t:_ ~dst:_ ~val_:_ ~count:_ () _state =
    not_impl "Unsupported intrinsic: volatile_set_memory"

  let volatile_store ~t:_ ~dst:_ ~val_:_ () _state =
    not_impl "Unsupported intrinsic: volatile_store"

  let vtable_align ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: vtable_align"

  let vtable_size ~ptr:_ () _state =
    not_impl "Unsupported intrinsic: vtable_size"

  let wrapping_add ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: wrapping_add"

  let wrapping_mul ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: wrapping_mul"

  let wrapping_sub ~t:_ ~a:_ ~b:_ () _state =
    not_impl "Unsupported intrinsic: wrapping_sub"

  let write_bytes ~t:_ ~dst:_ ~val_:_ ~count:_ () _state =
    not_impl "Unsupported intrinsic: write_bytes"

  let write_via_move ~t:_ ~ptr:_ ~value:_ () _state =
    not_impl "Unsupported intrinsic: write_via_move"

  include Intrinsics_impl.M (State)

  let eval_fun name fun_exec (generics : Charon.Types.generic_args) args =
    match (name, generics.types, args) with
    | "abort", [], [] ->
        let+ () = abort in
        Tuple []
    | "add_with_overflow", [ t ], [ x; y ] -> add_with_overflow ~t ~x ~y
    | "aggregate_raw_ptr", [ p; d; m ], [ data; meta ] ->
        aggregate_raw_ptr ~p ~d ~m ~data ~meta
    | "align_of", [ t ], [] ->
        let+ ret = align_of ~t in
        Int ret
    | "align_of_val", [ t ], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = align_of_val ~t ~ptr in
        Int ret
    | "arith_offset", [ t ], [ dst; offset ] ->
        let dst = as_ptr dst in
        let offset = as_base_i Usize offset in
        let+ ret = arith_offset ~t ~dst ~offset in
        Ptr ret
    | "assert_inhabited", [ t ], [] ->
        let+ () = assert_inhabited ~t in
        Tuple []
    | "assert_mem_uninitialized_valid", [ t ], [] ->
        let+ () = assert_mem_uninitialized_valid ~t in
        Tuple []
    | "assert_zero_valid", [ t ], [] ->
        let+ () = assert_zero_valid ~t in
        Tuple []
    | "assume", [], [ b ] ->
        let b = Typed.BitVec.to_bool (as_base TBool b) in
        let+ () = assume ~b in
        Tuple []
    | "autodiff", [ t_f; g; t; r ], [ f; df; args ] ->
        autodiff ~t_f ~g ~t ~r ~f ~df ~args
    | "bitreverse", [ t ], [ x ] -> bitreverse ~t ~x
    | "black_box", [ t ], [ dummy ] -> black_box ~t ~dummy
    | "breakpoint", [], [] ->
        let+ () = breakpoint in
        Tuple []
    | "bswap", [ t ], [ x ] -> bswap ~t ~x
    | "caller_location", [], [] ->
        let+ ret = caller_location in
        Ptr ret
    | "carrying_mul_add", [ t; u ], [ multiplier; multiplicand; addend; carry ]
      ->
        carrying_mul_add ~t ~u ~multiplier ~multiplicand ~addend ~carry
    | "catch_unwind", [], [ _try_fn; _data; _catch_fn ] ->
        let _data = as_ptr _data in
        let+ ret = catch_unwind fun_exec ~_try_fn ~_data ~_catch_fn in
        Int ret
    | "ceilf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = ceilf128 ~x in
        Float ret
    | "ceilf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = ceilf16 ~x in
        Float ret
    | "ceilf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = ceilf32 ~x in
        Float ret
    | "ceilf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = ceilf64 ~x in
        Float ret
    | "cold_path", [], [] ->
        let+ () = cold_path in
        Tuple []
    | "compare_bytes", [], [ left; right; bytes ] ->
        let left = as_ptr left in
        let right = as_ptr right in
        let bytes = as_base_i Usize bytes in
        let+ ret = compare_bytes ~left ~right ~bytes in
        Int ret
    | "const_deallocate", [], [ _ptr; _size; _align ] ->
        let _ptr = as_ptr _ptr in
        let _size = as_base_i Usize _size in
        let _align = as_base_i Usize _align in
        let+ () = const_deallocate ~_ptr ~_size ~_align in
        Tuple []
    | ( "const_eval_select",
        [ arg; f; g; ret ],
        [ _arg; _called_in_const; _called_at_rt ] ) ->
        const_eval_select ~arg ~f ~g ~ret ~_arg ~_called_in_const ~_called_at_rt
    | "const_make_global", [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = const_make_global ~ptr in
        Ptr ret
    | "contract_check_ensures", [ c; t_ret ], [ cond; ret ] ->
        contract_check_ensures ~c ~t_ret ~cond ~ret
    | "contract_check_requires", [ c ], [ cond ] ->
        let+ () = contract_check_requires ~c ~cond in
        Tuple []
    | "copy", [ t ], [ src; dst; count ] ->
        let src = as_ptr src in
        let dst = as_ptr dst in
        let count = as_base_i Usize count in
        let+ () = copy ~t ~src ~dst ~count in
        Tuple []
    | "copy_nonoverlapping", [ t ], [ src; dst; count ] ->
        let src = as_ptr src in
        let dst = as_ptr dst in
        let count = as_base_i Usize count in
        let+ () = copy_nonoverlapping ~t ~src ~dst ~count in
        Tuple []
    | "copysignf128", [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = copysignf128 ~x ~y in
        Float ret
    | "copysignf16", [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = copysignf16 ~x ~y in
        Float ret
    | "copysignf32", [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = copysignf32 ~x ~y in
        Float ret
    | "copysignf64", [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = copysignf64 ~x ~y in
        Float ret
    | "cosf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = cosf128 ~x in
        Float ret
    | "cosf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = cosf16 ~x in
        Float ret
    | "cosf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = cosf32 ~x in
        Float ret
    | "cosf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = cosf64 ~x in
        Float ret
    | "ctlz", [ t ], [ x ] ->
        let+ ret = ctlz ~t ~x in
        Int ret
    | "ctlz_nonzero", [ t ], [ x ] ->
        let+ ret = ctlz_nonzero ~t ~x in
        Int ret
    | "ctpop", [ t ], [ x ] ->
        let+ ret = ctpop ~t ~x in
        Int ret
    | "cttz", [ t ], [ x ] ->
        let+ ret = cttz ~t ~x in
        Int ret
    | "cttz_nonzero", [ t ], [ x ] ->
        let+ ret = cttz_nonzero ~t ~x in
        Int ret
    | "discriminant_value", [ t ], [ v ] ->
        let v = as_ptr v in
        discriminant_value ~t ~v
    | "disjoint_bitor", [ t ], [ a; b ] -> disjoint_bitor ~t ~a ~b
    | "exact_div", [ t ], [ x; y ] -> exact_div ~t ~x ~y
    | "exp2f128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = exp2f128 ~x in
        Float ret
    | "exp2f16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = exp2f16 ~x in
        Float ret
    | "exp2f32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = exp2f32 ~x in
        Float ret
    | "exp2f64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = exp2f64 ~x in
        Float ret
    | "expf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = expf128 ~x in
        Float ret
    | "expf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = expf16 ~x in
        Float ret
    | "expf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = expf32 ~x in
        Float ret
    | "expf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = expf64 ~x in
        Float ret
    | "fabsf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = fabsf128 ~x in
        Float ret
    | "fabsf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = fabsf16 ~x in
        Float ret
    | "fabsf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = fabsf32 ~x in
        Float ret
    | "fabsf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = fabsf64 ~x in
        Float ret
    | "fadd_algebraic", [ t ], [ a; b ] -> fadd_algebraic ~t ~a ~b
    | "fadd_fast", [ t ], [ a; b ] -> fadd_fast ~t ~a ~b
    | "fdiv_algebraic", [ t ], [ a; b ] -> fdiv_algebraic ~t ~a ~b
    | "fdiv_fast", [ t ], [ a; b ] -> fdiv_fast ~t ~a ~b
    | "float_to_int_unchecked", [ float; int ], [ value ] ->
        float_to_int_unchecked ~float ~int ~value
    | "floorf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = floorf128 ~x in
        Float ret
    | "floorf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = floorf16 ~x in
        Float ret
    | "floorf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = floorf32 ~x in
        Float ret
    | "floorf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = floorf64 ~x in
        Float ret
    | "fmaf128", [], [ a; b; c ] ->
        let a = as_base_f F128 a in
        let b = as_base_f F128 b in
        let c = as_base_f F128 c in
        let+ ret = fmaf128 ~a ~b ~c in
        Float ret
    | "fmaf16", [], [ a; b; c ] ->
        let a = as_base_f F16 a in
        let b = as_base_f F16 b in
        let c = as_base_f F16 c in
        let+ ret = fmaf16 ~a ~b ~c in
        Float ret
    | "fmaf32", [], [ a; b; c ] ->
        let a = as_base_f F32 a in
        let b = as_base_f F32 b in
        let c = as_base_f F32 c in
        let+ ret = fmaf32 ~a ~b ~c in
        Float ret
    | "fmaf64", [], [ a; b; c ] ->
        let a = as_base_f F64 a in
        let b = as_base_f F64 b in
        let c = as_base_f F64 c in
        let+ ret = fmaf64 ~a ~b ~c in
        Float ret
    | "fmul_algebraic", [ t ], [ a; b ] -> fmul_algebraic ~t ~a ~b
    | "fmul_fast", [ t ], [ a; b ] -> fmul_fast ~t ~a ~b
    | "fmuladdf128", [], [ a; b; c ] ->
        let a = as_base_f F128 a in
        let b = as_base_f F128 b in
        let c = as_base_f F128 c in
        let+ ret = fmuladdf128 ~a ~b ~c in
        Float ret
    | "fmuladdf16", [], [ a; b; c ] ->
        let a = as_base_f F16 a in
        let b = as_base_f F16 b in
        let c = as_base_f F16 c in
        let+ ret = fmuladdf16 ~a ~b ~c in
        Float ret
    | "fmuladdf32", [], [ a; b; c ] ->
        let a = as_base_f F32 a in
        let b = as_base_f F32 b in
        let c = as_base_f F32 c in
        let+ ret = fmuladdf32 ~a ~b ~c in
        Float ret
    | "fmuladdf64", [], [ a; b; c ] ->
        let a = as_base_f F64 a in
        let b = as_base_f F64 b in
        let c = as_base_f F64 c in
        let+ ret = fmuladdf64 ~a ~b ~c in
        Float ret
    | "forget", [ t ], [ arg ] ->
        let+ () = forget ~t ~arg in
        Tuple []
    | "frem_algebraic", [ t ], [ a; b ] -> frem_algebraic ~t ~a ~b
    | "frem_fast", [ t ], [ a; b ] -> frem_fast ~t ~a ~b
    | "fsub_algebraic", [ t ], [ a; b ] -> fsub_algebraic ~t ~a ~b
    | "fsub_fast", [ t ], [ a; b ] -> fsub_fast ~t ~a ~b
    | "is_val_statically_known", [ t ], [ _arg ] ->
        let+ ret = is_val_statically_known ~t ~_arg in
        Int (Typed.BitVec.of_bool ret)
    | "likely", [], [ b ] ->
        let b = Typed.BitVec.to_bool (as_base TBool b) in
        let+ ret = likely ~b in
        Int (Typed.BitVec.of_bool ret)
    | "log10f128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = log10f128 ~x in
        Float ret
    | "log10f16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = log10f16 ~x in
        Float ret
    | "log10f32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = log10f32 ~x in
        Float ret
    | "log10f64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = log10f64 ~x in
        Float ret
    | "log2f128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = log2f128 ~x in
        Float ret
    | "log2f16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = log2f16 ~x in
        Float ret
    | "log2f32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = log2f32 ~x in
        Float ret
    | "log2f64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = log2f64 ~x in
        Float ret
    | "logf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = logf128 ~x in
        Float ret
    | "logf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = logf16 ~x in
        Float ret
    | "logf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = logf32 ~x in
        Float ret
    | "logf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = logf64 ~x in
        Float ret
    | "maximumf128", [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = maximumf128 ~x ~y in
        Float ret
    | "maximumf16", [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = maximumf16 ~x ~y in
        Float ret
    | "maximumf32", [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = maximumf32 ~x ~y in
        Float ret
    | "maximumf64", [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = maximumf64 ~x ~y in
        Float ret
    | "maxnumf128", [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = maxnumf128 ~x ~y in
        Float ret
    | "maxnumf16", [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = maxnumf16 ~x ~y in
        Float ret
    | "maxnumf32", [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = maxnumf32 ~x ~y in
        Float ret
    | "maxnumf64", [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = maxnumf64 ~x ~y in
        Float ret
    | "minimumf128", [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = minimumf128 ~x ~y in
        Float ret
    | "minimumf16", [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = minimumf16 ~x ~y in
        Float ret
    | "minimumf32", [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = minimumf32 ~x ~y in
        Float ret
    | "minimumf64", [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = minimumf64 ~x ~y in
        Float ret
    | "minnumf128", [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = minnumf128 ~x ~y in
        Float ret
    | "minnumf16", [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = minnumf16 ~x ~y in
        Float ret
    | "minnumf32", [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = minnumf32 ~x ~y in
        Float ret
    | "minnumf64", [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = minnumf64 ~x ~y in
        Float ret
    | "mul_with_overflow", [ t ], [ x; y ] -> mul_with_overflow ~t ~x ~y
    | "needs_drop", [ t ], [] ->
        let+ ret = needs_drop ~t in
        Int (Typed.BitVec.of_bool ret)
    | "nontemporal_store", [ t ], [ ptr; val_ ] ->
        let ptr = as_ptr ptr in
        let+ () = nontemporal_store ~t ~ptr ~val_ in
        Tuple []
    | "offset", [ ptr; delta ], [ dst; offset_ ] ->
        offset ~ptr ~delta ~dst ~offset:offset_
    | "offset_of", [ t ], [ variant; field ] ->
        let variant = as_base_i U32 variant in
        let field = as_base_i U32 field in
        let+ ret = offset_of ~t ~variant ~field in
        Int ret
    | "overflow_checks", [], [] ->
        let+ ret = overflow_checks in
        Int (Typed.BitVec.of_bool ret)
    | "powf128", [], [ a; x ] ->
        let a = as_base_f F128 a in
        let x = as_base_f F128 x in
        let+ ret = powf128 ~a ~x in
        Float ret
    | "powf16", [], [ a; x ] ->
        let a = as_base_f F16 a in
        let x = as_base_f F16 x in
        let+ ret = powf16 ~a ~x in
        Float ret
    | "powf32", [], [ a; x ] ->
        let a = as_base_f F32 a in
        let x = as_base_f F32 x in
        let+ ret = powf32 ~a ~x in
        Float ret
    | "powf64", [], [ a; x ] ->
        let a = as_base_f F64 a in
        let x = as_base_f F64 x in
        let+ ret = powf64 ~a ~x in
        Float ret
    | "powif128", [], [ a; x ] ->
        let a = as_base_f F128 a in
        let x = as_base_i U32 x in
        let+ ret = powif128 ~a ~x in
        Float ret
    | "powif16", [], [ a; x ] ->
        let a = as_base_f F16 a in
        let x = as_base_i U32 x in
        let+ ret = powif16 ~a ~x in
        Float ret
    | "powif32", [], [ a; x ] ->
        let a = as_base_f F32 a in
        let x = as_base_i U32 x in
        let+ ret = powif32 ~a ~x in
        Float ret
    | "powif64", [], [ a; x ] ->
        let a = as_base_f F64 a in
        let x = as_base_i U32 x in
        let+ ret = powif64 ~a ~x in
        Float ret
    | "prefetch_read_data", [ t ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_read_data ~t ~data in
        Tuple []
    | "prefetch_read_instruction", [ t ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_read_instruction ~t ~data in
        Tuple []
    | "prefetch_write_data", [ t ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_write_data ~t ~data in
        Tuple []
    | "prefetch_write_instruction", [ t ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_write_instruction ~t ~data in
        Tuple []
    | "ptr_guaranteed_cmp", [ t ], [ ptr; other ] ->
        let ptr = as_ptr ptr in
        let other = as_ptr other in
        let+ ret = ptr_guaranteed_cmp ~t ~ptr ~other in
        Int ret
    | "ptr_mask", [ t ], [ ptr; mask ] ->
        let ptr = as_ptr ptr in
        let mask = as_base_i Usize mask in
        let+ ret = ptr_mask ~t ~ptr ~mask in
        Ptr ret
    | "ptr_metadata", [ p; m ], [ ptr ] ->
        let ptr = as_ptr ptr in
        ptr_metadata ~p ~m ~ptr
    | "ptr_offset_from", [ t ], [ ptr; base ] ->
        let ptr = as_ptr ptr in
        let base = as_ptr base in
        let+ ret = ptr_offset_from ~t ~ptr ~base in
        Int ret
    | "ptr_offset_from_unsigned", [ t ], [ ptr; base ] ->
        let ptr = as_ptr ptr in
        let base = as_ptr base in
        let+ ret = ptr_offset_from_unsigned ~t ~ptr ~base in
        Int ret
    | "raw_eq", [ t ], [ a; b ] ->
        let a = as_ptr a in
        let b = as_ptr b in
        let+ ret = raw_eq ~t ~a ~b in
        Int (Typed.BitVec.of_bool ret)
    | "read_via_copy", [ t ], [ ptr ] ->
        let ptr = as_ptr ptr in
        read_via_copy ~t ~ptr
    | "rotate_left", [ t ], [ x; shift ] ->
        let shift = as_base_i U32 shift in
        rotate_left ~t ~x ~shift
    | "rotate_right", [ t ], [ x; shift ] ->
        let shift = as_base_i U32 shift in
        rotate_right ~t ~x ~shift
    | "round_ties_even_f128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = round_ties_even_f128 ~x in
        Float ret
    | "round_ties_even_f16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = round_ties_even_f16 ~x in
        Float ret
    | "round_ties_even_f32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = round_ties_even_f32 ~x in
        Float ret
    | "round_ties_even_f64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = round_ties_even_f64 ~x in
        Float ret
    | "roundf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = roundf128 ~x in
        Float ret
    | "roundf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = roundf16 ~x in
        Float ret
    | "roundf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = roundf32 ~x in
        Float ret
    | "roundf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = roundf64 ~x in
        Float ret
    | "rustc_peek", [ t ], [ arg ] -> rustc_peek ~t ~arg
    | "saturating_add", [ t ], [ a; b ] -> saturating_add ~t ~a ~b
    | "saturating_sub", [ t ], [ a; b ] -> saturating_sub ~t ~a ~b
    | "select_unpredictable", [ t ], [ b; true_val; false_val ] ->
        let b = Typed.BitVec.to_bool (as_base TBool b) in
        select_unpredictable ~t ~b ~true_val ~false_val
    | "sinf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = sinf128 ~x in
        Float ret
    | "sinf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = sinf16 ~x in
        Float ret
    | "sinf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = sinf32 ~x in
        Float ret
    | "sinf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = sinf64 ~x in
        Float ret
    | "size_of", [ t ], [] ->
        let+ ret = size_of ~t in
        Int ret
    | "size_of_val", [ t ], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = size_of_val ~t ~ptr in
        Int ret
    | "slice_get_unchecked", [ itemptr; sliceptr; t ], [ slice_ptr; index ] ->
        let index = as_base_i Usize index in
        slice_get_unchecked ~itemptr ~sliceptr ~t ~slice_ptr ~index
    | "sqrtf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = sqrtf128 ~x in
        Float ret
    | "sqrtf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = sqrtf16 ~x in
        Float ret
    | "sqrtf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = sqrtf32 ~x in
        Float ret
    | "sqrtf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = sqrtf64 ~x in
        Float ret
    | "sub_with_overflow", [ t ], [ x; y ] -> sub_with_overflow ~t ~x ~y
    | "three_way_compare", [ t ], [ lhs; rhss ] ->
        three_way_compare ~t ~lhs ~rhss
    | "transmute", [ t_src; dst ], [ src ] -> transmute ~t_src ~dst ~src
    | "transmute_unchecked", [ t_src; dst ], [ src ] ->
        transmute_unchecked ~t_src ~dst ~src
    | "truncf128", [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = truncf128 ~x in
        Float ret
    | "truncf16", [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = truncf16 ~x in
        Float ret
    | "truncf32", [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = truncf32 ~x in
        Float ret
    | "truncf64", [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = truncf64 ~x in
        Float ret
    | "type_id", [ t ], [] -> type_id ~t
    | "type_id_eq", [], [ a; b ] ->
        let+ ret = type_id_eq ~a ~b in
        Int (Typed.BitVec.of_bool ret)
    | "type_name", [ t ], [] ->
        let+ ret = type_name ~t in
        Ptr ret
    | "typed_swap_nonoverlapping", [ t ], [ x; y ] ->
        let x = as_ptr x in
        let y = as_ptr y in
        let+ () = typed_swap_nonoverlapping ~t ~x ~y in
        Tuple []
    | "ub_checks", [], [] ->
        let+ ret = ub_checks in
        Int (Typed.BitVec.of_bool ret)
    | "unaligned_volatile_load", [ t ], [ src ] ->
        let src = as_ptr src in
        unaligned_volatile_load ~t ~src
    | "unaligned_volatile_store", [ t ], [ dst; val_ ] ->
        let dst = as_ptr dst in
        let+ () = unaligned_volatile_store ~t ~dst ~val_ in
        Tuple []
    | "unchecked_add", [ t ], [ x; y ] -> unchecked_add ~t ~x ~y
    | "unchecked_div", [ t ], [ x; y ] -> unchecked_div ~t ~x ~y
    | "unchecked_funnel_shl", [ t ], [ a; b; shift ] ->
        let shift = as_base_i U32 shift in
        unchecked_funnel_shl ~t ~a ~b ~shift
    | "unchecked_funnel_shr", [ t ], [ a; b; shift ] ->
        let shift = as_base_i U32 shift in
        unchecked_funnel_shr ~t ~a ~b ~shift
    | "unchecked_mul", [ t ], [ x; y ] -> unchecked_mul ~t ~x ~y
    | "unchecked_rem", [ t ], [ x; y ] -> unchecked_rem ~t ~x ~y
    | "unchecked_shl", [ t; u ], [ x; y ] -> unchecked_shl ~t ~u ~x ~y
    | "unchecked_shr", [ t; u ], [ x; y ] -> unchecked_shr ~t ~u ~x ~y
    | "unchecked_sub", [ t ], [ x; y ] -> unchecked_sub ~t ~x ~y
    | "unlikely", [], [ b ] ->
        let b = Typed.BitVec.to_bool (as_base TBool b) in
        let+ ret = unlikely ~b in
        Int (Typed.BitVec.of_bool ret)
    | "unreachable", [], [] ->
        let+ () = unreachable in
        Tuple []
    | "va_arg", [ t ], [ ap ] ->
        let ap = as_ptr ap in
        va_arg ~t ~ap
    | "va_copy", [], [ dest; src ] ->
        let dest = as_ptr dest in
        let src = as_ptr src in
        let+ () = va_copy ~dest ~src in
        Tuple []
    | "va_end", [], [ ap ] ->
        let ap = as_ptr ap in
        let+ () = va_end ~ap in
        Tuple []
    | "variant_count", [ t ], [] ->
        let+ ret = variant_count ~t in
        Int ret
    | "volatile_copy_memory", [ t ], [ dst; src; count ] ->
        let dst = as_ptr dst in
        let src = as_ptr src in
        let count = as_base_i Usize count in
        let+ () = volatile_copy_memory ~t ~dst ~src ~count in
        Tuple []
    | "volatile_copy_nonoverlapping_memory", [ t ], [ dst; src; count ] ->
        let dst = as_ptr dst in
        let src = as_ptr src in
        let count = as_base_i Usize count in
        let+ () = volatile_copy_nonoverlapping_memory ~t ~dst ~src ~count in
        Tuple []
    | "volatile_load", [ t ], [ src ] ->
        let src = as_ptr src in
        volatile_load ~t ~src
    | "volatile_set_memory", [ t ], [ dst; val_; count ] ->
        let dst = as_ptr dst in
        let val_ = as_base_i U8 val_ in
        let count = as_base_i Usize count in
        let+ () = volatile_set_memory ~t ~dst ~val_ ~count in
        Tuple []
    | "volatile_store", [ t ], [ dst; val_ ] ->
        let dst = as_ptr dst in
        let+ () = volatile_store ~t ~dst ~val_ in
        Tuple []
    | "vtable_align", [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = vtable_align ~ptr in
        Int ret
    | "vtable_size", [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = vtable_size ~ptr in
        Int ret
    | "wrapping_add", [ t ], [ a; b ] -> wrapping_add ~t ~a ~b
    | "wrapping_mul", [ t ], [ a; b ] -> wrapping_mul ~t ~a ~b
    | "wrapping_sub", [ t ], [ a; b ] -> wrapping_sub ~t ~a ~b
    | "write_bytes", [ t ], [ dst; val_; count ] ->
        let dst = as_ptr dst in
        let val_ = as_base_i U8 val_ in
        let count = as_base_i Usize count in
        let+ () = write_bytes ~t ~dst ~val_ ~count in
        Tuple []
    | "write_via_move", [ t ], [ ptr; value ] ->
        let ptr = as_ptr ptr in
        let+ () = write_via_move ~t ~ptr ~value in
        Tuple []
    | name, _, _ ->
        fun () _ ->
          Fmt.kstr not_impl
            "Intrinsic %s not found, or not called with the right arguments"
            name
end
