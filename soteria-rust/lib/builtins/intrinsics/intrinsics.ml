(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

open Rust_val
open Common

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  let as_ptr (v : rust_val) = Rust_val.as_ptr v
  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  include Impl.M (StateM)

  let[@inline] eval_fun name fun_exec (generics : Charon.Types.generic_args)
      args =
    match (name, generics.types, generics.const_generics, args) with
    | "abort", [], [], [] ->
        let+ () = abort () in
        mk_tuple []
    | "add_with_overflow", [ t ], [], [ x; y ] -> add_with_overflow ~t ~x ~y
    | "aggregate_raw_ptr", [ p; d; m ], [], [ data; meta ] ->
        aggregate_raw_ptr ~p ~d ~m ~data ~meta
    | "align_of", [ t ], [], [] ->
        let+ ret = align_of ~t in
        mk_int ret
    | "align_of_val", [ t ], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = align_of_val ~t ~ptr in
        mk_int ret
    | "arith_offset", [ t ], [], [ dst; offset ] ->
        let dst = as_ptr dst in
        let offset = as_base_i Usize offset in
        let+ ret = arith_offset ~t ~dst ~offset in
        mk_ptr (fst ret) (snd ret)
    | "assert_inhabited", [ t ], [], [] ->
        let+ () = assert_inhabited ~t in
        mk_tuple []
    | "assert_mem_uninitialized_valid", [ t ], [], [] ->
        let+ () = assert_mem_uninitialized_valid ~t in
        mk_tuple []
    | "assert_zero_valid", [ t ], [], [] ->
        let+ () = assert_zero_valid ~t in
        mk_tuple []
    | "assume", [], [], [ b ] ->
        let b = Typed.BitVec.to_bool (as_base TBool b) in
        let+ () = assume ~b in
        mk_tuple []
    | "atomic_and", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_and ~t ~u ~ord ~dst ~src
    | "atomic_cxchg", [ t ], [ ord_succ; ord_fail ], [ dst; old; src ] ->
        let dst = as_ptr dst in
        atomic_cxchg ~t ~ord_succ ~ord_fail ~dst ~old ~src
    | "atomic_cxchgweak", [ t ], [ ord_succ; ord_fail ], [ dst; old; src ] ->
        let dst = as_ptr dst in
        atomic_cxchgweak ~t ~ord_succ ~ord_fail ~dst ~old ~src
    | "atomic_fence", [], [ ord ], [] ->
        let+ () = atomic_fence ~ord in
        mk_tuple []
    | "atomic_load", [ t ], [ ord ], [ src ] ->
        let src = as_ptr src in
        atomic_load ~t ~ord ~src
    | "atomic_max", [ t ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_max ~t ~ord ~dst ~src
    | "atomic_min", [ t ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_min ~t ~ord ~dst ~src
    | "atomic_nand", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_nand ~t ~u ~ord ~dst ~src
    | "atomic_or", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_or ~t ~u ~ord ~dst ~src
    | "atomic_singlethreadfence", [], [ ord ], [] ->
        let+ () = atomic_singlethreadfence ~ord in
        mk_tuple []
    | "atomic_store", [ t ], [ ord ], [ dst; val_ ] ->
        let dst = as_ptr dst in
        let+ () = atomic_store ~t ~ord ~dst ~val_ in
        mk_tuple []
    | "atomic_umax", [ t ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_umax ~t ~ord ~dst ~src
    | "atomic_umin", [ t ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_umin ~t ~ord ~dst ~src
    | "atomic_xadd", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_xadd ~t ~u ~ord ~dst ~src
    | "atomic_xchg", [ t ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_xchg ~t ~ord ~dst ~src
    | "atomic_xor", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_xor ~t ~u ~ord ~dst ~src
    | "atomic_xsub", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = as_ptr dst in
        atomic_xsub ~t ~u ~ord ~dst ~src
    | "autodiff", [ t_f; g; t; r ], [], [ f; df; args ] ->
        autodiff ~t_f ~g ~t ~r ~f ~df ~args
    | "bitreverse", [ t ], [], [ x ] -> bitreverse ~t ~x
    | "black_box", [ t ], [], [ dummy ] -> black_box ~t ~dummy
    | "breakpoint", [], [], [] ->
        let+ () = breakpoint () in
        mk_tuple []
    | "bswap", [ t ], [], [ x ] -> bswap ~t ~x
    | "caller_location", [], [], [] ->
        let+ ret = caller_location () in
        mk_ptr (fst ret) (snd ret)
    | ( "carrying_mul_add",
        [ t; u ],
        [],
        [ multiplier; multiplicand; addend; carry ] ) ->
        carrying_mul_add ~t ~u ~multiplier ~multiplicand ~addend ~carry
    | "carryless_mul", [ t ], [], [ a; b ] -> carryless_mul ~t ~a ~b
    | "catch_unwind", [ t_data ], [], [ try_fn; data; catch_fn ] ->
        let try_fn = as_ptr try_fn in
        let data = as_ptr data in
        let catch_fn = as_ptr catch_fn in
        let+ ret = catch_unwind ~fun_exec ~t_data ~try_fn ~data ~catch_fn in
        mk_int (Typed.BitVec.of_bool ret)
    | "ceilf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = ceilf128 ~x in
        mk_float ret
    | "ceilf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = ceilf16 ~x in
        mk_float ret
    | "ceilf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = ceilf32 ~x in
        mk_float ret
    | "ceilf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = ceilf64 ~x in
        mk_float ret
    | "cold_path", [], [], [] ->
        let+ () = cold_path () in
        mk_tuple []
    | "compare_bytes", [], [], [ left; right; bytes ] ->
        let left = as_ptr left in
        let right = as_ptr right in
        let bytes = as_base_i Usize bytes in
        let+ ret = compare_bytes ~left ~right ~bytes in
        mk_int ret
    | "const_deallocate", [], [], [ ptr; size; align ] ->
        let ptr = as_ptr ptr in
        let size = as_base_i Usize size in
        let align = as_base_i Usize align in
        let+ () = const_deallocate ~ptr ~size ~align in
        mk_tuple []
    | ( "const_eval_select",
        [ t_arg; f; g; ret ],
        [],
        [ arg; called_in_const; called_at_rt ] ) ->
        const_eval_select ~t_arg ~f ~g ~ret ~arg ~called_in_const ~called_at_rt
    | "const_make_global", [], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = const_make_global ~ptr in
        mk_ptr (fst ret) (snd ret)
    | "contract_check_ensures", [ c; t_ret ], [], [ cond; ret ] ->
        contract_check_ensures ~c ~t_ret ~cond ~ret
    | "contract_check_requires", [ c ], [], [ cond ] ->
        let+ () = contract_check_requires ~c ~cond in
        mk_tuple []
    | "copy", [ t ], [], [ src; dst; count ] ->
        let src = as_ptr src in
        let dst = as_ptr dst in
        let count = as_base_i Usize count in
        let+ () = copy ~t ~src ~dst ~count in
        mk_tuple []
    | "copy_nonoverlapping", [ t ], [], [ src; dst; count ] ->
        let src = as_ptr src in
        let dst = as_ptr dst in
        let count = as_base_i Usize count in
        let+ () = copy_nonoverlapping ~t ~src ~dst ~count in
        mk_tuple []
    | "copysignf128", [], [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = copysignf128 ~x ~y in
        mk_float ret
    | "copysignf16", [], [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = copysignf16 ~x ~y in
        mk_float ret
    | "copysignf32", [], [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = copysignf32 ~x ~y in
        mk_float ret
    | "copysignf64", [], [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = copysignf64 ~x ~y in
        mk_float ret
    | "cosf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = cosf128 ~x in
        mk_float ret
    | "cosf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = cosf16 ~x in
        mk_float ret
    | "cosf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = cosf32 ~x in
        mk_float ret
    | "cosf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = cosf64 ~x in
        mk_float ret
    | "ctlz", [ t ], [], [ x ] ->
        let+ ret = ctlz ~t ~x in
        mk_int ret
    | "ctlz_nonzero", [ t ], [], [ x ] ->
        let+ ret = ctlz_nonzero ~t ~x in
        mk_int ret
    | "ctpop", [ t ], [], [ x ] ->
        let+ ret = ctpop ~t ~x in
        mk_int ret
    | "cttz", [ t ], [], [ x ] ->
        let+ ret = cttz ~t ~x in
        mk_int ret
    | "cttz_nonzero", [ t ], [], [ x ] ->
        let+ ret = cttz_nonzero ~t ~x in
        mk_int ret
    | "discriminant_value", [ t ], [], [ v ] ->
        let v = as_ptr v in
        discriminant_value ~t ~v
    | "disjoint_bitor", [ t ], [], [ a; b ] -> disjoint_bitor ~t ~a ~b
    | "exact_div", [ t ], [], [ x; y ] -> exact_div ~t ~x ~y
    | "exp2f128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = exp2f128 ~x in
        mk_float ret
    | "exp2f16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = exp2f16 ~x in
        mk_float ret
    | "exp2f32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = exp2f32 ~x in
        mk_float ret
    | "exp2f64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = exp2f64 ~x in
        mk_float ret
    | "expf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = expf128 ~x in
        mk_float ret
    | "expf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = expf16 ~x in
        mk_float ret
    | "expf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = expf32 ~x in
        mk_float ret
    | "expf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = expf64 ~x in
        mk_float ret
    | "fabs", [ t ], [], [ x ] -> fabs ~t ~x
    | "fadd_algebraic", [ t ], [], [ a; b ] -> fadd_algebraic ~t ~a ~b
    | "fadd_fast", [ t ], [], [ a; b ] -> fadd_fast ~t ~a ~b
    | "fdiv_algebraic", [ t ], [], [ a; b ] -> fdiv_algebraic ~t ~a ~b
    | "fdiv_fast", [ t ], [], [ a; b ] -> fdiv_fast ~t ~a ~b
    | "field_offset", [ f ], [], [] ->
        let+ ret = field_offset ~f in
        mk_int ret
    | "field_representing_type_actual_type_id", [], [], [ frt_type_id ] ->
        field_representing_type_actual_type_id ~frt_type_id
    | "float_to_int_unchecked", [ float; int ], [], [ value ] ->
        float_to_int_unchecked ~float ~int ~value
    | "floorf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = floorf128 ~x in
        mk_float ret
    | "floorf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = floorf16 ~x in
        mk_float ret
    | "floorf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = floorf32 ~x in
        mk_float ret
    | "floorf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = floorf64 ~x in
        mk_float ret
    | "fmaf128", [], [], [ a; b; c ] ->
        let a = as_base_f F128 a in
        let b = as_base_f F128 b in
        let c = as_base_f F128 c in
        let+ ret = fmaf128 ~a ~b ~c in
        mk_float ret
    | "fmaf16", [], [], [ a; b; c ] ->
        let a = as_base_f F16 a in
        let b = as_base_f F16 b in
        let c = as_base_f F16 c in
        let+ ret = fmaf16 ~a ~b ~c in
        mk_float ret
    | "fmaf32", [], [], [ a; b; c ] ->
        let a = as_base_f F32 a in
        let b = as_base_f F32 b in
        let c = as_base_f F32 c in
        let+ ret = fmaf32 ~a ~b ~c in
        mk_float ret
    | "fmaf64", [], [], [ a; b; c ] ->
        let a = as_base_f F64 a in
        let b = as_base_f F64 b in
        let c = as_base_f F64 c in
        let+ ret = fmaf64 ~a ~b ~c in
        mk_float ret
    | "fmul_algebraic", [ t ], [], [ a; b ] -> fmul_algebraic ~t ~a ~b
    | "fmul_fast", [ t ], [], [ a; b ] -> fmul_fast ~t ~a ~b
    | "fmuladdf128", [], [], [ a; b; c ] ->
        let a = as_base_f F128 a in
        let b = as_base_f F128 b in
        let c = as_base_f F128 c in
        let+ ret = fmuladdf128 ~a ~b ~c in
        mk_float ret
    | "fmuladdf16", [], [], [ a; b; c ] ->
        let a = as_base_f F16 a in
        let b = as_base_f F16 b in
        let c = as_base_f F16 c in
        let+ ret = fmuladdf16 ~a ~b ~c in
        mk_float ret
    | "fmuladdf32", [], [], [ a; b; c ] ->
        let a = as_base_f F32 a in
        let b = as_base_f F32 b in
        let c = as_base_f F32 c in
        let+ ret = fmuladdf32 ~a ~b ~c in
        mk_float ret
    | "fmuladdf64", [], [], [ a; b; c ] ->
        let a = as_base_f F64 a in
        let b = as_base_f F64 b in
        let c = as_base_f F64 c in
        let+ ret = fmuladdf64 ~a ~b ~c in
        mk_float ret
    | "forget", [ t ], [], [ arg ] ->
        let+ () = forget ~t ~arg in
        mk_tuple []
    | "frem_algebraic", [ t ], [], [ a; b ] -> frem_algebraic ~t ~a ~b
    | "frem_fast", [ t ], [], [ a; b ] -> frem_fast ~t ~a ~b
    | "fsub_algebraic", [ t ], [], [ a; b ] -> fsub_algebraic ~t ~a ~b
    | "fsub_fast", [ t ], [], [ a; b ] -> fsub_fast ~t ~a ~b
    | "is_val_statically_known", [ t ], [], [ arg ] ->
        let+ ret = is_val_statically_known ~t ~arg in
        mk_int (Typed.BitVec.of_bool ret)
    | "log10f128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = log10f128 ~x in
        mk_float ret
    | "log10f16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = log10f16 ~x in
        mk_float ret
    | "log10f32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = log10f32 ~x in
        mk_float ret
    | "log10f64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = log10f64 ~x in
        mk_float ret
    | "log2f128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = log2f128 ~x in
        mk_float ret
    | "log2f16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = log2f16 ~x in
        mk_float ret
    | "log2f32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = log2f32 ~x in
        mk_float ret
    | "log2f64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = log2f64 ~x in
        mk_float ret
    | "logf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = logf128 ~x in
        mk_float ret
    | "logf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = logf16 ~x in
        mk_float ret
    | "logf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = logf32 ~x in
        mk_float ret
    | "logf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = logf64 ~x in
        mk_float ret
    | "maximum_number_nsz_f128", [], [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = maximum_number_nsz_f128 ~x ~y in
        mk_float ret
    | "maximum_number_nsz_f16", [], [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = maximum_number_nsz_f16 ~x ~y in
        mk_float ret
    | "maximum_number_nsz_f32", [], [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = maximum_number_nsz_f32 ~x ~y in
        mk_float ret
    | "maximum_number_nsz_f64", [], [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = maximum_number_nsz_f64 ~x ~y in
        mk_float ret
    | "maximumf128", [], [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = maximumf128 ~x ~y in
        mk_float ret
    | "maximumf16", [], [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = maximumf16 ~x ~y in
        mk_float ret
    | "maximumf32", [], [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = maximumf32 ~x ~y in
        mk_float ret
    | "maximumf64", [], [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = maximumf64 ~x ~y in
        mk_float ret
    | "minimum_number_nsz_f128", [], [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = minimum_number_nsz_f128 ~x ~y in
        mk_float ret
    | "minimum_number_nsz_f16", [], [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = minimum_number_nsz_f16 ~x ~y in
        mk_float ret
    | "minimum_number_nsz_f32", [], [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = minimum_number_nsz_f32 ~x ~y in
        mk_float ret
    | "minimum_number_nsz_f64", [], [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = minimum_number_nsz_f64 ~x ~y in
        mk_float ret
    | "minimumf128", [], [], [ x; y ] ->
        let x = as_base_f F128 x in
        let y = as_base_f F128 y in
        let+ ret = minimumf128 ~x ~y in
        mk_float ret
    | "minimumf16", [], [], [ x; y ] ->
        let x = as_base_f F16 x in
        let y = as_base_f F16 y in
        let+ ret = minimumf16 ~x ~y in
        mk_float ret
    | "minimumf32", [], [], [ x; y ] ->
        let x = as_base_f F32 x in
        let y = as_base_f F32 y in
        let+ ret = minimumf32 ~x ~y in
        mk_float ret
    | "minimumf64", [], [], [ x; y ] ->
        let x = as_base_f F64 x in
        let y = as_base_f F64 y in
        let+ ret = minimumf64 ~x ~y in
        mk_float ret
    | "mul_with_overflow", [ t ], [], [ x; y ] -> mul_with_overflow ~t ~x ~y
    | "needs_drop", [ t ], [], [] ->
        let+ ret = needs_drop ~t in
        mk_int (Typed.BitVec.of_bool ret)
    | "nontemporal_store", [ t ], [], [ ptr; val_ ] ->
        let ptr = as_ptr ptr in
        let+ () = nontemporal_store ~t ~ptr ~val_ in
        mk_tuple []
    | ( "offload",
        [ t_f; t; r ],
        [],
        [ f; workgroup_dim; thread_dim; dyn_cache; args ] ) ->
        let dyn_cache = as_base_i U32 dyn_cache in
        offload ~t_f ~t ~r ~f ~workgroup_dim ~thread_dim ~dyn_cache ~args
    | "offset", [ ptr; delta ], [], [ dst; offset_ ] ->
        offset ~ptr ~delta ~dst ~offset:offset_
    | "offset_of", [ t ], [], [ variant; field ] ->
        let variant = as_base_i U32 variant in
        let field = as_base_i U32 field in
        let+ ret = offset_of ~t ~variant ~field in
        mk_int ret
    | "overflow_checks", [], [], [] ->
        let+ ret = overflow_checks () in
        mk_int (Typed.BitVec.of_bool ret)
    | "powf128", [], [], [ a; x ] ->
        let a = as_base_f F128 a in
        let x = as_base_f F128 x in
        let+ ret = powf128 ~a ~x in
        mk_float ret
    | "powf16", [], [], [ a; x ] ->
        let a = as_base_f F16 a in
        let x = as_base_f F16 x in
        let+ ret = powf16 ~a ~x in
        mk_float ret
    | "powf32", [], [], [ a; x ] ->
        let a = as_base_f F32 a in
        let x = as_base_f F32 x in
        let+ ret = powf32 ~a ~x in
        mk_float ret
    | "powf64", [], [], [ a; x ] ->
        let a = as_base_f F64 a in
        let x = as_base_f F64 x in
        let+ ret = powf64 ~a ~x in
        mk_float ret
    | "powif128", [], [], [ a; x ] ->
        let a = as_base_f F128 a in
        let x = as_base_i U32 x in
        let+ ret = powif128 ~a ~x in
        mk_float ret
    | "powif16", [], [], [ a; x ] ->
        let a = as_base_f F16 a in
        let x = as_base_i U32 x in
        let+ ret = powif16 ~a ~x in
        mk_float ret
    | "powif32", [], [], [ a; x ] ->
        let a = as_base_f F32 a in
        let x = as_base_i U32 x in
        let+ ret = powif32 ~a ~x in
        mk_float ret
    | "powif64", [], [], [ a; x ] ->
        let a = as_base_f F64 a in
        let x = as_base_i U32 x in
        let+ ret = powif64 ~a ~x in
        mk_float ret
    | "prefetch_read_data", [ t ], [ locality ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_read_data ~t ~locality ~data in
        mk_tuple []
    | "prefetch_read_instruction", [ t ], [ locality ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_read_instruction ~t ~locality ~data in
        mk_tuple []
    | "prefetch_write_data", [ t ], [ locality ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_write_data ~t ~locality ~data in
        mk_tuple []
    | "prefetch_write_instruction", [ t ], [ locality ], [ data ] ->
        let data = as_ptr data in
        let+ () = prefetch_write_instruction ~t ~locality ~data in
        mk_tuple []
    | "ptr_guaranteed_cmp", [ t ], [], [ ptr; other ] ->
        let ptr = as_ptr ptr in
        let other = as_ptr other in
        let+ ret = ptr_guaranteed_cmp ~t ~ptr ~other in
        mk_int ret
    | "ptr_mask", [ t ], [], [ ptr; mask ] ->
        let ptr = as_ptr ptr in
        let mask = as_base_i Usize mask in
        let+ ret = ptr_mask ~t ~ptr ~mask in
        mk_ptr (fst ret) (snd ret)
    | "ptr_metadata", [ p; m ], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        ptr_metadata ~p ~m ~ptr
    | "ptr_offset_from", [ t ], [], [ ptr; base ] ->
        let ptr = as_ptr ptr in
        let base = as_ptr base in
        let+ ret = ptr_offset_from ~t ~ptr ~base in
        mk_int ret
    | "ptr_offset_from_unsigned", [ t ], [], [ ptr; base ] ->
        let ptr = as_ptr ptr in
        let base = as_ptr base in
        let+ ret = ptr_offset_from_unsigned ~t ~ptr ~base in
        mk_int ret
    | "raw_eq", [ t ], [], [ a; b ] ->
        let a = as_ptr a in
        let b = as_ptr b in
        let+ ret = raw_eq ~t ~a ~b in
        mk_int (Typed.BitVec.of_bool ret)
    | "read_via_copy", [ t ], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        read_via_copy ~t ~ptr
    | "return_address", [], [], [] ->
        let+ ret = return_address () in
        mk_ptr (fst ret) (snd ret)
    | "rotate_left", [ t ], [], [ x; shift ] ->
        let shift = as_base_i U32 shift in
        rotate_left ~t ~x ~shift
    | "rotate_right", [ t ], [], [ x; shift ] ->
        let shift = as_base_i U32 shift in
        rotate_right ~t ~x ~shift
    | "round_ties_even_f128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = round_ties_even_f128 ~x in
        mk_float ret
    | "round_ties_even_f16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = round_ties_even_f16 ~x in
        mk_float ret
    | "round_ties_even_f32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = round_ties_even_f32 ~x in
        mk_float ret
    | "round_ties_even_f64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = round_ties_even_f64 ~x in
        mk_float ret
    | "roundf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = roundf128 ~x in
        mk_float ret
    | "roundf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = roundf16 ~x in
        mk_float ret
    | "roundf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = roundf32 ~x in
        mk_float ret
    | "roundf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = roundf64 ~x in
        mk_float ret
    | "rustc_peek", [ t ], [], [ arg ] -> rustc_peek ~t ~arg
    | "saturating_add", [ t ], [], [ a; b ] -> saturating_add ~t ~a ~b
    | "saturating_sub", [ t ], [], [ a; b ] -> saturating_sub ~t ~a ~b
    | "select_unpredictable", [ t ], [], [ b; true_val; false_val ] ->
        let b = Typed.BitVec.to_bool (as_base TBool b) in
        select_unpredictable ~t ~b ~true_val ~false_val
    | "simd_add", [ t ], [], [ x; y ] -> simd_add ~t ~x ~y
    | "simd_and", [ t ], [], [ x; y ] -> simd_and ~t ~x ~y
    | "simd_arith_offset", [ t; u ], [], [ ptr; offset ] ->
        simd_arith_offset ~t ~u ~ptr ~offset
    | "simd_as", [ t; u ], [], [ x ] -> simd_as ~t ~u ~x
    | "simd_bitmask", [ t; u ], [], [ x ] -> simd_bitmask ~t ~u ~x
    | "simd_bitreverse", [ t ], [], [ x ] -> simd_bitreverse ~t ~x
    | "simd_bswap", [ t ], [], [ x ] -> simd_bswap ~t ~x
    | "simd_carryless_mul", [ t ], [], [ a; b ] -> simd_carryless_mul ~t ~a ~b
    | "simd_cast", [ t; u ], [], [ x ] -> simd_cast ~t ~u ~x
    | "simd_cast_ptr", [ t; u ], [], [ ptr ] -> simd_cast_ptr ~t ~u ~ptr
    | "simd_ceil", [ t ], [], [ x ] -> simd_ceil ~t ~x
    | "simd_ctlz", [ t ], [], [ x ] -> simd_ctlz ~t ~x
    | "simd_ctpop", [ t ], [], [ x ] -> simd_ctpop ~t ~x
    | "simd_cttz", [ t ], [], [ x ] -> simd_cttz ~t ~x
    | "simd_div", [ t ], [], [ lhs; rhs ] -> simd_div ~t ~lhs ~rhs
    | "simd_eq", [ t; u ], [], [ x; y ] -> simd_eq ~t ~u ~x ~y
    | "simd_expose_provenance", [ t; u ], [], [ ptr ] ->
        simd_expose_provenance ~t ~u ~ptr
    | "simd_extract", [ t; u ], [], [ x; idx ] ->
        let idx = as_base_i U32 idx in
        simd_extract ~t ~u ~x ~idx
    | "simd_extract_dyn", [ t; u ], [], [ x; idx ] ->
        let idx = as_base_i U32 idx in
        simd_extract_dyn ~t ~u ~x ~idx
    | "simd_fabs", [ t ], [], [ x ] -> simd_fabs ~t ~x
    | "simd_fcos", [ t ], [], [ a ] -> simd_fcos ~t ~a
    | "simd_fexp", [ t ], [], [ a ] -> simd_fexp ~t ~a
    | "simd_fexp2", [ t ], [], [ a ] -> simd_fexp2 ~t ~a
    | "simd_flog", [ t ], [], [ a ] -> simd_flog ~t ~a
    | "simd_flog10", [ t ], [], [ a ] -> simd_flog10 ~t ~a
    | "simd_flog2", [ t ], [], [ a ] -> simd_flog2 ~t ~a
    | "simd_floor", [ t ], [], [ x ] -> simd_floor ~t ~x
    | "simd_fma", [ t ], [], [ x; y; z ] -> simd_fma ~t ~x ~y ~z
    | "simd_fsin", [ t ], [], [ a ] -> simd_fsin ~t ~a
    | "simd_fsqrt", [ t ], [], [ x ] -> simd_fsqrt ~t ~x
    | "simd_funnel_shl", [ t ], [], [ a; b; shift ] ->
        simd_funnel_shl ~t ~a ~b ~shift
    | "simd_funnel_shr", [ t ], [], [ a; b; shift ] ->
        simd_funnel_shr ~t ~a ~b ~shift
    | "simd_gather", [ t; u; v ], [], [ val_; ptr; mask ] ->
        simd_gather ~t ~u ~v ~val_ ~ptr ~mask
    | "simd_ge", [ t; u ], [], [ x; y ] -> simd_ge ~t ~u ~x ~y
    | "simd_gt", [ t; u ], [], [ x; y ] -> simd_gt ~t ~u ~x ~y
    | "simd_insert", [ t; u ], [], [ x; idx; val_ ] ->
        let idx = as_base_i U32 idx in
        simd_insert ~t ~u ~x ~idx ~val_
    | "simd_insert_dyn", [ t; u ], [], [ x; idx; val_ ] ->
        let idx = as_base_i U32 idx in
        simd_insert_dyn ~t ~u ~x ~idx ~val_
    | "simd_le", [ t; u ], [], [ x; y ] -> simd_le ~t ~u ~x ~y
    | "simd_lt", [ t; u ], [], [ x; y ] -> simd_lt ~t ~u ~x ~y
    | "simd_masked_load", [ v; u; t ], [ align ], [ mask; ptr; val_ ] ->
        simd_masked_load ~v ~u ~t ~align ~mask ~ptr ~val_
    | "simd_masked_store", [ v; u; t ], [ align ], [ mask; ptr; val_ ] ->
        let+ () = simd_masked_store ~v ~u ~t ~align ~mask ~ptr ~val_ in
        mk_tuple []
    | "simd_maximum_number_nsz", [ t ], [], [ x; y ] ->
        simd_maximum_number_nsz ~t ~x ~y
    | "simd_minimum_number_nsz", [ t ], [], [ x; y ] ->
        simd_minimum_number_nsz ~t ~x ~y
    | "simd_mul", [ t ], [], [ x; y ] -> simd_mul ~t ~x ~y
    | "simd_ne", [ t; u ], [], [ x; y ] -> simd_ne ~t ~u ~x ~y
    | "simd_neg", [ t ], [], [ x ] -> simd_neg ~t ~x
    | "simd_or", [ t ], [], [ x; y ] -> simd_or ~t ~x ~y
    | "simd_reduce_add_ordered", [ t; u ], [], [ x; y ] ->
        simd_reduce_add_ordered ~t ~u ~x ~y
    | "simd_reduce_add_unordered", [ t; u ], [], [ x ] ->
        simd_reduce_add_unordered ~t ~u ~x
    | "simd_reduce_all", [ t ], [], [ x ] ->
        let+ ret = simd_reduce_all ~t ~x in
        mk_int (Typed.BitVec.of_bool ret)
    | "simd_reduce_and", [ t; u ], [], [ x ] -> simd_reduce_and ~t ~u ~x
    | "simd_reduce_any", [ t ], [], [ x ] ->
        let+ ret = simd_reduce_any ~t ~x in
        mk_int (Typed.BitVec.of_bool ret)
    | "simd_reduce_max", [ t; u ], [], [ x ] -> simd_reduce_max ~t ~u ~x
    | "simd_reduce_min", [ t; u ], [], [ x ] -> simd_reduce_min ~t ~u ~x
    | "simd_reduce_mul_ordered", [ t; u ], [], [ x; y ] ->
        simd_reduce_mul_ordered ~t ~u ~x ~y
    | "simd_reduce_mul_unordered", [ t; u ], [], [ x ] ->
        simd_reduce_mul_unordered ~t ~u ~x
    | "simd_reduce_or", [ t; u ], [], [ x ] -> simd_reduce_or ~t ~u ~x
    | "simd_reduce_xor", [ t; u ], [], [ x ] -> simd_reduce_xor ~t ~u ~x
    | "simd_relaxed_fma", [ t ], [], [ x; y; z ] -> simd_relaxed_fma ~t ~x ~y ~z
    | "simd_rem", [ t ], [], [ lhs; rhs ] -> simd_rem ~t ~lhs ~rhs
    | "simd_round", [ t ], [], [ x ] -> simd_round ~t ~x
    | "simd_round_ties_even", [ t ], [], [ x ] -> simd_round_ties_even ~t ~x
    | "simd_saturating_add", [ t ], [], [ x; y ] -> simd_saturating_add ~t ~x ~y
    | "simd_saturating_sub", [ t ], [], [ lhs; rhs ] ->
        simd_saturating_sub ~t ~lhs ~rhs
    | "simd_scatter", [ t; u; v ], [], [ val_; ptr; mask ] ->
        let+ () = simd_scatter ~t ~u ~v ~val_ ~ptr ~mask in
        mk_tuple []
    | "simd_select", [ m; t ], [], [ mask; if_true; if_false ] ->
        simd_select ~m ~t ~mask ~if_true ~if_false
    | "simd_select_bitmask", [ t_m; t ], [], [ m; yes; no ] ->
        simd_select_bitmask ~t_m ~t ~m ~yes ~no
    | "simd_shl", [ t ], [], [ lhs; rhs ] -> simd_shl ~t ~lhs ~rhs
    | "simd_shr", [ t ], [], [ lhs; rhs ] -> simd_shr ~t ~lhs ~rhs
    | "simd_shuffle", [ t; u; v ], [], [ x; y; idx ] ->
        simd_shuffle ~t ~u ~v ~x ~y ~idx
    | "simd_splat", [ t; u ], [], [ value ] -> simd_splat ~t ~u ~value
    | "simd_sub", [ t ], [], [ lhs; rhs ] -> simd_sub ~t ~lhs ~rhs
    | "simd_trunc", [ t ], [], [ x ] -> simd_trunc ~t ~x
    | "simd_with_exposed_provenance", [ t; u ], [], [ addr ] ->
        simd_with_exposed_provenance ~t ~u ~addr
    | "simd_xor", [ t ], [], [ x; y ] -> simd_xor ~t ~x ~y
    | "sinf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = sinf128 ~x in
        mk_float ret
    | "sinf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = sinf16 ~x in
        mk_float ret
    | "sinf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = sinf32 ~x in
        mk_float ret
    | "sinf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = sinf64 ~x in
        mk_float ret
    | "size_of", [ t ], [], [] ->
        let+ ret = size_of ~t in
        mk_int ret
    | "size_of_type_id", [], [], [ id ] -> size_of_type_id ~id
    | "size_of_val", [ t ], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = size_of_val ~t ~ptr in
        mk_int ret
    | "slice_get_unchecked", [ itemptr; sliceptr; t ], [], [ slice_ptr; index ]
      ->
        let index = as_base_i Usize index in
        slice_get_unchecked ~itemptr ~sliceptr ~t ~slice_ptr ~index
    | "sqrtf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = sqrtf128 ~x in
        mk_float ret
    | "sqrtf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = sqrtf16 ~x in
        mk_float ret
    | "sqrtf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = sqrtf32 ~x in
        mk_float ret
    | "sqrtf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = sqrtf64 ~x in
        mk_float ret
    | "sub_with_overflow", [ t ], [], [ x; y ] -> sub_with_overflow ~t ~x ~y
    | "sve_tuple_create2", [ svec; svectup ], [], [ x0; x1 ] ->
        sve_tuple_create2 ~svec ~svectup ~x0 ~x1
    | "sve_tuple_create3", [ svec; svectup ], [], [ x0; x1; x2 ] ->
        sve_tuple_create3 ~svec ~svectup ~x0 ~x1 ~x2
    | "sve_tuple_create4", [ svec; svectup ], [], [ x0; x1; x2; x3 ] ->
        sve_tuple_create4 ~svec ~svectup ~x0 ~x1 ~x2 ~x3
    | "sve_tuple_get", [ svectup; svec ], [ idx ], [ tuple ] ->
        sve_tuple_get ~svectup ~svec ~idx ~tuple
    | "sve_tuple_set", [ svectup; svec ], [ idx ], [ tuple; x ] ->
        sve_tuple_set ~svectup ~svec ~idx ~tuple ~x
    | "three_way_compare", [ t ], [], [ lhs; rhss ] ->
        three_way_compare ~t ~lhs ~rhss
    | "transmute", [ t_src; dst ], [], [ src ] -> transmute ~t_src ~dst ~src
    | "transmute_unchecked", [ t_src; dst ], [], [ src ] ->
        transmute_unchecked ~t_src ~dst ~src
    | "truncf128", [], [], [ x ] ->
        let x = as_base_f F128 x in
        let+ ret = truncf128 ~x in
        mk_float ret
    | "truncf16", [], [], [ x ] ->
        let x = as_base_f F16 x in
        let+ ret = truncf16 ~x in
        mk_float ret
    | "truncf32", [], [], [ x ] ->
        let x = as_base_f F32 x in
        let+ ret = truncf32 ~x in
        mk_float ret
    | "truncf64", [], [], [ x ] ->
        let x = as_base_f F64 x in
        let+ ret = truncf64 ~x in
        mk_float ret
    | "type_id_eq", [], [], [ a; b ] ->
        let+ ret = type_id_eq ~a ~b in
        mk_int (Typed.BitVec.of_bool ret)
    | ( "type_id_field_representing_type",
        [],
        [],
        [ id; variant_index; field_index ] ) ->
        let variant_index = as_base_i Usize variant_index in
        let field_index = as_base_i Usize field_index in
        type_id_field_representing_type ~id ~variant_index ~field_index
    | "type_id_fields", [], [], [ id; variant_index ] ->
        let variant_index = as_base_i Usize variant_index in
        let+ ret = type_id_fields ~id ~variant_index in
        mk_int ret
    | "type_id_variants", [], [], [ id ] ->
        let+ ret = type_id_variants ~id in
        mk_int ret
    | "type_id_vtable", [], [], [ id; trait ] -> type_id_vtable ~id ~trait
    | "type_name", [ t ], [], [] ->
        let+ ret = type_name ~t in
        mk_ptr (fst ret) (snd ret)
    | "type_of", [], [], [ id ] -> type_of ~id
    | "typed_swap_nonoverlapping", [ t ], [], [ x; y ] ->
        let x = as_ptr x in
        let y = as_ptr y in
        let+ () = typed_swap_nonoverlapping ~t ~x ~y in
        mk_tuple []
    | "ub_checks", [], [], [] ->
        let+ ret = ub_checks () in
        mk_int (Typed.BitVec.of_bool ret)
    | "unaligned_volatile_load", [ t ], [], [ src ] ->
        let src = as_ptr src in
        unaligned_volatile_load ~t ~src
    | "unaligned_volatile_store", [ t ], [], [ dst; val_ ] ->
        let dst = as_ptr dst in
        let+ () = unaligned_volatile_store ~t ~dst ~val_ in
        mk_tuple []
    | "unchecked_add", [ t ], [], [ x; y ] -> unchecked_add ~t ~x ~y
    | "unchecked_div", [ t ], [], [ x; y ] -> unchecked_div ~t ~x ~y
    | "unchecked_funnel_shl", [ t ], [], [ a; b; shift ] ->
        let shift = as_base_i U32 shift in
        unchecked_funnel_shl ~t ~a ~b ~shift
    | "unchecked_funnel_shr", [ t ], [], [ a; b; shift ] ->
        let shift = as_base_i U32 shift in
        unchecked_funnel_shr ~t ~a ~b ~shift
    | "unchecked_mul", [ t ], [], [ x; y ] -> unchecked_mul ~t ~x ~y
    | "unchecked_rem", [ t ], [], [ x; y ] -> unchecked_rem ~t ~x ~y
    | "unchecked_shl", [ t; u ], [], [ x; y ] -> unchecked_shl ~t ~u ~x ~y
    | "unchecked_shr", [ t; u ], [], [ x; y ] -> unchecked_shr ~t ~u ~x ~y
    | "unchecked_sub", [ t ], [], [ x; y ] -> unchecked_sub ~t ~x ~y
    | "unreachable", [], [], [] ->
        let+ () = unreachable () in
        mk_tuple []
    | "va_arg", [ t ], [], [ ap ] ->
        let ap = as_ptr ap in
        va_arg ~t ~ap
    | "va_copy", [], [], [ src ] ->
        let src = as_ptr src in
        va_copy ~src
    | "va_end", [], [], [ ap ] ->
        let ap = as_ptr ap in
        let+ () = va_end ~ap in
        mk_tuple []
    | "variant_count", [ t ], [], [] ->
        let+ ret = variant_count ~t in
        mk_int ret
    | "volatile_copy_memory", [ t ], [], [ dst; src; count ] ->
        let dst = as_ptr dst in
        let src = as_ptr src in
        let count = as_base_i Usize count in
        let+ () = volatile_copy_memory ~t ~dst ~src ~count in
        mk_tuple []
    | "volatile_copy_nonoverlapping_memory", [ t ], [], [ dst; src; count ] ->
        let dst = as_ptr dst in
        let src = as_ptr src in
        let count = as_base_i Usize count in
        let+ () = volatile_copy_nonoverlapping_memory ~t ~dst ~src ~count in
        mk_tuple []
    | "volatile_load", [ t ], [], [ src ] ->
        let src = as_ptr src in
        volatile_load ~t ~src
    | "volatile_set_memory", [ t ], [], [ dst; val_; count ] ->
        let dst = as_ptr dst in
        let val_ = as_base_i U8 val_ in
        let count = as_base_i Usize count in
        let+ () = volatile_set_memory ~t ~dst ~val_ ~count in
        mk_tuple []
    | "volatile_store", [ t ], [], [ dst; val_ ] ->
        let dst = as_ptr dst in
        let+ () = volatile_store ~t ~dst ~val_ in
        mk_tuple []
    | "vtable_align", [], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = vtable_align ~ptr in
        mk_int ret
    | "vtable_size", [], [], [ ptr ] ->
        let ptr = as_ptr ptr in
        let+ ret = vtable_size ~ptr in
        mk_int ret
    | "wrapping_add", [ t ], [], [ a; b ] -> wrapping_add ~t ~a ~b
    | "wrapping_mul", [ t ], [], [ a; b ] -> wrapping_mul ~t ~a ~b
    | "wrapping_sub", [ t ], [], [ a; b ] -> wrapping_sub ~t ~a ~b
    | "write_bytes", [ t ], [], [ dst; val_; count ] ->
        let dst = as_ptr dst in
        let val_ = as_base_i U8 val_ in
        let count = as_base_i Usize count in
        let+ () = write_bytes ~t ~dst ~val_ ~count in
        mk_tuple []
    | "write_via_move", [ t ], [], [ ptr; value ] ->
        let ptr = as_ptr ptr in
        let+ () = write_via_move ~t ~ptr ~value in
        mk_tuple []
    | name, tys, cs, args ->
        not_impl
          "Intrinsic %s not found, or not called with the right arguments; \
           got:@.Types: %a@.Consts: %a@.Args: %a"
          name
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma Rust_val.pp)
          args
end
