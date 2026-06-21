(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

open Svalue
open Common

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax
  include Impl.M (StateM)

  let[@inline] eval_fun name fun_exec (generics : Charon.Types.generic_args)
      args =
    match (name, generics.types, generics.const_generics, args) with
    | "abort", [], [], [] ->
        let+ () = abort () in
        Typed.Adt.mk_tuple []
    | "add_with_overflow", [ t ], [], [ x; y ] -> add_with_overflow ~t ~x ~y
    | "aggregate_raw_ptr", [ p; d; m ], [], [ data; meta ] ->
        aggregate_raw_ptr ~p ~d ~m ~data ~meta
    | "align_of", [ t ], [], [] -> align_of ~t
    | "align_of_val", [ t ], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        align_of_val ~t ~ptr
    | "arith_offset", [ t ], [], [ dst; offset ] ->
        let dst = Typed.cast_ptr_f dst in
        let offset = Typed.cast_i Usize offset in
        arith_offset ~t ~dst ~offset
    | "assert_inhabited", [ t ], [], [] ->
        let+ () = assert_inhabited ~t in
        Typed.Adt.mk_tuple []
    | "assert_mem_uninitialized_valid", [ t ], [], [] ->
        let+ () = assert_mem_uninitialized_valid ~t in
        Typed.Adt.mk_tuple []
    | "assert_zero_valid", [ t ], [], [] ->
        let+ () = assert_zero_valid ~t in
        Typed.Adt.mk_tuple []
    | "assume", [], [], [ b ] ->
        let b = Typed.BitVec.to_bool (Typed.cast_lit TBool b) in
        let+ () = assume ~b in
        Typed.Adt.mk_tuple []
    | "atomic_and", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_and ~t ~u ~ord ~dst ~src
    | "atomic_cxchg", [ t ], [ ord_succ; ord_fail ], [ dst; old; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_cxchg ~t ~ord_succ ~ord_fail ~dst ~old ~src
    | "atomic_cxchgweak", [ t ], [ ord_succ; ord_fail ], [ dst; old; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_cxchgweak ~t ~ord_succ ~ord_fail ~dst ~old ~src
    | "atomic_fence", [], [ ord ], [] ->
        let+ () = atomic_fence ~ord in
        Typed.Adt.mk_tuple []
    | "atomic_load", [ t ], [ ord ], [ src ] ->
        let src = Typed.cast_ptr_f src in
        atomic_load ~t ~ord ~src
    | "atomic_max", [ t ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_max ~t ~ord ~dst ~src
    | "atomic_min", [ t ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_min ~t ~ord ~dst ~src
    | "atomic_nand", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_nand ~t ~u ~ord ~dst ~src
    | "atomic_or", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_or ~t ~u ~ord ~dst ~src
    | "atomic_singlethreadfence", [], [ ord ], [] ->
        let+ () = atomic_singlethreadfence ~ord in
        Typed.Adt.mk_tuple []
    | "atomic_store", [ t ], [ ord ], [ dst; val_ ] ->
        let dst = Typed.cast_ptr_f dst in
        let+ () = atomic_store ~t ~ord ~dst ~val_ in
        Typed.Adt.mk_tuple []
    | "atomic_umax", [ t ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_umax ~t ~ord ~dst ~src
    | "atomic_umin", [ t ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_umin ~t ~ord ~dst ~src
    | "atomic_xadd", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_xadd ~t ~u ~ord ~dst ~src
    | "atomic_xchg", [ t ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_xchg ~t ~ord ~dst ~src
    | "atomic_xor", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_xor ~t ~u ~ord ~dst ~src
    | "atomic_xsub", [ t; u ], [ ord ], [ dst; src ] ->
        let dst = Typed.cast_ptr_f dst in
        atomic_xsub ~t ~u ~ord ~dst ~src
    | "autodiff", [ t_f; g; t; r ], [], [ f; df; args ] ->
        autodiff ~t_f ~g ~t ~r ~f ~df ~args
    | "bitreverse", [ t ], [], [ x ] -> bitreverse ~t ~x
    | "black_box", [ t ], [], [ dummy ] -> black_box ~t ~dummy
    | "breakpoint", [], [], [] ->
        let+ () = breakpoint () in
        Typed.Adt.mk_tuple []
    | "bswap", [ t ], [], [ x ] -> bswap ~t ~x
    | "caller_location", [], [], [] -> caller_location ()
    | ( "carrying_mul_add",
        [ t; u ],
        [],
        [ multiplier; multiplicand; addend; carry ] ) ->
        carrying_mul_add ~t ~u ~multiplier ~multiplicand ~addend ~carry
    | "carryless_mul", [ t ], [], [ a; b ] -> carryless_mul ~t ~a ~b
    | "catch_unwind", [ t_data ], [], [ try_fn; data; catch_fn ] ->
        let try_fn = Typed.cast_ptr_f try_fn in
        let data = Typed.cast_ptr_f data in
        let catch_fn = Typed.cast_ptr_f catch_fn in
        let+ ret = catch_unwind ~fun_exec ~t_data ~try_fn ~data ~catch_fn in
        Typed.BitVec.of_bool ret
    | "ceilf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        ceilf128 ~x
    | "ceilf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        ceilf16 ~x
    | "ceilf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        ceilf32 ~x
    | "ceilf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        ceilf64 ~x
    | "cold_path", [], [], [] ->
        let+ () = cold_path () in
        Typed.Adt.mk_tuple []
    | "compare_bytes", [], [], [ left; right; bytes ] ->
        let left = Typed.cast_ptr_f left in
        let right = Typed.cast_ptr_f right in
        let bytes = Typed.cast_i Usize bytes in
        compare_bytes ~left ~right ~bytes
    | "const_deallocate", [], [], [ ptr; size; align ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let size = Typed.cast_i Usize size in
        let align = Typed.cast_i Usize align in
        let+ () = const_deallocate ~ptr ~size ~align in
        Typed.Adt.mk_tuple []
    | ( "const_eval_select",
        [ t_arg; f; g; ret ],
        [],
        [ arg; called_in_const; called_at_rt ] ) ->
        const_eval_select ~t_arg ~f ~g ~ret ~arg ~called_in_const ~called_at_rt
    | "const_make_global", [], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        const_make_global ~ptr
    | "contract_check_ensures", [ c; t_ret ], [], [ cond; ret ] ->
        let cond = Typed.cast_enum cond in
        contract_check_ensures ~c ~t_ret ~cond ~ret
    | "contract_check_requires", [ c ], [], [ cond ] ->
        let+ () = contract_check_requires ~c ~cond in
        Typed.Adt.mk_tuple []
    | "copy", [ t ], [], [ src; dst; count ] ->
        let src = Typed.cast_ptr_f src in
        let dst = Typed.cast_ptr_f dst in
        let count = Typed.cast_i Usize count in
        let+ () = copy ~t ~src ~dst ~count in
        Typed.Adt.mk_tuple []
    | "copy_nonoverlapping", [ t ], [], [ src; dst; count ] ->
        let src = Typed.cast_ptr_f src in
        let dst = Typed.cast_ptr_f dst in
        let count = Typed.cast_i Usize count in
        let+ () = copy_nonoverlapping ~t ~src ~dst ~count in
        Typed.Adt.mk_tuple []
    | "copysignf128", [], [], [ x; y ] ->
        let x = Typed.cast_f F128 x in
        let y = Typed.cast_f F128 y in
        copysignf128 ~x ~y
    | "copysignf16", [], [], [ x; y ] ->
        let x = Typed.cast_f F16 x in
        let y = Typed.cast_f F16 y in
        copysignf16 ~x ~y
    | "copysignf32", [], [], [ x; y ] ->
        let x = Typed.cast_f F32 x in
        let y = Typed.cast_f F32 y in
        copysignf32 ~x ~y
    | "copysignf64", [], [], [ x; y ] ->
        let x = Typed.cast_f F64 x in
        let y = Typed.cast_f F64 y in
        copysignf64 ~x ~y
    | "cosf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        cosf128 ~x
    | "cosf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        cosf16 ~x
    | "cosf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        cosf32 ~x
    | "cosf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        cosf64 ~x
    | "ctlz", [ t ], [], [ x ] -> ctlz ~t ~x
    | "ctlz_nonzero", [ t ], [], [ x ] -> ctlz_nonzero ~t ~x
    | "ctpop", [ t ], [], [ x ] -> ctpop ~t ~x
    | "cttz", [ t ], [], [ x ] -> cttz ~t ~x
    | "cttz_nonzero", [ t ], [], [ x ] -> cttz_nonzero ~t ~x
    | "discriminant_value", [ t ], [], [ v ] ->
        let v = Typed.cast_ptr_f v in
        discriminant_value ~t ~v
    | "disjoint_bitor", [ t ], [], [ a; b ] -> disjoint_bitor ~t ~a ~b
    | "exact_div", [ t ], [], [ x; y ] -> exact_div ~t ~x ~y
    | "exp2f128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        exp2f128 ~x
    | "exp2f16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        exp2f16 ~x
    | "exp2f32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        exp2f32 ~x
    | "exp2f64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        exp2f64 ~x
    | "expf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        expf128 ~x
    | "expf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        expf16 ~x
    | "expf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        expf32 ~x
    | "expf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        expf64 ~x
    | "fabs", [ t ], [], [ x ] -> fabs ~t ~x
    | "fadd_algebraic", [ t ], [], [ a; b ] -> fadd_algebraic ~t ~a ~b
    | "fadd_fast", [ t ], [], [ a; b ] -> fadd_fast ~t ~a ~b
    | "fdiv_algebraic", [ t ], [], [ a; b ] -> fdiv_algebraic ~t ~a ~b
    | "fdiv_fast", [ t ], [], [ a; b ] -> fdiv_fast ~t ~a ~b
    | "field_offset", [ f ], [], [] -> field_offset ~f
    | "field_representing_type_actual_type_id", [], [], [ frt_type_id ] ->
        let frt_type_id = Typed.cast_tuple frt_type_id in
        field_representing_type_actual_type_id ~frt_type_id
    | "float_to_int_unchecked", [ float; int ], [], [ value ] ->
        float_to_int_unchecked ~float ~int ~value
    | "floorf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        floorf128 ~x
    | "floorf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        floorf16 ~x
    | "floorf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        floorf32 ~x
    | "floorf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        floorf64 ~x
    | "fmaf128", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F128 a in
        let b = Typed.cast_f F128 b in
        let c = Typed.cast_f F128 c in
        fmaf128 ~a ~b ~c
    | "fmaf16", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F16 a in
        let b = Typed.cast_f F16 b in
        let c = Typed.cast_f F16 c in
        fmaf16 ~a ~b ~c
    | "fmaf32", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F32 a in
        let b = Typed.cast_f F32 b in
        let c = Typed.cast_f F32 c in
        fmaf32 ~a ~b ~c
    | "fmaf64", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F64 a in
        let b = Typed.cast_f F64 b in
        let c = Typed.cast_f F64 c in
        fmaf64 ~a ~b ~c
    | "fmul_algebraic", [ t ], [], [ a; b ] -> fmul_algebraic ~t ~a ~b
    | "fmul_fast", [ t ], [], [ a; b ] -> fmul_fast ~t ~a ~b
    | "fmuladdf128", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F128 a in
        let b = Typed.cast_f F128 b in
        let c = Typed.cast_f F128 c in
        fmuladdf128 ~a ~b ~c
    | "fmuladdf16", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F16 a in
        let b = Typed.cast_f F16 b in
        let c = Typed.cast_f F16 c in
        fmuladdf16 ~a ~b ~c
    | "fmuladdf32", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F32 a in
        let b = Typed.cast_f F32 b in
        let c = Typed.cast_f F32 c in
        fmuladdf32 ~a ~b ~c
    | "fmuladdf64", [], [], [ a; b; c ] ->
        let a = Typed.cast_f F64 a in
        let b = Typed.cast_f F64 b in
        let c = Typed.cast_f F64 c in
        fmuladdf64 ~a ~b ~c
    | "forget", [ t ], [], [ arg ] ->
        let+ () = forget ~t ~arg in
        Typed.Adt.mk_tuple []
    | "frem_algebraic", [ t ], [], [ a; b ] -> frem_algebraic ~t ~a ~b
    | "frem_fast", [ t ], [], [ a; b ] -> frem_fast ~t ~a ~b
    | "fsub_algebraic", [ t ], [], [ a; b ] -> fsub_algebraic ~t ~a ~b
    | "fsub_fast", [ t ], [], [ a; b ] -> fsub_fast ~t ~a ~b
    | "is_val_statically_known", [ t ], [], [ arg ] ->
        let+ ret = is_val_statically_known ~t ~arg in
        Typed.BitVec.of_bool ret
    | "log10f128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        log10f128 ~x
    | "log10f16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        log10f16 ~x
    | "log10f32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        log10f32 ~x
    | "log10f64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        log10f64 ~x
    | "log2f128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        log2f128 ~x
    | "log2f16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        log2f16 ~x
    | "log2f32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        log2f32 ~x
    | "log2f64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        log2f64 ~x
    | "logf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        logf128 ~x
    | "logf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        logf16 ~x
    | "logf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        logf32 ~x
    | "logf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        logf64 ~x
    | "maximum_number_nsz_f128", [], [], [ x; y ] ->
        let x = Typed.cast_f F128 x in
        let y = Typed.cast_f F128 y in
        maximum_number_nsz_f128 ~x ~y
    | "maximum_number_nsz_f16", [], [], [ x; y ] ->
        let x = Typed.cast_f F16 x in
        let y = Typed.cast_f F16 y in
        maximum_number_nsz_f16 ~x ~y
    | "maximum_number_nsz_f32", [], [], [ x; y ] ->
        let x = Typed.cast_f F32 x in
        let y = Typed.cast_f F32 y in
        maximum_number_nsz_f32 ~x ~y
    | "maximum_number_nsz_f64", [], [], [ x; y ] ->
        let x = Typed.cast_f F64 x in
        let y = Typed.cast_f F64 y in
        maximum_number_nsz_f64 ~x ~y
    | "maximumf128", [], [], [ x; y ] ->
        let x = Typed.cast_f F128 x in
        let y = Typed.cast_f F128 y in
        maximumf128 ~x ~y
    | "maximumf16", [], [], [ x; y ] ->
        let x = Typed.cast_f F16 x in
        let y = Typed.cast_f F16 y in
        maximumf16 ~x ~y
    | "maximumf32", [], [], [ x; y ] ->
        let x = Typed.cast_f F32 x in
        let y = Typed.cast_f F32 y in
        maximumf32 ~x ~y
    | "maximumf64", [], [], [ x; y ] ->
        let x = Typed.cast_f F64 x in
        let y = Typed.cast_f F64 y in
        maximumf64 ~x ~y
    | "minimum_number_nsz_f128", [], [], [ x; y ] ->
        let x = Typed.cast_f F128 x in
        let y = Typed.cast_f F128 y in
        minimum_number_nsz_f128 ~x ~y
    | "minimum_number_nsz_f16", [], [], [ x; y ] ->
        let x = Typed.cast_f F16 x in
        let y = Typed.cast_f F16 y in
        minimum_number_nsz_f16 ~x ~y
    | "minimum_number_nsz_f32", [], [], [ x; y ] ->
        let x = Typed.cast_f F32 x in
        let y = Typed.cast_f F32 y in
        minimum_number_nsz_f32 ~x ~y
    | "minimum_number_nsz_f64", [], [], [ x; y ] ->
        let x = Typed.cast_f F64 x in
        let y = Typed.cast_f F64 y in
        minimum_number_nsz_f64 ~x ~y
    | "minimumf128", [], [], [ x; y ] ->
        let x = Typed.cast_f F128 x in
        let y = Typed.cast_f F128 y in
        minimumf128 ~x ~y
    | "minimumf16", [], [], [ x; y ] ->
        let x = Typed.cast_f F16 x in
        let y = Typed.cast_f F16 y in
        minimumf16 ~x ~y
    | "minimumf32", [], [], [ x; y ] ->
        let x = Typed.cast_f F32 x in
        let y = Typed.cast_f F32 y in
        minimumf32 ~x ~y
    | "minimumf64", [], [], [ x; y ] ->
        let x = Typed.cast_f F64 x in
        let y = Typed.cast_f F64 y in
        minimumf64 ~x ~y
    | "mul_with_overflow", [ t ], [], [ x; y ] -> mul_with_overflow ~t ~x ~y
    | "needs_drop", [ t ], [], [] ->
        let+ ret = needs_drop ~t in
        Typed.BitVec.of_bool ret
    | "nontemporal_store", [ t ], [], [ ptr; val_ ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let+ () = nontemporal_store ~t ~ptr ~val_ in
        Typed.Adt.mk_tuple []
    | ( "offload",
        [ t_f; t; r ],
        [],
        [ f; workgroup_dim; thread_dim; dyn_cache; args ] ) ->
        let workgroup_dim = Typed.cast_tuple workgroup_dim in
        let thread_dim = Typed.cast_tuple thread_dim in
        let dyn_cache = Typed.cast_i U32 dyn_cache in
        offload ~t_f ~t ~r ~f ~workgroup_dim ~thread_dim ~dyn_cache ~args
    | "offset", [ ptr; delta ], [], [ dst; offset_ ] ->
        offset ~ptr ~delta ~dst ~offset:offset_
    | "offset_of", [ t ], [], [ variant; field ] ->
        let variant = Typed.cast_i U32 variant in
        let field = Typed.cast_i U32 field in
        offset_of ~t ~variant ~field
    | "overflow_checks", [], [], [] ->
        let+ ret = overflow_checks () in
        Typed.BitVec.of_bool ret
    | "powf128", [], [], [ a; x ] ->
        let a = Typed.cast_f F128 a in
        let x = Typed.cast_f F128 x in
        powf128 ~a ~x
    | "powf16", [], [], [ a; x ] ->
        let a = Typed.cast_f F16 a in
        let x = Typed.cast_f F16 x in
        powf16 ~a ~x
    | "powf32", [], [], [ a; x ] ->
        let a = Typed.cast_f F32 a in
        let x = Typed.cast_f F32 x in
        powf32 ~a ~x
    | "powf64", [], [], [ a; x ] ->
        let a = Typed.cast_f F64 a in
        let x = Typed.cast_f F64 x in
        powf64 ~a ~x
    | "powif128", [], [], [ a; x ] ->
        let a = Typed.cast_f F128 a in
        let x = Typed.cast_i U32 x in
        powif128 ~a ~x
    | "powif16", [], [], [ a; x ] ->
        let a = Typed.cast_f F16 a in
        let x = Typed.cast_i U32 x in
        powif16 ~a ~x
    | "powif32", [], [], [ a; x ] ->
        let a = Typed.cast_f F32 a in
        let x = Typed.cast_i U32 x in
        powif32 ~a ~x
    | "powif64", [], [], [ a; x ] ->
        let a = Typed.cast_f F64 a in
        let x = Typed.cast_i U32 x in
        powif64 ~a ~x
    | "prefetch_read_data", [ t ], [ locality ], [ data ] ->
        let data = Typed.cast_ptr_f data in
        let+ () = prefetch_read_data ~t ~locality ~data in
        Typed.Adt.mk_tuple []
    | "prefetch_read_instruction", [ t ], [ locality ], [ data ] ->
        let data = Typed.cast_ptr_f data in
        let+ () = prefetch_read_instruction ~t ~locality ~data in
        Typed.Adt.mk_tuple []
    | "prefetch_write_data", [ t ], [ locality ], [ data ] ->
        let data = Typed.cast_ptr_f data in
        let+ () = prefetch_write_data ~t ~locality ~data in
        Typed.Adt.mk_tuple []
    | "prefetch_write_instruction", [ t ], [ locality ], [ data ] ->
        let data = Typed.cast_ptr_f data in
        let+ () = prefetch_write_instruction ~t ~locality ~data in
        Typed.Adt.mk_tuple []
    | "ptr_guaranteed_cmp", [ t ], [], [ ptr; other ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let other = Typed.cast_ptr_f other in
        ptr_guaranteed_cmp ~t ~ptr ~other
    | "ptr_mask", [ t ], [], [ ptr; mask ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let mask = Typed.cast_i Usize mask in
        ptr_mask ~t ~ptr ~mask
    | "ptr_metadata", [ p; m ], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        ptr_metadata ~p ~m ~ptr
    | "ptr_offset_from", [ t ], [], [ ptr; base ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let base = Typed.cast_ptr_f base in
        ptr_offset_from ~t ~ptr ~base
    | "ptr_offset_from_unsigned", [ t ], [], [ ptr; base ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let base = Typed.cast_ptr_f base in
        ptr_offset_from_unsigned ~t ~ptr ~base
    | "raw_eq", [ t ], [], [ a; b ] ->
        let a = Typed.cast_ptr_f a in
        let b = Typed.cast_ptr_f b in
        let+ ret = raw_eq ~t ~a ~b in
        Typed.BitVec.of_bool ret
    | "read_via_copy", [ t ], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        read_via_copy ~t ~ptr
    | "return_address", [], [], [] -> return_address ()
    | "rotate_left", [ t ], [], [ x; shift ] ->
        let shift = Typed.cast_i U32 shift in
        rotate_left ~t ~x ~shift
    | "rotate_right", [ t ], [], [ x; shift ] ->
        let shift = Typed.cast_i U32 shift in
        rotate_right ~t ~x ~shift
    | "round_ties_even_f128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        round_ties_even_f128 ~x
    | "round_ties_even_f16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        round_ties_even_f16 ~x
    | "round_ties_even_f32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        round_ties_even_f32 ~x
    | "round_ties_even_f64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        round_ties_even_f64 ~x
    | "roundf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        roundf128 ~x
    | "roundf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        roundf16 ~x
    | "roundf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        roundf32 ~x
    | "roundf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        roundf64 ~x
    | "rustc_peek", [ t ], [], [ arg ] -> rustc_peek ~t ~arg
    | "saturating_add", [ t ], [], [ a; b ] -> saturating_add ~t ~a ~b
    | "saturating_sub", [ t ], [], [ a; b ] -> saturating_sub ~t ~a ~b
    | "select_unpredictable", [ t ], [], [ b; true_val; false_val ] ->
        let b = Typed.BitVec.to_bool (Typed.cast_lit TBool b) in
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
        let idx = Typed.cast_i U32 idx in
        simd_extract ~t ~u ~x ~idx
    | "simd_extract_dyn", [ t; u ], [], [ x; idx ] ->
        let idx = Typed.cast_i U32 idx in
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
        let idx = Typed.cast_i U32 idx in
        simd_insert ~t ~u ~x ~idx ~val_
    | "simd_insert_dyn", [ t; u ], [], [ x; idx; val_ ] ->
        let idx = Typed.cast_i U32 idx in
        simd_insert_dyn ~t ~u ~x ~idx ~val_
    | "simd_le", [ t; u ], [], [ x; y ] -> simd_le ~t ~u ~x ~y
    | "simd_lt", [ t; u ], [], [ x; y ] -> simd_lt ~t ~u ~x ~y
    | "simd_masked_load", [ v; u; t ], [ align ], [ mask; ptr; val_ ] ->
        simd_masked_load ~v ~u ~t ~align ~mask ~ptr ~val_
    | "simd_masked_store", [ v; u; t ], [ align ], [ mask; ptr; val_ ] ->
        let+ () = simd_masked_store ~v ~u ~t ~align ~mask ~ptr ~val_ in
        Typed.Adt.mk_tuple []
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
        Typed.BitVec.of_bool ret
    | "simd_reduce_and", [ t; u ], [], [ x ] -> simd_reduce_and ~t ~u ~x
    | "simd_reduce_any", [ t ], [], [ x ] ->
        let+ ret = simd_reduce_any ~t ~x in
        Typed.BitVec.of_bool ret
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
        Typed.Adt.mk_tuple []
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
        let x = Typed.cast_f F128 x in
        sinf128 ~x
    | "sinf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        sinf16 ~x
    | "sinf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        sinf32 ~x
    | "sinf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        sinf64 ~x
    | "size_of", [ t ], [], [] -> size_of ~t
    | "size_of_type_id", [], [], [ id ] ->
        let id = Typed.cast_tuple id in
        size_of_type_id ~id
    | "size_of_val", [ t ], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        size_of_val ~t ~ptr
    | "slice_get_unchecked", [ itemptr; sliceptr; t ], [], [ slice_ptr; index ]
      ->
        let index = Typed.cast_i Usize index in
        slice_get_unchecked ~itemptr ~sliceptr ~t ~slice_ptr ~index
    | "sqrtf128", [], [], [ x ] ->
        let x = Typed.cast_f F128 x in
        sqrtf128 ~x
    | "sqrtf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        sqrtf16 ~x
    | "sqrtf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        sqrtf32 ~x
    | "sqrtf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        sqrtf64 ~x
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
        let x = Typed.cast_f F128 x in
        truncf128 ~x
    | "truncf16", [], [], [ x ] ->
        let x = Typed.cast_f F16 x in
        truncf16 ~x
    | "truncf32", [], [], [ x ] ->
        let x = Typed.cast_f F32 x in
        truncf32 ~x
    | "truncf64", [], [], [ x ] ->
        let x = Typed.cast_f F64 x in
        truncf64 ~x
    | "type_id_eq", [], [], [ a; b ] ->
        let a = Typed.cast_tuple a in
        let b = Typed.cast_tuple b in
        let+ ret = type_id_eq ~a ~b in
        Typed.BitVec.of_bool ret
    | ( "type_id_field_representing_type",
        [],
        [],
        [ id; variant_index; field_index ] ) ->
        let id = Typed.cast_tuple id in
        let variant_index = Typed.cast_i Usize variant_index in
        let field_index = Typed.cast_i Usize field_index in
        type_id_field_representing_type ~id ~variant_index ~field_index
    | "type_id_fields", [], [], [ id; variant_index ] ->
        let id = Typed.cast_tuple id in
        let variant_index = Typed.cast_i Usize variant_index in
        type_id_fields ~id ~variant_index
    | "type_id_variants", [], [], [ id ] ->
        let id = Typed.cast_tuple id in
        type_id_variants ~id
    | "type_id_vtable", [], [], [ id; trait ] ->
        let id = Typed.cast_tuple id in
        let trait = Typed.cast_tuple trait in
        type_id_vtable ~id ~trait
    | "type_name", [ t ], [], [] -> type_name ~t
    | "type_of", [], [], [ id ] ->
        let id = Typed.cast_tuple id in
        type_of ~id
    | "typed_swap_nonoverlapping", [ t ], [], [ x; y ] ->
        let x = Typed.cast_ptr_f x in
        let y = Typed.cast_ptr_f y in
        let+ () = typed_swap_nonoverlapping ~t ~x ~y in
        Typed.Adt.mk_tuple []
    | "ub_checks", [], [], [] ->
        let+ ret = ub_checks () in
        Typed.BitVec.of_bool ret
    | "unaligned_volatile_load", [ t ], [], [ src ] ->
        let src = Typed.cast_ptr_f src in
        unaligned_volatile_load ~t ~src
    | "unaligned_volatile_store", [ t ], [], [ dst; val_ ] ->
        let dst = Typed.cast_ptr_f dst in
        let+ () = unaligned_volatile_store ~t ~dst ~val_ in
        Typed.Adt.mk_tuple []
    | "unchecked_add", [ t ], [], [ x; y ] -> unchecked_add ~t ~x ~y
    | "unchecked_div", [ t ], [], [ x; y ] -> unchecked_div ~t ~x ~y
    | "unchecked_funnel_shl", [ t ], [], [ a; b; shift ] ->
        let shift = Typed.cast_i U32 shift in
        unchecked_funnel_shl ~t ~a ~b ~shift
    | "unchecked_funnel_shr", [ t ], [], [ a; b; shift ] ->
        let shift = Typed.cast_i U32 shift in
        unchecked_funnel_shr ~t ~a ~b ~shift
    | "unchecked_mul", [ t ], [], [ x; y ] -> unchecked_mul ~t ~x ~y
    | "unchecked_rem", [ t ], [], [ x; y ] -> unchecked_rem ~t ~x ~y
    | "unchecked_shl", [ t; u ], [], [ x; y ] -> unchecked_shl ~t ~u ~x ~y
    | "unchecked_shr", [ t; u ], [], [ x; y ] -> unchecked_shr ~t ~u ~x ~y
    | "unchecked_sub", [ t ], [], [ x; y ] -> unchecked_sub ~t ~x ~y
    | "unreachable", [], [], [] ->
        let+ () = unreachable () in
        Typed.Adt.mk_tuple []
    | "va_arg", [ t ], [], [ ap ] ->
        let ap = Typed.cast_ptr_f ap in
        va_arg ~t ~ap
    | "va_copy", [], [], [ src ] ->
        let src = Typed.cast_ptr_f src in
        va_copy ~src
    | "va_end", [], [], [ ap ] ->
        let ap = Typed.cast_ptr_f ap in
        let+ () = va_end ~ap in
        Typed.Adt.mk_tuple []
    | "variant_count", [ t ], [], [] -> variant_count ~t
    | "volatile_copy_memory", [ t ], [], [ dst; src; count ] ->
        let dst = Typed.cast_ptr_f dst in
        let src = Typed.cast_ptr_f src in
        let count = Typed.cast_i Usize count in
        let+ () = volatile_copy_memory ~t ~dst ~src ~count in
        Typed.Adt.mk_tuple []
    | "volatile_copy_nonoverlapping_memory", [ t ], [], [ dst; src; count ] ->
        let dst = Typed.cast_ptr_f dst in
        let src = Typed.cast_ptr_f src in
        let count = Typed.cast_i Usize count in
        let+ () = volatile_copy_nonoverlapping_memory ~t ~dst ~src ~count in
        Typed.Adt.mk_tuple []
    | "volatile_load", [ t ], [], [ src ] ->
        let src = Typed.cast_ptr_f src in
        volatile_load ~t ~src
    | "volatile_set_memory", [ t ], [], [ dst; val_; count ] ->
        let dst = Typed.cast_ptr_f dst in
        let val_ = Typed.cast_i U8 val_ in
        let count = Typed.cast_i Usize count in
        let+ () = volatile_set_memory ~t ~dst ~val_ ~count in
        Typed.Adt.mk_tuple []
    | "volatile_store", [ t ], [], [ dst; val_ ] ->
        let dst = Typed.cast_ptr_f dst in
        let+ () = volatile_store ~t ~dst ~val_ in
        Typed.Adt.mk_tuple []
    | "vtable_align", [], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        vtable_align ~ptr
    | "vtable_size", [], [], [ ptr ] ->
        let ptr = Typed.cast_ptr_f ptr in
        vtable_size ~ptr
    | "wrapping_add", [ t ], [], [ a; b ] -> wrapping_add ~t ~a ~b
    | "wrapping_mul", [ t ], [], [ a; b ] -> wrapping_mul ~t ~a ~b
    | "wrapping_sub", [ t ], [], [ a; b ] -> wrapping_sub ~t ~a ~b
    | "write_bytes", [ t ], [], [ dst; val_; count ] ->
        let dst = Typed.cast_ptr_f dst in
        let val_ = Typed.cast_i U8 val_ in
        let count = Typed.cast_i Usize count in
        let+ () = write_bytes ~t ~dst ~val_ ~count in
        Typed.Adt.mk_tuple []
    | "write_via_move", [ t ], [], [ ptr; value ] ->
        let ptr = Typed.cast_ptr_f ptr in
        let+ () = write_via_move ~t ~ptr ~value in
        Typed.Adt.mk_tuple []
    | name, tys, cs, args ->
        not_impl
          "Intrinsic %s not found, or not called with the right arguments; \
           got:@.Types: %a@.Consts: %a@.Args: %a"
          name
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma Typed.ppa)
          args
end
