(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Rust_val

type fn =
  | AllocAllocAllocImpl
  | AllocAllocHandleAllocError
  | AllocRawVecHandleError
  | CoreF128IsFinite
  | CoreF128IsInfinite
  | CoreF128IsNan
  | CoreF128IsNormal
  | CoreF128IsSignNegative
  | CoreF128IsSignPositive
  | CoreF128IsSubnormal
  | CoreF16IsFinite
  | CoreF16IsInfinite
  | CoreF16IsNan
  | CoreF16IsNormal
  | CoreF16IsSignNegative
  | CoreF16IsSignPositive
  | CoreF16IsSubnormal
  | CoreF32IsFinite
  | CoreF32IsInfinite
  | CoreF32IsNan
  | CoreF32IsNormal
  | CoreF32IsSignNegative
  | CoreF32IsSignPositive
  | CoreF32IsSubnormal
  | CoreF64IsFinite
  | CoreF64IsInfinite
  | CoreF64IsNan
  | CoreF64IsNormal
  | CoreF64IsSignNegative
  | CoreF64IsSignPositive
  | CoreF64IsSubnormal
  | CoreOptionUnwrapFailed
  | CorePanickingAssertFailedInner
  | CorePanickingPanic
  | CorePanickingPanicFmt
  | CorePanickingPanicNounwindFmt
  | CoreResultUnwrapFailed
  | StdIoStdioEprint
  | StdIoStdioPrint
  | StdIoStdioPrintTo
  | StdIoStdioPrintToBufferIfCaptureUsed
  | StdPanickingBeginPanic
  | StdRtBeginPanic

let fn_pats : (string * fn) list =
  [
    ("alloc::alloc::_::alloc_impl", AllocAllocAllocImpl);
    ("alloc::alloc::handle_alloc_error", AllocAllocHandleAllocError);
    ("alloc::raw_vec::handle_error", AllocRawVecHandleError);
    ("core::f128::_::is_finite", CoreF128IsFinite);
    ("core::f128::_::is_infinite", CoreF128IsInfinite);
    ("core::f128::_::is_nan", CoreF128IsNan);
    ("core::f128::_::is_normal", CoreF128IsNormal);
    ("core::f128::_::is_sign_negative", CoreF128IsSignNegative);
    ("core::f128::_::is_sign_positive", CoreF128IsSignPositive);
    ("core::f128::_::is_subnormal", CoreF128IsSubnormal);
    ("core::f16::_::is_finite", CoreF16IsFinite);
    ("core::f16::_::is_infinite", CoreF16IsInfinite);
    ("core::f16::_::is_nan", CoreF16IsNan);
    ("core::f16::_::is_normal", CoreF16IsNormal);
    ("core::f16::_::is_sign_negative", CoreF16IsSignNegative);
    ("core::f16::_::is_sign_positive", CoreF16IsSignPositive);
    ("core::f16::_::is_subnormal", CoreF16IsSubnormal);
    ("core::f32::_::is_finite", CoreF32IsFinite);
    ("core::f32::_::is_infinite", CoreF32IsInfinite);
    ("core::f32::_::is_nan", CoreF32IsNan);
    ("core::f32::_::is_normal", CoreF32IsNormal);
    ("core::f32::_::is_sign_negative", CoreF32IsSignNegative);
    ("core::f32::_::is_sign_positive", CoreF32IsSignPositive);
    ("core::f32::_::is_subnormal", CoreF32IsSubnormal);
    ("core::f64::_::is_finite", CoreF64IsFinite);
    ("core::f64::_::is_infinite", CoreF64IsInfinite);
    ("core::f64::_::is_nan", CoreF64IsNan);
    ("core::f64::_::is_normal", CoreF64IsNormal);
    ("core::f64::_::is_sign_negative", CoreF64IsSignNegative);
    ("core::f64::_::is_sign_positive", CoreF64IsSignPositive);
    ("core::f64::_::is_subnormal", CoreF64IsSubnormal);
    ("core::option::unwrap_failed", CoreOptionUnwrapFailed);
    ("core::panicking::assert_failed_inner", CorePanickingAssertFailedInner);
    ("core::panicking::panic", CorePanickingPanic);
    ("core::panicking::panic_fmt", CorePanickingPanicFmt);
    ("core::panicking::panic_nounwind_fmt", CorePanickingPanicNounwindFmt);
    ("core::result::unwrap_failed", CoreResultUnwrapFailed);
    ("std::io::stdio::_eprint", StdIoStdioEprint);
    ("std::io::stdio::_print", StdIoStdioPrint);
    ("std::io::stdio::print_to", StdIoStdioPrintTo);
    ( "std::io::stdio::print_to_buffer_if_capture_used",
      StdIoStdioPrintToBufferIfCaptureUsed );
    ("std::panicking::begin_panic", StdPanickingBeginPanic);
    ("std::rt::begin_panic", StdRtBeginPanic);
  ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  let[@inline] as_ptr (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Int v ->
        let v = Typed.cast_i Usize v in
        let ptr = Sptr.null_ptr_of v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  include Impl.M (StateM)

  let[@inline] fn_to_stub stub _fun_exec (generics : Charon.Types.generic_args)
      args =
    match (stub, generics.types, generics.const_generics, args) with
    | AllocAllocAllocImpl, [], [], [ self; layout; zeroed ] ->
        let self = as_ptr self in
        let zeroed = Typed.BitVec.to_bool (as_base TBool zeroed) in
        alloc_impl ~self ~layout ~zeroed
    | AllocAllocHandleAllocError, [], [], [ layout ] ->
        let+ () = handle_alloc_error ~layout in
        Tuple []
    | AllocRawVecHandleError, [], [], [ e ] ->
        let+ () = handle_error ~e in
        Tuple []
    | CoreF128IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsNan, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = f128_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsNan, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = f16_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsNan, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = f32_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsNan, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = f64_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreOptionUnwrapFailed, [], [], [] ->
        let+ () = option_unwrap_failed () in
        Tuple []
    | CorePanickingAssertFailedInner, [], [], [ kind; left; right; args ] ->
        let left = as_ptr left in
        let right = as_ptr right in
        let+ () = assert_failed_inner ~kind ~left ~right ~args in
        Tuple []
    | CorePanickingPanic, [], [], [ expr ] ->
        let expr = as_ptr expr in
        let+ () = panic ~expr in
        Tuple []
    | CorePanickingPanicFmt, [], [], [ fmt ] ->
        let+ () = panic_fmt ~fmt in
        Tuple []
    | CorePanickingPanicNounwindFmt, [], [], [ fmt; force_no_backtrace ] ->
        let force_no_backtrace =
          Typed.BitVec.to_bool (as_base TBool force_no_backtrace)
        in
        let+ () = panic_nounwind_fmt ~fmt ~force_no_backtrace in
        Tuple []
    | CoreResultUnwrapFailed, [], [], [ msg; error ] ->
        let msg = as_ptr msg in
        let error = as_ptr error in
        let+ () = result_unwrap_failed ~msg ~error in
        Tuple []
    | StdIoStdioEprint, [], [], [ args ] ->
        let+ () = _eprint ~args in
        Tuple []
    | StdIoStdioPrint, [], [], [ args ] ->
        let+ () = _print ~args in
        Tuple []
    | StdIoStdioPrintTo, [ t ], [], [ args; global_s; label ] ->
        let global_s = as_ptr global_s in
        let label = as_ptr label in
        let+ () = print_to ~t ~args ~global_s ~label in
        Tuple []
    | StdIoStdioPrintToBufferIfCaptureUsed, [], [], [ args ] ->
        let+ ret = print_to_buffer_if_capture_used ~args in
        Int (Typed.BitVec.of_bool ret)
    | StdPanickingBeginPanic, [ m ], [], [ msg ] ->
        let+ () = panicking_begin_panic ~m ~msg in
        Tuple []
    | StdRtBeginPanic, _, _, _ ->
        rt_begin_panic ~fun_exec:_fun_exec ~types:generics.types
          ~consts:generics.const_generics ~args
    | _, tys, cs, args ->
        Fmt.kstr not_impl
          "Custom stub found but called with the wrong arguments; got:@.Types: \
           %a@.Consts: %a@.Args: %a"
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma pp_rust_val)
          args
end
