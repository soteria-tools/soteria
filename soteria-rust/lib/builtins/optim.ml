(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Charon
open Common
open Rust_val
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

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

  module type Intf = sig
    val alloc_impl :
      self:full_ptr ->
      layout:rust_val ->
      zeroed:[< Typed.T.sbool ] Typed.t ->
      rust_val ret

    (** {@markdown[
          Signals a memory allocation error.

           Callers of memory allocation APIs wishing to cease execution
           in response to an allocation error are encouraged to call this function,
           rather than directly invoking [`panic!`] or similar.

           This function is guaranteed to diverge (not return normally with a value), but depending on
           global configuration, it may either panic (resulting in unwinding or aborting as per
           configuration for all panics), or abort the process (with no unwinding).

           The default behavior is:

            * If the binary links against `std` (typically the case), then
             print a message to standard error and abort the process.
             This behavior can be replaced with [`set_alloc_error_hook`] and [`take_alloc_error_hook`].
             Future versions of Rust may panic by default instead.

           * If the binary does not link against `std` (all of its crates are marked
             [`#![no_std]`][no_std]), then call [`panic!`] with a message.
             [The panic handler] applies as to any panic.

           [`set_alloc_error_hook`]: ../../std/alloc/fn.set_alloc_error_hook.html
           [`take_alloc_error_hook`]: ../../std/alloc/fn.take_alloc_error_hook.html
           [The panic handler]: https://doc.rust-lang.org/reference/runtime.html#the-panic_handler-attribute
           [no_std]: https://doc.rust-lang.org/reference/names/preludes.html#the-no_std-attribute
        ]} *)
    val handle_alloc_error : layout:rust_val -> unit ret

    val handle_error : e:rust_val -> unit ret

    (** {@markdown[
          Returns `true` if this number is neither infinite nor NaN.

           ```
           #![feature(f128)]
           # #[cfg(target_has_reliable_f128)] {

           let f = 7.0f128;
           let inf: f128 = f128::INFINITY;
           let neg_inf: f128 = f128::NEG_INFINITY;
           let nan: f128 = f128::NAN;

           assert!(f.is_finite());

           assert!(!nan.is_finite());
           assert!(!inf.is_finite());
           assert!(!neg_inf.is_finite());
           # }
           ```
        ]} *)
    val f128_is_finite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is positive infinity or negative infinity, and
           `false` otherwise.

           ```
           #![feature(f128)]
           # #[cfg(target_has_reliable_f128)] {

           let f = 7.0f128;
           let inf = f128::INFINITY;
           let neg_inf = f128::NEG_INFINITY;
           let nan = f128::NAN;

           assert!(!f.is_infinite());
           assert!(!nan.is_infinite());

           assert!(inf.is_infinite());
           assert!(neg_inf.is_infinite());
           # }
           ```
        ]} *)
    val f128_is_infinite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is NaN.

           ```
           #![feature(f128)]
           # #[cfg(target_has_reliable_f128)] {

           let nan = f128::NAN;
           let f = 7.0_f128;

           assert!(nan.is_nan());
           assert!(!f.is_nan());
           # }
           ```
        ]} *)
    val f128_is_nan :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is neither zero, infinite, [subnormal], or NaN.

           ```
           #![feature(f128)]
           # #[cfg(target_has_reliable_f128)] {

           let min = f128::MIN_POSITIVE; // 3.362103143e-4932f128
           let max = f128::MAX;
           let lower_than_min = 1.0e-4960_f128;
           let zero = 0.0_f128;

           assert!(min.is_normal());
           assert!(max.is_normal());

           assert!(!zero.is_normal());
           assert!(!f128::NAN.is_normal());
           assert!(!f128::INFINITY.is_normal());
           // Values between `0` and `min` are Subnormal.
           assert!(!lower_than_min.is_normal());
           # }
           ```

           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f128_is_normal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a negative sign, including `-0.0`, NaNs with
           negative sign bit and negative infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_negative` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == -1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           #![feature(f128)]

           let f = 7.0_f128;
           let g = -7.0_f128;

           assert!(!f.is_sign_negative());
           assert!(g.is_sign_negative());
           ```
        ]} *)
    val f128_is_sign_negative :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a positive sign, including `+0.0`, NaNs with
           positive sign bit and positive infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_positive` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == 1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           #![feature(f128)]

           let f = 7.0_f128;
           let g = -7.0_f128;

           assert!(f.is_sign_positive());
           assert!(!g.is_sign_positive());
           ```
        ]} *)
    val f128_is_sign_positive :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is [subnormal].

           ```
           #![feature(f128)]
           # #[cfg(target_has_reliable_f128)] {

           let min = f128::MIN_POSITIVE; // 3.362103143e-4932f128
           let max = f128::MAX;
           let lower_than_min = 1.0e-4960_f128;
           let zero = 0.0_f128;

           assert!(!min.is_subnormal());
           assert!(!max.is_subnormal());

           assert!(!zero.is_subnormal());
           assert!(!f128::NAN.is_subnormal());
           assert!(!f128::INFINITY.is_subnormal());
           // Values between `0` and `min` are Subnormal.
           assert!(lower_than_min.is_subnormal());
           # }
           ```

           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f128_is_subnormal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this number is neither infinite nor NaN.

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let f = 7.0f16;
           let inf: f16 = f16::INFINITY;
           let neg_inf: f16 = f16::NEG_INFINITY;
           let nan: f16 = f16::NAN;

           assert!(f.is_finite());

           assert!(!nan.is_finite());
           assert!(!inf.is_finite());
           assert!(!neg_inf.is_finite());
           # }
           ```
        ]} *)
    val f16_is_finite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is positive infinity or negative infinity, and
           `false` otherwise.

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let f = 7.0f16;
           let inf = f16::INFINITY;
           let neg_inf = f16::NEG_INFINITY;
           let nan = f16::NAN;

           assert!(!f.is_infinite());
           assert!(!nan.is_infinite());

           assert!(inf.is_infinite());
           assert!(neg_inf.is_infinite());
           # }
           ```
        ]} *)
    val f16_is_infinite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is NaN.

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let nan = f16::NAN;
           let f = 7.0_f16;

           assert!(nan.is_nan());
           assert!(!f.is_nan());
           # }
           ```
        ]} *)
    val f16_is_nan :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is neither zero, infinite, [subnormal], or NaN.

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let min = f16::MIN_POSITIVE; // 6.1035e-5
           let max = f16::MAX;
           let lower_than_min = 1.0e-7_f16;
           let zero = 0.0_f16;

           assert!(min.is_normal());
           assert!(max.is_normal());

           assert!(!zero.is_normal());
           assert!(!f16::NAN.is_normal());
           assert!(!f16::INFINITY.is_normal());
           // Values between `0` and `min` are Subnormal.
           assert!(!lower_than_min.is_normal());
           # }
           ```
           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f16_is_normal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a negative sign, including `-0.0`, NaNs with
           negative sign bit and negative infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_negative` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == -1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let f = 7.0_f16;
           let g = -7.0_f16;

           assert!(!f.is_sign_negative());
           assert!(g.is_sign_negative());
           # }
           ```
        ]} *)
    val f16_is_sign_negative :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a positive sign, including `+0.0`, NaNs with
           positive sign bit and positive infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_positive` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == 1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let f = 7.0_f16;
           let g = -7.0_f16;

           assert!(f.is_sign_positive());
           assert!(!g.is_sign_positive());
           # }
           ```
        ]} *)
    val f16_is_sign_positive :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is [subnormal].

           ```
           #![feature(f16)]
           # #[cfg(target_has_reliable_f16)] {

           let min = f16::MIN_POSITIVE; // 6.1035e-5
           let max = f16::MAX;
           let lower_than_min = 1.0e-7_f16;
           let zero = 0.0_f16;

           assert!(!min.is_subnormal());
           assert!(!max.is_subnormal());

           assert!(!zero.is_subnormal());
           assert!(!f16::NAN.is_subnormal());
           assert!(!f16::INFINITY.is_subnormal());
           // Values between `0` and `min` are Subnormal.
           assert!(lower_than_min.is_subnormal());
           # }
           ```
           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f16_is_subnormal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this number is neither infinite nor NaN.

           ```
           let f = 7.0f32;
           let inf = f32::INFINITY;
           let neg_inf = f32::NEG_INFINITY;
           let nan = f32::NAN;

           assert!(f.is_finite());

           assert!(!nan.is_finite());
           assert!(!inf.is_finite());
           assert!(!neg_inf.is_finite());
           ```
        ]} *)
    val f32_is_finite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is positive infinity or negative infinity, and
           `false` otherwise.

           ```
           let f = 7.0f32;
           let inf = f32::INFINITY;
           let neg_inf = f32::NEG_INFINITY;
           let nan = f32::NAN;

           assert!(!f.is_infinite());
           assert!(!nan.is_infinite());

           assert!(inf.is_infinite());
           assert!(neg_inf.is_infinite());
           ```
        ]} *)
    val f32_is_infinite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is NaN.

           ```
           let nan = f32::NAN;
           let f = 7.0_f32;

           assert!(nan.is_nan());
           assert!(!f.is_nan());
           ```
        ]} *)
    val f32_is_nan :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is neither zero, infinite,
           [subnormal], or NaN.

           ```
           let min = f32::MIN_POSITIVE; // 1.17549435e-38f32
           let max = f32::MAX;
           let lower_than_min = 1.0e-40_f32;
           let zero = 0.0_f32;

           assert!(min.is_normal());
           assert!(max.is_normal());

           assert!(!zero.is_normal());
           assert!(!f32::NAN.is_normal());
           assert!(!f32::INFINITY.is_normal());
           // Values between `0` and `min` are Subnormal.
           assert!(!lower_than_min.is_normal());
           ```
           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f32_is_normal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a negative sign, including `-0.0`, NaNs with
           negative sign bit and negative infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_negative` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == -1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           let f = 7.0f32;
           let g = -7.0f32;

           assert!(!f.is_sign_negative());
           assert!(g.is_sign_negative());
           ```
        ]} *)
    val f32_is_sign_negative :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a positive sign, including `+0.0`, NaNs with
           positive sign bit and positive infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_positive` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == 1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           let f = 7.0_f32;
           let g = -7.0_f32;

           assert!(f.is_sign_positive());
           assert!(!g.is_sign_positive());
           ```
        ]} *)
    val f32_is_sign_positive :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is [subnormal].

           ```
           let min = f32::MIN_POSITIVE; // 1.17549435e-38f32
           let max = f32::MAX;
           let lower_than_min = 1.0e-40_f32;
           let zero = 0.0_f32;

           assert!(!min.is_subnormal());
           assert!(!max.is_subnormal());

           assert!(!zero.is_subnormal());
           assert!(!f32::NAN.is_subnormal());
           assert!(!f32::INFINITY.is_subnormal());
           // Values between `0` and `min` are Subnormal.
           assert!(lower_than_min.is_subnormal());
           ```
           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f32_is_subnormal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this number is neither infinite nor NaN.

           ```
           let f = 7.0f64;
           let inf: f64 = f64::INFINITY;
           let neg_inf: f64 = f64::NEG_INFINITY;
           let nan: f64 = f64::NAN;

           assert!(f.is_finite());

           assert!(!nan.is_finite());
           assert!(!inf.is_finite());
           assert!(!neg_inf.is_finite());
           ```
        ]} *)
    val f64_is_finite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is positive infinity or negative infinity, and
           `false` otherwise.

           ```
           let f = 7.0f64;
           let inf = f64::INFINITY;
           let neg_inf = f64::NEG_INFINITY;
           let nan = f64::NAN;

           assert!(!f.is_infinite());
           assert!(!nan.is_infinite());

           assert!(inf.is_infinite());
           assert!(neg_inf.is_infinite());
           ```
        ]} *)
    val f64_is_infinite :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if this value is NaN.

           ```
           let nan = f64::NAN;
           let f = 7.0_f64;

           assert!(nan.is_nan());
           assert!(!f.is_nan());
           ```
        ]} *)
    val f64_is_nan :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is neither zero, infinite,
           [subnormal], or NaN.

           ```
           let min = f64::MIN_POSITIVE; // 2.2250738585072014e-308f64
           let max = f64::MAX;
           let lower_than_min = 1.0e-308_f64;
           let zero = 0.0f64;

           assert!(min.is_normal());
           assert!(max.is_normal());

           assert!(!zero.is_normal());
           assert!(!f64::NAN.is_normal());
           assert!(!f64::INFINITY.is_normal());
           // Values between `0` and `min` are Subnormal.
           assert!(!lower_than_min.is_normal());
           ```
           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f64_is_normal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a negative sign, including `-0.0`, NaNs with
           negative sign bit and negative infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_negative` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == -1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           let f = 7.0_f64;
           let g = -7.0_f64;

           assert!(!f.is_sign_negative());
           assert!(g.is_sign_negative());
           ```
        ]} *)
    val f64_is_sign_negative :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if `self` has a positive sign, including `+0.0`, NaNs with
           positive sign bit and positive infinity.

           Note that IEEE 754 doesn't assign any meaning to the sign bit in case of
           a NaN, and as Rust doesn't guarantee that the bit pattern of NaNs are
           conserved over arithmetic operations, the result of `is_sign_positive` on
           a NaN might produce an unexpected or non-portable result. See the [specification
           of NaN bit patterns](f32#nan-bit-patterns) for more info. Use `self.signum() == 1.0`
           if you need fully portable behavior (will return `false` for all NaNs).

           ```
           let f = 7.0_f64;
           let g = -7.0_f64;

           assert!(f.is_sign_positive());
           assert!(!g.is_sign_positive());
           ```
        ]} *)
    val f64_is_sign_positive :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns `true` if the number is [subnormal].

           ```
           let min = f64::MIN_POSITIVE; // 2.2250738585072014e-308_f64
           let max = f64::MAX;
           let lower_than_min = 1.0e-308_f64;
           let zero = 0.0_f64;

           assert!(!min.is_subnormal());
           assert!(!max.is_subnormal());

           assert!(!zero.is_subnormal());
           assert!(!f64::NAN.is_subnormal());
           assert!(!f64::INFINITY.is_subnormal());
           // Values between `0` and `min` are Subnormal.
           assert!(lower_than_min.is_subnormal());
           ```
           [subnormal]: https://en.wikipedia.org/wiki/Denormal_number
        ]} *)
    val f64_is_subnormal :
      arg:[< Typed.T.sfloat ] Typed.t -> Typed.T.sbool Typed.t ret

    val option_unwrap_failed : unit -> unit ret

    val assert_failed_inner :
      kind:rust_val ->
      left:full_ptr ->
      right:full_ptr ->
      args:rust_val ->
      unit ret

    (** {@markdown[
          The underlying implementation of core's `panic!` macro when no formatting is used.
        ]} *)
    val panic : expr:full_ptr -> unit ret

    (** {@markdown[
          The entry point for panicking with a formatted message.

           This is designed to reduce the amount of code required at the call
           site as much as possible (so that `panic!()` has as low an impact
           on (e.g.) the inlining of other functions as possible), by moving
           the actual formatting into this shared place.
        ]} *)
    val panic_fmt : fmt:rust_val -> unit ret

    (** {@markdown[
          Like `panic_fmt`, but for non-unwinding panics.

           Has to be a separate function so that it can carry the `rustc_nounwind` attribute.
        ]} *)
    val panic_nounwind_fmt :
      fmt:rust_val -> force_no_backtrace:[< Typed.T.sbool ] Typed.t -> unit ret

    val result_unwrap_failed : msg:full_ptr -> error:full_ptr -> unit ret
    val _eprint : args:rust_val -> unit ret
    val _print : args:rust_val -> unit ret

    val print_to :
      t:Types.ty ->
      args:rust_val ->
      global_s:full_ptr ->
      label:full_ptr ->
      unit ret

    val print_to_buffer_if_capture_used :
      args:rust_val -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          This is the entry point of panicking for the non-format-string variants of
           panic!() and assert!(). In particular, this is the only entry point that supports
           arbitrary payloads, not just format strings.
        ]} *)
    val panicking_begin_panic : m:Types.ty -> msg:rust_val -> unit ret

    val rt_begin_panic :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret
  end

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

  let fn_map : fn NameMatcherMap.t =
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
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  (* BEGIN USER IMPLEMENTATION *)
  module Impl : Intf = struct
    open Typed.Infix
    open Typed.Syntax
    module Soteria_lib = Soteria_lib.M (StateM)
    module Alloc = Alloc.M (StateM)

    let do_panic ?msg () =
      match msg with
      | Some msg ->
          let* msg = Soteria_lib.parse_string msg in
          error (`Panic msg)
      | _ -> error (`Panic None)

    (* ---- alloc ---- *)

    let alloc_impl ~self:_ ~layout ~zeroed =
      let zeroed = (zeroed :> Typed.T.sbool Typed.t) in
      let zero = Usize.(0s) in
      let size, align =
        match layout with
        | Tuple [ Int size; Tuple [ Enum (align, []) ] ] ->
            (Typed.cast_i Usize size, Typed.cast_i Usize align)
        | _ -> Fmt.failwith "alloc_impl: invalid layout: %a" pp_rust_val layout
      in
      let mk_res ptr len = Enum (zero, [ Tuple [ Ptr (ptr, Len len) ] ]) in
      if%sat size ==@ zero then
        let dangling = Sptr.null_ptr_of align in
        ok (mk_res dangling zero)
      else
        let* zeroed = if%sat zeroed then ok true else ok false in
        let+ ptr = Alloc.alloc ~zeroed [ Int size; Int align ] in
        let ptr =
          match ptr with Ptr (p, _) -> p | _ -> failwith "Expected Ptr"
        in
        mk_res ptr size

    let handle_alloc_error ~layout:_ = do_panic ()
    let handle_error ~e:_ = do_panic ()

    (* ---- float helpers ---- *)

    let float_is (fp : Svalue.FloatClass.t) =
      match fp with
      | Zero -> Typed.Float.is_zero
      | NaN -> Typed.Float.is_nan
      | Normal -> Typed.Float.is_normal
      | Infinite -> Typed.Float.is_infinite
      | Subnormal -> Typed.Float.is_subnormal

    let float_is_finite v =
      Typed.((not (Float.is_nan v)) &&@ not (Float.is_infinite v))

    let float_is_sign sign v =
      let res =
        match sign with
        | `Pos -> Typed.Float.(leq (like v 0.) v)
        | `Neg -> Typed.Float.(leq v (like v (-0.)))
      in
      Typed.(res ||@ Float.is_nan v)

    (* ---- f16 ----- *)

    let f16_is_finite ~arg = ok (float_is_finite arg)
    let f16_is_infinite ~arg = ok (float_is Infinite arg)
    let f16_is_nan ~arg = ok (float_is NaN arg)
    let f16_is_normal ~arg = ok (float_is Normal arg)
    let f16_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
    let f16_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
    let f16_is_subnormal ~arg = ok (float_is Subnormal arg)

    (* ---- f32 ---- *)

    let f32_is_finite ~arg = ok (float_is_finite arg)
    let f32_is_infinite ~arg = ok (float_is Infinite arg)
    let f32_is_nan ~arg = ok (float_is NaN arg)
    let f32_is_normal ~arg = ok (float_is Normal arg)
    let f32_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
    let f32_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
    let f32_is_subnormal ~arg = ok (float_is Subnormal arg)

    (* ---- f64 ---- *)

    let f64_is_finite ~arg = ok (float_is_finite arg)
    let f64_is_infinite ~arg = ok (float_is Infinite arg)
    let f64_is_nan ~arg = ok (float_is NaN arg)
    let f64_is_normal ~arg = ok (float_is Normal arg)
    let f64_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
    let f64_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
    let f64_is_subnormal ~arg = ok (float_is Subnormal arg)

    (* ---- f128 ---- *)

    let f128_is_finite ~arg = ok (float_is_finite arg)
    let f128_is_infinite ~arg = ok (float_is Infinite arg)
    let f128_is_nan ~arg = ok (float_is NaN arg)
    let f128_is_normal ~arg = ok (float_is Normal arg)
    let f128_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
    let f128_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
    let f128_is_subnormal ~arg = ok (float_is Subnormal arg)

    (* ---- panics ---- *)

    let option_unwrap_failed () = do_panic ()
    let result_unwrap_failed ~msg:_ ~error:_ = do_panic ()

    let assert_failed_inner ~kind:_ ~left:_ ~right:_ ~args:_ =
      error (`FailedAssert None)

    let panic ~expr = do_panic ~msg:expr ()
    let panic_fmt ~fmt:_ = do_panic ()
    let panic_nounwind_fmt ~fmt:_ ~force_no_backtrace:_ = do_panic ()
    let panicking_begin_panic ~m:_ ~msg:_ = do_panic ()

    let rt_begin_panic ~fun_exec:_ ~types:_ ~consts:_ ~args =
      match args with Ptr msg :: _ -> do_panic ~msg () | _ -> do_panic ()

    (* ---- I/O (no-ops) ---- *)

    let _eprint ~args:_ = ok ()
    let _print ~args:_ = ok ()
    let print_to ~t:_ ~args:_ ~global_s:_ ~label:_ = ok ()
    let print_to_buffer_if_capture_used ~args:_ = ok Typed.v_true
  end
  (* END USER IMPLEMENTATION *)

  let fn_of_stub stub _fun_exec (generics : Charon.Types.generic_args) args =
    match (stub, generics.types, generics.const_generics, args) with
    | AllocAllocAllocImpl, [], [], [ self; layout; zeroed ] ->
        let self = as_ptr self in
        let zeroed = Typed.BitVec.to_bool (as_base TBool zeroed) in
        Impl.alloc_impl ~self ~layout ~zeroed
    | AllocAllocHandleAllocError, [], [], [ layout ] ->
        let+ () = Impl.handle_alloc_error ~layout in
        Tuple []
    | AllocRawVecHandleError, [], [], [ e ] ->
        let+ () = Impl.handle_error ~e in
        Tuple []
    | CoreF128IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsNan, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF128IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F128 arg in
        let+ ret = Impl.f128_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsNan, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF16IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F16 arg in
        let+ ret = Impl.f16_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsNan, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF32IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F32 arg in
        let+ ret = Impl.f32_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsFinite, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_finite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsInfinite, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_infinite ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsNan, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_nan ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsNormal, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_normal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsSignNegative, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_sign_negative ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsSignPositive, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_sign_positive ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreF64IsSubnormal, [], [], [ arg ] ->
        let arg = as_base_f F64 arg in
        let+ ret = Impl.f64_is_subnormal ~arg in
        Int (Typed.BitVec.of_bool ret)
    | CoreOptionUnwrapFailed, [], [], [] ->
        let+ () = Impl.option_unwrap_failed () in
        Tuple []
    | CorePanickingAssertFailedInner, [], [], [ kind; left; right; args ] ->
        let left = as_ptr left in
        let right = as_ptr right in
        let+ () = Impl.assert_failed_inner ~kind ~left ~right ~args in
        Tuple []
    | CorePanickingPanic, [], [], [ expr ] ->
        let expr = as_ptr expr in
        let+ () = Impl.panic ~expr in
        Tuple []
    | CorePanickingPanicFmt, [], [], [ fmt ] ->
        let+ () = Impl.panic_fmt ~fmt in
        Tuple []
    | CorePanickingPanicNounwindFmt, [], [], [ fmt; force_no_backtrace ] ->
        let force_no_backtrace =
          Typed.BitVec.to_bool (as_base TBool force_no_backtrace)
        in
        let+ () = Impl.panic_nounwind_fmt ~fmt ~force_no_backtrace in
        Tuple []
    | CoreResultUnwrapFailed, [], [], [ msg; error ] ->
        let msg = as_ptr msg in
        let error = as_ptr error in
        let+ () = Impl.result_unwrap_failed ~msg ~error in
        Tuple []
    | StdIoStdioEprint, [], [], [ args ] ->
        let+ () = Impl._eprint ~args in
        Tuple []
    | StdIoStdioPrint, [], [], [ args ] ->
        let+ () = Impl._print ~args in
        Tuple []
    | StdIoStdioPrintTo, [ t ], [], [ args; global_s; label ] ->
        let global_s = as_ptr global_s in
        let label = as_ptr label in
        let+ () = Impl.print_to ~t ~args ~global_s ~label in
        Tuple []
    | StdIoStdioPrintToBufferIfCaptureUsed, [], [], [ args ] ->
        let+ ret = Impl.print_to_buffer_if_capture_used ~args in
        Int (Typed.BitVec.of_bool ret)
    | StdPanickingBeginPanic, [ m ], [], [ msg ] ->
        let+ () = Impl.panicking_begin_panic ~m ~msg in
        Tuple []
    | StdRtBeginPanic, _, _, _ ->
        Impl.rt_begin_panic ~fun_exec:_fun_exec ~types:generics.types
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

  let eval_fun (f : UllbcAst.fun_decl) (fun_exec : fun_exec)
      (generics : Charon.Types.generic_args) =
    let ctx = Crate.as_namematcher_ctx () in
    let stub =
      NameMatcherMap.find_opt ctx match_config f.item_meta.name fn_map
    in
    Option.map (fun stub -> fn_of_stub stub fun_exec generics) stub
end
