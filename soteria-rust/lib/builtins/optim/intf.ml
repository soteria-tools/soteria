(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Charon
open Common

module M (StateM : State.StateM.S) = struct
  open StateM

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  module type S = sig
    val _eprint : args:rust_val -> unit ret

    val alloc_impl :
      self:full_ptr ->
      layout:rust_val ->
      zeroed:[< Typed.T.sbool ] Typed.t ->
      rust_val ret

    val assert_failed_inner :
      kind:rust_val ->
      left:full_ptr ->
      right:full_ptr ->
      args:rust_val ->
      unit ret

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
    val option_unwrap_failed : unit -> unit ret

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

    (** {@markdown[
          This is the entry point of panicking for the non-format-string variants of
           panic!() and assert!(). In particular, this is the only entry point that supports
           arbitrary payloads, not just format strings.
        ]} *)
    val panicking_begin_panic : m:Types.ty -> msg:rust_val -> unit ret

    val print_to :
      t:Types.ty ->
      args:rust_val ->
      global_s:full_ptr ->
      label:full_ptr ->
      unit ret

    val print_to_buffer_if_capture_used :
      args:rust_val -> Typed.T.sbool Typed.t ret

    val result_unwrap_failed : msg:full_ptr -> error:full_ptr -> unit ret
    val rt_begin_panic : args:rust_val list -> rust_val ret
    val stdio__print : args:rust_val -> unit ret
  end
end
