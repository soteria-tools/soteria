(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

open Charon
open Rustsymex

module M (State : State_intf.S) = struct
  module type Impl = sig
    type rust_val := State.Sptr.t Rust_val.t

    type 'a ret :=
      unit ->
      State.t ->
      ( 'a * unit * State.t,
        Error.t State.err * State.t,
        State.serialized )
      Result.t

    type fun_exec :=
      UllbcAst.fun_decl ->
      rust_val list ->
      unit ->
      State.t ->
      ( rust_val * unit * State.t,
        Error.t State.err * State.t,
        State.serialized )
      Result.t

    type full_ptr := State.Sptr.t Rust_val.full_ptr

    (** {@markdown[
          Aborts the execution of the process.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           [`std::process::abort`](../../std/process/fn.abort.html) is to be preferred if possible,
           as its behavior is more user-friendly and more stable.

           The current implementation of `intrinsics::abort` is to invoke an invalid instruction,
           on most platforms.
           On Unix, the
           process will probably terminate with a signal like `SIGABRT`, `SIGILL`, `SIGTRAP`, `SIGSEGV` or
           `SIGBUS`.  The precise behavior is not guaranteed and not stable.
        ]} *)
    val abort : unit ret

    (** {@markdown[
          Performs checked integer addition.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `overflowing_add` method. For example,
           [`u32::overflowing_add`]
        ]} *)
    val add_with_overflow :
      t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Lowers in MIR to `Rvalue::Aggregate` with `AggregateKind::RawPtr`.

           This is used to implement functions like `slice::from_raw_parts_mut` and
           `ptr::from_raw_parts` in a way compatible with the compiler being able to
           change the possible layouts of pointers.
        ]} *)
    val aggregate_raw_ptr :
      p:Types.ty ->
      d:Types.ty ->
      m:Types.ty ->
      data:rust_val ->
      meta:rust_val ->
      rust_val ret

    (** {@markdown[
          The minimum alignment of a type.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           Note that, unlike most intrinsics, this can only be called at compile-time
           as backends do not have an implementation for it. The only caller (its
           stable counterpart) wraps this intrinsic call in a `const` block so that
           backends only see an evaluated constant.

           The stabilized version of this intrinsic is [`core::mem::align_of`].
        ]} *)
    val align_of : t:Types.ty -> Typed.T.sint Typed.t ret

    (** {@markdown[
          The required alignment of the referenced value.

           The stabilized version of this intrinsic is [`core::mem::align_of_val`].

           # Safety

           See [`crate::mem::align_of_val_raw`] for safety conditions.
        ]} *)
    val align_of_val : t:Types.ty -> ptr:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Calculates the offset from a pointer, potentially wrapping.

           This is implemented as an intrinsic to avoid converting to and from an
           integer, since the conversion inhibits certain optimizations.

           # Safety

           Unlike the `offset` intrinsic, this intrinsic does not restrict the
           resulting pointer to point into or at the end of an allocated
           object, and it wraps with two's complement arithmetic. The resulting
           value is not necessarily valid to be used to actually access memory.

           The stabilized version of this intrinsic is [`pointer::wrapping_offset`].
        ]} *)
    val arith_offset :
      t:Types.ty ->
      dst:full_ptr ->
      offset:[< Typed.T.sint ] Typed.t ->
      full_ptr ret

    (** {@markdown[
          A guard for unsafe functions that cannot ever be executed if `T` is uninhabited:
           This will statically either panic, or do nothing. It does not *guarantee* to ever panic,
           and should only be called if an assertion failure will imply language UB in the following code.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val assert_inhabited : t:Types.ty -> unit ret

    (** {@markdown[
          A guard for `std::mem::uninitialized`. This will statically either panic, or do nothing. It does
           not *guarantee* to ever panic, and should only be called if an assertion failure will imply
           language UB in the following code.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val assert_mem_uninitialized_valid : t:Types.ty -> unit ret

    (** {@markdown[
          A guard for unsafe functions that cannot ever be executed if `T` does not permit
           zero-initialization: This will statically either panic, or do nothing. It does not *guarantee*
           to ever panic, and should only be called if an assertion failure will imply language UB in the
           following code.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val assert_zero_valid : t:Types.ty -> unit ret

    (** {@markdown[
          Informs the optimizer that a condition is always true.
           If the condition is false, the behavior is undefined.

           No code is generated for this intrinsic, but the optimizer will try
           to preserve it (and its condition) between passes, which may interfere
           with optimization of surrounding code and reduce performance. It should
           not be used if the invariant can be discovered by the optimizer on its
           own, or if it does not enable any significant optimizations.

           The stabilized version of this intrinsic is [`core::hint::assert_unchecked`].
        ]} *)
    val assume : b:[< Typed.T.sbool ] Typed.t -> unit ret

    (** {@markdown[
          Generates the LLVM body for the automatic differentiation of `f` using Enzyme,
           with `df` as the derivative function and `args` as its arguments.

           Used internally as the body of `df` when expanding the `#[autodiff_forward]`
           and `#[autodiff_reverse]` attribute macros.

           Type Parameters:
           - `F`: The original function to differentiate. Must be a function item.
           - `G`: The derivative function. Must be a function item.
           - `T`: A tuple of arguments passed to `df`.
           - `R`: The return type of the derivative function.

           This shows where the `autodiff` intrinsic is used during macro expansion:

           ```rust,ignore (macro example)
           #[autodiff_forward(df1, Dual, Const, Dual)]
           pub fn f1(x: &[f64], y: f64) -> f64 {
               unimplemented!()
           }
           ```

           expands to:

           ```rust,ignore (macro example)
           #[rustc_autodiff]
           #[inline(never)]
           pub fn f1(x: &[f64], y: f64) -> f64 {
               ::core::panicking::panic("not implemented")
           }
           #[rustc_autodiff(Forward, 1, Dual, Const, Dual)]
           pub fn df1(x: &[f64], bx_0: &[f64], y: f64) -> (f64, f64) {
               ::core::intrinsics::autodiff(f1::<>, df1::<>, (x, bx_0, y))
           }
           ```
        ]} *)
    val autodiff :
      t_f:Types.ty ->
      g:Types.ty ->
      t:Types.ty ->
      r:Types.ty ->
      f:rust_val ->
      df:rust_val ->
      args:rust_val ->
      rust_val ret

    (** {@markdown[
          Reverses the bits in an integer type `T`.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `reverse_bits` method. For example,
           [`u32::reverse_bits`]
        ]} *)
    val bitreverse : t:Types.ty -> x:rust_val -> rust_val ret

    (** {@markdown[
          See documentation of [`std::hint::black_box`] for details.

           [`std::hint::black_box`]: crate::hint::black_box
        ]} *)
    val black_box : t:Types.ty -> dummy:rust_val -> rust_val ret

    (** {@markdown[
          Executes a breakpoint trap, for inspection by a debugger.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val breakpoint : unit ret

    (** {@markdown[
          Reverses the bytes in an integer type `T`.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `swap_bytes` method. For example,
           [`u32::swap_bytes`]
        ]} *)
    val bswap : t:Types.ty -> x:rust_val -> rust_val ret

    (** {@markdown[
          Gets a reference to a static `Location` indicating where it was called.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           Consider using [`core::panic::Location::caller`] instead.
        ]} *)
    val caller_location : full_ptr ret

    (** {@markdown[
          Performs full-width multiplication and addition with a carry:
           `multiplier * multiplicand + addend + carry`.

           This is possible without any overflow.  For `uN`:
              MAX * MAX + MAX + MAX
           => (2ⁿ-1) × (2ⁿ-1) + (2ⁿ-1) + (2ⁿ-1)
           => (2²ⁿ - 2ⁿ⁺¹ + 1) + (2ⁿ⁺¹ - 2)
           => 2²ⁿ - 1

           For `iN`, the upper bound is MIN * MIN + MAX + MAX => 2²ⁿ⁻² + 2ⁿ - 2,
           and the lower bound is MAX * MIN + MIN + MIN => -2²ⁿ⁻² - 2ⁿ + 2ⁿ⁺¹.

           This currently supports unsigned integers *only*, no signed ones.
           The stabilized versions of this intrinsic are available on integers.
        ]} *)
    val carrying_mul_add :
      t:Types.ty ->
      u:Types.ty ->
      multiplier:rust_val ->
      multiplicand:rust_val ->
      addend:rust_val ->
      carry:rust_val ->
      rust_val ret

    (** {@markdown[
          Rust's "try catch" construct for unwinding. Invokes the function pointer `try_fn` with the
           data pointer `data`, and calls `catch_fn` if unwinding occurs while `try_fn` runs.
           Returns `1` if unwinding occurred and `catch_fn` was called; returns `0` otherwise.

           `catch_fn` must not unwind.

           The third argument is a function called if an unwind occurs (both Rust `panic` and foreign
           unwinds). This function takes the data pointer and a pointer to the target- and
           runtime-specific exception object that was caught.

           Note that in the case of a foreign unwinding operation, the exception object data may not be
           safely usable from Rust, and should not be directly exposed via the standard library. To
           prevent unsafe access, the library implementation may either abort the process or present an
           opaque error type to the user.

           For more information, see the compiler's source, as well as the documentation for the stable
           version of this intrinsic, `std::panic::catch_unwind`.
        ]} *)
    val catch_unwind :
      fun_exec ->
      _try_fn:rust_val ->
      _data:full_ptr ->
      _catch_fn:rust_val ->
      Typed.T.sint Typed.t ret

    (** {@markdown[
          Returns the smallest integer greater than or equal to an `f128`.

           The stabilized version of this intrinsic is
           [`f128::ceil`](../../std/primitive.f128.html#method.ceil)
        ]} *)
    val ceilf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the smallest integer greater than or equal to an `f16`.

           The stabilized version of this intrinsic is
           [`f16::ceil`](../../std/primitive.f16.html#method.ceil)
        ]} *)
    val ceilf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the smallest integer greater than or equal to an `f32`.

           The stabilized version of this intrinsic is
           [`f32::ceil`](../../std/primitive.f32.html#method.ceil)
        ]} *)
    val ceilf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the smallest integer greater than or equal to an `f64`.

           The stabilized version of this intrinsic is
           [`f64::ceil`](../../std/primitive.f64.html#method.ceil)
        ]} *)
    val ceilf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Hints to the compiler that current code path is cold.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val cold_path : unit ret

    (** {@markdown[
          Lexicographically compare `[left, left + bytes)` and `[right, right + bytes)`
           as unsigned bytes, returning negative if `left` is less, zero if all the
           bytes match, or positive if `left` is greater.

           This underlies things like `<[u8]>::cmp`, and will usually lower to `memcmp`.

           # Safety

           `left` and `right` must each be [valid] for reads of `bytes` bytes.

           Note that this applies to the whole range, not just until the first byte
           that differs.  That allows optimizations that can read in large chunks.

           [valid]: crate::ptr#safety
        ]} *)
    val compare_bytes :
      left:full_ptr ->
      right:full_ptr ->
      bytes:[< Typed.T.sint ] Typed.t ->
      Typed.T.sint Typed.t ret

    (** {@markdown[
          Deallocates a memory which allocated by `intrinsics::const_allocate` at compile time.
           At runtime, does nothing.

           # Safety

           - The `align` argument must be a power of two.
              - At compile time, a compile error occurs if this constraint is violated.
              - At runtime, it is not checked.
           - If the `ptr` is created in an another const, this intrinsic doesn't deallocate it.
           - If the `ptr` is pointing to a local variable, this intrinsic doesn't deallocate it.
        ]} *)
    val const_deallocate :
      _ptr:full_ptr ->
      _size:[< Typed.T.sint ] Typed.t ->
      _align:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          Selects which function to call depending on the context.

           If this function is evaluated at compile-time, then a call to this
           intrinsic will be replaced with a call to `called_in_const`. It gets
           replaced with a call to `called_at_rt` otherwise.

           This function is safe to call, but note the stability concerns below.

           # Type Requirements

           The two functions must be both function items. They cannot be function
           pointers or closures. The first function must be a `const fn`.

           `arg` will be the tupled arguments that will be passed to either one of
           the two functions, therefore, both functions must accept the same type of
           arguments. Both functions must return RET.

           # Stability concerns

           Rust has not yet decided that `const fn` are allowed to tell whether
           they run at compile-time or at runtime. Therefore, when using this
           intrinsic anywhere that can be reached from stable, it is crucial that
           the end-to-end behavior of the stable `const fn` is the same for both
           modes of execution. (Here, Undefined Behavior is considered "the same"
           as any other behavior, so if the function exhibits UB at runtime then
           it may do whatever it wants at compile-time.)

           Here is an example of how this could cause a problem:
           ```no_run
           #![feature(const_eval_select)]
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]
           use std::intrinsics::const_eval_select;

           // Standard library
           pub const fn inconsistent() -> i32 {
               fn runtime() -> i32 { 1 }
               const fn compiletime() -> i32 { 2 }

               // ⚠ This code violates the required equivalence of `compiletime`
               // and `runtime`.
               const_eval_select((), compiletime, runtime)
           }

           // User Crate
           const X: i32 = inconsistent();
           let x = inconsistent();
           assert_eq!(x, X);
           ```

           Currently such an assertion would always succeed; until Rust decides
           otherwise, that principle should not be violated.
        ]} *)
    val const_eval_select :
      arg:Types.ty ->
      f:Types.ty ->
      g:Types.ty ->
      ret:Types.ty ->
      _arg:rust_val ->
      _called_in_const:rust_val ->
      _called_at_rt:rust_val ->
      rust_val ret

    (** {@markdown[

]} *)
    val const_make_global : ptr:full_ptr -> full_ptr ret

    (** {@markdown[
          Check if the post-condition `cond` has been met.

           By default, if `contract_checks` is enabled, this will panic with no unwind if the condition
           returns false.

           If `cond` is `None`, then no postcondition checking is performed.

           Note that this function is a no-op during constant evaluation.
        ]} *)
    val contract_check_ensures :
      c:Types.ty ->
      t_ret:Types.ty ->
      cond:rust_val ->
      ret:rust_val ->
      rust_val ret

    (** {@markdown[
          Check if the pre-condition `cond` has been met.

           By default, if `contract_checks` is enabled, this will panic with no unwind if the condition
           returns false.

           Note that this function is a no-op during constant evaluation.
        ]} *)
    val contract_check_requires : c:Types.ty -> cond:rust_val -> unit ret

    (** {@markdown[
          This is an accidentally-stable alias to [`ptr::copy`]; use that instead.
        ]} *)
    val copy :
      t:Types.ty ->
      src:full_ptr ->
      dst:full_ptr ->
      count:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          This is an accidentally-stable alias to [`ptr::copy_nonoverlapping`]; use that instead.
        ]} *)
    val copy_nonoverlapping :
      t:Types.ty ->
      src:full_ptr ->
      dst:full_ptr ->
      count:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          Copies the sign from `y` to `x` for `f128` values.

           The stabilized version of this intrinsic is
           [`f128::copysign`](../../std/primitive.f128.html#method.copysign)
        ]} *)
    val copysignf128 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Copies the sign from `y` to `x` for `f16` values.

           The stabilized version of this intrinsic is
           [`f16::copysign`](../../std/primitive.f16.html#method.copysign)
        ]} *)
    val copysignf16 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Copies the sign from `y` to `x` for `f32` values.

           The stabilized version of this intrinsic is
           [`f32::copysign`](../../std/primitive.f32.html#method.copysign)
        ]} *)
    val copysignf32 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Copies the sign from `y` to `x` for `f64` values.

           The stabilized version of this intrinsic is
           [`f64::copysign`](../../std/primitive.f64.html#method.copysign)
        ]} *)
    val copysignf64 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the cosine of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::cos`](../../std/primitive.f128.html#method.cos)
        ]} *)
    val cosf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the cosine of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::cos`](../../std/primitive.f16.html#method.cos)
        ]} *)
    val cosf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the cosine of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::cos`](../../std/primitive.f32.html#method.cos)
        ]} *)
    val cosf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the cosine of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::cos`](../../std/primitive.f64.html#method.cos)
        ]} *)
    val cosf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the number of leading unset bits (zeroes) in an integer type `T`.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `leading_zeros` method. For example,
           [`u32::leading_zeros`]

           # Examples

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]

           use std::intrinsics::ctlz;

           let x = 0b0001_1100_u8;
           let num_leading = ctlz(x);
           assert_eq!(num_leading, 3);
           ```

           An `x` with value `0` will return the bit width of `T`.

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]

           use std::intrinsics::ctlz;

           let x = 0u16;
           let num_leading = ctlz(x);
           assert_eq!(num_leading, 16);
           ```
        ]} *)
    val ctlz : t:Types.ty -> x:rust_val -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Like `ctlz`, but extra-unsafe as it returns `undef` when
           given an `x` with value `0`.

           This intrinsic does not have a stable counterpart.

           # Examples

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]

           use std::intrinsics::ctlz_nonzero;

           let x = 0b0001_1100_u8;
           let num_leading = unsafe { ctlz_nonzero(x) };
           assert_eq!(num_leading, 3);
           ```
        ]} *)
    val ctlz_nonzero : t:Types.ty -> x:rust_val -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Returns the number of bits set in an integer type `T`

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `count_ones` method. For example,
           [`u32::count_ones`]
        ]} *)
    val ctpop : t:Types.ty -> x:rust_val -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Returns the number of trailing unset bits (zeroes) in an integer type `T`.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `trailing_zeros` method. For example,
           [`u32::trailing_zeros`]

           # Examples

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]

           use std::intrinsics::cttz;

           let x = 0b0011_1000_u8;
           let num_trailing = cttz(x);
           assert_eq!(num_trailing, 3);
           ```

           An `x` with value `0` will return the bit width of `T`:

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]

           use std::intrinsics::cttz;

           let x = 0u16;
           let num_trailing = cttz(x);
           assert_eq!(num_trailing, 16);
           ```
        ]} *)
    val cttz : t:Types.ty -> x:rust_val -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Like `cttz`, but extra-unsafe as it returns `undef` when
           given an `x` with value `0`.

           This intrinsic does not have a stable counterpart.

           # Examples

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]

           use std::intrinsics::cttz_nonzero;

           let x = 0b0011_1000_u8;
           let num_trailing = unsafe { cttz_nonzero(x) };
           assert_eq!(num_trailing, 3);
           ```
        ]} *)
    val cttz_nonzero : t:Types.ty -> x:rust_val -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Returns the value of the discriminant for the variant in 'v';
           if `T` has no discriminant, returns `0`.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is [`core::mem::discriminant`].
        ]} *)
    val discriminant_value : t:Types.ty -> v:full_ptr -> rust_val ret

    (** {@markdown[
          Combine two values which have no bits in common.

           This allows the backend to implement it as `a + b` *or* `a | b`,
           depending which is easier to implement on a specific target.

           # Safety

           Requires that `(a & b) == 0`, or equivalently that `(a | b) == (a + b)`.

           Otherwise it's immediate UB.
        ]} *)
    val disjoint_bitor : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Performs an exact division, resulting in undefined behavior where
           `x % y != 0` or `y == 0` or `x == T::MIN && y == -1`

           This intrinsic does not have a stable counterpart.
        ]} *)
    val exact_div : t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Returns 2 raised to the power of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::exp2`](../../std/primitive.f128.html#method.exp2)
        ]} *)
    val exp2f128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns 2 raised to the power of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::exp2`](../../std/primitive.f16.html#method.exp2)
        ]} *)
    val exp2f16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns 2 raised to the power of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::exp2`](../../std/primitive.f32.html#method.exp2)
        ]} *)
    val exp2f32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns 2 raised to the power of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::exp2`](../../std/primitive.f64.html#method.exp2)
        ]} *)
    val exp2f64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the exponential of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::exp`](../../std/primitive.f128.html#method.exp)
        ]} *)
    val expf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the exponential of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::exp`](../../std/primitive.f16.html#method.exp)
        ]} *)
    val expf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the exponential of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::exp`](../../std/primitive.f32.html#method.exp)
        ]} *)
    val expf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the exponential of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::exp`](../../std/primitive.f64.html#method.exp)
        ]} *)
    val expf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the absolute value of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::abs`](../../std/primitive.f128.html#method.abs)
        ]} *)
    val fabsf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the absolute value of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::abs`](../../std/primitive.f16.html#method.abs)
        ]} *)
    val fabsf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the absolute value of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::abs`](../../std/primitive.f32.html#method.abs)
        ]} *)
    val fabsf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the absolute value of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::abs`](../../std/primitive.f64.html#method.abs)
        ]} *)
    val fabsf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Float addition that allows optimizations based on algebraic rules.

           Stabilized as [`f16::algebraic_add`], [`f32::algebraic_add`], [`f64::algebraic_add`] and [`f128::algebraic_add`].
        ]} *)
    val fadd_algebraic : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float addition that allows optimizations based on algebraic rules.
           Requires that inputs and output of the operation are finite, causing UB otherwise.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val fadd_fast : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float division that allows optimizations based on algebraic rules.

           Stabilized as [`f16::algebraic_div`], [`f32::algebraic_div`], [`f64::algebraic_div`] and [`f128::algebraic_div`].
        ]} *)
    val fdiv_algebraic : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float division that allows optimizations based on algebraic rules.
           Requires that inputs and output of the operation are finite, causing UB otherwise.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val fdiv_fast : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Converts with LLVM’s fptoui/fptosi, which may return undef for values out of range
           (<https://github.com/rust-lang/rust/issues/10184>)

           Stabilized as [`f32::to_int_unchecked`] and [`f64::to_int_unchecked`].
        ]} *)
    val float_to_int_unchecked :
      float:Types.ty -> int:Types.ty -> value:rust_val -> rust_val ret

    (** {@markdown[
          Returns the largest integer less than or equal to an `f128`.

           The stabilized version of this intrinsic is
           [`f128::floor`](../../std/primitive.f128.html#method.floor)
        ]} *)
    val floorf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the largest integer less than or equal to an `f16`.

           The stabilized version of this intrinsic is
           [`f16::floor`](../../std/primitive.f16.html#method.floor)
        ]} *)
    val floorf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the largest integer less than or equal to an `f32`.

           The stabilized version of this intrinsic is
           [`f32::floor`](../../std/primitive.f32.html#method.floor)
        ]} *)
    val floorf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the largest integer less than or equal to an `f64`.

           The stabilized version of this intrinsic is
           [`f64::floor`](../../std/primitive.f64.html#method.floor)
        ]} *)
    val floorf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f128` values.

           The stabilized version of this intrinsic is
           [`f128::mul_add`](../../std/primitive.f128.html#method.mul_add)
        ]} *)
    val fmaf128 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f16` values.

           The stabilized version of this intrinsic is
           [`f16::mul_add`](../../std/primitive.f16.html#method.mul_add)
        ]} *)
    val fmaf16 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f32` values.

           The stabilized version of this intrinsic is
           [`f32::mul_add`](../../std/primitive.f32.html#method.mul_add)
        ]} *)
    val fmaf32 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f64` values.

           The stabilized version of this intrinsic is
           [`f64::mul_add`](../../std/primitive.f64.html#method.mul_add)
        ]} *)
    val fmaf64 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Float multiplication that allows optimizations based on algebraic rules.

           Stabilized as [`f16::algebraic_mul`], [`f32::algebraic_mul`], [`f64::algebraic_mul`] and [`f128::algebraic_mul`].
        ]} *)
    val fmul_algebraic : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float multiplication that allows optimizations based on algebraic rules.
           Requires that inputs and output of the operation are finite, causing UB otherwise.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val fmul_fast : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Returns `a * b + c` for `f128` values, non-deterministically executing
           either a fused multiply-add or two operations with rounding of the
           intermediate result.

           The operation is fused if the code generator determines that target
           instruction set has support for a fused operation, and that the fused
           operation is more efficient than the equivalent, separate pair of mul
           and add instructions. It is unspecified whether or not a fused operation
           is selected, and that may depend on optimization level and context, for
           example.
        ]} *)
    val fmuladdf128 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f16` values, non-deterministically executing
           either a fused multiply-add or two operations with rounding of the
           intermediate result.

           The operation is fused if the code generator determines that target
           instruction set has support for a fused operation, and that the fused
           operation is more efficient than the equivalent, separate pair of mul
           and add instructions. It is unspecified whether or not a fused operation
           is selected, and that may depend on optimization level and context, for
           example.
        ]} *)
    val fmuladdf16 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f32` values, non-deterministically executing
           either a fused multiply-add or two operations with rounding of the
           intermediate result.

           The operation is fused if the code generator determines that target
           instruction set has support for a fused operation, and that the fused
           operation is more efficient than the equivalent, separate pair of mul
           and add instructions. It is unspecified whether or not a fused operation
           is selected, and that may depend on optimization level and context, for
           example.
        ]} *)
    val fmuladdf32 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns `a * b + c` for `f64` values, non-deterministically executing
           either a fused multiply-add or two operations with rounding of the
           intermediate result.

           The operation is fused if the code generator determines that target
           instruction set has support for a fused operation, and that the fused
           operation is more efficient than the equivalent, separate pair of mul
           and add instructions. It is unspecified whether or not a fused operation
           is selected, and that may depend on optimization level and context, for
           example.
        ]} *)
    val fmuladdf64 :
      a:[< Typed.T.sfloat ] Typed.t ->
      b:[< Typed.T.sfloat ] Typed.t ->
      c:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Moves a value out of scope without running drop glue.

           This exists solely for [`crate::mem::forget_unsized`]; normal `forget` uses
           `ManuallyDrop` instead.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val forget : t:Types.ty -> arg:rust_val -> unit ret

    (** {@markdown[
          Float remainder that allows optimizations based on algebraic rules.

           Stabilized as [`f16::algebraic_rem`], [`f32::algebraic_rem`], [`f64::algebraic_rem`] and [`f128::algebraic_rem`].
        ]} *)
    val frem_algebraic : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float remainder that allows optimizations based on algebraic rules.
           Requires that inputs and output of the operation are finite, causing UB otherwise.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val frem_fast : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float subtraction that allows optimizations based on algebraic rules.

           Stabilized as [`f16::algebraic_sub`], [`f32::algebraic_sub`], [`f64::algebraic_sub`] and [`f128::algebraic_sub`].
        ]} *)
    val fsub_algebraic : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Float subtraction that allows optimizations based on algebraic rules.
           Requires that inputs and output of the operation are finite, causing UB otherwise.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val fsub_fast : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Returns whether the argument's value is statically known at
           compile-time.

           This is useful when there is a way of writing the code that will
           be *faster* when some variables have known values, but *slower*
           in the general case: an `if is_val_statically_known(var)` can be used
           to select between these two variants. The `if` will be optimized away
           and only the desired branch remains.

           Formally speaking, this function non-deterministically returns `true`
           or `false`, and the caller has to ensure sound behavior for both cases.
           In other words, the following code has *Undefined Behavior*:

           ```no_run
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]
           use std::hint::unreachable_unchecked;
           use std::intrinsics::is_val_statically_known;

           if !is_val_statically_known(0) { unsafe { unreachable_unchecked(); } }
           ```

           This also means that the following code's behavior is unspecified; it
           may panic, or it may not:

           ```no_run
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]
           use std::intrinsics::is_val_statically_known;

           assert_eq!(is_val_statically_known(0), is_val_statically_known(0));
           ```

           Unsafe code may not rely on `is_val_statically_known` returning any
           particular value, ever. However, the compiler will generally make it
           return `true` only if the value of the argument is actually known.

           # Stability concerns

           While it is safe to call, this intrinsic may behave differently in
           a `const` context than otherwise. See the [`const_eval_select()`]
           documentation for an explanation of the issues this can cause. Unlike
           `const_eval_select`, this intrinsic isn't guaranteed to behave
           deterministically even in a `const` context.

           # Type Requirements

           `T` must be either a `bool`, a `char`, a primitive numeric type (e.g. `f32`,
           but not `NonZeroISize`), or any thin pointer (e.g. `*mut String`).
           Any other argument types *may* cause a compiler error.

           ## Pointers

           When the input is a pointer, only the pointer itself is
           ever considered. The pointee has no effect. Currently, these functions
           behave identically:

           ```
           #![feature(core_intrinsics)]
           # #![allow(internal_features)]
           use std::intrinsics::is_val_statically_known;

           fn foo(x: &i32) -> bool {
               is_val_statically_known(x)
           }

           fn bar(x: &i32) -> bool {
               is_val_statically_known(
                   (x as *const i32).addr()
               )
           }
           # _ = foo(&5_i32);
           # _ = bar(&5_i32);
           ```
        ]} *)
    val is_val_statically_known :
      t:Types.ty -> _arg:rust_val -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Hints to the compiler that branch condition is likely to be true.
           Returns the value passed to it.

           Any use other than with `if` statements will probably not have an effect.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val likely : b:[< Typed.T.sbool ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Returns the base 10 logarithm of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::log10`](../../std/primitive.f128.html#method.log10)
        ]} *)
    val log10f128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 10 logarithm of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::log10`](../../std/primitive.f16.html#method.log10)
        ]} *)
    val log10f16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 10 logarithm of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::log10`](../../std/primitive.f32.html#method.log10)
        ]} *)
    val log10f32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 10 logarithm of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::log10`](../../std/primitive.f64.html#method.log10)
        ]} *)
    val log10f64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 2 logarithm of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::log2`](../../std/primitive.f128.html#method.log2)
        ]} *)
    val log2f128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 2 logarithm of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::log2`](../../std/primitive.f16.html#method.log2)
        ]} *)
    val log2f16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 2 logarithm of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::log2`](../../std/primitive.f32.html#method.log2)
        ]} *)
    val log2f32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the base 2 logarithm of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::log2`](../../std/primitive.f64.html#method.log2)
        ]} *)
    val log2f64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the natural logarithm of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::ln`](../../std/primitive.f128.html#method.ln)
        ]} *)
    val logf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the natural logarithm of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::ln`](../../std/primitive.f16.html#method.ln)
        ]} *)
    val logf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the natural logarithm of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::ln`](../../std/primitive.f32.html#method.ln)
        ]} *)
    val logf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the natural logarithm of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::ln`](../../std/primitive.f64.html#method.ln)
        ]} *)
    val logf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2019 maximum) of two `f128` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val maximumf128 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2019 maximum) of two `f16` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val maximumf16 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2019 maximum) of two `f32` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val maximumf32 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2019 maximum) of two `f64` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val maximumf64 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2008 maxNum) of two `f128` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f128::max`]
        ]} *)
    val maxnumf128 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2008 maxNum) of two `f16` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f16::max`]
        ]} *)
    val maxnumf16 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2008 maxNum) of two `f32` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f32::max`]
        ]} *)
    val maxnumf32 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the maximum (IEEE 754-2008 maxNum) of two `f64` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f64::max`]
        ]} *)
    val maxnumf64 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2019 minimum) of two `f128` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val minimumf128 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2019 minimum) of two `f16` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val minimumf16 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2019 minimum) of two `f32` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val minimumf32 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2019 minimum) of two `f64` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.
        ]} *)
    val minimumf64 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2008 minNum) of two `f128` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f128::min`]
        ]} *)
    val minnumf128 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2008 minNum) of two `f16` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f16::min`]
        ]} *)
    val minnumf16 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2008 minNum) of two `f32` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f32::min`]
        ]} *)
    val minnumf32 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the minimum (IEEE 754-2008 minNum) of two `f64` values.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized version of this intrinsic is
           [`f64::min`]
        ]} *)
    val minnumf64 :
      x:[< Typed.T.sfloat ] Typed.t ->
      y:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Performs checked integer multiplication

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `overflowing_mul` method. For example,
           [`u32::overflowing_mul`]
        ]} *)
    val mul_with_overflow :
      t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Returns `true` if the actual type given as `T` requires drop
           glue; returns `false` if the actual type provided for `T`
           implements `Copy`.

           If the actual type neither requires drop glue nor implements
           `Copy`, then the return value of this function is unspecified.

           Note that, unlike most intrinsics, this can only be called at compile-time
           as backends do not have an implementation for it. The only caller (its
           stable counterpart) wraps this intrinsic call in a `const` block so that
           backends only see an evaluated constant.

           The stabilized version of this intrinsic is [`mem::needs_drop`](crate::mem::needs_drop).
        ]} *)
    val needs_drop : t:Types.ty -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Emits a `nontemporal` store, which gives a hint to the CPU that the data should not be held
           in cache. Except for performance, this is fully equivalent to `ptr.write(val)`.

           Not all architectures provide such an operation. For instance, x86 does not: while `MOVNT`
           exists, that operation is *not* equivalent to `ptr.write(val)` (`MOVNT` writes can be reordered
           in ways that are not allowed for regular writes).
        ]} *)
    val nontemporal_store :
      t:Types.ty -> ptr:full_ptr -> val_:rust_val -> unit ret

    (** {@markdown[
          Calculates the offset from a pointer.

           This is implemented as an intrinsic to avoid converting to and from an
           integer, since the conversion would throw away aliasing information.

           This can only be used with `Ptr` as a raw pointer type (`*mut` or `*const`)
           to a `Sized` pointee and with `Delta` as `usize` or `isize`.  Any other
           instantiations may arbitrarily misbehave, and that's *not* a compiler bug.

           # Safety

           If the computed offset is non-zero, then both the starting and resulting pointer must be
           either in bounds or at the end of an allocation. If either pointer is out
           of bounds or arithmetic overflow occurs then this operation is undefined behavior.

           The stabilized version of this intrinsic is [`pointer::offset`].
        ]} *)
    val offset :
      ptr:Types.ty ->
      delta:Types.ty ->
      dst:rust_val ->
      offset:rust_val ->
      rust_val ret

    (** {@markdown[
          The offset of a field inside a type.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           This intrinsic can only be evaluated at compile-time, and should only appear in
           constants or inline const blocks.

           The stabilized version of this intrinsic is [`core::mem::offset_of`].
           This intrinsic is also a lang item so `offset_of!` can desugar to calls to it.
        ]} *)
    val offset_of :
      t:Types.ty ->
      variant:[< Typed.T.sint ] Typed.t ->
      field:[< Typed.T.sint ] Typed.t ->
      Typed.T.sint Typed.t ret

    (** {@markdown[
          Returns whether we should perform some overflow-checking at runtime. This eventually evaluates to
           `cfg!(overflow_checks)`, but behaves different from `cfg!` when mixing crates built with different
           flags: if the crate has overflow checks enabled or carries the `#[rustc_inherit_overflow_checks]`
           attribute, evaluation is delayed until monomorphization (or until the call gets inlined into
           a crate that does not delay evaluation further); otherwise it can happen any time.

           The common case here is a user program built with overflow_checks linked against the distributed
           sysroot which is built without overflow_checks but with `#[rustc_inherit_overflow_checks]`.
           For code that gets monomorphized in the user crate (i.e., generic functions and functions with
           `#[inline]`), gating assertions on `overflow_checks()` rather than `cfg!(overflow_checks)` means that
           assertions are enabled whenever the *user crate* has overflow checks enabled. However if the
           user has overflow checks disabled, the checks will still get optimized out.
        ]} *)
    val overflow_checks : Typed.T.sbool Typed.t ret

    (** {@markdown[
          Raises an `f128` to an `f128` power.

           The stabilized version of this intrinsic is
           [`f128::powf`](../../std/primitive.f128.html#method.powf)
        ]} *)
    val powf128 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f16` to an `f16` power.

           The stabilized version of this intrinsic is
           [`f16::powf`](../../std/primitive.f16.html#method.powf)
        ]} *)
    val powf16 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f32` to an `f32` power.

           The stabilized version of this intrinsic is
           [`f32::powf`](../../std/primitive.f32.html#method.powf)
        ]} *)
    val powf32 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f64` to an `f64` power.

           The stabilized version of this intrinsic is
           [`f64::powf`](../../std/primitive.f64.html#method.powf)
        ]} *)
    val powf64 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sfloat ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f128` to an integer power.

           The stabilized version of this intrinsic is
           [`f128::powi`](../../std/primitive.f128.html#method.powi)
        ]} *)
    val powif128 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sint ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f16` to an integer power.

           The stabilized version of this intrinsic is
           [`f16::powi`](../../std/primitive.f16.html#method.powi)
        ]} *)
    val powif16 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sint ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f32` to an integer power.

           The stabilized version of this intrinsic is
           [`f32::powi`](../../std/primitive.f32.html#method.powi)
        ]} *)
    val powif32 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sint ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Raises an `f64` to an integer power.

           The stabilized version of this intrinsic is
           [`f64::powi`](../../std/primitive.f64.html#method.powi)
        ]} *)
    val powif64 :
      a:[< Typed.T.sfloat ] Typed.t ->
      x:[< Typed.T.sint ] Typed.t ->
      Typed.T.sfloat Typed.t ret

    (** {@markdown[
          The `prefetch` intrinsic is a hint to the code generator to insert a prefetch instruction
           for the given address if supported; otherwise, it is a no-op.
           Prefetches have no effect on the behavior of the program but can change its performance
           characteristics.

           The `LOCALITY` argument is a temporal locality specifier ranging from (0) - no locality,
           to (3) - extremely local keep in cache.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val prefetch_read_data : t:Types.ty -> data:full_ptr -> unit ret

    (** {@markdown[
          The `prefetch` intrinsic is a hint to the code generator to insert a prefetch instruction
           for the given address if supported; otherwise, it is a no-op.
           Prefetches have no effect on the behavior of the program but can change its performance
           characteristics.

           The `LOCALITY` argument is a temporal locality specifier ranging from (0) - no locality,
           to (3) - extremely local keep in cache.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val prefetch_read_instruction : t:Types.ty -> data:full_ptr -> unit ret

    (** {@markdown[
          The `prefetch` intrinsic is a hint to the code generator to insert a prefetch instruction
           for the given address if supported; otherwise, it is a no-op.
           Prefetches have no effect on the behavior of the program but can change its performance
           characteristics.

           The `LOCALITY` argument is a temporal locality specifier ranging from (0) - no locality,
           to (3) - extremely local keep in cache.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val prefetch_write_data : t:Types.ty -> data:full_ptr -> unit ret

    (** {@markdown[
          The `prefetch` intrinsic is a hint to the code generator to insert a prefetch instruction
           for the given address if supported; otherwise, it is a no-op.
           Prefetches have no effect on the behavior of the program but can change its performance
           characteristics.

           The `LOCALITY` argument is a temporal locality specifier ranging from (0) - no locality,
           to (3) - extremely local keep in cache.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val prefetch_write_instruction : t:Types.ty -> data:full_ptr -> unit ret

    (** {@markdown[
          See documentation of `<*const T>::guaranteed_eq` for details.
           Returns `2` if the result is unknown.
           Returns `1` if the pointers are guaranteed equal.
           Returns `0` if the pointers are guaranteed inequal.
        ]} *)
    val ptr_guaranteed_cmp :
      t:Types.ty -> ptr:full_ptr -> other:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Masks out bits of the pointer according to a mask.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           Consider using [`pointer::mask`] instead.
        ]} *)
    val ptr_mask :
      t:Types.ty ->
      ptr:full_ptr ->
      mask:[< Typed.T.sint ] Typed.t ->
      full_ptr ret

    (** {@markdown[
          Lowers in MIR to `Rvalue::UnaryOp` with `UnOp::PtrMetadata`.

           This is used to implement functions like `ptr::metadata`.
        ]} *)
    val ptr_metadata : p:Types.ty -> m:Types.ty -> ptr:full_ptr -> rust_val ret

    (** {@markdown[
          See documentation of `<*const T>::offset_from` for details.
        ]} *)
    val ptr_offset_from :
      t:Types.ty -> ptr:full_ptr -> base:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          See documentation of `<*const T>::offset_from_unsigned` for details.
        ]} *)
    val ptr_offset_from_unsigned :
      t:Types.ty -> ptr:full_ptr -> base:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Determines whether the raw bytes of the two values are equal.

           This is particularly handy for arrays, since it allows things like just
           comparing `i96`s instead of forcing `alloca`s for `[6 x i16]`.

           Above some backend-decided threshold this will emit calls to `memcmp`,
           like slice equality does, instead of causing massive code size.

           Since this works by comparing the underlying bytes, the actual `T` is
           not particularly important.  It will be used for its size and alignment,
           but any validity restrictions will be ignored, not enforced.

           # Safety

           It's UB to call this if any of the *bytes* in `*a` or `*b` are uninitialized.
           Note that this is a stricter criterion than just the *values* being
           fully-initialized: if `T` has padding, it's UB to call this intrinsic.

           At compile-time, it is furthermore UB to call this if any of the bytes
           in `*a` or `*b` have provenance.

           (The implementation is allowed to branch on the results of comparisons,
           which is UB if any of their inputs are `undef`.)
        ]} *)
    val raw_eq :
      t:Types.ty -> a:full_ptr -> b:full_ptr -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          This is an implementation detail of [`crate::ptr::read`] and should
           not be used anywhere else.  See its comments for why this exists.

           This intrinsic can *only* be called where the pointer is a local without
           projections (`read_via_copy(ptr)`, not `read_via_copy( *ptr)`) so that it
           trivially obeys runtime-MIR rules about derefs in operands.
        ]} *)
    val read_via_copy : t:Types.ty -> ptr:full_ptr -> rust_val ret

    (** {@markdown[
          Performs rotate left.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `rotate_left` method. For example,
           [`u32::rotate_left`]
        ]} *)
    val rotate_left :
      t:Types.ty ->
      x:rust_val ->
      shift:[< Typed.T.sint ] Typed.t ->
      rust_val ret

    (** {@markdown[
          Performs rotate right.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `rotate_right` method. For example,
           [`u32::rotate_right`]
        ]} *)
    val rotate_right :
      t:Types.ty ->
      x:rust_val ->
      shift:[< Typed.T.sint ] Typed.t ->
      rust_val ret

    (** {@markdown[
          Returns the nearest integer to an `f128`. Rounds half-way cases to the number with an even
           least significant digit.

           The stabilized version of this intrinsic is
           [`f128::round_ties_even`](../../std/primitive.f128.html#method.round_ties_even)
        ]} *)
    val round_ties_even_f128 :
      x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f16`. Rounds half-way cases to the number with an even
           least significant digit.

           The stabilized version of this intrinsic is
           [`f16::round_ties_even`](../../std/primitive.f16.html#method.round_ties_even)
        ]} *)
    val round_ties_even_f16 :
      x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f32`. Rounds half-way cases to the number with an even
           least significant digit.

           The stabilized version of this intrinsic is
           [`f32::round_ties_even`](../../std/primitive.f32.html#method.round_ties_even)
        ]} *)
    val round_ties_even_f32 :
      x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f64`. Rounds half-way cases to the number with an even
           least significant digit.

           The stabilized version of this intrinsic is
           [`f64::round_ties_even`](../../std/primitive.f64.html#method.round_ties_even)
        ]} *)
    val round_ties_even_f64 :
      x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f128`. Rounds half-way cases away from zero.

           The stabilized version of this intrinsic is
           [`f128::round`](../../std/primitive.f128.html#method.round)
        ]} *)
    val roundf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f16`. Rounds half-way cases away from zero.

           The stabilized version of this intrinsic is
           [`f16::round`](../../std/primitive.f16.html#method.round)
        ]} *)
    val roundf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f32`. Rounds half-way cases away from zero.

           The stabilized version of this intrinsic is
           [`f32::round`](../../std/primitive.f32.html#method.round)
        ]} *)
    val roundf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the nearest integer to an `f64`. Rounds half-way cases away from zero.

           The stabilized version of this intrinsic is
           [`f64::round`](../../std/primitive.f64.html#method.round)
        ]} *)
    val roundf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Magic intrinsic that derives its meaning from attributes
           attached to the function.

           For example, dataflow uses this to inject static assertions so
           that `rustc_peek(potentially_uninitialized)` would actually
           double-check that dataflow did indeed compute that it is
           uninitialized at that point in the control flow.

           This intrinsic should not be used outside of the compiler.
        ]} *)
    val rustc_peek : t:Types.ty -> arg:rust_val -> rust_val ret

    (** {@markdown[
          Computes `a + b`, saturating at numeric bounds.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `saturating_add` method. For example,
           [`u32::saturating_add`]
        ]} *)
    val saturating_add : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Computes `a - b`, saturating at numeric bounds.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `saturating_sub` method. For example,
           [`u32::saturating_sub`]
        ]} *)
    val saturating_sub : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Returns either `true_val` or `false_val` depending on condition `b` with a
           hint to the compiler that this condition is unlikely to be correctly
           predicted by a CPU's branch predictor (e.g. a binary search).

           This is otherwise functionally equivalent to `if b { true_val } else { false_val }`.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The public form of this intrinsic is [`core::hint::select_unpredictable`].
           However unlike the public form, the intrinsic will not drop the value that
           is not selected.
        ]} *)
    val select_unpredictable :
      t:Types.ty ->
      b:[< Typed.T.sbool ] Typed.t ->
      true_val:rust_val ->
      false_val:rust_val ->
      rust_val ret

    (** {@markdown[
          Returns the sine of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::sin`](../../std/primitive.f128.html#method.sin)
        ]} *)
    val sinf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the sine of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::sin`](../../std/primitive.f16.html#method.sin)
        ]} *)
    val sinf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the sine of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::sin`](../../std/primitive.f32.html#method.sin)
        ]} *)
    val sinf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the sine of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::sin`](../../std/primitive.f64.html#method.sin)
        ]} *)
    val sinf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          The size of a type in bytes.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           More specifically, this is the offset in bytes between successive
           items of the same type, including alignment padding.

           Note that, unlike most intrinsics, this can only be called at compile-time
           as backends do not have an implementation for it. The only caller (its
           stable counterpart) wraps this intrinsic call in a `const` block so that
           backends only see an evaluated constant.

           The stabilized version of this intrinsic is [`core::mem::size_of`].
        ]} *)
    val size_of : t:Types.ty -> Typed.T.sint Typed.t ret

    (** {@markdown[
          The size of the referenced value in bytes.

           The stabilized version of this intrinsic is [`core::mem::size_of_val`].

           # Safety

           See [`crate::mem::size_of_val_raw`] for safety conditions.
        ]} *)
    val size_of_val : t:Types.ty -> ptr:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Projects to the `index`-th element of `slice_ptr`, as the same kind of pointer
           as the slice was provided -- so `&mut [T] → &mut T`, `&[T] → &T`,
           `*mut [T] → *mut T`, or `*const [T] → *const T` -- without a bounds check.

           This is exposed via `<usize as SliceIndex>::get(_unchecked)(_mut)`,
           and isn't intended to be used elsewhere.

           Expands in MIR to `{&, &mut, &raw const, &raw mut} ( *slice_ptr)[index]`,
           depending on the types involved, so no backend support is needed.

           # Safety

           - `index < PtrMetadata(slice_ptr)`, so the indexing is in-bounds for the slice
           - the resulting offsetting is in-bounds of the allocation, which is
             always the case for references, but needs to be upheld manually for pointers
        ]} *)
    val slice_get_unchecked :
      itemptr:Types.ty ->
      sliceptr:Types.ty ->
      t:Types.ty ->
      slice_ptr:rust_val ->
      index:[< Typed.T.sint ] Typed.t ->
      rust_val ret

    (** {@markdown[
          Returns the square root of an `f128`

           The stabilized version of this intrinsic is
           [`f128::sqrt`](../../std/primitive.f128.html#method.sqrt)
        ]} *)
    val sqrtf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the square root of an `f16`

           The stabilized version of this intrinsic is
           [`f16::sqrt`](../../std/primitive.f16.html#method.sqrt)
        ]} *)
    val sqrtf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the square root of an `f32`

           The stabilized version of this intrinsic is
           [`f32::sqrt`](../../std/primitive.f32.html#method.sqrt)
        ]} *)
    val sqrtf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the square root of an `f64`

           The stabilized version of this intrinsic is
           [`f64::sqrt`](../../std/primitive.f64.html#method.sqrt)
        ]} *)
    val sqrtf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Performs checked integer subtraction

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `overflowing_sub` method. For example,
           [`u32::overflowing_sub`]
        ]} *)
    val sub_with_overflow :
      t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Does a three-way comparison between the two arguments,
           which must be of character or integer (signed or unsigned) type.

           This was originally added because it greatly simplified the MIR in `cmp`
           implementations, and then LLVM 20 added a backend intrinsic for it too.

           The stabilized version of this intrinsic is [`Ord::cmp`].
        ]} *)
    val three_way_compare :
      t:Types.ty -> lhs:rust_val -> rhss:rust_val -> rust_val ret

    (** {@markdown[
          Reinterprets the bits of a value of one type as another type.

           Both types must have the same size. Compilation will fail if this is not guaranteed.

           `transmute` is semantically equivalent to a bitwise move of one type
           into another. It copies the bits from the source value into the
           destination value, then forgets the original. Note that source and destination
           are passed by-value, which means if `Src` or `Dst` contain padding, that padding
           is *not* guaranteed to be preserved by `transmute`.

           Both the argument and the result must be [valid](../../nomicon/what-unsafe-does.html) at
           their given type. Violating this condition leads to [undefined behavior][ub]. The compiler
           will generate code *assuming that you, the programmer, ensure that there will never be
           undefined behavior*. It is therefore your responsibility to guarantee that every value
           passed to `transmute` is valid at both types `Src` and `Dst`. Failing to uphold this condition
           may lead to unexpected and unstable compilation results. This makes `transmute` **incredibly
           unsafe**. `transmute` should be the absolute last resort.

           Because `transmute` is a by-value operation, alignment of the *transmuted values
           themselves* is not a concern. As with any other function, the compiler already ensures
           both `Src` and `Dst` are properly aligned. However, when transmuting values that *point
           elsewhere* (such as pointers, references, boxes…), the caller has to ensure proper
           alignment of the pointed-to values.

           The [nomicon](../../nomicon/transmutes.html) has additional documentation.

           [ub]: ../../reference/behavior-considered-undefined.html

           # Transmutation between pointers and integers

           Special care has to be taken when transmuting between pointers and integers, e.g.
           transmuting between `*const ()` and `usize`.

           Transmuting *pointers to integers* in a `const` context is [undefined behavior][ub], unless
           the pointer was originally created *from* an integer. (That includes this function
           specifically, integer-to-pointer casts, and helpers like [`dangling`][crate::ptr::dangling],
           but also semantically-equivalent conversions such as punning through `repr(C)` union
           fields.) Any attempt to use the resulting value for integer operations will abort
           const-evaluation. (And even outside `const`, such transmutation is touching on many
           unspecified aspects of the Rust memory model and should be avoided. See below for
           alternatives.)

           Transmuting *integers to pointers* is a largely unspecified operation. It is likely *not*
           equivalent to an `as` cast. Doing non-zero-sized memory accesses with a pointer constructed
           this way is currently considered undefined behavior.

           All this also applies when the integer is nested inside an array, tuple, struct, or enum.
           However, `MaybeUninit<usize>` is not considered an integer type for the purpose of this
           section. Transmuting `*const ()` to `MaybeUninit<usize>` is fine---but then calling
           `assume_init()` on that result is considered as completing the pointer-to-integer transmute
           and thus runs into the issues discussed above.

           In particular, doing a pointer-to-integer-to-pointer roundtrip via `transmute` is *not* a
           lossless process. If you want to round-trip a pointer through an integer in a way that you
           can get back the original pointer, you need to use `as` casts, or replace the integer type
           by `MaybeUninit<$int>` (and never call `assume_init()`). If you are looking for a way to
           store data of arbitrary type, also use `MaybeUninit<T>` (that will also handle uninitialized
           memory due to padding). If you specifically need to store something that is "either an
           integer or a pointer", use `*mut ()`: integers can be converted to pointers and back without
           any loss (via `as` casts or via `transmute`).

           # Examples

           There are a few things that `transmute` is really useful for.

           Turning a pointer into a function pointer. This is *not* portable to
           machines where function pointers and data pointers have different sizes.

           ```
           fn foo() -> i32 {
               0
           }
           // Crucially, we `as`-cast to a raw pointer before `transmute`ing to a function pointer.
           // This avoids an integer-to-pointer `transmute`, which can be problematic.
           // Transmuting between raw pointers and function pointers (i.e., two pointer types) is fine.
           let pointer = foo as fn() -> i32 as *const ();
           let function = unsafe {
               std::mem::transmute::<*const (), fn() -> i32>(pointer)
           };
           assert_eq!(function(), 0);
           ```

           Extending a lifetime, or shortening an invariant lifetime. This is
           advanced, very unsafe Rust!

           ```
           struct R<'a>(&'a i32);
           unsafe fn extend_lifetime<'b>(r: R<'b>) -> R<'static> {
               unsafe { std::mem::transmute::<R<'b>, R<'static>>(r) }
           }

           unsafe fn shorten_invariant_lifetime<'b, 'c>(r: &'b mut R<'static>)
                                                        -> &'b mut R<'c> {
               unsafe { std::mem::transmute::<&'b mut R<'static>, &'b mut R<'c>>(r) }
           }
           ```

           # Alternatives

           Don't despair: many uses of `transmute` can be achieved through other means.
           Below are common applications of `transmute` which can be replaced with safer
           constructs.

           Turning raw bytes (`[u8; SZ]`) into `u32`, `f64`, etc.:

           ```
           # #![allow(unnecessary_transmutes)]
           let raw_bytes = [0x78, 0x56, 0x34, 0x12];

           let num = unsafe {
               std::mem::transmute::<[u8; 4], u32>(raw_bytes)
           };

           // use `u32::from_ne_bytes` instead
           let num = u32::from_ne_bytes(raw_bytes);
           // or use `u32::from_le_bytes` or `u32::from_be_bytes` to specify the endianness
           let num = u32::from_le_bytes(raw_bytes);
           assert_eq!(num, 0x12345678);
           let num = u32::from_be_bytes(raw_bytes);
           assert_eq!(num, 0x78563412);
           ```

           Turning a pointer into a `usize`:

           ```no_run
           let ptr = &0;
           let ptr_num_transmute = unsafe {
               std::mem::transmute::<&i32, usize>(ptr)
           };

           // Use an `as` cast instead
           let ptr_num_cast = ptr as *const i32 as usize;
           ```

           Note that using `transmute` to turn a pointer to a `usize` is (as noted above) [undefined
           behavior][ub] in `const` contexts. Also outside of consts, this operation might not behave
           as expected -- this is touching on many unspecified aspects of the Rust memory model.
           Depending on what the code is doing, the following alternatives are preferable to
           pointer-to-integer transmutation:
           - If the code just wants to store data of arbitrary type in some buffer and needs to pick a
             type for that buffer, it can use [`MaybeUninit`][crate::mem::MaybeUninit].
           - If the code actually wants to work on the address the pointer points to, it can use `as`
             casts or [`ptr.addr()`][pointer::addr].

           Turning a `*mut T` into a `&mut T`:

           ```
           let ptr: *mut i32 = &mut 0;
           let ref_transmuted = unsafe {
               std::mem::transmute::<*mut i32, &mut i32>(ptr)
           };

           // Use a reborrow instead
           let ref_casted = unsafe { &mut *ptr };
           ```

           Turning a `&mut T` into a `&mut U`:

           ```
           let ptr = &mut 0;
           let val_transmuted = unsafe {
               std::mem::transmute::<&mut i32, &mut u32>(ptr)
           };

           // Now, put together `as` and reborrowing - note the chaining of `as`
           // `as` is not transitive
           let val_casts = unsafe { &mut *(ptr as *mut i32 as *mut u32) };
           ```

           Turning a `&str` into a `&[u8]`:

           ```
           // this is not a good way to do this.
           let slice = unsafe { std::mem::transmute::<&str, &[u8]>("Rust") };
           assert_eq!(slice, &[82, 117, 115, 116]);

           // You could use `str::as_bytes`
           let slice = "Rust".as_bytes();
           assert_eq!(slice, &[82, 117, 115, 116]);

           // Or, just use a byte string, if you have control over the string
           // literal
           assert_eq!(b"Rust", &[82, 117, 115, 116]);
           ```

           Turning a `Vec<&T>` into a `Vec<Option<&T>>`.

           To transmute the inner type of the contents of a container, you must make sure to not
           violate any of the container's invariants. For `Vec`, this means that both the size
           *and alignment* of the inner types have to match. Other containers might rely on the
           size of the type, alignment, or even the `TypeId`, in which case transmuting wouldn't
           be possible at all without violating the container invariants.

           ```
           let store = [0, 1, 2, 3];
           let v_orig = store.iter().collect::<Vec<&i32>>();

           // clone the vector as we will reuse them later
           let v_clone = v_orig.clone();

           // Using transmute: this relies on the unspecified data layout of `Vec`, which is a
           // bad idea and could cause Undefined Behavior.
           // However, it is no-copy.
           let v_transmuted = unsafe {
               std::mem::transmute::<Vec<&i32>, Vec<Option<&i32>>>(v_clone)
           };

           let v_clone = v_orig.clone();

           // This is the suggested, safe way.
           // It may copy the entire vector into a new one though, but also may not.
           let v_collected = v_clone.into_iter()
                                    .map(Some)
                                    .collect::<Vec<Option<&i32>>>();

           let v_clone = v_orig.clone();

           // This is the proper no-copy, unsafe way of "transmuting" a `Vec`, without relying on the
           // data layout. Instead of literally calling `transmute`, we perform a pointer cast, but
           // in terms of converting the original inner type (`&i32`) to the new one (`Option<&i32>`),
           // this has all the same caveats. Besides the information provided above, also consult the
           // [`from_raw_parts`] documentation.
           let (ptr, len, capacity) = v_clone.into_raw_parts();
           let v_from_raw = unsafe {
               Vec::from_raw_parts(ptr.cast::<*mut Option<&i32>>(), len, capacity)
           };
           ```

           [`from_raw_parts`]: ../../std/vec/struct.Vec.html#method.from_raw_parts

           Implementing `split_at_mut`:

           ```
           use std::{slice, mem};

           // There are multiple ways to do this, and there are multiple problems
           // with the following (transmute) way.
           fn split_at_mut_transmute<T>(slice: &mut [T], mid: usize)
                                        -> (&mut [T], &mut [T]) {
               let len = slice.len();
               assert!(mid <= len);
               unsafe {
                   let slice2 = mem::transmute::<&mut [T], &mut [T]>(slice);
                   // first: transmute is not type safe; all it checks is that T and
                   // U are of the same size. Second, right here, you have two
                   // mutable references pointing to the same memory.
                   (&mut slice[0..mid], &mut slice2[mid..len])
               }
           }

           // This gets rid of the type safety problems; `&mut *` will *only* give
           // you a `&mut T` from a `&mut T` or `*mut T`.
           fn split_at_mut_casts<T>(slice: &mut [T], mid: usize)
                                    -> (&mut [T], &mut [T]) {
               let len = slice.len();
               assert!(mid <= len);
               unsafe {
                   let slice2 = &mut *(slice as *mut [T]);
                   // however, you still have two mutable references pointing to
                   // the same memory.
                   (&mut slice[0..mid], &mut slice2[mid..len])
               }
           }

           // This is how the standard library does it. This is the best method, if
           // you need to do something like this
           fn split_at_stdlib<T>(slice: &mut [T], mid: usize)
                                 -> (&mut [T], &mut [T]) {
               let len = slice.len();
               assert!(mid <= len);
               unsafe {
                   let ptr = slice.as_mut_ptr();
                   // This now has three mutable references pointing at the same
                   // memory. `slice`, the rvalue ret.0, and the rvalue ret.1.
                   // `slice` is never used after `let ptr = ...`, and so one can
                   // treat it as "dead", and therefore, you only have two real
                   // mutable slices.
                   (slice::from_raw_parts_mut(ptr, mid),
                    slice::from_raw_parts_mut(ptr.add(mid), len - mid))
               }
           }
           ```
        ]} *)
    val transmute :
      t_src:Types.ty -> dst:Types.ty -> src:rust_val -> rust_val ret

    (** {@markdown[
          Like [`transmute`], but even less checked at compile-time: rather than
           giving an error for `size_of::<Src>() != size_of::<Dst>()`, it's
           **Undefined Behavior** at runtime.

           Prefer normal `transmute` where possible, for the extra checking, since
           both do exactly the same thing at runtime, if they both compile.

           This is not expected to ever be exposed directly to users, rather it
           may eventually be exposed through some more-constrained API.
        ]} *)
    val transmute_unchecked :
      t_src:Types.ty -> dst:Types.ty -> src:rust_val -> rust_val ret

    (** {@markdown[
          Returns the integer part of an `f128`.

           The stabilized version of this intrinsic is
           [`f128::trunc`](../../std/primitive.f128.html#method.trunc)
        ]} *)
    val truncf128 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the integer part of an `f16`.

           The stabilized version of this intrinsic is
           [`f16::trunc`](../../std/primitive.f16.html#method.trunc)
        ]} *)
    val truncf16 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the integer part of an `f32`.

           The stabilized version of this intrinsic is
           [`f32::trunc`](../../std/primitive.f32.html#method.trunc)
        ]} *)
    val truncf32 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Returns the integer part of an `f64`.

           The stabilized version of this intrinsic is
           [`f64::trunc`](../../std/primitive.f64.html#method.trunc)
        ]} *)
    val truncf64 : x:[< Typed.T.sfloat ] Typed.t -> Typed.T.sfloat Typed.t ret

    (** {@markdown[
          Gets an identifier which is globally unique to the specified type. This
           function will return the same value for a type regardless of whichever
           crate it is invoked in.

           Note that, unlike most intrinsics, this can only be called at compile-time
           as backends do not have an implementation for it. The only caller (its
           stable counterpart) wraps this intrinsic call in a `const` block so that
           backends only see an evaluated constant.

           The stabilized version of this intrinsic is [`core::any::TypeId::of`].
        ]} *)
    val type_id : t:Types.ty -> rust_val ret

    (** {@markdown[
          Tests (at compile-time) if two [`crate::any::TypeId`] instances identify the
           same type. This is necessary because at const-eval time the actual discriminating
           data is opaque and cannot be inspected directly.

           The stabilized version of this intrinsic is the [PartialEq] impl for [`core::any::TypeId`].
        ]} *)
    val type_id_eq : a:rust_val -> b:rust_val -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Gets a static string slice containing the name of a type.

           Note that, unlike most intrinsics, this can only be called at compile-time
           as backends do not have an implementation for it. The only caller (its
           stable counterpart) wraps this intrinsic call in a `const` block so that
           backends only see an evaluated constant.

           The stabilized version of this intrinsic is [`core::any::type_name`].
        ]} *)
    val type_name : t:Types.ty -> full_ptr ret

    (** {@markdown[
          Non-overlapping *typed* swap of a single value.

           The codegen backends will replace this with a better implementation when
           `T` is a simple type that can be loaded and stored as an immediate.

           The stabilized form of this intrinsic is [`crate::mem::swap`].

           # Safety
           Behavior is undefined if any of the following conditions are violated:

           * Both `x` and `y` must be [valid] for both reads and writes.

           * Both `x` and `y` must be properly aligned.

           * The region of memory beginning at `x` must *not* overlap with the region of memory
             beginning at `y`.

           * The memory pointed by `x` and `y` must both contain values of type `T`.

           [valid]: crate::ptr#safety
        ]} *)
    val typed_swap_nonoverlapping :
      t:Types.ty -> x:full_ptr -> y:full_ptr -> unit ret

    (** {@markdown[
          Returns whether we should perform some UB-checking at runtime. This eventually evaluates to
           `cfg!(ub_checks)`, but behaves different from `cfg!` when mixing crates built with different
           flags: if the crate has UB checks enabled or carries the `#[rustc_preserve_ub_checks]`
           attribute, evaluation is delayed until monomorphization (or until the call gets inlined into
           a crate that does not delay evaluation further); otherwise it can happen any time.

           The common case here is a user program built with ub_checks linked against the distributed
           sysroot which is built without ub_checks but with `#[rustc_preserve_ub_checks]`.
           For code that gets monomorphized in the user crate (i.e., generic functions and functions with
           `#[inline]`), gating assertions on `ub_checks()` rather than `cfg!(ub_checks)` means that
           assertions are enabled whenever the *user crate* has UB checks enabled. However, if the
           user has UB checks disabled, the checks will still get optimized out. This intrinsic is
           primarily used by [`crate::ub_checks::assert_unsafe_precondition`].
        ]} *)
    val ub_checks : Typed.T.sbool Typed.t ret

    (** {@markdown[
          Performs a volatile load from the `src` pointer
           The pointer is not required to be aligned.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val unaligned_volatile_load : t:Types.ty -> src:full_ptr -> rust_val ret

    (** {@markdown[
          Performs a volatile store to the `dst` pointer.
           The pointer is not required to be aligned.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val unaligned_volatile_store :
      t:Types.ty -> dst:full_ptr -> val_:rust_val -> unit ret

    (** {@markdown[
          Returns the result of an unchecked addition, resulting in
           undefined behavior when `x + y > T::MAX` or `x + y < T::MIN`.

           The stable counterpart of this intrinsic is `unchecked_add` on the various
           integer types, such as [`u16::unchecked_add`] and [`i64::unchecked_add`].
        ]} *)
    val unchecked_add : t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Performs an unchecked division, resulting in undefined behavior
           where `y == 0` or `x == T::MIN && y == -1`

           Safe wrappers for this intrinsic are available on the integer
           primitives via the `checked_div` method. For example,
           [`u32::checked_div`]
        ]} *)
    val unchecked_div : t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Funnel Shift left.

           Concatenates `a` and `b` (with `a` in the most significant half),
           creating an integer twice as wide. Then shift this integer left
           by `shift`), and extract the most significant half. If `a` and `b`
           are the same, this is equivalent to a rotate left operation.

           It is undefined behavior if `shift` is greater than or equal to the
           bit size of `T`.

           Safe versions of this intrinsic are available on the integer primitives
           via the `funnel_shl` method. For example, [`u32::funnel_shl`].
        ]} *)
    val unchecked_funnel_shl :
      t:Types.ty ->
      a:rust_val ->
      b:rust_val ->
      shift:[< Typed.T.sint ] Typed.t ->
      rust_val ret

    (** {@markdown[
          Funnel Shift right.

           Concatenates `a` and `b` (with `a` in the most significant half),
           creating an integer twice as wide. Then shift this integer right
           by `shift` (taken modulo the bit size of `T`), and extract the
           least significant half. If `a` and `b` are the same, this is equivalent
           to a rotate right operation.

           It is undefined behavior if `shift` is greater than or equal to the
           bit size of `T`.

           Safer versions of this intrinsic are available on the integer primitives
           via the `funnel_shr` method. For example, [`u32::funnel_shr`]
        ]} *)
    val unchecked_funnel_shr :
      t:Types.ty ->
      a:rust_val ->
      b:rust_val ->
      shift:[< Typed.T.sint ] Typed.t ->
      rust_val ret

    (** {@markdown[
          Returns the result of an unchecked multiplication, resulting in
           undefined behavior when `x * y > T::MAX` or `x * y < T::MIN`.

           The stable counterpart of this intrinsic is `unchecked_mul` on the various
           integer types, such as [`u16::unchecked_mul`] and [`i64::unchecked_mul`].
        ]} *)
    val unchecked_mul : t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Returns the remainder of an unchecked division, resulting in
           undefined behavior when `y == 0` or `x == T::MIN && y == -1`

           Safe wrappers for this intrinsic are available on the integer
           primitives via the `checked_rem` method. For example,
           [`u32::checked_rem`]
        ]} *)
    val unchecked_rem : t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Performs an unchecked left shift, resulting in undefined behavior when
           `y < 0` or `y >= N`, where N is the width of T in bits.

           Safe wrappers for this intrinsic are available on the integer
           primitives via the `checked_shl` method. For example,
           [`u32::checked_shl`]
        ]} *)
    val unchecked_shl :
      t:Types.ty -> u:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Performs an unchecked right shift, resulting in undefined behavior when
           `y < 0` or `y >= N`, where N is the width of T in bits.

           Safe wrappers for this intrinsic are available on the integer
           primitives via the `checked_shr` method. For example,
           [`u32::checked_shr`]
        ]} *)
    val unchecked_shr :
      t:Types.ty -> u:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Returns the result of an unchecked subtraction, resulting in
           undefined behavior when `x - y > T::MAX` or `x - y < T::MIN`.

           The stable counterpart of this intrinsic is `unchecked_sub` on the various
           integer types, such as [`u16::unchecked_sub`] and [`i64::unchecked_sub`].
        ]} *)
    val unchecked_sub : t:Types.ty -> x:rust_val -> y:rust_val -> rust_val ret

    (** {@markdown[
          Hints to the compiler that branch condition is likely to be false.
           Returns the value passed to it.

           Any use other than with `if` statements will probably not have an effect.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val unlikely : b:[< Typed.T.sbool ] Typed.t -> Typed.T.sbool Typed.t ret

    (** {@markdown[
          Informs the optimizer that this point in the code is not reachable,
           enabling further optimizations.

           N.B., this is very different from the `unreachable!()` macro: Unlike the
           macro, which panics when it is executed, it is *undefined behavior* to
           reach code marked with this function.

           The stabilized version of this intrinsic is [`core::hint::unreachable_unchecked`].
        ]} *)
    val unreachable : unit ret

    (** {@markdown[
          Loads an argument of type `T` from the `va_list` `ap` and increment the
           argument `ap` points to.

           # Safety

           This function is only sound to call when:

           - there is a next variable argument available.
           - the next argument's type must be ABI-compatible with the type `T`.
           - the next argument must have a properly initialized value of type `T`.

           Calling this function with an incompatible type, an invalid value, or when there
           are no more variable arguments, is unsound.
        ]} *)
    val va_arg : t:Types.ty -> ap:full_ptr -> rust_val ret

    (** {@markdown[
          Copies the current location of arglist `src` to the arglist `dst`.

           # Safety

           You must check the following invariants before you call this function:

           - `dest` must be non-null and point to valid, writable memory.
           - `dest` must not alias `src`.
        ]} *)
    val va_copy : dest:full_ptr -> src:full_ptr -> unit ret

    (** {@markdown[
          Destroy the arglist `ap` after initialization with `va_start` or `va_copy`.

           # Safety

           `ap` must not be used to access variable arguments after this call.
        ]} *)
    val va_end : ap:full_ptr -> unit ret

    (** {@markdown[
          Returns the number of variants of the type `T` cast to a `usize`;
           if `T` has no variants, returns `0`. Uninhabited variants will be counted.

           Note that, unlike most intrinsics, this can only be called at compile-time
           as backends do not have an implementation for it. The only caller (its
           stable counterpart) wraps this intrinsic call in a `const` block so that
           backends only see an evaluated constant.

           The to-be-stabilized version of this intrinsic is [`crate::mem::variant_count`].
        ]} *)
    val variant_count : t:Types.ty -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Equivalent to the appropriate `llvm.memmove.p0i8.0i8.*` intrinsic, with
           a size of `count * size_of::<T>()` and an alignment of `align_of::<T>()`.

           The volatile parameter is set to `true`, so it will not be optimized out
           unless size is equal to zero.

           This intrinsic does not have a stable counterpart.
        ]} *)
    val volatile_copy_memory :
      t:Types.ty ->
      dst:full_ptr ->
      src:full_ptr ->
      count:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          Equivalent to the appropriate `llvm.memcpy.p0i8.0i8.*` intrinsic, with
           a size of `count` * `size_of::<T>()` and an alignment of `align_of::<T>()`.

           This intrinsic does not have a stable counterpart.
           # Safety

           The safety requirements are consistent with [`copy_nonoverlapping`]
           while the read and write behaviors are volatile,
           which means it will not be optimized out unless `_count` or `size_of::<T>()` is equal to zero.

           [`copy_nonoverlapping`]: ptr::copy_nonoverlapping
        ]} *)
    val volatile_copy_nonoverlapping_memory :
      t:Types.ty ->
      dst:full_ptr ->
      src:full_ptr ->
      count:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          Performs a volatile load from the `src` pointer.

           The stabilized version of this intrinsic is [`core::ptr::read_volatile`].
        ]} *)
    val volatile_load : t:Types.ty -> src:full_ptr -> rust_val ret

    (** {@markdown[
          Equivalent to the appropriate `llvm.memset.p0i8.*` intrinsic, with a
           size of `count * size_of::<T>()` and an alignment of `align_of::<T>()`.

           This intrinsic does not have a stable counterpart.
           # Safety

           The safety requirements are consistent with [`write_bytes`] while the write behavior is volatile,
           which means it will not be optimized out unless `_count` or `size_of::<T>()` is equal to zero.

           [`write_bytes`]: ptr::write_bytes
        ]} *)
    val volatile_set_memory :
      t:Types.ty ->
      dst:full_ptr ->
      val_:[< Typed.T.sint ] Typed.t ->
      count:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          Performs a volatile store to the `dst` pointer.

           The stabilized version of this intrinsic is [`core::ptr::write_volatile`].
        ]} *)
    val volatile_store : t:Types.ty -> dst:full_ptr -> val_:rust_val -> unit ret

    (** {@markdown[
          The intrinsic will return the alignment stored in that vtable.

           # Safety

           `ptr` must point to a vtable.
        ]} *)
    val vtable_align : ptr:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          The intrinsic will return the size stored in that vtable.

           # Safety

           `ptr` must point to a vtable.
        ]} *)
    val vtable_size : ptr:full_ptr -> Typed.T.sint Typed.t ret

    (** {@markdown[
          Returns (a + b) mod 2<sup>N</sup>, where N is the width of T in bits.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `wrapping_add` method. For example,
           [`u32::wrapping_add`]
        ]} *)
    val wrapping_add : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Returns (a * b) mod 2<sup>N</sup>, where N is the width of T in bits.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `wrapping_mul` method. For example,
           [`u32::wrapping_mul`]
        ]} *)
    val wrapping_mul : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          Returns (a - b) mod 2<sup>N</sup>, where N is the width of T in bits.

           Note that, unlike most intrinsics, this is safe to call;
           it does not require an `unsafe` block.
           Therefore, implementations must not require the user to uphold
           any safety invariants.

           The stabilized versions of this intrinsic are available on the integer
           primitives via the `wrapping_sub` method. For example,
           [`u32::wrapping_sub`]
        ]} *)
    val wrapping_sub : t:Types.ty -> a:rust_val -> b:rust_val -> rust_val ret

    (** {@markdown[
          This is an accidentally-stable alias to [`ptr::write_bytes`]; use that instead.
        ]} *)
    val write_bytes :
      t:Types.ty ->
      dst:full_ptr ->
      val_:[< Typed.T.sint ] Typed.t ->
      count:[< Typed.T.sint ] Typed.t ->
      unit ret

    (** {@markdown[
          This is an implementation detail of [`crate::ptr::write`] and should
           not be used anywhere else.  See its comments for why this exists.

           This intrinsic can *only* be called where the pointer is a local without
           projections (`write_via_move(ptr, x)`, not `write_via_move( *ptr, x)`) so
           that it trivially obeys runtime-MIR rules about derefs in operands.
        ]} *)
    val write_via_move :
      t:Types.ty -> ptr:full_ptr -> value:rust_val -> unit ret
  end

  module type S = sig
    include Impl

    type rust_val := State.Sptr.t Rust_val.t

    type 'a ret :=
      unit ->
      State.t ->
      ( 'a * unit * State.t,
        Error.t State.err * State.t,
        State.serialized )
      Result.t

    type fun_exec :=
      UllbcAst.fun_decl ->
      rust_val list ->
      unit ->
      State.t ->
      ( rust_val * unit * State.t,
        Error.t State.err * State.t,
        State.serialized )
      Result.t

    val eval_fun :
      string -> fun_exec -> Types.generic_args -> rust_val list -> rust_val ret
  end
end
