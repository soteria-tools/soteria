open Svalue
open Common.Charon_util

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax
  open Typed.Infix
  open Typed.Syntax
  module Core = Core.M (StateM)
  module Alloc = Extern.Alloc.M (StateM)

  let do_panic ?msg () =
    match msg with
    | Some msg ->
        let* msg = Core.parse_string msg in
        error (`Panic msg)
    | _ -> error (`Panic None)

  (* ---- alloc ---- *)

  (** {@rust[
        const fn alloc_impl(&self, layout: Layout, zeroed: bool)
          -> Result<NonNull<[u8]>, AllocError> { ... }
      ]}

      with
      {@rust[
        pub struct Layout {
            size: usize,
            align: Alignment,
        }

        pub struct Alignment {
            _inner_repr_trick: AlignmentEnum,
        }

        enum AlignmentEnum { ... }
      ]} *)
  let alloc_impl ~(fun_sig : Charon.Types.fun_sig) ~self:_ ~layout ~zeroed =
    let zeroed = (zeroed :> Typed.T.sbool Typed.t) in
    let size, align =
      let size, align = Typed.Adt.as_tuple2 (Typed.cast_tuple layout) in
      let size = Typed.cast_i Usize size in
      let align_enum =
        Typed.cast_enum (Typed.Adt.as_tuple1 (Typed.cast_tuple align))
      in
      let align = Typed.Adt.discriminant_of align_enum in
      (size, Typed.cast_i Usize align)
    in
    let mk_res ptr len =
      let out_res = ty_as_adt fun_sig.output in
      let ptr = Typed.Ptr.mk_ptr_f ptr (Some len) in
      let nonnull = Typed.Adt.mk_tuple [ ptr ] in
      Typed.Adt.Checked.mk_enum out_res "Ok" [ nonnull ]
    in
    if%sat size ==@ Usize.(0s) then
      let dangling = Typed.Ptr.of_address align in
      ok (mk_res dangling Usize.(0s))
    else
      let* zeroed = if%sat zeroed then ok true else ok false in
      let+ ptr = Alloc.alloc ~zeroed [ size; align ] in
      let ptr, _ = Typed.Ptr.split (Typed.cast_ptr_f ptr) in
      mk_res ptr size

  let handle_alloc_error ~layout:_ = do_panic ()
  let handle_error ~e:_ = do_panic ()

  (* ---- float helpers ---- *)

  let float_is (fp : Typed.FloatClass.t) =
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

  let begin_panic ~m:_ ~msg =
    match%ty msg with
    | TExtension TFullPtr -> do_panic ~msg ()
    | _ -> do_panic ()

  (* ---- hashing ---- *)

  let hash_one_ux =
    String.Interned.intern
      "std::hash::BuildHasher::hash_one was stubbed to always hash to 0, to \
       avoid path explosion. This is an under-approximation, some paths may be \
       missed."

  (** Replace the real [SipHasher] with the *constant* hash: every value hashes
      to 0. This is a valid hash function, and avoid branch explosion from
      symbolic hashes, as e.g. hashbrown uses the lower bits of the hash to pick
      a bucket. *)
  let hash_one ~types:_ ~t_self:_ ~t:_ ~self:_ ~x:_ =
    if Soteria.Symex.Approx.As_ctx.is_ox () then
      Soteria.Terminal.Warn.warn_once hash_one_ux;
    ok (Typed.BitVec.u64i 0)

  (* ---- I/O (no-ops) ---- *)

  let _eprint ~args:_ = ok ()
  let _print ~args:_ = ok ()
  let print_to ~t:_ ~args:_ ~global_s:_ ~label:_ = ok ()
  let print_to_buffer_if_capture_used ~args:_ = ok Typed.v_true
end
