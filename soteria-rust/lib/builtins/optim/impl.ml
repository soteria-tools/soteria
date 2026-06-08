open Svalue

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
      ]} *)
  let alloc_impl ~self:_ ~layout ~zeroed =
    let zeroed = (zeroed :> Typed.T.sbool Typed.t) in
    let size, align =
      let layout = Typed.cast_any_adt layout in
      match Typed.Adt.as_tuple layout with
      | [ size; align ] -> (
          let size = Typed.cast_i Usize size in
          let align = Typed.cast_any_adt align in
          match Typed.Adt.as_tuple align with
          | [ align_enum ] ->
              let align_enum = Typed.cast_any_adt align_enum in
              let align = Typed.Adt.discriminant_of align_enum in
              (size, Typed.cast_i Usize align)
          | _ -> Fmt.failwith "alloc_impl: invalid layout: %a" Typed.ppa layout)
      | _ -> Fmt.failwith "alloc_impl: invalid layout: %a" Typed.ppa layout
    in
    let mk_res ptr len =
      let ptr = Typed.Ptr.mk_ptr_f ptr (Some len) in
      let nonnull = Typed.Adt.mk_tuple [ ptr ] in
      Typed.Adt.mk_enum Usize.(0s) [ nonnull ]
    in
    if%sat size ==@ Usize.(0s) then
      let dangling = Sptr.of_address align in
      ok (mk_res dangling Usize.(0s))
    else
      let* zeroed = if%sat zeroed then ok true else ok false in
      let align = Typed.Adt.mk_tuple [ Typed.Adt.mk_enum align [] ] in
      let+ ptr = Alloc.alloc ~zeroed [ size; align ] in
      let ptr = Typed.Ptr.ptr_of ptr in
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
    match Typed.get_ty msg with
    | TExtension FullPtr -> do_panic ~msg:(Typed.cast msg) ()
    | _ -> do_panic ()

  (* ---- I/O (no-ops) ---- *)

  let _eprint ~args:_ = ok ()
  let _print ~args:_ = ok ()
  let print_to ~t:_ ~args:_ ~global_s:_ ~label:_ = ok ()
  let print_to_buffer_if_capture_used ~args:_ = ok Typed.v_true
end
