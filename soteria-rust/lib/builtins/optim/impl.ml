open Rust_val

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
      let dangling = Sptr.of_address align in
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

  let rt_begin_panic ~args =
    match args with Ptr msg :: _ -> do_panic ~msg () | _ -> do_panic ()

  (* ---- I/O (no-ops) ---- *)

  let _eprint ~args:_ = ok ()
  let stdio__print ~args:_ = ok ()
  let print_to ~t:_ ~args:_ ~global_s:_ ~label:_ = ok ()
  let print_to_buffer_if_capture_used ~args:_ = ok Typed.v_true
end
