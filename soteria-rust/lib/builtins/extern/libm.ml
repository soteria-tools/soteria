open Svalue
open Typed.Syntax
open Typed.Infix

(** The [libm] symbols [std] declares in [sys::cmath]; everything else in
    [f*::{sin, exp, ...}] goes through an intrinsic instead. Like the intrinsics
    these are approximated: a concrete argument is evaluated with the host's
    libm, a symbolic one only gets the range the function is known to lie in. *)
type fn =
  | Unary of (float -> float) option * bounds
  | Binary of (float -> float -> float) * bounds
  | Lgamma_r
  | Ldexp

(** The values the result is known to lie between, as float literals. These only
    hold when the arguments aren't NaN. *)
and bounds = { lo : string option; hi : string option }

let unbounded = { lo = None; hi = None }
let atleast lo = { lo = Some lo; hi = None }
let between lo hi = { lo = Some lo; hi = Some hi }

(** [fdim x y = max (x - y) 0], with NaN propagated. *)
let fdim x y =
  if Stdlib.Float.is_nan x then x
  else if Stdlib.Float.is_nan y then y
  else if x > y then x -. y
  else 0.0

(** [f32], [f64] and [f128] versions of a symbol; [f16] is downcasted from [f32]
*)
let variants (name, fn) = [ (name, fn); (name ^ "f", fn); (name ^ "f128", fn) ]

let fn_pats =
  List.concat_map variants
    [
      ("acos", Unary (Some Stdlib.Float.acos, unbounded));
      ("acosh", Unary (Some Stdlib.Float.acosh, atleast "0"));
      ("asin", Unary (Some Stdlib.Float.asin, unbounded));
      ("asinh", Unary (Some Stdlib.Float.asinh, unbounded));
      ("atan", Unary (Some Stdlib.Float.atan, unbounded));
      ("atanh", Unary (Some Stdlib.Float.atanh, unbounded));
      ("atan2", Binary (Stdlib.Float.atan2, unbounded));
      ("cbrt", Unary (Some Stdlib.Float.cbrt, unbounded));
      ("cosh", Unary (Some Stdlib.Float.cosh, atleast "1"));
      ("erf", Unary (Some Stdlib.Float.erf, between "-1" "1"));
      ("erfc", Unary (Some Stdlib.Float.erfc, between "0" "2"));
      ("expm1", Unary (Some Stdlib.Float.expm1, atleast "-1"));
      ("fdim", Binary (fdim, atleast "0"));
      ("hypot", Binary (Stdlib.Float.hypot, atleast "0"));
      ("log1p", Unary (Some Stdlib.Float.log1p, unbounded));
      ("sinh", Unary (Some Stdlib.Float.sinh, unbounded));
      ("tan", Unary (Some Stdlib.Float.tan, unbounded));
      ("tanh", Unary (Some Stdlib.Float.tanh, between "-1" "1"));
      (* OCaml has no gamma, so these are always over-approximated *)
      ("tgamma", Unary (None, unbounded));
    ]
  @ [
      ("ldexp", Ldexp);
      ("lgamma_r", Lgamma_r);
      ("lgammaf_r", Lgamma_r);
      ("lgammaf128_r", Lgamma_r);
    ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax
  module Core = Core.M (StateM)

  (** A nondeterministic result of the same precision as [x], within [bounds]
      unless one of [args] is NaN. *)
  let over_approximate ~bounds ~args x =
    let fp = Typed.Float.fp_of x in
    let any_nan =
      List.fold_left (fun acc a -> acc ||@ Typed.Float.is_nan a) Typed.v_false
    in
    if%sat any_nan args then ok (Typed.Float.nan fp)
    else
      let* res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
      let res = Typed.cast_float res in
      let bound cmp = Option.map (fun l -> cmp res (Typed.Float.mk fp l)) in
      let+ () =
        assume
          (List.filter_map Fun.id
             [ bound ( >=.@ ) bounds.lo; bound ( <=.@ ) bounds.hi ])
      in
      res

  let unary host bounds args =
    let x =
      match args with
      | [ x ] -> Typed.cast_float x
      | _ -> L.failwith "libm: invalid arguments"
    in
    let* () = Core.floating_inaccuracy_warn () in
    match Option.bind (fun host -> Typed.Float.approx host x) host with
    | Some res -> ok res
    | None -> over_approximate ~bounds ~args:[ x ] x

  let binary host bounds args =
    let x, y =
      match args with
      | [ x; y ] -> (Typed.cast_float x, Typed.cast_float y)
      | _ -> L.failwith "libm: invalid arguments"
    in
    let* () = Core.floating_inaccuracy_warn () in
    match Typed.Float.approx2 host x y with
    | Some res -> ok res
    | None -> over_approximate ~bounds ~args:[ x; y ] x

  (** [lgamma_r(x, &mut sign)] returns [ln |Γ(x)|] and writes the sign of [Γ(x)]
      to its out parameter. *)
  let lgamma_r args =
    let x, sign =
      match args with
      | [ x; sign ] -> (Typed.cast_float x, Typed.cast_ptr_f sign)
      | _ -> L.failwith "lgamma_r: invalid arguments"
    in
    let* () = Core.floating_inaccuracy_warn () in
    let* sign_val = Value_codec.nondet_valid (TLiteral (TInt I32)) in
    let* () = assume [ sign_val ==@ U32.(1s) ||@ (sign_val ==@ U32.(-1s)) ] in
    let* () = State.store sign (TLiteral (TInt I32)) sign_val in
    over_approximate ~bounds:unbounded ~args:[ x ] x

  (** [ldexp(x, n) = x * 2^n]; exact whenever it doesn't over- or underflow. *)
  let ldexp args =
    let x, n =
      match args with
      | [ x; n ] -> (Typed.cast_float x, Typed.cast_lit (TInt I32) n)
      | _ -> L.failwith "ldexp: invalid arguments"
    in
    let* () = Core.floating_inaccuracy_warn () in
    let host n =
      Z.to_int_opt (Typed.BitVec.bv_to_z (Signed I32) n)
      |> Option.bind (fun n ->
          Typed.Float.approx (fun x -> Stdlib.Float.ldexp x n) x)
    in
    match Option.bind host (Typed.BitVec.to_z n) with
    | Some res -> ok res
    | None -> over_approximate ~bounds:unbounded ~args:[ x ] x

  let[@inline] fn_to_stub = function
    | Unary (host, bounds) -> unary host bounds
    | Binary (host, bounds) -> binary host bounds
    | Lgamma_r -> lgamma_r
    | Ldexp -> ldexp
end
