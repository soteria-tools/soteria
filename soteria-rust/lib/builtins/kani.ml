open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Charon_util

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr

  type nonrec rust_val = Sptr.t rust_val

  let assert_ ~crate:_ ~(args : rust_val list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Base t; _msg ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error `FailedAssert state
    else Result.ok (Charon_util.unit_, state)

  let assume ~crate:_ ~args ~state =
    let* to_assume =
      match args with
      | [ Base t ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "assume with non-one arguments"
    in
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
    let* () = assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let kani_nondet (fun_sig : Charon.UllbcAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)
end
