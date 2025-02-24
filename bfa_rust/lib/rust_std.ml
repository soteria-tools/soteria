open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
module T = Typed.T

module M (Heap : Heap_intf.S) = struct
  let assert_ ~prog:_ ~(args : T.cval Typed.t list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ t ] ->
          Rustsymex.of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error `FailedAssert state
    else Result.ok (Charon_util.Base 0s, state)

  let nondet ~prog:_ ~args:_ ~state ty =
    let* value = Layout.nondet ty in
    Result.ok (value, state)
end
