open Svalue

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax
  module Core = Core.M (StateM)

  let soteria_assert ~args =
    let to_assert, msg =
      match args with
      | [ t; msg ] -> (Typed.cast_lit TBool t, Typed.cast_ptr_f msg)
      | _ -> failwith "to_assert with non-one arguments"
    in
    if%sat Typed.(not (BV.to_bool to_assert)) then
      let* str = Core.parse_string msg in
      error (`FailedAssert str)
    else ok (Typed.Adt.mk_tuple [])

  let soteria_assume ~args =
    let to_assume =
      match args with
      | [ t ] -> Typed.cast_lit TBool t
      | _ -> failwith "assume with non-one arguments"
    in
    [%l.debug "Assuming: %a\n" Typed.ppa to_assume];
    let+ () = assume [ Typed.BitVec.to_bool to_assume ] in
    Typed.Adt.mk_tuple []

  let nondet_bytes ~types ~args:_ =
    let output =
      match types with
      | [ ty ] -> ty
      | _ -> failwith "unexpected type args in nondet_bytes"
    in
    let+ res = Value_codec.nondet_valid output in
    Typed.cast res

  let soteria_panic ~args =
    match args with
    | [ msg ] ->
        let msg = Typed.cast_ptr_f msg in
        let* msg = Core.parse_string msg in
        error (`Panic msg)
    | _ -> error (`Panic None)

  let kani_assert = soteria_assert
  let kani_assume = soteria_assume
  let kani_panic = soteria_panic
end
