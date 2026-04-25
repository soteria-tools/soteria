open Rust_val

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax
  module Core = Core.M (StateM)

  let soteria_assert ~args =
    let to_assert, msg =
      match args with
      | [ Int t; Ptr msg ] -> (Typed.cast_lit TBool t, msg)
      | _ -> failwith "to_assert with non-one arguments"
    in
    if%sat Typed.not (Typed.BitVec.to_bool to_assert) then
      let* str = Core.parse_string msg in
      error (`FailedAssert str)
    else ok unit_

  let soteria_assume ~args =
    let to_assume =
      match args with
      | [ Int t ] -> Typed.cast_lit TBool t
      | _ -> failwith "assume with non-one arguments"
    in
    [%l.debug "Assuming: %a\n" Typed.ppa to_assume];
    let+ () = assume [ Typed.BitVec.to_bool to_assume ] in
    unit_

  let nondet_bytes ~types ~args:_ =
    let output =
      match types with
      | [ ty ] -> ty
      | _ -> failwith "unexpected type args in nondet_bytes"
    in
    Encoder.nondet_valid output

  let soteria_panic ~args =
    let* msg =
      match args with [ Ptr msg ] -> Core.parse_string msg | _ -> ok None
    in
    error (`Panic msg)

  let kani_assert = soteria_assert
  let kani_assume = soteria_assume
  let kani_panic = soteria_panic
end
