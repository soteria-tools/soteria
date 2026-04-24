open Soteria.Symex.Make (Tiny_solver.Z3_solver)
open Syntax
module Compo_res = Soteria.Symex.Compo_res
module Or_gave_up = Soteria.Symex.Or_gave_up

module Testables = struct
  module Compo_res = struct
    let equal (ok_eq : 'ok -> 'ok -> bool) (err_eq : 'err -> 'err -> bool)
        (fix_eq : 'fix -> 'fix -> bool) (a : ('ok, 'err, 'fix) Compo_res.t)
        (b : ('ok, 'err, 'fix) Compo_res.t) : bool =
      match (a, b) with
      | Compo_res.Ok x, Compo_res.Ok y -> ok_eq x y
      | Compo_res.Error e1, Compo_res.Error e2 -> err_eq e1 e2
      | Compo_res.Missing f1, Compo_res.Missing f2 ->
          List.length f1 = List.length f2 && List.for_all2 fix_eq f1 f2
      | _ -> false

    let pp = Compo_res.pp
  end

  module Or_gave_up = struct
    let equal eq (a : 'a Or_gave_up.t) (b : 'a Or_gave_up.t) : bool =
      match (a, b) with
      | Or_gave_up.Gave_up x, Or_gave_up.Gave_up y -> eq x y
      | Or_gave_up.E e1, Or_gave_up.E e2 -> e1 = e2
      | _ -> false

    let pp pp_a fmt = function
      | Or_gave_up.Gave_up e -> Format.fprintf fmt "Gave up(%s)" e
      | Or_gave_up.E x -> Format.fprintf fmt "E (%a)" pp_a x
  end
end

let compo_res ok err miss =
  let pp =
    Testables.Compo_res.pp ~ok:(Alcotest.pp ok) ~err:(Alcotest.pp err)
      ~miss:(Alcotest.pp miss)
  in
  let equal =
    Testables.Compo_res.equal (Alcotest.equal ok) (Alcotest.equal err)
      (Alcotest.equal miss)
  in
  Alcotest.testable pp equal

let or_gave_up a =
  let pp = Testables.Or_gave_up.pp (Alcotest.pp a) in
  let equal = Testables.Or_gave_up.equal (Alcotest.equal a) in
  Alcotest.testable pp equal

let fuel = Soteria.Symex.Fuel_gauge.infinite

let process =
  let* b = nondet Typed.t_bool in
  if%sat b then Result.error "Left error" else Result.error "Right error"

let process_give_up =
  let* b = nondet Typed.t_bool in
  if%sat b then give_up "Left gave up" else Result.error "Right error"

let test_fail_fast () =
  let results =
    Result.run ~fuel ~mode:OX ~fail_fast:true process |> List.map fst
  in
  Alcotest.(check (list (compo_res unit (or_gave_up string) unit)))
    "The first branch returns an error and the second is not executed"
    [ Compo_res.Error (Or_gave_up.E "Left error") ]
    results

let test_no_fail_fast () =
  let results =
    Result.run ~fuel ~mode:OX ~fail_fast:false process |> List.map fst
  in
  Alcotest.(check (list (compo_res unit (or_gave_up string) unit)))
    "Both branches return errors"
    [
      Compo_res.Error (Or_gave_up.E "Left error");
      Compo_res.Error (Or_gave_up.E "Right error");
    ]
    results

let test_fail_fast_give_up () =
  let results =
    Result.run ~fuel ~mode:OX ~fail_fast:true process_give_up |> List.map fst
  in
  Alcotest.(check (list (compo_res unit (or_gave_up string) unit)))
    "The first branch returns an error and the second is not executed"
    [ Compo_res.Error (Or_gave_up.Gave_up "Left gave up") ]
    results

let test_no_fail_fast_give_up () =
  let results =
    Result.run ~fuel ~mode:OX ~fail_fast:false process_give_up |> List.map fst
  in
  Alcotest.(check (list (compo_res unit (or_gave_up string) unit)))
    "Both branches return errors"
    [
      Compo_res.Error (Or_gave_up.Gave_up "Left gave up");
      Compo_res.Error (Or_gave_up.E "Right error");
    ]
    results

let () =
  Alcotest.run "Fail_fast"
    [
      ( "Fail_fast",
        [
          Alcotest.test_case "Running with fail_fast stops early" `Quick
            test_fail_fast;
          Alcotest.test_case "Running without fail_fast explores all branches"
            `Quick test_no_fail_fast;
          Alcotest.test_case "Running with fail_fast stops early on give_up"
            `Quick test_fail_fast_give_up;
          Alcotest.test_case
            "Running without fail_fast explores all branches even on give_up"
            `Quick test_no_fail_fast_give_up;
        ] );
    ]
