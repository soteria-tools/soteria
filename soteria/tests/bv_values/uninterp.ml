(* Uninterpreted functions in the solver: applications must be re-checked
   together with the constraints they may interact with, even when they contain
   no variable. *)

open Soteria.Bv_values
module Typed = Typed.Make (Svalue.Dummy_ext) ()
module Solver = Bv_solver.Z3_solver (Typed)
open Typed
open Typed.Infix

let t_int = t_bv 8
let int n = BitVec.mki 8 n
let f = Soteria.Soteria_std.String.Interned.intern "f"
let f_ x = mk_uninterp f [ untyped x ] t_int
let result = Alcotest.testable Soteria.Symex.Solver_result.pp ( = )

let check_sat solver expected msg =
  Alcotest.check result msg expected (Solver.sat solver)

let fresh solver = mk_var (Solver.fresh_var solver t_int) t_int

let same_ground_application () =
  let solver = Solver.init () in
  Solver.add_constraints solver [ f_ (int 3) <@ int 10 ];
  check_sat solver Sat "f(3) < 10";
  Solver.add_constraints solver [ f_ (int 3) >@ int 20 ];
  check_sat solver Unsat "f(3) < 10 && f(3) > 20"

let distinct_ground_applications () =
  let solver = Solver.init () in
  Solver.add_constraints solver [ f_ (int 3) <@ int 10 ];
  check_sat solver Sat "f(3) < 10";
  Solver.add_constraints solver [ f_ (int 4) >@ int 20 ];
  check_sat solver Sat "f(3) < 10 && f(4) > 20"

let ground_and_symbolic_applications () =
  let solver = Solver.init () in
  let x = fresh solver in
  Solver.add_constraints solver [ f_ (int 3) <@ int 3 ];
  check_sat solver Sat "f(3) < 3";
  Solver.add_constraints solver [ f_ x >@ int 5 ];
  check_sat solver Sat "f(3) < 3 && f(x) > 5";
  Solver.add_constraints solver [ x ==@ int 3 ];
  check_sat solver Unsat "f(3) < 3 && f(x) > 5 && x = 3"

let symbolic_then_ground_application () =
  let solver = Solver.init () in
  let x = fresh solver in
  Solver.add_constraints solver [ f_ x >@ int 5; x ==@ int 3 ];
  check_sat solver Sat "f(x) > 5 && x = 3";
  Solver.add_constraints solver [ f_ (int 3) <@ int 3 ];
  check_sat solver Unsat "f(x) > 5 && x = 3 && f(3) < 3"

let bound_application () =
  let solver = Solver.init () in
  let exists =
    exists_1 ~not_in:v_true t_int (fun y -> f_ y >@ int 5 &&@ (y ==@ int 3))
  in
  Solver.add_constraints solver [ exists ];
  check_sat solver Sat "exists y. f(y) > 5 && y = 3";
  Solver.add_constraints solver [ f_ (int 3) <@ int 3 ];
  check_sat solver Unsat "(exists y. f(y) > 5 && y = 3) && f(3) < 3"

(* [f(exists y. y = 3)] and [f(true)] have semantically equal arguments, so they
   must be related despite being syntactically distinct. *)
let quantified_argument () =
  let solver = Solver.init () in
  let closed =
    exists_1 ~not_in:v_true t_int (fun y -> y ==@ int 3) |> untyped
  in
  Solver.add_constraints solver [ mk_uninterp f [ closed ] t_int <@ int 3 ];
  check_sat solver Sat "f(exists y. y = 3) < 3";
  Solver.add_constraints solver
    [ mk_uninterp f [ untyped v_true ] t_int >@ int 5 ];
  check_sat solver Unsat "f(exists y. y = 3) < 3 && f(true) > 5"

let unrelated_application_stays_satisfiable () =
  let solver = Solver.init () in
  let x = fresh solver in
  Solver.add_constraints solver [ f_ (int 3) <@ int 3; x >@ int 5 ];
  check_sat solver Sat "f(3) < 3 && x > 5";
  Solver.add_constraints solver [ x <@ int 10 ];
  check_sat solver Sat "f(3) < 3 && x > 5 && x < 10"

let () =
  Alcotest.run "Uninterp"
    [
      ( "relevance",
        [
          Alcotest.test_case "same ground application" `Quick
            same_ground_application;
          Alcotest.test_case "distinct ground applications" `Quick
            distinct_ground_applications;
          Alcotest.test_case "ground and symbolic applications" `Quick
            ground_and_symbolic_applications;
          Alcotest.test_case "symbolic then ground application" `Quick
            symbolic_then_ground_application;
          Alcotest.test_case "bound application" `Quick bound_application;
          Alcotest.test_case "quantified argument" `Quick quantified_argument;
          Alcotest.test_case "unrelated application stays satisfiable" `Quick
            unrelated_application_stays_satisfiable;
        ] );
    ]
