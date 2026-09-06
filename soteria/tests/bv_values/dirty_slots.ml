open Soteria.Bv_values
module Typed = Typed.Make (Svalue.Dummy_ext) ()
module Solver = Bv_solver.Z3_solver (Typed)
open Typed
open Typed.Infix

let t_int = t_bv 8
let int n = BitVec.mki 8 n
let result = Alcotest.testable Soteria.Symex.Solver_result.pp ( = )

let check_sat solver expected msg =
  Alcotest.check result msg expected (Solver.sat solver)

let fresh solver = mk_var (Solver.fresh_var solver t_int) t_int

(* [c < 5] and [b = c] are absorbed by the analyses into Dirty slots, [a < b] is
   a plain assertion. Once everything is checked, adding [a > 10] only starts
   the dependency set with [a]; [b] is reached through [a < b], and [c] must
   then be reached through the Dirty slot [{b, c}] even though that slot was
   scanned (and found irrelevant) before [b] was added. *)
let dirty_slot_revisited () =
  let solver = Solver.init () in
  let a = fresh solver in
  let b = fresh solver in
  let c = fresh solver in
  Solver.add_constraints solver [ c <@ int 5 ];
  Solver.add_constraints solver [ a <@ b ];
  Solver.add_constraints solver [ b ==@ c ];
  check_sat solver Sat "c < 5 && a < b && b = c";
  Solver.add_constraints solver [ a >@ int 10 ];
  check_sat solver Unsat "c < 5 && a < b && b = c && a > 10"

let () =
  Alcotest.run "Dirty_slots"
    [
      ( "relevance",
        [
          Alcotest.test_case "dirty slot revisited" `Quick dirty_slot_revisited;
        ] );
    ]
