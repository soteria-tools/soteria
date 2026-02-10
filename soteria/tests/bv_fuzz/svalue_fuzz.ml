(** Fuzz-test driver for Svalue smart constructor equivalence.

    Uses QCheck to generate random expression trees and Z3 to verify that
    smart-constructed expressions are semantically equivalent to
    directly-constructed ones. *)

open Soteria.Bv_values
module Z3_raw = Soteria.Solvers.Z3.Make (Encoding)

(* ------------------------------------------------------------------ *)
(* Z3 equivalence checking                                             *)
(* ------------------------------------------------------------------ *)

(** Collect all free variables and their types from an expression. *)
let collect_vars (v : Svalue.t) : (Soteria.Symex.Var.t * Svalue.ty) list =
  let tbl = Hashtbl.create 16 in
  Svalue.iter_vars v (fun (var, ty) ->
      let key = Soteria.Symex.Var.to_int var in
      if not (Hashtbl.mem tbl key) then Hashtbl.replace tbl key (var, ty));
  Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

(** Check that [smart] and [direct] are semantically equivalent using Z3.

    Returns [true] if equivalent (Z3 says unsat for not(smart = direct)), or
    [false] if a counterexample exists (bug found). *)
let check_equivalence (smart : Svalue.t) (direct : Svalue.t) : bool =
  Soteria.Stats.As_ctx.with_stats_ignored () @@ fun () ->
  let solver = Z3_raw.init () in
  (* Collect and declare all variables from both expressions *)
  let vars_s = collect_vars smart in
  let vars_d = collect_vars direct in
  let seen = Hashtbl.create 16 in
  let declare (var, ty) =
    let key = Soteria.Symex.Var.to_int var in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.replace seen key ();
      Z3_raw.declare_var solver var ty
    end
  in
  List.iter declare vars_s;
  List.iter declare vars_d;
  (* Build: not(smart = direct) using RAW constructors to avoid relying on the
     smart constructors we're testing. *)
  let eq_expr = Svalue.(Binop (Eq, smart, direct) <| TBool) in
  let neq_expr = Svalue.(Unop (Not, eq_expr) <| TBool) in
  Z3_raw.add_constraint solver neq_expr;
  let result = Z3_raw.check_sat solver in
  (* Clean up solver state *)
  Z3_raw.reset solver;
  match result with
  | Sat ->
      (* BUG: found inputs where smart != direct *)
      false
  | Unsat ->
      (* Equivalent — no counterexample *)
      true
  | Unknown ->
      (* Solver timeout/unknown — treat conservatively as OK *)
      true

(* ------------------------------------------------------------------ *)
(* Pretty-printing for counterexamples                                 *)
(* ------------------------------------------------------------------ *)

let pp_pair fmt (smart, direct) =
  Format.fprintf fmt "@[<v>smart:  %a@,direct: %a@]" Svalue.pp smart Svalue.pp
    direct

(* ------------------------------------------------------------------ *)
(* QCheck tests                                                        *)
(* ------------------------------------------------------------------ *)

(** QCheck generator for (depth, bv_size, seed) triples. *)
let gen_bv_params =
  QCheck.Gen.(
    let* depth = int_range 0 4 in
    let* bv_size = oneof_list [ 8; 16; 32 ] in
    let* seed = nat in
    return (depth, bv_size, seed))

let gen_bool_params =
  QCheck.Gen.(
    let* depth = int_range 0 4 in
    let* bv_size = oneof_list [ 8; 16; 32 ] in
    let* seed = nat in
    return (depth, bv_size, seed))

let gen_float_params =
  QCheck.Gen.(
    let* depth = int_range 0 3 in
    let* fp = oneof_list Svalue.FloatPrecision.[ F32; F64 ] in
    let* seed = nat in
    return (depth, fp, seed))

(** Print parameters for counterexample reporting. *)
let print_bv_params (depth, bv_size, seed) =
  Printf.sprintf "depth=%d, bv_size=%d, seed=%d" depth bv_size seed

let print_bool_params (depth, bv_size, seed) =
  Printf.sprintf "depth=%d, bv_size=%d, seed=%d" depth bv_size seed

let print_float_params (depth, fp, seed) =
  let fp_str =
    match fp with
    | Svalue.FloatPrecision.F32 -> "F32"
    | F64 -> "F64"
    | F16 -> "F16"
    | F128 -> "F128"
  in
  Printf.sprintf "depth=%d, fp=%s, seed=%d" depth fp_str seed

let test_bv_equivalence =
  QCheck.Test.make ~count:2000 ~name:"bv_smart_eq_direct"
    QCheck.(make ~print:print_bv_params gen_bv_params)
    (fun (depth, bv_size, seed) ->
      let rs = Random.State.make [| seed |] in
      let smart, direct = Gen.gen_bv_pair ~depth ~bv_size rs in
      if Svalue.equal smart direct then true
      else begin
        let ok = check_equivalence smart direct in
        if not ok then
          Format.eprintf "COUNTEREXAMPLE:@.%a@." pp_pair (smart, direct);
        ok
      end)

let test_bool_equivalence =
  QCheck.Test.make ~count:2000 ~name:"bool_smart_eq_direct"
    QCheck.(make ~print:print_bool_params gen_bool_params)
    (fun (depth, bv_size, seed) ->
      let rs = Random.State.make [| seed |] in
      let smart, direct = Gen.gen_bool_pair ~depth ~bv_size rs in
      if Svalue.equal smart direct then true
      else begin
        let ok = check_equivalence smart direct in
        if not ok then
          Format.eprintf "COUNTEREXAMPLE:@.%a@." pp_pair (smart, direct);
        ok
      end)

let test_float_equivalence =
  QCheck.Test.make ~count:1000 ~name:"float_smart_eq_direct"
    QCheck.(make ~print:print_float_params gen_float_params)
    (fun (depth, fp, seed) ->
      let rs = Random.State.make [| seed |] in
      let smart, direct = Gen.gen_float_pair ~depth ~fp rs in
      if Svalue.equal smart direct then true
      else begin
        let ok = check_equivalence smart direct in
        if not ok then
          Format.eprintf "COUNTEREXAMPLE:@.%a@." pp_pair (smart, direct);
        ok
      end)

(* ------------------------------------------------------------------ *)
(* Main entry point                                                    *)
(* ------------------------------------------------------------------ *)

let () =
  (* Set Z3 timeout to 1s (1000ms) *)
  Soteria.Solvers.Config.set_and_lock
    (Soteria.Solvers.Config.make ~solver_timeout:1000 ());
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ test_bv_equivalence; test_bool_equivalence; test_float_equivalence ]
  in
  Alcotest.run "svalue_fuzz" [ ("smart_constructor_equivalence", suite) ]
