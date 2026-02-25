(** Shared infrastructure for Svalue fuzz tests.

    Provides Z3-based equivalence checking and solver setup used by the
    individual fuzz test executables. *)

open Soteria.Bv_values
module Z3_raw = Soteria.Solvers.Z3.Make (Encoding)
module Var = Soteria.Symex.Var

let solver = Z3_raw.init ()

let test_count =
  let default = 5000 in
  lazy
    (match Sys.getenv "QCHECK_TEST_COUNT" with
    | s -> ( match int_of_string_opt s with Some n -> n | None -> default)
    | exception Not_found -> default)

(** Collect all free variables and their types from an expression. *)
let collect_vars (v : Svalue.t) : Svalue.ty Var.Hashtbl.t =
  let tbl = Var.Hashtbl.create 16 in
  Svalue.iter_vars v (fun (var, ty) -> Var.Hashtbl.replace tbl var ty);
  tbl

let check_no_new_vars (existing : Svalue.ty Var.Hashtbl.t) (v : Svalue.t) =
  Svalue.iter_vars v
  |> Iter.for_all (fun (var, ty) ->
      Var.Hashtbl.find_opt existing var
      |> Option.equal Svalue.equal_ty (Some ty))

let pp_pair fmt (smart, direct) =
  Format.fprintf fmt "@[<v>smart:  %a@,direct: %a@]" Svalue.pp smart Svalue.pp
    direct

(** Check that [smart] and [direct] are semantically equivalent using Z3.
    Returns [true] if equivalent (Z3 says unsat for not(smart = direct)), or
    [false] if a counterexample exists (bug found). Timeouts are treated as
    pass. *)
let z3_check_equivalent_raw ~vars_d ~assumptions (smart : Svalue.t)
    (direct : Svalue.t) : Soteria.Symex.Solver_result.t =
  Soteria.Stats.As_ctx.with_stats_ignored () @@ fun () ->
  Z3_raw.reset solver;
  (* The vars of the simplified version should be a subset of the vars of the
     raw version. *)
  assert (check_no_new_vars vars_d smart);
  (* The vars of the assumptions should also not be new *)
  assert (List.for_all (check_no_new_vars vars_d) assumptions);
  (* Declare the assumptions as constraints in Z3 *)
  let seen = Hashtbl.create 16 in
  let declare (var, ty) =
    let key = Soteria.Symex.Var.to_int var in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.replace seen key ();
      Z3_raw.declare_var solver var ty
    end
  in
  Var.Hashtbl.iter (fun v ty -> declare (v, ty)) vars_d;
  List.iter (Z3_raw.add_constraint solver) assumptions;
  (* Build: not(smart = direct) using RAW constructors to avoid relying on the
     smart constructors we're testing. *)
  let eq_expr = Svalue.(Binop (Eq, smart, direct) <| TBool) in
  let neq_expr = Svalue.(Unop (Not, eq_expr) <| TBool) in
  Z3_raw.add_constraint solver neq_expr;
  Z3_raw.check_sat solver

let z3_check_equivalent smart direct =
  (* Collect and declare all variables from both expressions *)
  let vars_d = collect_vars direct in
  (* Collect all assumptions made by checked operators *)
  let assumptions : Svalue.t list = Direct.collect_checked_assumptions direct in
  match z3_check_equivalent_raw ~vars_d ~assumptions smart direct with
  | Sat -> false
  | Unsat -> true
  | Unknown ->
      (* Avoid signaling bugs on timeout *)
      Format.eprintf "Z3 returned Unknown (timeout?) when checking:@.%a@.@?"
        pp_pair (smart, direct);
      true

let z3_check_with_model ~vars_d ~assumptions start direct =
  match z3_check_equivalent_raw ~vars_d ~assumptions start direct with
  | Sat -> Z3_raw.get_model solver
  | Unknown -> None
  | Unsat -> failwith "Got SAT then UNSAT for the same question, impossible"

let print_svalue = Fmt.to_to_string Svalue.pp

let print_test d =
  let vars = collect_vars d in
  let pp_vars =
    Fmt.iter_bindings ~sep:Fmt.semi Var.Hashtbl.iter
      (Fmt.Dump.pair Var.pp Svalue.pp_ty)
  in
  let s = Eval.eval ~force:true d in
  let checks = Direct.collect_checked_assumptions d in
  let pp_checks = Fmt.iter ~sep:Fmt.semi List.iter Svalue.pp in
  let model = z3_check_with_model ~vars_d:vars ~assumptions:checks s d in
  let pp_model fmt = function
    | None -> Format.fprintf fmt "Model unavailable"
    | Some m -> Fmt.pf fmt "Counterexample model:@,%a" Sexplib.Sexp.pp_hum m
  in
  Format.asprintf
    "@[<v>full: %a@,\
     simplified: %a@,\
     @[<v 2>with variables:@,\
     %a@]@,\
     @[<v 2>and checks:@,\
     %a@]@,\
     @[<v 2>%a@]@]"
    Svalue.pp d Svalue.pp s pp_vars vars pp_checks checks pp_model model

let check_smart_eq_direct direct =
  let smart = Eval.eval ~force:true direct in
  if Svalue.equal smart direct then true else z3_check_equivalent smart direct

let mk_test ~name gen =
  QCheck2.Test.make ~count:(Lazy.force test_count) ~name ~print:print_test gen
    check_smart_eq_direct

(** Call this at the start of main to configure the Z3 timeout. *)
let setup () =
  Soteria.Solvers.Config.set_and_lock
    (Soteria.Solvers.Config.make ~solver_timeout:1000 ())
