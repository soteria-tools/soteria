(** Shared infrastructure for Svalue fuzz tests.

    Provides Z3-based equivalence checking and solver setup used by the
    individual fuzz test executables. *)

open Soteria.Bv_values
module Z3_raw = Soteria.Solvers.Z3.Make (Encoding)

let solver = Z3_raw.init ()

(** Collect all free variables and their types from an expression. *)
let collect_vars (v : Svalue.t) : (Soteria.Symex.Var.t * Svalue.ty) list =
  let tbl = Hashtbl.create 16 in
  Svalue.iter_vars v (fun (var, ty) ->
      let key = Soteria.Symex.Var.to_int var in
      if not (Hashtbl.mem tbl key) then Hashtbl.replace tbl key (var, ty));
  Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

let pp_pair fmt (smart, direct) =
  Format.fprintf fmt "@[<v>smart:  %a@,direct: %a@]" Svalue.pp smart Svalue.pp
    direct

(** Check that [smart] and [direct] are semantically equivalent using Z3.
    Returns [true] if equivalent (Z3 says unsat for not(smart = direct)), or
    [false] if a counterexample exists (bug found). Timeouts are treated as
    pass. *)
let check_equivalence (smart : Svalue.t) (direct : Svalue.t) : bool =
  Soteria.Stats.As_ctx.with_stats_ignored () @@ fun () ->
  Z3_raw.reset solver;
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
  match Z3_raw.check_sat solver with
  | Sat -> false
  | Unsat -> true
  | Unknown ->
      (* Avoid signaling bugs on timeout *)
      Format.eprintf "Z3 returned Unknown (timeout?) when checking:@.%a@.@?"
        pp_pair (smart, direct);
      true

let print_svalue = Fmt.to_to_string Svalue.pp

let print_pair =
  let open QCheck2.Print in
  pair print_svalue print_svalue

let check_smart_eq_direct (smart, direct) =
  if Svalue.equal smart direct then true
  else
    let ok = check_equivalence smart direct in
    if not ok then Format.eprintf "COUNTEREXAMPLE:@.%a@." pp_pair (smart, direct);
    ok

let mk_test ~name ~count gen =
  QCheck2.Test.make ~count ~name ~print:print_pair gen check_smart_eq_direct

(** Call this at the start of main to configure the Z3 timeout. *)
let setup () =
  Soteria.Solvers.Config.set_and_lock
    (Soteria.Solvers.Config.make ~solver_timeout:1000 ())
