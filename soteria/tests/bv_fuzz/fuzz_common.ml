(** Shared infrastructure for Svalue fuzz tests.

    Provides Z3-based equivalence checking and solver setup used by the
    individual fuzz test executables. *)

open Soteria.Bv_values
module Z3_raw = Soteria.Solvers.Z3.Make (Encoding)
module Var = Soteria.Symex.Var

let solver = Z3_raw.init ()

let test_count =
  let default = 10000 in
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
let z3_check_equivalent (smart : Svalue.t) (direct : Svalue.t) : bool =
  Soteria.Stats.As_ctx.with_stats_ignored () @@ fun () ->
  Z3_raw.reset solver;
  (* Collect and declare all variables from both expressions *)
  let vars_d = collect_vars direct in
  (* The vars of the simplified version should be a subset of the vars of the
     raw version. *)
  assert (check_no_new_vars vars_d smart);
  let seen = Hashtbl.create 16 in
  let declare (var, ty) =
    let key = Soteria.Symex.Var.to_int var in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.replace seen key ();
      Z3_raw.declare_var solver var ty
    end
  in
  Var.Hashtbl.iter (fun v ty -> declare (v, ty)) vars_d;
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

let print_pair (s, d) =
  let vars = collect_vars d in
  let pp_vars =
    Fmt.iter_bindings ~sep:Fmt.semi Var.Hashtbl.iter
      (Fmt.Dump.pair Var.pp Svalue.pp_ty)
  in
  Format.asprintf
    "@[<v>full: %a@,simplified: %a@,@[<v 2>with variables:@,%a@]@]" Svalue.pp d
    Svalue.pp s pp_vars vars

let check_smart_eq_direct (smart, direct) =
  if Svalue.equal smart direct then true else z3_check_equivalent smart direct

let mk_test ~name ~count gen =
  QCheck2.Test.make ~count ~name ~print:print_pair gen check_smart_eq_direct

(** Call this at the start of main to configure the Z3 timeout. *)
let setup () =
  Soteria.Solvers.Config.set_and_lock
    (Soteria.Solvers.Config.make ~solver_timeout:1000 ())
