(* Thread-safety stress test. Spawns several domains, each independently running
   the same symbolic execution many times. This concurrently exercises the
   genuinely-global shared state that was made thread-safe: - hash-consing of
   symbolic values ([Hc.Make_thread_safe]), - the shared encode / sat-check memo
   caches ([Concurrent_tbl]), - the per-analysis solver pool (one Z3 per domain,
   [Mutex]-protected), - the [trivial_model_works] RNG path (domain-local
   [Random.State]).

   It is a test harness only — the library itself stays sequential. A data race
   would surface either as an exception (re-raised by [Domain.join]) or as a
   branch result diverging from the single-threaded baseline. *)

open Soteria.Symex.Make (Tiny_solver.Z3_solver)
open Syntax

let fuel = Soteria.Symex.Fuel_gauge.infinite

(* Same shape as the determinism test in [run_twice.ml]: branches on a symbolic
   value so every run hits the solver and the memo caches. *)
let fn () =
  let open Typed.Infix in
  let* v = nondet Typed.t_int in
  let* () = assume [ v ==@ Typed.zero ] in
  if%sat Typed.not (v ==@ Typed.one) then return true else return false

let pp =
  let open Fmt in
  list ~sep:semi (braces (pair ~sep:comma bool (Dump.list Typed.ppa)))

let run_once () = Fmt.str "%a" pp (run ~fuel ~mode:OX (fn ()))

let parallel_runs_match_sequential () =
  (* Force the lazily-initialised logger while still single-threaded:
     [Lazy.force] is not safe under concurrent first force across domains. *)
  Soteria.Logs.warmup ();
  let baseline = run_once () in
  let ndomains = 4 and iters = 30 in
  let worker () =
    let all_ok = ref true in
    for _ = 1 to iters do
      if run_once () <> baseline then all_ok := false
    done;
    !all_ok
  in
  let domains = List.init ndomains (fun _ -> Domain.spawn worker) in
  (* [Domain.join] re-raises any exception thrown in the worker. *)
  let results = List.map Domain.join domains in
  Alcotest.(check bool)
    "every domain matched the sequential baseline" true
    (List.for_all Fun.id results);
  Alcotest.(check bool) "baseline produced branches" true (baseline <> "")

let () =
  Alcotest.run "Parallel_thread_safety"
    [
      ( "Parallel_thread_safety",
        [
          Alcotest.test_case "parallel_runs_match_sequential" `Quick
            parallel_runs_match_sequential;
        ] );
    ]
