module Stats = Soteria.Stats
module Compo_res = Soteria.Symex.Compo_res
open Soteria.Logs.Printers
open Syntaxes.FunctionWrap
module State = State.Tree_state
module Interp = Interp.Make (State)
open Frontend.Diagnostic

(** An error happened at runtime during execution *)
exception ExecutionError of string

let execution_err msg = raise (ExecutionError msg)
let pp_branches = pp_plural ~sing:"branch" ~plur:"branches"

let print_pcs pcs =
  let open Fmt in
  let pp_pc ft (pc, i) =
    let name = "PC " ^ string_of_int i ^ ":" in
    if List.is_empty pc then pf ft "%a empty" (pp_style `Bold) name
    else
      let pp_pc = list ~sep:(any " /\\@, ") Typed.ppa in
      pf ft "%a @[<-1>%a@]" (pp_style `Bold) name pp_pc pc
  in
  if not @@ (Soteria.Terminal.Config.get ()).compact then
    let pcs_indexed = List.mapi (fun i pc -> (pc, i + 1)) pcs in
    (list ~sep:(any "@\n") pp_pc) Fmt.stdout pcs_indexed

let print_outcomes entry_name f =
  let time = Unix.gettimeofday () in
  match f () with
  | Ok (pcs, ntotal) ->
      let time = Unix.gettimeofday () -. time in
      Fmt.kstr
        (print_diagnostic_simple ~severity:Note)
        "%s: done in %a, ran %a" entry_name pp_time time pp_branches ntotal;
      print_pcs pcs;
      Fmt.pr "@.@.";
      (entry_name, Outcome.Ok)
  | Error (errs, ntotal) ->
      let time = Unix.gettimeofday () -. time in
      let err_branches =
        List.map (fun (_, _, pcs) -> List.length pcs) errs
        |> List.fold_left ( + ) 0
      in
      if (Config.get ()).fail_fast then
        Fmt.kstr
          (print_diagnostic_simple ~severity:Error)
          "%s: found an issue in %a after exploring %a -- stopped immediately \
           (fail-fast)"
          entry_name pp_time time pp_branches ntotal
      else
        Fmt.kstr
          (print_diagnostic_simple ~severity:Error)
          "%s: found issues in %a, errors in %a (out of %d)" entry_name pp_time
          time pp_branches err_branches ntotal;
      let () =
        let@ error, call_trace, pcs = Fun.flip List.iter errs in
        print_diagnostic ~fname:entry_name ~call_trace ~error;
        Fmt.pr "@.";
        print_pcs pcs;
        Fmt.pr "@.@."
      in
      (entry_name, Outcome.Error)
  | exception e ->
      let time = Unix.gettimeofday () -. time in
      let error, msg =
        match e with
        | ExecutionError msg -> ("runtime error", msg)
        | Soteria.Symex.Gave_up reason -> ("unsupported feature", reason)
        | e ->
            ( "exception",
              Fmt.str "%a@\nTrace: %s" Fmt.exn e (Printexc.get_backtrace ()) )
      in
      Fmt.kstr
        (print_diagnostic_simple ~severity:Warning)
        "%s (%a): %s, %s@.@." entry_name pp_time time error msg;
      (entry_name, Outcome.Fatal)

let exec_crate (crate : Charon.UllbcAst.crate)
    (entry_points : Frontend.entry_point list) =
  let@ () = Crate.with_crate crate in

  (* get entry points to the crate *)
  if List.is_empty entry_points then execution_err "No entry points found";

  (* prepare executing the entry points *)
  let exec_fun = Interp.exec_fun ~args:[] ~state:State.empty in

  let@ { fuel; fun_decl; expect_error } : Frontend.entry_point =
    (Fun.flip List.map) entry_points
  in
  (* execute! *)
  let entry_name = Fmt.to_to_string Crate.pp_name fun_decl.item_meta.name in
  let@ () = print_outcomes entry_name in
  let { res = branches; stats } : 'res Soteria.Stats.with_stats =
    let@ () = L.entry_point_section fun_decl.item_meta.name in
    let@ () = Layout.Session.with_layout_cache in
    let@@ () =
      Rustsymex.Result.run_with_stats ~mode:OX ~fuel
        ~fail_fast:(Config.get ()).fail_fast
    in
    exec_fun fun_decl
  in
  Soteria.Stats.output stats;

  let nbranches = List.length branches in

  let branches =
    (* If any of the results were "Gave_up", we raise an exception to be caught
       by [print_outcomes] *)
    let map_first f (x, y) = (f x, y) in
    List.map
      (map_first
      @@ Fun.flip Compo_res.map_error Soteria.Symex.Or_gave_up.unwrap_exn)
      branches
  in
  (* inverse ok and errors if we expect a failure *)
  let branches =
    if not expect_error then branches
    else
      let open Compo_res in
      let oks, errors =
        branches
        |> List.partition_map @@ function
           | Ok _, pcs ->
               let trace =
                 Soteria.Terminal.Call_trace.singleton
                   ~loc:fun_decl.item_meta.span.data ()
               in
               Left (Error ((`MetaExpectedError, trace), State.empty), pcs)
           | Error _, pcs -> Right (Ok (Rust_val.unit_, State.empty), pcs)
           | v -> Left v
      in
      if List.is_empty errors then oks else errors
  in

  (* check for uncaught failure conditions *)
  let outcomes = List.map fst branches in
  let unexplored =
    Stats.get_int stats Soteria.Symex.StatKeys.unexplored_branches
  in
  if unexplored > 0 then
    if Option.is_some (Config.get ()).branch_fuel then
      Fmt.kstr Soteria.Terminal.Warn.warn
        "Note that %a were left unexplored due to branch fuel. Errors may have \
         been missed."
        pp_branches unexplored
    else Fmt.kstr execution_err "Missed %a" pp_branches unexplored
  else if List.exists Compo_res.is_missing outcomes then
    execution_err "Miss encountered in WPST";

  if not (List.exists Compo_res.is_error outcomes) then
    let pcs = List.map snd branches in
    Ok (pcs, nbranches)
  else
    (* join th errors by [error type * calltrace], and find all matching PCs *)
    let errors =
      List.filter_map
        (function
          | Compo_res.Error ((e, ct), _), pc -> Some (e, ct, pc) | _ -> None)
        branches
    in
    let errors =
      List.map (fun (e, ct, _) -> (e, ct)) errors
      |> List.sort_uniq compare
      |> List.map (fun (e, ct) ->
          let pcs =
            List.filter_map
              (fun (e', ct', pc) ->
                if e = e' && ct = ct' then Some pc else None)
              errors
          in
          (e, ct, pcs))
    in
    Error (errors, nbranches)

let print_outcomes_summary outcomes =
  let open Fmt in
  let pp_outcome ft (name, res) = Fmt.pf ft "• %s: %a" name Outcome.pp res in
  pr "%a:@\n%a@\n" (pp_style `Bold) "Summary"
    (list ~sep:(any "@\n") pp_outcome)
    outcomes

let exec (crate, entry_points) =
  let outcomes = exec_crate crate entry_points in
  if (Config.get ()).print_summary then print_outcomes_summary outcomes;
  Outcome.merge_list outcomes
