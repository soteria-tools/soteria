open Syntaxes.FunctionWrap
open Soteria.Logging.Logs
open Soteria.Terminal
module Bi_interp = Interp.Make (Bi_state)
open Ail_tys

module Summaries = struct
  module H = Hashtbl.Make (Symbol_std)
end

let default_abductor_fuel =
  Soteria.Symex.Fuel_gauge.{ steps = Finite 1000; branching = Finite 4 }

(** Generates summaries for a function given a function definitions. Has to be
    run within {{!Soteria_c_lib.Csymex.Stats.As_ctx.with_stats}with_stats} *)
let generate_summaries_for (fundef : fundef) =
  let open Syntaxes.List in
  let fid, (floc, _, _, _, _) = fundef in
  let section_name =
    "Generate summaries for " ^ Cerb_frontend.Symbol.show_symbol fid
  in
  let@ () = with_section section_name in
  L.info (fun m -> m "%s" section_name);
  let* arg_tys =
    match Ail_helpers.get_param_tys fid with
    | None ->
        L.info (fun m ->
            m "No argument types found for %a at loc %a" Fmt_ail.pp_sym fid
              Fmt_ail.pp_loc floc);
        []
    | Some arg_tys -> [ arg_tys ]
  in
  let process =
    let open Csymex.Syntax in
    let* args = Csymex.all Layout.nondet_c_ty_aggregate arg_tys in
    let* result = Bi_interp.exec_fun fundef ~args Bi_state.empty in
    match result with
    | Ok (ret, bi_state) -> Csymex.return (args, Ok ret, bi_state)
    | Error (err, bi_state) -> Csymex.return (args, Error err, bi_state)
    | Missing _ -> Csymex.vanish ()
  in
  let res =
    let@ () = with_section "Running symbolic execution" in
    Csymex.run_needs_stats ~mode:UX ~fuel:default_abductor_fuel process
  in
  let+ (args, ret, bi_state), pc = res in
  let@ () = with_section "Building summary" in
  L.trace (fun m ->
      m "Building summary for %a using bistate: %a" Fmt_ail.pp_sym fid
        Bi_state.pp bi_state);
  let pre, post = Bi_state.to_spec bi_state in
  let ret = Summary.make ~args ~ret ~pre ~post ~pc () in
  L.trace (fun m -> m "Obtained summary: %a" Summary.pp ret);
  ret

let generate_all_summaries ~functions_to_analyse prog =
  let order = Call_graph.weak_topological_order (Call_graph.of_prog prog) in
  let should_analyse =
    match functions_to_analyse with
    | None -> fun _ -> true
    | Some l -> fun fid -> List.exists (Ail_helpers.sym_is_id fid) l
  in
  let count = ref 0 in
  let to_analyse =
    List.filter
      (fun f ->
        should_analyse f
        &&
        (* Count how many function we should analyse *)
        (incr count;
         true))
      order
  in
  let@ () = Progress_bar.run ~msg:"Generating summaries" ~total:!count () in

  let@ () = Csymex.Stats.As_ctx.with_stats () in
  ListLabels.filter_map to_analyse ~f:(fun fid ->
      let open Syntaxes.Option in
      let res =
        let+ fundef = Ail_helpers.find_fun_def fid in
        let summaries = generate_summaries_for fundef in
        (fid, summaries)
      in
      Progress_bar.signal_progress 1;
      res)
