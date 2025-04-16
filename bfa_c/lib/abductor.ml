module Bi_interp = Interp.Make (Bi_state)
open Ail_tys

module Summaries = struct
  module H = Hashtbl.Make (Ail_helpers.Symbol_std)
end

let generate_summaries_for ~prog (fundef : fundef) =
  let open Syntaxes.List in
  let fid, (floc, _, _, _, _) = fundef in
  let* arg_tys =
    match Ail_helpers.get_param_tys ~prog fid with
    | None ->
        L.info (fun m ->
            m "No argument types found for %a at loc %a" Fmt_ail.pp_sym fid
              Fmt_ail.pp_loc floc);
        []
    | Some arg_tys -> [ arg_tys ]
  in
  let process =
    let open Csymex.Syntax in
    let* args = Csymex.all Layout.nondet_c_ty arg_tys in
    let* result = Bi_interp.exec_fun ~prog ~args ~state:Bi_state.empty fundef in
    match result with
    | Ok (ret, bi_state) -> Csymex.return (args, Ok ret, bi_state)
    | Error (err, bi_state) -> Csymex.return (args, Error err, bi_state)
    | Missing _ -> Csymex.vanish ()
  in
  let+ (args, ret, bi_state), pc = Csymex.run process in
  let pre, post = Bi_state.to_spec bi_state in
  Summary.make ~args ~ret ~pre ~post ~pc ()

let generate_all_summaries prog =
  Initialize_analysis.reinit prog.sigma;
  let order = Call_graph.weak_topological_order (Call_graph.of_prog prog) in
  ListLabels.filter_map order ~f:(fun fid ->
      let open Syntaxes.Option in
      let+ fundef = Ail_helpers.find_fun_def ~prog fid in
      let summaries = generate_summaries_for ~prog fundef in
      (fid, summaries))
