open Charon
open Syntaxes.FunctionWrap
open Error.Diagnostic
open Soteria.Logs.Printers

let analyse_summaries results =
  let total =
    Iter.of_list results
    |> Iter.map (fun (_, l) -> List.length l)
    |> Iter.fold ( + ) 0
  in
  let open Syntaxes.List in
  let@ () =
    Soteria.Terminal.Progress_bar.run ~color:`Magenta
      ~msg:"Analysing summaries " ~total ()
  in
  let+ fn, summaries = results in
  let results =
    let+ summary = summaries in
    let res = Summary.analyse ~fn summary in
    Soteria.Terminal.Progress_bar.signal_progress 1;
    res
  in
  (fn, results)

let get_entry_points (crate : UllbcAst.crate) =
  Types.FunDeclId.Map.values crate.fun_decls
  |> List.filter (fun (fn : UllbcAst.fun_decl) ->
      Option.is_none fn.is_global_initializer && fn.item_meta.is_local)

let generate_summaries (crate : UllbcAst.crate) =
  let entry_points = get_entry_points crate in
  if List.is_empty entry_points then
    Common.fatal "no functions to analyse found";
  let@ () = Crate.with_crate crate in
  let@ () = Layout.Session.with_layout_cache in
  let start = Unix.gettimeofday () in
  let Soteria.Stats.{ res; stats } =
    Abductor.generate_all_summaries entry_points
  in
  let results = analyse_summaries res in
  Fmt.pr "@\n@?";
  Soteria.Stats.output stats;
  let time = Unix.gettimeofday () -. start in
  let bugs_iter =
    results
    |> Iter.of_list
    |> Iter.flat_map (fun (fn, summaries) ->
        summaries
        |> Iter.of_list
        |> Iter.flat_map_l (fun s ->
            let (Summary.Analysed { raw = _; manifest_bugs }) = s in
            manifest_bugs)
        |> Iter.map (fun b -> (fn, b)))
    |> Iter.sort_uniq ~cmp:(fun (_, e1) (_, e2) -> compare e1 e2)
  in
  if Iter.is_empty bugs_iter then (
    print_diagnostic_simple ~severity:Note "No bugs found";
    Outcome.Ok)
  else (
    Fmt.kstr
      (print_diagnostic_simple ~severity:Error)
      "found issues in %a" pp_time time;
    bugs_iter (fun (fn, error) ->
        let fname = Fmt.to_to_string Crate.pp_name fn.item_meta.name in
        print_diagnostic ~fname ~error;
        Fmt.pr "@.");
    Outcome.Error)
