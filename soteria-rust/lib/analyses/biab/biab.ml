open Charon
open Syntaxes.FunctionWrap
open Error.Diagnostic
open Soteria.Logs.Printers
open Util

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

(* TODO: it is common to declare small helper functions that are designed to
   panic (e.g. [std::alloc::handle_alloc_error]); these shouldn't be analysed,
   or at least not have their panics reported as manifest bugs, since they are
   designed to panic.

   We could do a small analysis of the terminators of a function, and if all
   terminators are a panic or are functions that panic, then we mark this
   function as "panic-only" and ignore it / only report errors in it if they are
   not a panic (e.g. if it has UB). *)
let get_entry_points (crate : UllbcAst.crate) =
  Types.FunDeclId.Map.values crate.fun_decls
  |> List.filter (fun (fn : UllbcAst.fun_decl) ->
      Option.is_none fn.is_global_initializer
      && fn.item_meta.is_local
      && fn.src = TopLevelItem
      && Frontend.filter_name crate fn.item_meta.name)

let generate_summaries (crate : UllbcAst.crate) =
  let entry_points = get_entry_points crate in
  if List.is_empty entry_points then fatal "no functions to analyse found";
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
        |> Iter.flat_map (fun s ->
            let (Summary.Analysed { raw; manifest_bugs }) = s in
            Iter.of_list manifest_bugs |> Iter.map (fun b -> (b, (fn, raw)))))
    |> Iter.group_pairs_by
  in
  if Iter.is_empty bugs_iter then (
    print_diagnostic_simple ~severity:Note "No bugs found";
    Outcome.Ok)
  else
    let pp_list pp = Fmt.(list ~sep:comma (box pp)) in
    Fmt.kstr
      (print_diagnostic_simple ~severity:Error)
      "found issues in %a" pp_time time;
    bugs_iter (fun (error, occurences) ->
        let fnames, summaries = List.split occurences in
        let fname =
          Fmt.to_to_string (pp_list Crate.pp_name)
            (List.map (fun (f : UllbcAst.fun_decl) -> f.item_meta.name) fnames)
        in
        print_diagnostic ~fname ~error;
        if (Config.get ()).show_manifest_summaries then
          Fmt.pr "@[Corresponding %a:@ %a@]@\n" pp_summary
            (List.length summaries) (pp_list Summary.pp_raw) summaries;
        Fmt.pr "@.");
    Outcome.Error
