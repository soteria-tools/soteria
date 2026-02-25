module Call_trace = Soteria.Terminal.Call_trace
module SState = State (* Clashes with Cerb_frontend.State *)
open Cerb_frontend
open Syntaxes.FunctionWrap
open Soteria.Logs.Printers
open Soteria.Terminal.Warn
module Wpst_interp = Interp.Make (SState)

exception Tool_error

let tool_error msg =
  Fmt.epr "%a" pp_fatal msg;
  raise Tool_error

let with_tool_errors_caught () f =
  try f () with Tool_error -> Error.Exit_code.Tool_error

let default_wpst_fuel =
  Soteria.Symex.Fuel_gauge.{ steps = Finite 150; branching = Finite 4 }

let as_nonempty_list functions_to_analyse =
  match functions_to_analyse with [] -> None | _ -> Some functions_to_analyse

let impl_name =
  match Sys.getenv_opt "IMPL_NAME" with
  | Some impl -> impl
  | None -> "gcc_4.9.0_x86_64-apple-darwin10.8.0"

let set_cerb_conf () =
  let open Cerb_global in
  let lexicon =
    {
      with_c23 = not (Config.current ()).no_c23;
      with_gnu = true;
      without_cerb = false;
    }
  in
  set_cerb_conf ~lexicon ~backend_name:"soteria-c" ~exec:false Random
    ~concurrency:false Basic ~defacto:false ~permissive:true ~agnostic:false
    ~ignore_bitfields:true

let io : Cerb_backend.Pipeline.io_helpers =
  let open Cerb_backend.Pipeline in
  let return = Exception.except_return in
  let pass_message =
    let ref = ref 0 in
    fun str ->
      Cerb_debug.print_success (Printf.sprintf "%i. %s" !ref str);
      incr ref;
      return ()
  in
  let set_progress _ = return () in
  let run_pp opts doc =
    run_pp opts doc;
    return ()
  in
  let print_endline str =
    L.debug (fun m -> m "%s" str);
    return ()
  in
  let print_debug n mk_str =
    (* Cerb_debug.print_debug n [] mk_str; *)
    if n == 0 then L.debug (fun m -> m "%s" (mk_str ()));
    return ()
  in
  let warn ?always:_ mk_str =
    L.warn (fun m -> m "%s" (mk_str ()));
    return ()
  in
  { pass_message; set_progress; run_pp; print_endline; print_debug; warn }

module Frontend = struct
  let lift_parsing_result res =
    match res with
    | Exception.Result (_, (_, ast)) -> Ok ast
    | Exception (loc, err) ->
        let msg =
          Printf.sprintf "Failed to parse AIL: %s"
            (Pp_errors.to_string (loc, err))
        in
        Error (`ParsingError msg, Call_trace.singleton ~loc ())

  let use_cerb_libc_if_asked () =
    if (Config.current ()).use_cerb_headers then
      let root_includes = Cerb_runtime.in_runtime "libc/include" in
      let posix = Filename.concat root_includes "posix" in
      "-I" ^ root_includes ^ " -I" ^ posix ^ " -nostdinc"
    else ""

  let init () =
    let result =
      let open Cerb_backend.Pipeline in
      let include_soteria_c_h =
        let filename =
          Filename.concat (Config.current ()).auto_include_path "soteria-c.h"
        in
        if Sys.file_exists filename then "-include " ^ filename ^ " "
        else (
          warn (filename ^ " not found");
          "")
      in
      let ( let* ) = Exception.except_bind in
      let conf cpp_cmd =
        {
          debug_level = 0;
          pprints = [];
          astprints = [];
          ppflags = [];
          ppouts = [];
          typecheck_core = false;
          rewrite_core = false;
          sequentialise_core = false;
          cpp_cmd;
          cpp_stderr = false;
          cpp_save = None;
        }
      in
      set_cerb_conf ();
      Ocaml_implementation.(set HafniumImpl.impl);
      let* stdlib = load_core_stdlib () in
      let* impl = load_core_impl stdlib impl_name in
      Exception.Result
        (fun ~cpp_cmd filename ->
          let cpp_cmd =
            cpp_cmd
            ^ " -E -x c -CC "
            ^ include_soteria_c_h
            ^ use_cerb_libc_if_asked ()
          in
          c_frontend (conf cpp_cmd, io) (stdlib, impl) ~filename)
    in
    let () = Cerb_colour.do_colour := false in
    match result with
    | Exception.Result f -> f
    | Exception.Exception err ->
        Fmt.failwith "Failed to initialize frontend: %s"
          (Pp_errors.to_string err)

  let frontend = lazy (init ())

  let frontend ?cwd ~cpp_cmd filename =
    L.debug (fun m -> m "Parsing %s" filename);
    try
      let cerb_res =
        match cwd with
        | None -> (Lazy.force frontend) ~cpp_cmd filename
        | Some dir ->
            Sys.with_working_dir dir @@ fun () ->
            (Lazy.force frontend) ~cpp_cmd filename
      in
      lift_parsing_result cerb_res
    with Sys_error err ->
      let pos = Cerb_position.(set_source (filename, 0) dummy) in
      let loc = Cerb_location.point pos in

      Error (`ParsingError err, Call_trace.singleton ~loc ())

  let simple_frontend ~includes filename =
    let cmd = "cc" :: List.map (fun s -> "-I" ^ s) includes in
    let cpp_cmd = String.concat " " cmd in
    L.trace (fun m -> m "Cpp_cmd: %s" cpp_cmd);
    frontend ~cpp_cmd filename
end

let parse_ail_raw_default ~includes file =
  Frontend.simple_frontend ~includes file

let parse_and_link_ail ~includes files =
  let open Syntaxes.Result in
  let parse_and_signal file =
    let res = parse_ail_raw_default ~includes file in
    Soteria.Terminal.Progress_bar.signal_progress 1;
    res
  in
  let@ () =
    Soteria.Terminal.Progress_bar.run ~color:`Yellow ~msg:"Parsing files"
      ~total:(List.length files) ()
  in
  match files with
  | [] -> Error (`ParsingError "No files to parse?", Call_trace.empty)
  | files ->
      let* parsed =
        if (Config.current ()).no_ignore_parse_failures then
          Monad.ResultM.all parse_and_signal files
        else
          let parsed =
            List.filter_map
              (fun file ->
                match parse_and_signal file with
                | Ok ast -> Some ast
                | Error (msg, _loc) ->
                    let msg =
                      match msg with
                      | `ParsingError s -> s
                      | _ -> "Unknown error"
                    in
                    Fmt.kstr warn
                      "Ignoring file that did not parse correctly: %s@\n%s" file
                      msg;
                    None)
              files
          in
          Ok parsed
      in
      Ail_linking.link parsed

let parse_compilation_item (item : Compilation_database.cmd) =
  Frontend.frontend ~cwd:item.directory
    ~cpp_cmd:(String.concat " " item.command)
    item.file

let is_main (def : Cabs.function_definition) =
  let decl = match def with FunDef (_, _, _, decl, _) -> decl in
  match decl with
  | Cabs.Declarator (_, DDecl_identifier (_, Identifier (_, name))) ->
      String.equal name "main"
  | _ -> false

let pp_err_and_call_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" Error.pp err
    (Call_trace.pp Fmt_ail.pp_loc)
    call_trace

let resolve_function (linked : Ail_tys.linked_program) entry_point =
  Ail_helpers.find_fun_name ~prog:linked entry_point
  |> Result.of_opt
       ~err:
         ( `ParsingError (Fmt.str "Entry point \"%s\" not found" entry_point),
           Call_trace.empty )

let with_function_context prog f =
  let open Effect.Deep in
  let fctx = Fun_ctx.of_linked_program prog in
  Layout.Tag_defs.run_with_prog prog.sigma @@ fun () ->
  Ail_helpers.run_with_prog prog @@ fun () ->
  try f () with effect Interp.Get_fun_ctx, k -> continue k fctx

let exec_function ~includes ~fuel file_names function_name =
  let open Syntaxes.Result in
  let result =
    let* linked = parse_and_link_ail ~includes file_names in
    if (Config.current ()).parse_only then Ok []
    else
      let* entry_point = resolve_function linked function_name in
      let open Wpst_interp.InterpM in
      let symex =
        let open StateM.Syntax in
        let@@ () = StateM.Result.run_with_state ~state:SState.empty in
        let** () = Wpst_interp.init_prog_state linked in
        let* state = StateM.get_state () in
        L.debug (fun m ->
            m "@[<2>Initial state:@ %a@]" (Fmt.Dump.option SState.pp) state);
        Wpst_interp.exec_fun entry_point ~args:[]
      in
      let@ () = with_function_context linked in
      Ok (Csymex.Result.run_needs_stats ~mode:OX ~fuel symex)
  in
  match result with
  | Ok v -> v
  | Error e ->
      let e = (e, SState.empty) in
      [ (Soteria.Symex.Compo_res.Error (Soteria.Symex.Or_gave_up.E e), []) ]

let temp_file = lazy (Filename.temp_file "soteria_c" ".c")

let generate_errors content =
  let (lazy file_name) = temp_file in
  let () =
    let oc = open_out file_name in
    output_string oc content;
    close_out oc
  in
  match parse_and_link_ail ~includes:[] [ file_name ] with
  | Error e -> [ e ]
  | Ok prog ->
      let@ () = with_function_context prog in
      Abductor.generate_all_summaries ~functions_to_analyse:None prog
      |> List.concat_map (fun (fid, summaries) ->
          let@ () =
            L.with_section
              ("Analysing summaries for function" ^ Symbol.show_symbol fid)
          in
          List.concat_map (Summary.manifest_bugs ~fid) summaries)
      |> List.sort_uniq Stdlib.compare

(** {2 Entry points} *)

(* Helper for all main entry points *)
let initialise ?soteria_config mode config f =
  Option.iter Soteria.Config.set_and_lock soteria_config;
  let@ () = Config.with_config ~config ~mode in
  Soteria.Stats.As_ctx.with_stats_dumped () f

let print_states result =
  let pp_state ft state =
    (Fmt.Dump.list SState.pp_serialized) ft (SState.serialize state)
  in
  Fmt.pr "@[<v 2>Symex terminated with the following outcomes:@ %a@]@\n@?"
    Fmt.Dump.(
      list @@ fun ft (r, _) ->
      (Soteria.Symex.Compo_res.pp
         ~ok:(pair Aggregate_val.pp (Fmt.Dump.option pp_state))
         ~err:
           (Soteria.Symex.Or_gave_up.pp
              (pair pp_err_and_call_trace (option SState.pp)))
         ~miss:Fmt.Dump.(list SState.pp_serialized))
        ft r)
    result

(* Entry point function *)
let exec_and_print soteria_config config fuel includes file_names entry_point :
    Error.Exit_code.t =
  (* The following line is not set as an initialiser so that it is executed
     before initialising z3 *)
  let fuel = Soteria.Symex.Fuel_gauge.Cli.validate_or_exit fuel in
  let@ () = initialise ~soteria_config Whole_program config in
  let result = exec_function ~includes ~fuel file_names entry_point in
  if (Config.current ()).parse_only then Error.Exit_code.Success
  else (
    if (Config.current ()).print_states then print_states result;
    let errors_to_signal =
      List.filter_map
        (function
          | ( Soteria.Symex.Compo_res.Error
                (Soteria.Symex.Or_gave_up.Gave_up _ as err),
              _ ) ->
              Some err
          | Error (E (((e, _) as err_with_trace), _)), _
          (* Test-Comp requires filtering out UBs... *)
            when not ((Config.current ()).ignore_ub && Error.is_ub e) ->
              Some (E err_with_trace)
          | _ -> None)
        result
    in

    let has_found_bugs = ref false in
    ListLabels.iter errors_to_signal
      ~f:
        (let open Error.Diagnostic in
         function
         | Soteria.Symex.Or_gave_up.E (err, trace) ->
             has_found_bugs := true;
             print_diagnostic ~fid:entry_point ~call_trace:trace ~error:err
         | Gave_up msg ->
             print_diagnostic ~fid:entry_point ~call_trace:Call_trace.empty
               ~error:(`Gave_up msg));
    let success = List.is_empty errors_to_signal in
    let steps_number =
      let stats = Soteria.Stats.As_ctx.get_copy () in
      Soteria.Stats.get_int stats Soteria.Symex.StatKeys.steps
    in
    Fmt.pr "@.Executed %d statements" steps_number;
    if success then (
      Fmt.pr "@.%a@.@?" pp_ok "Verification Success!";
      Error.Exit_code.Success)
    else if !has_found_bugs then (
      Fmt.pr "@.%a@.@?" pp_err "Verification Failure!";
      Error.Exit_code.Found_bug)
    else (
      Fmt.pr "@.%a@.@?" pp_err "Verification Failure! (Unsupported features)";
      Error.Exit_code.Tool_error))

let dump_report diagnostics =
  match (Config.current ()).dump_report with
  | None -> ()
  | Some file ->
      let json = `List diagnostics in
      Yojson.Safe.to_file file json

let dump_summaries results =
  match (Config.current ()).dump_summaries_file with
  | None -> ()
  | Some file ->
      let pp_summary ~fid ft summary =
        Fmt.pf ft "@[<v 2>%a@]" Summary.pp (Summary.analyse ~fid summary)
      in
      let@ oc = Out_channel.with_open_text file in
      let ft = Format.formatter_of_out_channel oc in
      List.iter
        (fun (fid, summaries) ->
          Fmt.pf ft "@[<v 2>Summaries for %a:@ %a@]@ @." Fmt_ail.pp_sym fid
            (Fmt.list ~sep:Fmt.sp (pp_summary ~fid))
            summaries)
        results

let analyse_summaries results =
  let total =
    Iter.of_list results
    |> Iter.map (fun (_, l) -> List.length l)
    |> Iter.fold ( + ) 0
  in
  Soteria.Stats.As_ctx.add_int "soteria-c.num_summaries_generated" total;
  let open Syntaxes.List in
  let@ () =
    Soteria.Terminal.Progress_bar.run ~color:`Magenta
      ~msg:"Analysing summaries " ~total ()
  in
  let+ fid, summaries = results in
  let results =
    let+ summary = summaries in
    let res = Summary.analyse ~fid summary in
    Soteria.Terminal.Progress_bar.signal_progress 1;
    res
  in
  (fid, results)

let generate_summaries ~functions_to_analyse prog =
  let@ () = with_function_context prog in
  let res =
    let@ () =
      Soteria.Stats.As_ctx.add_time_of_to "soteria-c.time_summary_generation"
    in
    Abductor.generate_all_summaries ~functions_to_analyse prog
  in

  let results =
    let@ () =
      Soteria.Stats.As_ctx.add_time_of_to "soteria-c.time_summary_analysis"
    in
    analyse_summaries res
  in
  dump_summaries results;
  Fmt.pr "@\n@?";
  let found_bugs = ref false in
  let diagnostics = ref [] in
  let list_iter r f = List.iter f r in
  let () =
    let@ fid, summaries = list_iter results in
    let already_signaled = ref [] in
    let@ (Summary.Analysed { raw; manifest_bugs }) = list_iter summaries in
    let remaining_to_signal =
      List.filter (fun b -> not (List.mem b !already_signaled)) manifest_bugs
    in
    already_signaled := remaining_to_signal @ !already_signaled;
    let@ error, call_trace = list_iter remaining_to_signal in
    found_bugs := true;
    let fid_str = Fmt.to_to_string Ail_helpers.pp_sym_hum fid in
    Error.Diagnostic.print_diagnostic ~fid:fid_str ~call_trace ~error;
    (* Collect diagnostic for JSON report *)
    let diag_json = Error.Diagnostic.to_json ~fid:fid_str ~call_trace ~error in
    diagnostics := diag_json :: !diagnostics;
    if (Config.current ()).show_manifest_summaries then
      Fmt.pr "@\n@[Corresponding summary:@ %a@]" Summary.pp_raw raw;
    Fmt.pr "@\n@?"
  in
  dump_report !diagnostics;
  if !found_bugs then Error.Exit_code.Found_bug
  else (
    Fmt.pr "%a@.@?" pp_ok "No bugs found";
    Error.Exit_code.Success)

(* Entry point function *)
let lsp config () =
  Config.with_config ~config ~mode:Compositional
  @@ Soteria_c_lsp.run ~generate_errors

(* Entry point function *)
let show_ail soteria_config config (includes : string list)
    (files : string list) =
  let@ () = initialise ~soteria_config Compositional config in
  match parse_and_link_ail ~includes files with
  | Ok { symmap = _; sigma; entry_point } ->
      Fmt.pr "%a@." Fmt_ail.pp_program_ast (entry_point, sigma);
      Error.Exit_code.Success
  | Error err ->
      Fmt.pr "%a@." pp_err_and_call_trace err;
      Error.Exit_code.Tool_error

(* Entry point function *)
let generate_all_summaries soteria_config config includes functions_to_analyse
    file_names =
  (* TODO: generate a compilation database directly, to simplify the interface
     in this file. *)
  let@ () = with_tool_errors_caught () in
  let functions_to_analyse = as_nonempty_list functions_to_analyse in
  let@ () = initialise ~soteria_config Compositional config in
  let prog =
    let@ () = L.with_section "Parsing and Linking" in
    parse_and_link_ail ~includes file_names
    |> Result.get_or ~err:(fun e ->
        Fmt.epr "%a@\n@?" pp_err_and_call_trace e;
        tool_error "Failed to parse AIL")
  in
  if (Config.current ()).parse_only then Error.Exit_code.Success
  else generate_summaries ~functions_to_analyse prog

let linked_prog_of_db json_file =
  let open Syntaxes.Result in
  let linked_prog =
    let@ () = L.with_section "Parsing and Linking from database" in
    let db = Compilation_database.from_file json_file in
    let parse_and_signal item =
      let res = parse_compilation_item item in
      Soteria.Terminal.Progress_bar.signal_progress 1;
      res
    in
    let db_size = List.length db in
    let@ () =
      Soteria.Terminal.Progress_bar.run ~color:`Yellow
        ~msg:"Parsing files       " ~total:db_size ()
    in
    let* ails =
      let ails_and_items =
        List.filter_map
          (fun item ->
            match parse_and_signal item with
            | Ok ail -> Some (ail, item)
            | Error (`ParsingError msg, _loc) ->
                L.debug (fun m ->
                    m "Ignoring file that did not parse correctly: %s@\n%s"
                      item.file msg);
                None)
          db
      in
      let () =
        let parsed = List.length ails_and_items in
        if parsed < db_size then
          Fmt.kstr warn
            "Some files failed to parse, successfully parsed %d out of %d files"
            parsed db_size
      in
      (* Write parsed compilation database if requested *)
      let () =
        match (Config.current ()).write_parsed_db with
        | None -> ()
        | Some output_file ->
            let parsed_items = List.map snd ails_and_items in
            Compilation_database.dump_originals output_file parsed_items;
            Fmt.pr "Wrote parsed compilation database to %s@\n@?" output_file
      in
      Ok (List.map fst ails_and_items)
    in
    Ail_linking.link ails
  in
  Result.get_or
    ~err:(function
      | `ParsingError msg, _ ->
          Fmt.epr "%s@\n@?" msg;
          tool_error "Failed to parse AIL"
      | `LinkError msg, _ ->
          let msg =
            if (Config.current ()).no_ignore_parse_failures then msg
            else "All files failed to parse, no analysis will be performed"
          in
          Fmt.pr "Error: %a@\n@?" pp_err msg;
          if (Config.current ()).no_ignore_parse_failures then
            tool_error "Failed to link AIL"
          else Ail_tys.empty_linked_program)
    linked_prog

(* Entry point function *)
let capture_db soteria_config config json_file functions_to_analyse =
  let@ () = with_tool_errors_caught () in
  let functions_to_analyse = as_nonempty_list functions_to_analyse in
  let@ () = initialise ~soteria_config Compositional config in
  let linked_prog =
    let@ () = Soteria.Stats.As_ctx.add_time_of_to "soteria-c.time_parsing" in
    linked_prog_of_db json_file
  in
  if (Config.current ()).parse_only then Error.Exit_code.Success
  else generate_summaries ~functions_to_analyse linked_prog
