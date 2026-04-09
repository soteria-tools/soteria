module Lsp = Linol.Lsp

let get_abort_diagnostics (stats : Soteria.Stats.t) =
  let open Syntaxes.List in
  let list_reasons =
    Soteria.Stats.get_map stats Csymex.StatKeys.give_up_reasons
    |> Hashtbl.Hstring.to_seq
    |> Seq.concat_map (fun (reason, locs) ->
        Soteria.Stats.as_yojson locs
        |> Yojson.Safe.Util.to_list
        |> List.to_seq
        |> Seq.map (fun loc -> (reason, loc)))
    |> List.of_seq
  in
  let+ msg, loc = list_reasons in
  let range = Ail_helpers.yojson_loc_to_range loc in
  Lsp.Types.Diagnostic.create ~message:(`String msg) ~severity:Information
    ~range ~source:"soteria" ()

let lift_severity :
    Soteria.Terminal.Diagnostic.severity -> Lsp.Types.DiagnosticSeverity.t =
  function
  | Error -> Error
  | Bug -> Error
  | Warning -> Warning
  | Note -> Information
  | Help -> Hint

let error_to_diagnostic_opt ~uri (err, call_trace) =
  let open Lsp.Types in
  let severity = lift_severity (Error.severity err) in
  let message = (Fmt.to_to_string Error.pp) err in
  let parens_if_non_empty = function "" -> "" | s -> " (" ^ s ^ ")" in
  let range, relatedInformation, msg_addendum =
    match (call_trace : Cerb_location.t Soteria.Terminal.Call_trace.t) with
    | [] -> (Ail_helpers.cerb_loc_to_range Cerb_location.unknown, None, "")
    | [ { loc; msg } ] ->
        (Ail_helpers.cerb_loc_to_range loc, None, parens_if_non_empty msg)
    | { loc; msg } :: locs ->
        let related_info =
          List.map
            (fun Soteria.Terminal.Call_trace.{ loc; msg } ->
              let location =
                Location.create ~range:(Ail_helpers.cerb_loc_to_range loc) ~uri
              in
              DiagnosticRelatedInformation.create ~location ~message:msg)
            locs
        in
        ( Ail_helpers.cerb_loc_to_range loc,
          Some related_info,
          parens_if_non_empty msg )
  in
  Lsp.Types.Diagnostic.create
    ~message:(`String (message ^ msg_addendum))
    ~severity ~range ?relatedInformation ~source:"soteria" ()

class soteria_lsp_server generate_errors =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super
    val mutable debug_mode = false

    (* one env per document *)

    method spawn_query_handler f = Linol_eio.spawn f

    (* We define here a helper method that will: - process a document - store
       the state resulting from the processing - return the diagnostics from the
       new state *)
    method private _on_doc ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      let errors = generate_errors contents in
      let diags = List.map (error_to_diagnostic_opt ~uri) errors in
      (* FIXME: re-establish those when we want that. let diags = if debug_mode
         then get_abort_diagnostics stats @ diags else diags in *)
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that
       will be called by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the
       global hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ _d : unit Linol_eio.t = ()

    method! on_unknown_notification ~notify_back notif =
      match notif.method_ with
      | "soteria/toggleDebugMode" ->
          L.debug (fun m -> m "Toggling debug mode");
          debug_mode <- not debug_mode
      | _ -> super#on_unknown_notification ~notify_back notif
  end

let run ~generate_errors () =
  Eio_main.run @@ fun env ->
  let s = new soteria_lsp_server generate_errors in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  let task () =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_eio.Jsonrpc2.run ~shutdown server
  in
  match task () with
  | () -> Error.Exit_code.Success
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      Error.Exit_code.Tool_error
