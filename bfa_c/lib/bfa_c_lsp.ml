let cerb_loc_to_range loc =
  let open Lsp.Types in
  let (start_l, start_c), (end_l, end_c) =
    Option.value ~default:((0, 0), (0, 0)) (Cerb_location.to_cartesian loc)
  in
  Range.
    {
      start = { character = start_c; line = start_l };
      end_ = { character = end_c; line = end_l };
    }

(* This gathers all the reasons why the analysis gave up. *)
let get_abort_diagnostics () =
  let open Syntaxes.List in
  let+ msg, loc = Csymex.flush_give_up () in
  let range = cerb_loc_to_range loc in
  Lsp.Types.Diagnostic.create ~message:msg ~severity:Information ~range
    ~source:"bfa" ()

let error_to_diagnostic_opt (err, loc) =
  let open Lsp.Types in
  let open Syntaxes.Option in
  let open DiagnosticSeverity in
  let+ severity, message =
    (* When we hide non-bug things, some patterns will return None *)
    match err with
    | `NullDereference -> Some (Error, "Null Dereference")
    | `OutOfBounds -> Some (Error, "Out of Bounds")
    | `UninitializedMemoryAccess -> Some (Error, "Uninitialized Memory Access")
    | `ParsingError s -> Some (Error, s)
    | `UseAfterFree -> Some (Error, "Use After Free")
    | `DivisionByZero -> Some (Error, "Division By Zero")
    | `UBPointerComparison ->
        Some (Error, "Undefined Behavior for Pointer Comparison")
    | `UBPointerArithmetic ->
        Some (Error, "Undefined Behavior for Pointer Arithmetic")
    | `DoubleFree -> Some (Error, "Double Free")
    | `InvalidFree -> Some (Error, "Invalid Pointer passed to free")
    | `MissingKey -> Some (Information, "Missing Key")
    | `MissingResource -> Some (Information, "Missing Resource")
    | `MissingOwnership -> Some (Information, "MissingOwnership")
  in
  Lsp.Types.Diagnostic.create ~message ~severity ~range:(cerb_loc_to_range loc)
    ~source:"bfa" ()

class bfa_lsp_server run_to_errors =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super
    val mutable debug_mode = false

    (* one env per document *)

    method spawn_query_handler f = Linol_eio.spawn f

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (_uri : Lsp.Types.DocumentUri.t) (contents : string) =
      let errors = run_to_errors contents in
      let diags = List.filter_map error_to_diagnostic_opt errors in
      let diags =
        if debug_mode then get_abort_diagnostics () @ diags else diags
      in
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ _d : unit Linol_eio.t = ()

    method! on_unknown_notification ~notify_back notif =
      match notif.method_ with
      | "bfa/toggleDebugMode" ->
          L.debug (fun m -> m "Toggling debug mode");
          debug_mode <- not debug_mode
      | _ -> super#on_unknown_notification ~notify_back notif
  end

let run ~run_to_errors () =
  Eio_main.run @@ fun env ->
  let s = new bfa_lsp_server run_to_errors in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  let task () =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_eio.Jsonrpc2.run ~shutdown server
  in
  match task () with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1
