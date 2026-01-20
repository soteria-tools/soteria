type log_kind = Stderr | Html

let pp_log_kind fmt = function
  | Stderr -> Format.fprintf fmt "stderr"
  | Html -> Format.fprintf fmt "html"

type t = { level : Level.t option; kind : log_kind; always_log_smt : bool }

let default = { level = Some Warn; kind = Stderr; always_log_smt = false }
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Logs" ~default ()

let check_set_and_lock config =
  match config with
  | Ok config -> set_and_lock config
  | Error msg ->
      Fmt.epr "Invalid CLI arguments: %s" msg;
      exit Cmdliner.Cmd.Exit.cli_error

let logs_enabled () =
  let conf = get () in
  Option.is_some conf.level

let should_log level =
  match get () with
  | { always_log_smt = true; _ } when Level.(level = Smt) -> true
  | { level = None; _ } -> false
  | { level = Some l; _ } -> Level.(level >= l)

let channel =
  let oc_ref = ref None in
  fun () ->
    match !oc_ref with
    | Some oc -> oc
    | None -> (
        match (get ()).kind with
        | Stderr -> Out_channel.stderr
        | Html ->
            let oc = open_out "soteria_logs.html" in
            at_exit (fun () -> close_out oc);
            oc_ref := Some oc;
            oc)

let console_trace : t =
  { level = Some Trace; kind = Stderr; always_log_smt = false }

let html_trace : t = { level = Some Trace; kind = Html; always_log_smt = false }

type interject = (unit -> unit) -> unit

let cur_interject : interject ref = ref (fun f -> f ())

let with_interject : interject:interject -> (unit -> 'a) -> 'a =
 fun ~interject f ->
  let old = !cur_interject in
  cur_interject := interject;
  Fun.protect ~finally:(fun () -> cur_interject := old) f
