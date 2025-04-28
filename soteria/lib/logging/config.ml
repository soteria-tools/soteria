type log_kind = Stderr | Html

let pp_log_kind fmt = function
  | Stderr -> Format.fprintf fmt "stderr"
  | Html -> Format.fprintf fmt "html"

type t = { level : Level.t option; kind : log_kind }

let set, get, lock =
  let current_config = ref { level = Some Warn; kind = Stderr } in
  let locked = ref false in
  let lock () = locked := true in
  let set_config config =
    if !locked then failwith "Logging configuration cannot be changed anymore";
    current_config := config
  in
  let current_config () = !current_config in
  (set_config, current_config, lock)

let check_set_and_lock config =
  match config with
  | Ok config ->
      set config;
      lock ()
  | Error msg -> Format.kasprintf failwith "Invalid CLI arguments: %s" msg

let logs_enabled () =
  let conf = get () in
  Option.is_some conf.level

let should_log level =
  match (get ()).level with None -> false | Some l -> Level.(level >= l)

let channel =
  let oc_ref = ref None in
  fun () ->
    match !oc_ref with
    | Some oc -> oc
    | None -> (
        match (get ()).kind with
        | Stderr -> Out_channel.stderr
        | Html ->
            let oc = open_out "soterial_logs.html" in
            at_exit (fun () -> close_out oc);
            oc_ref := Some oc;
            oc)

let console_trace : t = { level = Some Trace; kind = Stderr }
let html_trace : t = { level = Some Trace; kind = Html }
