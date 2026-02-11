open Soteria_std
open Level

type log_kind = Stderr | Html
[@@deriving subliner_enum, show { with_path = false }]

type t = {
  level : Level.t option; [@default None]
  kind : log_kind; [@default Stderr]
  always_log_smt : bool; [@default false]
  no_color : bool; [@default false]
  hide_unstable : bool; [@default false]
}
[@@deriving make]

let v_list =
  Cmdliner.Arg.(
    value
    & flag_all
    & info [ "v"; "verbose" ] ~docv:"v" ~doc:"Verbosity level, clashes with -q")

type cli = {
  v_list : bool list; [@term v_list]
      (** Verbosity level. One -v includes ERROR, WARN, INFO, and every
          subsequent -v then includes DEBUG, TRACE and SMT respectively *)
  log_kind : (log_kind[@conv log_kind_cmdliner_conv ()]) option;
      [@names [ "l"; "log_kind" ]]
      (** Log kind, clashes with --html *)
  html : bool; [@names [ "html" ]]  (** HTML logging, clashes with --log-kind *)
  always_log_smt : bool; [@names [ "log-smt" ]]
      (** Always log SMT queries, even in silent mode *)
  no_color : bool; [@names [ "no-color"; "no-colour" ]] [@env "NO_COLOR"]
      (** Disables coloured output. *)
  hide_unstable : bool;
      [@names [ "hide-unstable"; "diffable" ]] [@env "HIDE_UNSTABLE"]
      (** Do not display unstable values like durations (e.g. for diffing
          purposes). *)
}
[@@deriving subliner]

let cmdliner_term = cli_cmdliner_term
let default = make ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Logs" ~default ()

let check_set_and_lock args =
  let level =
    match List.length args.v_list with
    | 0 -> None
    | 1 -> Some Info
    | 2 -> Some Debug
    | 3 -> Some Trace
    | _ -> Some Smt
  in
  let kind =
    match (args.log_kind, args.html) with
    | Some _, true ->
        Exn.config_error "Cannot use --html and --log-kind at the same time"
    | Some k, false -> k
    | None, true -> Html
    | None, false -> Stderr
  in
  let config =
    {
      level;
      kind;
      always_log_smt = args.always_log_smt;
      no_color = args.no_color;
      hide_unstable = args.hide_unstable;
    }
  in
  set_and_lock config;
  Profile.check_set_and_lock ~no_color:config.no_color ()

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

type interject = (unit -> unit) -> unit

let cur_interject : interject ref = ref (fun f -> f ())

let with_interject : interject:interject -> (unit -> 'a) -> 'a =
 fun ~interject f ->
  let old = !cur_interject in
  cur_interject := interject;
  Fun.protect ~finally:(fun () -> cur_interject := old) f
