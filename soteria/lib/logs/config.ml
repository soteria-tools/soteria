open Soteria_std
open Level

type log_kind = Stderr | Html
[@@deriving subliner_enum, show { with_path = false }]

type t = {
  level : Level.t option; [@default Some Warn]
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
  v_list : bool list; [@term v_list]  (** Verbosity level, clashes with -q *)
  silent : bool; [@names [ "q"; "silent"; "quiet" ]]
      (** Silent mode, clashes with -v *)
  log_kind : (log_kind[@conv log_kind_cmdliner_conv ()]) option;
      [@names [ "l"; "log_kind" ]]
      (** Log kind, clashes with --html *)
  html : bool; [@name "html"]  (** HTML logging, clashes with --log-kind *)
  always_log_smt : bool; [@name "log-smt"]
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

include struct
  open Soteria_std.Effectful.Read_only_state (struct
    type nonrec t = t
  end)

  let get = get
  let with_config = run
end

type _ Effect.t += Interject_logs : (unit -> unit) -> unit Effect.t

let interject f = Effect.perform (Interject_logs f)

let with_interject : interject:((unit -> unit) -> unit) -> (unit -> 'a) -> 'a =
 fun ~interject f ->
  try f ()
  with effect Interject_logs interj, k ->
    interject interj;
    Effect.Deep.continue k ()

let with_config_raw ?(config = default) (f : unit -> 'a) : 'a =
  (* The following line still prevents one from running this function several times... *)
  Profile.check_set_and_lock ~no_color:config.no_color ();
  with_interject ~interject:(fun interj -> interj ()) @@ fun () ->
  with_config config f

let check (args : cli) : t =
  let level =
    match (List.length args.v_list, args.silent) with
    | 0, true -> None
    | _, true -> Exn.config_error "Cannot use -v and --silent at the same time"
    | 0, false -> Some Warn
    | 1, false -> Some Info
    | 2, false -> Some Debug
    | 3, false -> Some Trace
    | _, false -> Some Smt
  in
  let kind =
    match (args.log_kind, args.html) with
    | Some _, true ->
        Exn.config_error "Cannot use --html and --log-kind at the same time"
    | Some k, false -> k
    | None, true -> Html
    | None, false -> Stderr
  in
  {
    level;
    kind;
    always_log_smt = args.always_log_smt;
    no_color = args.no_color;
    hide_unstable = args.hide_unstable;
  }

let with_config ?config (f : unit -> 'a) : 'a =
  let config = Option.map check config in
  with_config_raw ?config f

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
