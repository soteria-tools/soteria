module Level = Level
module Config = Config
module Profile = Profile
module Color = Color
module Printers = Printers

let level_color : Level.t -> Color.t = function
  | Smt -> `Purple
  | Trace -> `Gray
  | Debug -> `DarkBlue
  | Info -> `Teal
  | Warn -> `Orange
  | Error -> `Maroon

let format_level level =
  let level_str = Level.to_string level in
  if (Config.get ()).no_color then Printf.sprintf "[%s]" level_str
  else
    let color = level_color level in
    let buf = Buffer.create 32 in
    let fmt = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer fmt `Ansi_tty;
    Format.fprintf fmt "[%a]" (Printers.pp_clr color) level_str;
    Format.pp_print_flush fmt ();
    Buffer.contents buf

type logger = { oc : Out_channel.t }
type ('a, 'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

(* Serialises all writes to the shared log channel across analysis domains. *)
let io_mutex = Mutex.create ()

(* Domain-local: HTML section nesting is per-execution display state, so
   per-domain counters are the correct semantics and avoid races. *)
let depth_counter : int ref Soteria_std.Dls.t =
  Soteria_std.Dls.make (fun () -> ref 0)

let init () =
  let oc =
    (* This already sets up the filedesc closing *)
    Config.channel ()
  in
  let () =
    match (Config.get ()).kind with
    | Html ->
        Out_channel.output_string oc Html.header;
        Out_channel.output_char oc '\n';
        Out_channel.flush oc
    | _ -> ()
  in
  let logger = { oc } in
  let () =
    match (Config.get ()).kind with
    | Html ->
        at_exit (fun () ->
            (* If program is interrupted and not all sections have been closed,
               close them all! Best-effort using the exiting (main) domain's
               nesting counter. *)
            for _ = 0 to !(Soteria_std.Dls.get depth_counter) - 1 do
              Out_channel.output_string oc Html.section_closing;
              Out_channel.output_char oc '\n'
            done;
            Out_channel.output_string oc Html.footer)
    | Stderr -> ()
  in
  logger

let logger = lazy (init ())
let[@inline] logger () = Lazy.force logger

(* Force the logger (and its channel/header) while single-threaded: [Lazy.force]
   is not safe under concurrent first force across domains. The intended
   parallel model is to call this before spawning analysis domains. *)
let warmup () = ignore (logger ())

let write_string str =
  let logger = logger () in
  let do_ () =
    Mutex.protect io_mutex (fun () ->
        Out_channel.output_string logger.oc str;
        Out_channel.output_char logger.oc '\n';
        Out_channel.flush logger.oc)
  in
  (* We could avoid the below by defining loggers à la Logs, so that interject
     would be part of a given logger. *)
  match (Config.get ()).kind with
  | Html -> do_ ()
  | Stderr -> Config.get_interject () do_

let incr_depth_counter () =
  let d = Soteria_std.Dls.get depth_counter in
  d := !d + 1

let decr_depth_counter () =
  let d = Soteria_std.Dls.get depth_counter in
  d := !d - 1

module L = struct
  let start_section ?(is_branch = false) str =
    if Config.logs_enabled () then (
      incr_depth_counter ();
      match (Config.get ()).kind with
      | Html ->
          write_string (Html.section_opening ~is_branch);
          write_string (Html.section_title str)
      | Stderr ->
          (* Not writing start/end sections in text format *)
          ())

  let end_section () =
    if Config.logs_enabled () then (
      decr_depth_counter ();
      match (Config.get ()).kind with
      | Html -> write_string Html.section_closing
      | Stderr -> ())

  let with_section ?(is_branch = false) str f =
    start_section ~is_branch str;
    Fun.protect ~finally:end_section f

  let log ~level msgf =
    if Config.should_log level then
      msgf @@ fun fmt ->
      Format.kasprintf
        (fun msg ->
          let msg =
            (* TODO: Write a different logger for each kind, instead of pattern
               matching in each function *)
            match (Config.get ()).kind with
            | Html -> Html.message level msg
            | Stderr -> Printf.sprintf "%s %s" (format_level level) msg
          in
          write_string msg)
        fmt

  let trace msgf = log ~level:Level.Trace msgf
  let debug msgf = log ~level:Level.Debug msgf
  let info msgf = log ~level:Level.Info msgf
  let warn msgf = log ~level:Level.Warn msgf
  let error msgf = log ~level:Level.Error msgf
  let smt msgf = log ~level:Level.Smt msgf
end

module Import = struct
  module L = L
end
