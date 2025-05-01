type logger = { oc : Out_channel.t; mutable depth_counter : int }
type ('a, 'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

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
  let logger = { oc; depth_counter = 0 } in
  let () =
    match (Config.get ()).kind with
    | Html ->
        at_exit (fun () ->
            (* If program is interrupted and not all sections have been closed, close them all! *)
            for _ = 0 to logger.depth_counter - 1 do
              Out_channel.output_string oc Html.section_closing;
              Out_channel.output_char oc '\n'
            done;
            Out_channel.output_string oc Html.footer)
    | Stderr -> ()
  in
  logger

let logger = lazy (init ())
let[@inline] logger () = Lazy.force logger

let write_string str =
  let logger = logger () in
  Out_channel.output_string logger.oc str;
  Out_channel.output_char logger.oc '\n';
  Out_channel.flush logger.oc

let incr_depth_counter () =
  let logger = logger () in
  logger.depth_counter <- logger.depth_counter + 1

let decr_depth_counter () =
  let logger = logger () in
  logger.depth_counter <- logger.depth_counter - 1

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
  try
    let x = f () in
    end_section ();
    x
  with e ->
    end_section ();
    raise e

module L = struct
  let log ~level msgf =
    if Config.should_log level then
      msgf @@ fun fmt ->
      Format.kasprintf
        (fun msg ->
          let msg =
            (* TODO: Write a different logger for each kind, instead of pattern matching in each function *)
            match (Config.get ()).kind with
            | Html -> Html.message level msg
            | Stderr -> Printf.sprintf "[%s] %s" (Level.to_string level) msg
          in
          write_string msg)
        fmt

  let trace msgf = log ~level:Level.Trace msgf
  let debug msgf = log ~level:Level.Debug msgf
  let info msgf = log ~level:Level.Info msgf
  let warn msgf = log ~level:Level.Warn msgf
  let app msgf = log ~level:Level.App msgf
  let error msgf = log ~level:Level.Error msgf
  let smt msgf = log ~level:Level.Smt msgf
end
