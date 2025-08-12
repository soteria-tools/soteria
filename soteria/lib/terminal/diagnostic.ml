(** line * col, in characters -- we handle UTF-8 *)
type pos = int * int

type 'a diag = 'a Grace.Diagnostic.t
type range = Grace.Range.t

type severity = Grace.Diagnostic.Severity.t =
  | Help
  | Note
  | Warning
  | Error
  | Bug

let read_file file =
  let ic = open_in file in
  let rec loop acc =
    match input_line ic with
    | s -> loop (s :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc |> String.concat "\n"
  in
  loop []

let utf8_to_byte_offset str idx =
  let i = ref 0 in
  let ofs = ref 0 in
  while !i < idx && !ofs < String.length str do
    let len = Uchar.utf_decode_length (String.get_utf_8_uchar str !ofs) in
    ofs := !ofs + len;
    incr i
  done;
  !ofs

let real_index (file : string) ((line, col) : pos) =
  let open Syntaxes.FunctionWrap in
  let@ ic = Channels.with_in_file file in
  let current_index = ref 0 in
  let current_line = ref 0 in
  while !current_line <= line do
    match input_line ic with
    | str when !current_line = line ->
        let ofs = utf8_to_byte_offset str col in
        (* let ofs = if ofs <= 0 then 1 else ofs in *)
        current_index := !current_index + ofs;
        incr current_line
    | str ->
        current_index := !current_index + String.length str + 1;
        incr current_line
    | exception End_of_file ->
        (* Terminate here, return 0 *)
        current_index := 0;
        current_line := line + 1
  done;
  !current_index

let mk_range_file ?filename file from_ to_ =
  let open Grace.Range in
  let bi = Grace.Byte_index.of_int in
  (* Could be optimised if indexes are in the same file,
   but I don't think printing errors is what's going to take time.
   Also, it shouldn't be required anyway, see https://github.com/johnyob/grace/issues/46
*)
  let idx1 = real_index file from_ in
  let idx2 = real_index file to_ in
  let source : Grace.Source.t =
    match filename with
    | None -> `File file
    | Some name -> `String { name = Some name; content = read_file file }
  in
  create ~source (bi idx1) (bi idx2)

let call_trace_to_labels ~as_ranges (call_trace : 'a Call_trace.t) =
  let open Grace.Diagnostic in
  let rec aux i acc (call_trace : 'a Call_trace.t) =
    match call_trace with
    | [] -> acc
    | [ { loc; msg } ] ->
        let this =
          as_ranges loc
          |> List.map @@ fun range ->
             Label.primary ~range (Message.of_string msg)
        in
        this @ acc
    | { loc; msg } :: rest ->
        let msg = Fmt.str "%i: %s" i msg in
        let sec =
          as_ranges loc
          |> List.map @@ fun range ->
             Label.secondary ~range (Message.of_string msg)
        in
        aux (i + 1) (sec @ acc) rest
  in
  aux 1 [] call_trace

let with_unaltered_geo f =
  let geo = Format.get_geometry () in
  f ();
  Format.set_geometry ~max_indent:geo.max_indent ~margin:geo.margin

let pp ft diag =
  let module GConfig = Grace_ansi_renderer.Config in
  let { color; utf8 } : Profile.t = !Profile.profile in
  let styles, use_ansi =
    if color then (GConfig.Style_sheet.default, true)
    else (GConfig.Style_sheet.(no_color default), false)
  in
  let chars = if utf8 then GConfig.Chars.unicode else GConfig.Chars.ascii in
  let config = GConfig.{ chars; styles; use_ansi } in
  if Config.compact () then
    Grace_ansi_renderer.pp_compact_diagnostic ~config () ft diag
  else Grace_ansi_renderer.pp_diagnostic ~config () ft diag

let print_diagnostic ~severity ~error ~as_ranges ~fname ~call_trace =
  with_unaltered_geo @@ fun () ->
  let labels = call_trace_to_labels ~as_ranges call_trace in
  Grace.Diagnostic.createf ~labels severity "%s in %s" error fname
  |> Fmt.pr "%a@?" pp

let print_diagnostic_simple ~severity msg =
  with_unaltered_geo @@ fun () ->
  let msg = Grace.Diagnostic.Message.create msg in
  Grace.Diagnostic.create severity msg |> Fmt.pr "%a@?" pp
