open Soteria_std

(** line * col, in characters -- we handle UTF-8 *)
type pos = int * int

type 'a diag = 'a Grace.Diagnostic.t

type severity = Grace.Diagnostic.Severity.t =
  | Help
  | Note
  | Warning
  | Error
  | Bug
[@@deriving eq, ord, show { with_path = false }]

let pp_severity ft = function
  | Help -> Logs.Printers.pp_clr2 `Cyan `Bold ft "help"
  | Error -> Logs.Printers.pp_err ft "error"
  | Warning -> Logs.Printers.pp_warn ft "warning"
  | Note -> Logs.Printers.pp_ok ft "note"
  | Bug -> Logs.Printers.pp_err ft "bug"

let read_file file =
  let ic = open_in file in
  let rec iter () f =
    match input_line ic with
    | s ->
        f s;
        iter () f
    | exception End_of_file -> close_in ic
  in
  iter () |> Iter.intersperse "\n" |> Iter.concat_str

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
  let@ ic = In_channel.with_open_text file in
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

let real_index_str (content : string) ((line, col) : pos) =
  let lines = String.split_on_char '\n' content in
  let rec aux current_line acc = function
    | [] -> acc
    | str :: rest when current_line < line ->
        aux (current_line + 1) (acc + String.length str + 1) rest
    | str :: _ when current_line = line ->
        let ofs = utf8_to_byte_offset str col in
        acc + ofs
    | _ -> acc
  in
  aux 0 0 lines

let mk_range_file ?filename ?content file from_ to_ =
  let open Grace.Range in
  let bi = Grace.Byte_index.of_int in
  (* Could be optimised if indexes are in the same file, but I don't think
     printing errors is what's going to take time. Also, it shouldn't be
     required anyway, see https://github.com/johnyob/grace/issues/46 *)
  try
    let index_fn =
      Option.fold ~none:(real_index file) ~some:real_index_str content
    in
    let idx1, idx2 = (index_fn from_, index_fn to_) in
    let source : Grace.Source.t =
      match (filename, content) with
      | None, None -> `File file
      | Some name, None ->
          `String { name = Some name; content = read_file file }
      | Some name, Some content -> `String { name = Some name; content }
      | None, Some content -> `String { name = Some file; content }
    in
    create ~source (bi idx1) (bi idx2)
  with Sys_error _ ->
    create
      ~source:(`String { name = Some file; content = "File not found" })
      (bi 0) (bi 14)

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
  let { color; utf8 } : Logs.Profile.t = Logs.Profile.get () in
  let styles, use_ansi =
    if color then (GConfig.Style_sheet.default, Some true)
    else (GConfig.Style_sheet.(no_color default), Some false)
  in
  let chars = if utf8 then GConfig.Chars.unicode else GConfig.Chars.ascii in
  let config = GConfig.{ chars; styles; use_ansi } in
  if (Config.get ()).compact then
    Grace_ansi_renderer.pp_compact_diagnostic ~config ft diag
  else Grace_ansi_renderer.pp_diagnostic ~config ft diag

let print_diagnostic ~severity ~error ~as_ranges ~fname ~call_trace =
  with_unaltered_geo @@ fun () ->
  let labels = call_trace_to_labels ~as_ranges call_trace in
  try
    Grace.Diagnostic.createf ~labels severity "%s in %s" error fname
    |> Fmt.pr "%a@?" pp
  with _ ->
    Fmt.pr "%a: %a@?" pp_severity severity
      (Logs.Printers.pp_style `Bold)
      (error ^ " in " ^ fname)

let print_diagnostic_simple ~severity msg =
  with_unaltered_geo @@ fun () ->
  let msg = Grace.Diagnostic.Message.create msg in
  Grace.Diagnostic.create severity msg |> Fmt.pr "%a@\n@?" pp
