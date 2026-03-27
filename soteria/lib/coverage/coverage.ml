open Soteria_std
module Hstring = Hashtbl.Hstring
module Config = Config

type branch_side = [ `Then | `Else ]
type source_span = { file : string; line : int; branch_id : string }
type branch_coverage = { line : int; then_reached : bool; else_reached : bool }

type file_coverage = {
  lines : int Hstring.t;
  branches : branch_coverage Hstring.t;
}

type report = file_coverage Hstring.t
type t = { line_hits : int Hstring.t; branch_hits : branch_coverage Hstring.t }
type 'a with_coverage = { res : 'a; coverage : t }

type coverage_writer = {
  write_to_formatter : Format.formatter -> report -> unit;
  write_to_file : string -> report -> unit;
}

let sorted_bindings tbl =
  Hstring.to_seq tbl
  |> List.of_seq
  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)

let make_line_key ~file ~line = Fmt.str "%s:%d" file line

let make_branch_key ~file ~line ~branch_id =
  Fmt.str "%s:%d:%s" file line branch_id

let parse_line_key key =
  match String.index_of ~sub_str:":" key with
  | None -> None
  | Some i ->
      let file = String.sub key 0 i in
      let line_part = String.sub key (i + 1) (String.length key - i - 1) in
      Option.map (fun line -> (file, line)) (int_of_string_opt line_part)

let parse_branch_key key =
  match String.index_of ~sub_str:":" key with
  | None -> None
  | Some i ->
      let file = String.sub key 0 i in
      let rest = String.sub key (i + 1) (String.length key - i - 1) in
      Option.map
        (fun j ->
          let line_part = String.sub rest 0 j in
          let branch_id =
            String.sub rest (j + 1) (String.length rest - j - 1)
          in
          (file, int_of_string_opt line_part, branch_id))
        (String.index_of ~sub_str:":" rest)

let create () = { line_hits = Hstring.create 0; branch_hits = Hstring.create 0 }

let copy t =
  {
    line_hits = Hstring.copy t.line_hits;
    branch_hits = Hstring.copy t.branch_hits;
  }

let merge c1 c2 =
  let c = copy c1 in
  Hstring.iter
    (fun key n ->
      let prev = Option.value ~default:0 (Hstring.find_opt c.line_hits key) in
      Hstring.replace c.line_hits key (prev + n))
    c2.line_hits;
  Hstring.iter
    (fun key br ->
      match Hstring.find_opt c.branch_hits key with
      | None -> Hstring.replace c.branch_hits key br
      | Some prev ->
          Hstring.replace c.branch_hits key
            {
              line = prev.line;
              then_reached = prev.then_reached || br.then_reached;
              else_reached = prev.else_reached || br.else_reached;
            })
    c2.branch_hits;
  c

let with_empty_coverage res = { res; coverage = create () }
let map_with_coverage f { res; coverage } = { res = f res; coverage }

let to_report t =
  let report = Hstring.create 16 in
  let get_or_create_file_cov file =
    match Hstring.find_opt report file with
    | Some cov -> cov
    | None ->
        let cov = { lines = Hstring.create 16; branches = Hstring.create 16 } in
        Hstring.add report file cov;
        cov
  in
  Hstring.iter
    (fun key hits ->
      match parse_line_key key with
      | None -> ()
      | Some (file, line) ->
          let file_cov = get_or_create_file_cov file in
          Hstring.replace file_cov.lines (string_of_int line) hits)
    t.line_hits;
  Hstring.iter
    (fun key br ->
      match parse_branch_key key with
      | None -> ()
      | Some (file, Some _line, branch_id) ->
          let file_cov = get_or_create_file_cov file in
          Hstring.replace file_cov.branches branch_id br
      | Some (_file, None, _branch_id) -> ())
    t.branch_hits;
  report

let branch_coverage_to_yojson b =
  `Assoc
    [
      ("line", `Int b.line);
      ("then_reached", `Bool b.then_reached);
      ("else_reached", `Bool b.else_reached);
    ]

let file_coverage_to_yojson file_cov =
  let lines_json =
    `Assoc
      (List.map (fun (line, hits) -> (line, `Int hits))
      @@ sorted_bindings file_cov.lines)
  in
  let branches_json =
    `Assoc
      (List.map (fun (branch_id, branch_cov) ->
           (branch_id, branch_coverage_to_yojson branch_cov))
      @@ sorted_bindings file_cov.branches)
  in
  `Assoc [ ("lines", lines_json); ("branches", branches_json) ]

let report_to_yojson report =
  `Assoc
    (List.map (fun (file, file_cov) -> (file, file_coverage_to_yojson file_cov))
    @@ sorted_bindings report)

let to_yojson t = report_to_yojson (to_report t)

let write_json_to_formatter ft report =
  Yojson.Safe.pretty_print ft (report_to_yojson report)

let write_json_to_file file report =
  Yojson.Safe.to_file file (report_to_yojson report)

let pp_line_hits_json ft line_hits =
  Fmt.pf ft "{";
  let first = ref true in
  List.iter
    (fun (line, hits) ->
      if !first then first := false else Fmt.pf ft ",";
      Fmt.pf ft "\"%s\":%d" line hits)
    (sorted_bindings line_hits);
  Fmt.pf ft "}"

let write_codecov_json_to_formatter ft report =
  Fmt.pf ft "{\"coverage\":{";
  let first = ref true in
  List.iter
    (fun (file, file_cov) ->
      if !first then first := false else Fmt.pf ft ",";
      Fmt.pf ft "\"%s\":" file;
      pp_line_hits_json ft file_cov.lines)
    (sorted_bindings report);
  Fmt.pf ft "}}"

let write_cobertura_to_formatter ft report =
  let covered_lines = ref 0 in
  let total_lines = ref 0 in
  let covered_branches = ref 0 in
  let total_branches = ref 0 in
  Hstring.iter
    (fun _file file_cov ->
      Hstring.iter
        (fun _line hits ->
          incr total_lines;
          if hits > 0 then incr covered_lines)
        file_cov.lines;
      Hstring.iter
        (fun _branch_id br ->
          total_branches := !total_branches + 2;
          if br.then_reached then incr covered_branches;
          if br.else_reached then incr covered_branches)
        file_cov.branches)
    report;
  let line_rate =
    if !total_lines = 0 then 1.
    else float_of_int !covered_lines /. float_of_int !total_lines
  in
  let branch_rate =
    if !total_branches = 0 then 1.
    else float_of_int !covered_branches /. float_of_int !total_branches
  in
  Fmt.pf ft
    "<?xml version=\"1.0\" ?>\n\
     <coverage lines-valid=\"%d\" lines-covered=\"%d\" line-rate=\"%.6f\" \
     branches-valid=\"%d\" branches-covered=\"%d\" branch-rate=\"%.6f\" \
     version=\"soteria\">\n\
    \  <packages>\n\
    \    <package name=\"soteria\" line-rate=\"%.6f\" branch-rate=\"%.6f\">\n\
    \      <classes>\n"
    !total_lines !covered_lines line_rate !total_branches !covered_branches
    branch_rate line_rate branch_rate;
  List.iter
    (fun (file, file_cov) ->
      Fmt.pf ft
        "        <class name=\"%s\" filename=\"%s\" line-rate=\"0.0\" \
         branch-rate=\"0.0\">\n\
        \          <methods/>\n\
        \          <lines>\n"
        file file;
      List.iter
        (fun (line, hits) ->
          Fmt.pf ft "            <line number=\"%s\" hits=\"%d\"/>\n" line hits)
        (sorted_bindings file_cov.lines);
      List.iter
        (fun (_branch_id, br) ->
          let taken =
            (if br.then_reached then 1 else 0)
            + if br.else_reached then 1 else 0
          in
          Fmt.pf ft
            "            <line number=\"%d\" hits=\"1\" branch=\"true\" \
             condition-coverage=\"%d%% (%d/2)\"/>\n"
            br.line
            (taken * 100 / 2)
            taken)
        (sorted_bindings file_cov.branches);
      Fmt.pf ft "          </lines>\n        </class>\n")
    (sorted_bindings report);
  Fmt.pf ft "      </classes>\n    </package>\n  </packages>\n</coverage>\n"

let write_cobertura_to_file file report =
  let oc = Out_channel.open_text file in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      let ft = Format.formatter_of_out_channel oc in
      write_cobertura_to_formatter ft report;
      Format.pp_print_flush ft ())

module Writers = struct
  let json =
    {
      write_to_formatter = write_json_to_formatter;
      write_to_file = write_json_to_file;
    }

  let cobertura =
    {
      write_to_formatter = write_cobertura_to_formatter;
      write_to_file = write_cobertura_to_file;
    }

  let codecov_json =
    {
      write_to_formatter = write_codecov_json_to_formatter;
      write_to_file =
        (fun file report ->
          let oc = Out_channel.open_text file in
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () ->
              let ft = Format.formatter_of_out_channel oc in
              write_codecov_json_to_formatter ft report;
              Format.pp_print_flush ft ()));
    }
end

let writer_of_format format =
  match String.lowercase_ascii format with
  | "json" -> Some Writers.json
  | "cobertura" | "cobertura-xml" | "xml" -> Some Writers.cobertura
  | "codecov-json" -> Some Writers.codecov_json
  | _ -> None

let output_with writer t =
  match (Config.get ()).output_coverage with
  | None -> ()
  | Some "stdout" -> writer.write_to_formatter Fmt.stdout (to_report t)
  | Some file -> writer.write_to_file file (to_report t)

let output t =
  let cfg = Config.get () in
  match writer_of_format cfg.coverage_format with
  | Some writer -> output_with writer t
  | None ->
      Fmt.failwith
        "Unsupported coverage format: %s (supported: json, cobertura, \
         codecov-json)"
        cfg.coverage_format

module As_ctx = struct
  type _ Effect.t += Apply : (t -> unit) -> unit Effect.t

  let with_coverage () f =
    let coverage = create () in
    let res =
      try f ()
      with effect Apply f, k ->
        f coverage;
        Effect.Deep.continue k ()
    in
    { res; coverage }

  let with_coverage_ignored () f =
    try f () with effect Apply _, k -> Effect.Deep.continue k ()

  let with_coverage_dumped () f =
    let { res; coverage } = with_coverage () f in
    if Option.is_some (Config.get ()).output_coverage then output coverage;
    res

  let[@inline] apply f = Effect.perform (Apply f)

  let mark_line ~file ~line =
    if line > 0 then
      apply (fun coverage ->
          let key = make_line_key ~file ~line in
          let prev =
            Option.value ~default:0 (Hstring.find_opt coverage.line_hits key)
          in
          Hstring.replace coverage.line_hits key (prev + 1))

  let mark_branch ({ file; line; branch_id } : source_span) side =
    if line > 0 then
      apply (fun coverage ->
          let key = make_branch_key ~file ~line ~branch_id in
          let prev =
            Option.value
              ~default:{ line; then_reached = false; else_reached = false }
              (Hstring.find_opt coverage.branch_hits key)
          in
          let next =
            match side with
            | `Then -> { prev with then_reached = true }
            | `Else -> { prev with else_reached = true }
          in
          Hstring.replace coverage.branch_hits key next)

  let get_copy () : t =
    let copy_ref = ref (create ()) in
    apply (fun coverage -> copy_ref := copy coverage);
    !copy_ref
end
