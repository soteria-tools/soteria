open Soteria_std
open Hashtbl
open Types

let effective_line_hits_for_file (file_cov : file_hits) =
  let hits = Hint.copy file_cov.lines in
  Hstring.iter
    (fun _branch_id br ->
      let execs = br.then_hits + br.else_hits in
      if execs > 0 then
        let prev_hits = Option.value ~default:0 (Hint.find_opt hits br.line) in
        Hint.replace hits br.line (prev_hits + execs))
    file_cov.branches;
  hits

let sorted_str_bindings tbl =
  Hstring.to_seq tbl
  |> List.of_seq
  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)

let sorted_int_bindings tbl =
  Hint.to_seq tbl
  |> List.of_seq
  |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)

let branch_coverage_to_yojson (b : branch_coverage) =
  `Assoc
    [
      ("line", `Int b.line);
      ("then_hits", `Int b.then_hits);
      ("else_hits", `Int b.else_hits);
    ]

let function_coverage_to_yojson fn_cov =
  let attrs =
    [
      Some ("line", `Int fn_cov.line);
      Some ("hits", `Int fn_cov.hits);
      Option.map (fun end_line -> ("end_line", `Int end_line)) fn_cov.end_line;
    ]
    |> List.filter_map Fun.id
  in
  `Assoc attrs

let file_coverage_to_yojson file_cov =
  let line_hits = effective_line_hits_for_file file_cov in
  let function_hits = file_cov.functions in
  let lines_json =
    `Assoc
      (List.map (fun (line, hits) -> (string_of_int line, `Int hits))
      @@ sorted_int_bindings line_hits)
  in
  let branches_json =
    `Assoc
      (List.map (fun (branch_id, branch_cov) ->
           (branch_id, branch_coverage_to_yojson branch_cov))
      @@ sorted_str_bindings file_cov.branches)
  in
  let functions_json =
    `Assoc
      (List.map (fun (fn_name, fn_cov) ->
           (fn_name, function_coverage_to_yojson fn_cov))
      @@ sorted_str_bindings function_hits)
  in
  `Assoc
    [
      ("lines", lines_json);
      ("branches", branches_json);
      ("functions", functions_json);
    ]

let to_yojson report =
  `Assoc
    (List.map (fun (file, file_cov) -> (file, file_coverage_to_yojson file_cov))
    @@ sorted_str_bindings report)

module type Writer = sig
  val to_formatter : Format.formatter -> t -> unit
  val to_file : string -> t -> unit
end

module JsonWriter : Writer = struct
  let to_formatter ft report = Yojson.Safe.pretty_print ft (to_yojson report)
  let to_file file report = Yojson.Safe.to_file file (to_yojson report)
end

module CoberturaWriter : Writer = struct
  let mk_elem tag attrs children = Xml.Element (tag, attrs, children)

  type totals = {
    mutable total_lines : int; [@default 0]
    mutable covered_lines : int; [@default 0]
    mutable total_branches : int; [@default 0]
    mutable covered_branches : int; [@default 0]
  }
  [@@deriving make]

  let add_line_totals totals =
    Hint.iter @@ fun _line line_hits ->
    totals.total_lines <- totals.total_lines + 1;
    if line_hits > 0 then totals.covered_lines <- totals.covered_lines + 1

  let add_branch_totals totals =
    Hstring.iter @@ fun _branch_id br ->
    totals.total_branches <- totals.total_branches + 2;
    if br.then_hits > 0 then
      totals.covered_branches <- totals.covered_branches + 1;
    if br.else_hits > 0 then
      totals.covered_branches <- totals.covered_branches + 1

  (** [Hstring.fold] but suitable for currying *)
  let my_hstring_fold init f h = Hstring.fold f h init

  let branches_by_line =
    my_hstring_fold IMap.empty @@ fun _branch_id br ->
    let taken = min br.then_hits 1 + min br.else_hits 1 in
    IMap.update br.line @@ function
    | None -> Some (taken, 2)
    | Some (prev_taken, prev_total) -> Some (prev_taken + taken, prev_total + 2)

  let method_xml fn_name (fn_cov : function_coverage) =
    let hits = fn_cov.hits in
    mk_elem "method"
      [
        ("name", fn_name);
        ("signature", "");
        ("line-rate", if hits > 0 then "1.0" else "0.0");
        ("branch-rate", "0.0");
      ]
      [
        mk_elem "lines" []
          [
            mk_elem "line"
              [
                ("number", string_of_int fn_cov.line);
                ("hits", string_of_int hits);
              ]
              [];
          ];
      ]

  let line_xml ~line ~hits =
    mk_elem "line"
      [ ("number", string_of_int line); ("hits", string_of_int hits) ]
      []

  let branch_line_xml ~line ~hits ~taken ~total =
    let pct = if total = 0 then 100 else taken * 100 / total in
    mk_elem "line"
      [
        ("number", string_of_int line);
        ("hits", string_of_int hits);
        ("branch", "true");
        ("condition-coverage", Printf.sprintf "%d%% (%d/%d)" pct taken total);
      ]
      []

  let class_lines_xml (hits : int Hint.t) (by_line : (int * int) IMap.t) =
    sorted_int_bindings hits
    |> List.map @@ fun (line, line_hits) ->
       match IMap.find_opt line by_line with
       | None -> line_xml ~line ~hits:line_hits
       | Some (taken, total) ->
           branch_line_xml ~line ~hits:line_hits ~taken ~total

  let class_xml file (file_cov : file_hits) =
    let hits = effective_line_hits_for_file file_cov in
    let fns = file_cov.functions in
    let by_line = branches_by_line file_cov.branches in
    let lines = class_lines_xml hits by_line in
    let methods =
      sorted_str_bindings fns
      |> List.map (fun (fn_name, fn_cov) -> method_xml fn_name fn_cov)
    in
    mk_elem "class"
      [
        ("name", file);
        ("filename", file);
        ("line-rate", "0.0");
        ("branch-rate", "0.0");
      ]
      [ mk_elem "methods" [] methods; mk_elem "lines" [] lines ]

  let write_xml_to_formatter ft xml =
    Format.pp_print_string ft "<?xml version=\"1.0\" ?>\n";
    Format.pp_print_string ft (Xml.to_string_fmt xml)

  let write_xml_to_file file xml =
    let oc = Out_channel.open_text file in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        Out_channel.output_string oc "<?xml version=\"1.0\" ?>\n";
        Out_channel.output_string oc (Xml.to_string_fmt xml))

  let cobertura_xml report =
    let totals = make_totals () in

    let classes =
      sorted_str_bindings report
      |> List.map @@ fun (file, (file_cov : file_hits)) ->
         let hits = effective_line_hits_for_file file_cov in
         add_line_totals totals hits;
         add_branch_totals totals file_cov.branches;
         class_xml file file_cov
    in
    let line_rate =
      if totals.total_lines = 0 then 1.
      else float_of_int totals.covered_lines /. float_of_int totals.total_lines
    in
    let branch_rate =
      if totals.total_branches = 0 then 1.
      else
        float_of_int totals.covered_branches
        /. float_of_int totals.total_branches
    in
    mk_elem "coverage"
      [
        ("lines-valid", string_of_int totals.total_lines);
        ("lines-covered", string_of_int totals.covered_lines);
        ("line-rate", Printf.sprintf "%.6f" line_rate);
        ("branches-valid", string_of_int totals.total_branches);
        ("branches-covered", string_of_int totals.covered_branches);
        ("branch-rate", Printf.sprintf "%.6f" branch_rate);
        ("version", "soteria");
      ]
      [
        mk_elem "packages" []
          [
            mk_elem "package"
              [
                ("name", "soteria");
                ("line-rate", Printf.sprintf "%.6f" line_rate);
                ("branch-rate", Printf.sprintf "%.6f" branch_rate);
              ]
              [ mk_elem "classes" [] classes ];
          ];
      ]

  let to_formatter ft report = write_xml_to_formatter ft (cobertura_xml report)
  let to_file file report = write_xml_to_file file (cobertura_xml report)
end

module LcovWriter : Writer = struct
  let next_block_index next_by_line line =
    let idx = Option.value ~default:0 (Hint.find_opt next_by_line line) in
    Hint.replace next_by_line line (idx + 1);
    idx

  let pp_file ft file (file_cov : file_hits) =
    let hits = effective_line_hits_for_file file_cov in
    let fns = file_cov.functions in
    let total_lines = ref 0 in
    let covered_lines = ref 0 in
    let total_functions = ref 0 in
    let covered_functions = ref 0 in
    let total_branches = ref 0 in
    let covered_branches = ref 0 in
    let next_by_line = Hint.create 8 in
    Format.fprintf ft "SF:%s\n" file;
    sorted_str_bindings fns
    |> List.iter (fun (fn_name, fn_cov) ->
        incr total_functions;
        if fn_cov.hits > 0 then incr covered_functions;
        let fn_name =
          String.map (fun c -> if c = ',' then ';' else c) fn_name
        in
        Format.fprintf ft "FN:%d,%s\n" fn_cov.line fn_name;
        Format.fprintf ft "FNDA:%d,%s\n" fn_cov.hits fn_name);
    Format.fprintf ft "FNF:%d\n" !total_functions;
    Format.fprintf ft "FNH:%d\n" !covered_functions;
    sorted_str_bindings file_cov.branches
    |> List.iter (fun (branch_id, (br : branch_coverage)) ->
        let _ = branch_id in
        let block = next_block_index next_by_line br.line in
        incr total_branches;
        if br.then_hits > 0 then incr covered_branches;
        Format.fprintf ft "BRDA:%d,%d,then,%d\n" br.line block br.then_hits;
        incr total_branches;
        if br.else_hits > 0 then incr covered_branches;
        Format.fprintf ft "BRDA:%d,%d,else,%d\n" br.line block br.else_hits);
    sorted_int_bindings hits
    |> List.iter (fun (line, line_hits) ->
        incr total_lines;
        if line_hits > 0 then incr covered_lines;
        Format.fprintf ft "DA:%d,%d\n" line line_hits);
    Format.fprintf ft "BRF:%d\n" !total_branches;
    Format.fprintf ft "BRH:%d\n" !covered_branches;
    Format.fprintf ft "LF:%d\n" !total_lines;
    Format.fprintf ft "LH:%d\n" !covered_lines;
    Format.fprintf ft "end_of_record\n"

  let to_formatter ft report =
    sorted_str_bindings report
    |> List.iter (fun (file, cov) -> pp_file ft file cov)

  let to_file file report =
    let oc = Out_channel.open_text file in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        let ft = Format.formatter_of_out_channel oc in
        to_formatter ft report;
        Format.pp_print_flush ft ())
end
