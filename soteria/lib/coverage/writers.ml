open Soteria_std
open Hashtbl
open Types

let effective_line_hits (file_cov : 'a file_hits) =
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

let sorted_fun_bindings tbl =
  Hfun.to_seq tbl
  |> List.of_seq
  |> List.sort (Pair.compare compare_function_id (fun _ _ -> 0))

let merge_hits (dst : unit file_hits) (src : 'a file_hits) =
  Hint.iter
    (fun line n ->
      let prev = Option.value ~default:0 (Hint.find_opt dst.lines line) in
      Hint.replace dst.lines line (prev + n))
    src.lines;
  Hstring.iter
    (fun branch_id br ->
      match Hstring.find_opt dst.branches branch_id with
      | None -> Hstring.replace dst.branches branch_id br
      | Some prev ->
          Hstring.replace dst.branches branch_id
            {
              line = prev.line;
              then_hits = prev.then_hits + br.then_hits;
              else_hits = prev.else_hits + br.else_hits;
            })
    src.branches

let aggregate_per_file (report : t) =
  let out =
    Hstring.create
      (Hstring.length report.per_file + Hfun.length report.per_function)
  in
  Hstring.iter
    (fun file fh ->
      Hstring.replace out file
        {
          lines = Hint.copy fh.lines;
          branches = Hstring.copy fh.branches;
          meta = ();
        })
    report.per_file;
  Hfun.iter
    (fun { file; _ } fh ->
      match Hstring.find_opt out file with
      | None ->
          Hstring.replace out file
            {
              lines = Hint.copy fh.lines;
              branches = Hstring.copy fh.branches;
              meta = ();
            }
      | Some dst -> merge_hits dst fh)
    report.per_function;
  out

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

  let branches_by_line_tbl (branches : branch_coverage Hstring.t) =
    let by_line = Hint.create 8 in
    Hstring.iter
      (fun _branch_id br ->
        let taken = min br.then_hits 1 + min br.else_hits 1 in
        match Hint.find_opt by_line br.line with
        | None -> Hint.replace by_line br.line (taken, 2)
        | Some (prev_taken, prev_total) ->
            Hint.replace by_line br.line (prev_taken + taken, prev_total + 2))
      branches;
    by_line

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

  let class_lines_xml (hits : int Hint.t) (by_line : (int * int) Hint.t) =
    sorted_int_bindings hits
    |> List.map (fun (line, line_hits) ->
        match Hint.find_opt by_line line with
        | None -> line_xml ~line ~hits:line_hits
        | Some (taken, total) ->
            branch_line_xml ~line ~hits:line_hits ~taken ~total)

  let line_and_branch_rates (line_hits : int Hint.t)
      (branches : branch_coverage Hstring.t) =
    let total_lines = ref 0 in
    let covered_lines = ref 0 in
    Hint.iter
      (fun _line hits ->
        incr total_lines;
        if hits > 0 then incr covered_lines)
      line_hits;
    let total_branches = ref 0 in
    let covered_branches = ref 0 in
    Hstring.iter
      (fun _id br ->
        total_branches := !total_branches + 2;
        if br.then_hits > 0 then incr covered_branches;
        if br.else_hits > 0 then incr covered_branches)
      branches;
    let line_rate =
      if !total_lines = 0 then 1.
      else float_of_int !covered_lines /. float_of_int !total_lines
    in
    let branch_rate =
      if !total_branches = 0 then 1.
      else float_of_int !covered_branches /. float_of_int !total_branches
    in
    (line_rate, branch_rate)

  type totals = {
    mutable total_lines : int;
    mutable covered_lines : int;
    mutable total_branches : int;
    mutable covered_branches : int;
  }

  let make_totals () =
    {
      total_lines = 0;
      covered_lines = 0;
      total_branches = 0;
      covered_branches = 0;
    }

  let add_line_totals totals hits =
    Hint.iter
      (fun _line line_hits ->
        totals.total_lines <- totals.total_lines + 1;
        if line_hits > 0 then totals.covered_lines <- totals.covered_lines + 1)
      hits

  let add_branch_totals totals branches =
    Hstring.iter
      (fun _branch_id br ->
        totals.total_branches <- totals.total_branches + 2;
        if br.then_hits > 0 then
          totals.covered_branches <- totals.covered_branches + 1;
        if br.else_hits > 0 then
          totals.covered_branches <- totals.covered_branches + 1)
      branches

  let class_xml file (file_cov : unit file_hits) (methods : Xml.xml list) =
    let hits = effective_line_hits file_cov in
    let by_line = branches_by_line_tbl file_cov.branches in
    mk_elem "class"
      [
        ("name", file);
        ("filename", file);
        ("line-rate", "0.0");
        ("branch-rate", "0.0");
      ]
      [
        mk_elem "methods" [] methods;
        mk_elem "lines" [] (class_lines_xml hits by_line);
      ]

  let method_xml name (fn_cov : function_meta file_hits) =
    let hits = effective_line_hits fn_cov in
    let by_line = branches_by_line_tbl fn_cov.branches in
    let line_rate, branch_rate = line_and_branch_rates hits fn_cov.branches in
    mk_elem "method"
      [
        ("name", name);
        ("signature", "");
        ("line-rate", Printf.sprintf "%.6f" line_rate);
        ("branch-rate", Printf.sprintf "%.6f" branch_rate);
        ("complexity", "0.0");
      ]
      [ mk_elem "lines" [] (class_lines_xml hits by_line) ]

  let cobertura_xml (report : t) =
    let per_file = aggregate_per_file report in
    let totals = make_totals () in
    let methods_by_file = Hstring.create 8 in
    sorted_fun_bindings report.per_function
    |> List.iter (fun (({ file; name } : function_id), fn_cov) ->
        let prev =
          Option.value ~default:[] (Hstring.find_opt methods_by_file file)
        in
        Hstring.replace methods_by_file file (method_xml name fn_cov :: prev));
    let classes =
      sorted_str_bindings per_file
      |> List.map (fun (file, file_cov) ->
          let hits = effective_line_hits file_cov in
          add_line_totals totals hits;
          add_branch_totals totals file_cov.branches;
          let methods =
            Option.value ~default:[] (Hstring.find_opt methods_by_file file)
            |> List.rev
          in
          class_xml file file_cov methods)
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

  let to_formatter ft report = write_xml_to_formatter ft (cobertura_xml report)
  let to_file file report = write_xml_to_file file (cobertura_xml report)
end

module LcovWriter : Writer = struct
  let next_block_index next_by_line line =
    let idx = Option.value ~default:0 (Hint.find_opt next_by_line line) in
    Hint.replace next_by_line line (idx + 1);
    idx

  let pp_file ft file (file_cov : unit file_hits)
      (functions : (string * function_meta) list) =
    let hits = effective_line_hits file_cov in
    let total_lines = ref 0 in
    let covered_lines = ref 0 in
    let total_functions = ref 0 in
    let covered_functions = ref 0 in
    let total_branches = ref 0 in
    let covered_branches = ref 0 in
    let next_by_line = Hint.create 8 in
    Format.fprintf ft "SF:%s\n" file;
    List.iter
      (fun (fn_name, fn_cov) ->
        incr total_functions;
        if fn_cov.hits > 0 then incr covered_functions;
        let line = Option.value ~default:0 fn_cov.line in
        let fn_name =
          String.map (fun c -> if c = ',' then ';' else c) fn_name
        in
        Format.fprintf ft "FN:%d,%s\n" line fn_name;
        Format.fprintf ft "FNDA:%d,%s\n" fn_cov.hits fn_name)
      functions;
    Format.fprintf ft "FNF:%d\n" !total_functions;
    Format.fprintf ft "FNH:%d\n" !covered_functions;
    sorted_str_bindings (file_cov.branches : branch_coverage Hstring.t)
    |> List.iter (fun (_branch_id, (br : branch_coverage)) ->
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

  let to_formatter ft (report : t) =
    let per_file = aggregate_per_file report in
    let fns_by_file = Hstring.create 8 in
    sorted_fun_bindings report.per_function
    |> List.iter (fun (({ file; name } : function_id), fn_cov) ->
        let fn_list =
          Option.value ~default:[] (Hstring.find_opt fns_by_file file)
        in
        Hstring.replace fns_by_file file ((name, fn_cov.meta) :: fn_list));
    sorted_str_bindings per_file
    |> List.iter (fun (file, cov) ->
        let functions =
          Option.value ~default:[] (Hstring.find_opt fns_by_file file)
          |> List.rev
        in
        pp_file ft file cov functions)

  let to_file file report =
    let oc = Out_channel.open_text file in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        let ft = Format.formatter_of_out_channel oc in
        to_formatter ft report;
        Format.pp_print_flush ft ())
end
