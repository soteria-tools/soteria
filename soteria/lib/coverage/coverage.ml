open Soteria_std
module Hstring = Hashtbl.Hstring
module Hint = Hashtbl.Hint
module Config = Config
module IMap = PatriciaTree.MakeMap (Int)

type branch_side = Then | Else
type branch_span = { file : string; line : int; branch_id : string }
type branch_coverage = { line : int; then_reached : bool; else_reached : bool }
type file_hits = { lines : int Hint.t; branches : branch_coverage Hstring.t }
type t = file_hits Hstring.t
type 'a with_coverage = { res : 'a; coverage : t }

let int_of_bool b = if b then 1 else 0

let effective_line_hits_for_file (file_cov : file_hits) =
  let hits = Hint.copy file_cov.lines in
  Hstring.iter
    (fun _branch_id br ->
      if br.then_reached || br.else_reached then
        let execs = int_of_bool br.then_reached + int_of_bool br.else_reached in
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

let empty_file_hits () = { lines = Hint.create 0; branches = Hstring.create 0 }

let get_or_create_file_hits (coverage : t) file =
  match Hstring.find_opt coverage file with
  | Some file_hits -> file_hits
  | None ->
      let file_hits = empty_file_hits () in
      Hstring.replace coverage file file_hits;
      file_hits

let create () = Hstring.create 0

let copy (t : t) : t =
  let c = Hstring.create (Hstring.length t) in
  Hstring.iter
    (fun file file_hits ->
      Hstring.replace c file
        {
          lines = Hint.copy file_hits.lines;
          branches = Hstring.copy file_hits.branches;
        })
    t;
  c

let merge c1 c2 =
  let c = copy c1 in
  Hstring.iter
    (fun file file_hits2 ->
      let file_hits = get_or_create_file_hits c file in
      Hint.iter
        (fun line n ->
          let prev =
            Option.value ~default:0 (Hint.find_opt file_hits.lines line)
          in
          Hint.replace file_hits.lines line (prev + n))
        file_hits2.lines;
      Hstring.iter
        (fun branch_id br ->
          match Hstring.find_opt file_hits.branches branch_id with
          | None -> Hstring.replace file_hits.branches branch_id br
          | Some prev ->
              Hstring.replace file_hits.branches branch_id
                {
                  line = prev.line;
                  then_reached = prev.then_reached || br.then_reached;
                  else_reached = prev.else_reached || br.else_reached;
                })
        file_hits2.branches)
    c2;
  c

let with_empty_coverage res = { res; coverage = create () }
let map_with_coverage f { res; coverage } = { res = f res; coverage }

let branch_coverage_to_yojson b =
  `Assoc
    [
      ("line", `Int b.line);
      ("then_reached", `Bool b.then_reached);
      ("else_reached", `Bool b.else_reached);
    ]

let file_coverage_to_yojson file_cov =
  let line_hits = effective_line_hits_for_file file_cov in
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
  `Assoc [ ("lines", lines_json); ("branches", branches_json) ]

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
    if br.then_reached then
      totals.covered_branches <- totals.covered_branches + 1;
    if br.else_reached then
      totals.covered_branches <- totals.covered_branches + 1

  (** [Hstring.fold] but suitable for currying *)
  let my_hstring_fold init f h = Hstring.fold f h init

  let branches_by_line =
    my_hstring_fold IMap.empty @@ fun _branch_id br ->
    let taken = int_of_bool br.then_reached + int_of_bool br.else_reached in
    IMap.update br.line @@ function
    | None -> Some (taken, 2)
    | Some (prev_taken, prev_total) -> Some (prev_taken + taken, prev_total + 2)

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
    let by_line = branches_by_line file_cov.branches in
    let lines = class_lines_xml hits by_line in
    mk_elem "class"
      [
        ("name", file);
        ("filename", file);
        ("line-rate", "0.0");
        ("branch-rate", "0.0");
      ]
      [ mk_elem "methods" [] []; mk_elem "lines" [] lines ]

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

let output t =
  let (module Writer : Writer) =
    match (Config.get ()).coverage_format with
    | Json -> (module JsonWriter)
    | Cobertura -> (module CoberturaWriter)
  in
  match (Config.get ()).output_coverage with
  | None -> ()
  | Some "stdout" -> Writer.to_formatter Fmt.stdout t
  | Some file -> Writer.to_file file t

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
    output coverage;
    res

  let[@inline] apply f = Effect.perform (Apply f)

  let mark_line ~file ~line =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        let prev =
          Option.value ~default:0 (Hint.find_opt file_hits.lines line)
        in
        Hint.replace file_hits.lines line (prev + 1))

  let mark_line_reachable ~file ~line =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        if Option.is_none (Hint.find_opt file_hits.lines line) then
          Hint.replace file_hits.lines line 0)

  let mark_lines_reachable ~file (lines : int Iter.t) =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        lines (fun line ->
            if Option.is_none (Hint.find_opt file_hits.lines line) then
              Hint.replace file_hits.lines line 0))

  let mark_branch side ({ file; line; branch_id } : branch_span) =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        let prev =
          Option.value
            ~default:{ line; then_reached = false; else_reached = false }
            (Hstring.find_opt file_hits.branches branch_id)
        in
        let next =
          match side with
          | Then -> { prev with then_reached = true }
          | Else -> { prev with else_reached = true }
        in
        Hstring.replace file_hits.branches branch_id next)

  let get_copy () : t =
    let copy_ref = ref (create ()) in
    apply (fun coverage -> copy_ref := copy coverage);
    !copy_ref
end
