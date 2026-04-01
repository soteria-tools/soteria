open Soteria_std
module Hint = Hashtbl.Hint
module Hstring = Hashtbl.Hstring

(* Definitions *)

module Config = Config
include Types
include Writers

type 'a with_coverage = { res : 'a; coverage : t }

let empty_file_hits () =
  {
    lines = Hint.create 16;
    branches = Hstring.create 8;
    functions = Hstring.create 8;
  }

let get_or_create_file_hits (coverage : t) file =
  match Hstring.find_opt coverage file with
  | Some file_hits -> file_hits
  | None ->
      let file_hits = empty_file_hits () in
      Hstring.replace coverage file file_hits;
      file_hits

let create () = Hstring.create 8

let copy (t : t) : t =
  let c = Hstring.create (Hstring.length t) in
  Hstring.iter
    (fun file file_hits ->
      Hstring.replace c file
        {
          lines = Hint.copy file_hits.lines;
          branches = Hstring.copy file_hits.branches;
          functions = Hstring.copy file_hits.functions;
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
                  then_hits = prev.then_hits + br.then_hits;
                  else_hits = prev.else_hits + br.else_hits;
                })
        file_hits2.branches;
      Hstring.iter
        (fun fn_name fn_cov ->
          match Hstring.find_opt file_hits.functions fn_name with
          | None -> Hstring.replace file_hits.functions fn_name fn_cov
          | Some prev ->
              Hstring.replace file_hits.functions fn_name
                {
                  line = prev.line;
                  end_line =
                    (match fn_cov.end_line with
                    | Some _ -> fn_cov.end_line
                    | None -> prev.end_line);
                  hits = prev.hits + fn_cov.hits;
                })
        file_hits2.functions)
    c2;
  c

let with_empty_coverage res = { res; coverage = create () }
let map_with_coverage f { res; coverage } = { res = f res; coverage }

let output t =
  let (module Writer : Writers.Writer) =
    match (Config.get ()).coverage_format with
    | Json -> (module JsonWriter)
    | Cobertura -> (module CoberturaWriter)
    | Lcov -> (module LcovWriter)
  in
  match (Config.get ()).output_coverage with
  | None -> ()
  | Some "stdout" -> Writer.to_formatter Fmt.stdout t
  | Some file -> Writer.to_file file t

let update_reachable_line hits line =
  if not (Hint.mem hits line) then Hint.replace hits line 0

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

  let register_line ~file ~line =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        update_reachable_line file_hits.lines line)

  let register_lines ~file (lines : int Iter.t) =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        lines (update_reachable_line file_hits.lines))

  let register_files_lines (spans : (string * int Iter.t) Iter.t) =
    apply (fun coverage ->
        spans (fun (file, lines) ->
            let file_hits = get_or_create_file_hits coverage file in
            lines (update_reachable_line file_hits.lines)))

  let register_branch ({ file; line; branch_id } : branch_span) =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        if not (Hstring.mem file_hits.branches branch_id) then
          Hstring.replace file_hits.branches branch_id
            { line; then_hits = 0; else_hits = 0 })

  let register_function ~file ~name ~line ?end_line () =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        let prev = Hstring.find_opt file_hits.functions name in
        let fn_cov =
          match prev with
          | None -> { line; end_line; hits = 0 }
          | Some prev ->
              {
                line = prev.line;
                end_line =
                  (match end_line with
                  | Some _ -> end_line
                  | None -> prev.end_line);
                hits = prev.hits;
              }
        in
        Hstring.replace file_hits.functions name fn_cov)

  let register_functions (fns : (string * string * int * int option) Iter.t) =
    apply (fun coverage ->
        fns (fun (file, name, line, end_line) ->
            let file_hits = get_or_create_file_hits coverage file in
            let prev = Hstring.find_opt file_hits.functions name in
            let fn_cov =
              match prev with
              | None -> { line; end_line; hits = 0 }
              | Some prev ->
                  {
                    line = prev.line;
                    end_line =
                      (match end_line with
                      | Some _ -> end_line
                      | None -> prev.end_line);
                    hits = prev.hits;
                  }
            in
            Hstring.replace file_hits.functions name fn_cov))

  let mark_line ~file ~line =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        let prev =
          Option.value ~default:0 (Hint.find_opt file_hits.lines line)
        in
        Hint.replace file_hits.lines line (prev + 1))

  let mark_branch side ({ file; line; branch_id } : branch_span) =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        let prev =
          Option.value
            ~default:{ line; then_hits = 0; else_hits = 0 }
            (Hstring.find_opt file_hits.branches branch_id)
        in
        let next =
          match side with
          | Then -> { prev with then_hits = prev.then_hits + 1 }
          | Else -> { prev with else_hits = prev.else_hits + 1 }
        in
        Hstring.replace file_hits.branches branch_id next)

  let mark_function ~file ~name ~line ?end_line () =
    apply (fun coverage ->
        let file_hits = get_or_create_file_hits coverage file in
        let prev = Hstring.find_opt file_hits.functions name in
        let fn_cov =
          match prev with
          | None -> { line; end_line; hits = 1 }
          | Some prev ->
              {
                line = prev.line;
                end_line =
                  (match end_line with
                  | Some _ -> end_line
                  | None -> prev.end_line);
                hits = prev.hits + 1;
              }
        in
        Hstring.replace file_hits.functions name fn_cov)

  let get_copy () : t =
    let copy_ref = ref (create ()) in
    apply (fun coverage -> copy_ref := copy coverage);
    !copy_ref
end
