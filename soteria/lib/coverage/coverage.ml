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

(** Optimised udpate that doesn't replace if the content didn't change *)
let[@inline] hint_update ~default h key f =
  match Hint.find_opt h key with
  | None -> Hint.replace h key (f default)
  | Some prev ->
      let res = f prev in
      if res <> prev then Hint.replace h key res

(** Optimised udpate that doesn't replace if the content didn't change *)
let[@inline] hstr_update ~default h key f =
  match Hstring.find_opt h key with
  | None -> Hstring.replace h key (f default)
  | Some prev ->
      let res = f prev in
      if res <> prev then Hstring.replace h key res

let update_file_hits (file_hits : file_hits) incr (item : code_item) =
  match item with
  | Line line -> hint_update ~default:0 file_hits.lines line (( + ) incr)
  | Conditional { line; branch_id; side } -> (
      hstr_update
        ~default:{ line; then_hits = 0; else_hits = 0 }
        file_hits.branches branch_id
      @@ fun prev ->
      match side with
      | Some Then -> { prev with then_hits = prev.then_hits + incr }
      | Some Else -> { prev with else_hits = prev.else_hits + incr }
      | None -> prev)
  | Function { name; line; end_line } ->
      hstr_update ~default:{ line; end_line; hits = 0 } file_hits.functions name
      @@ fun prev ->
      let end_line =
        match end_line with Some _ -> end_line | None -> prev.end_line
      in
      { line = prev.line; end_line; hits = prev.hits + incr }

let update_file file incr (item : code_item) cov =
  let file_hits = get_or_create_file_hits cov file in
  update_file_hits file_hits incr item

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
  let register ~file item = apply (update_file file 0 item)

  let register_bulk (items : (string * code_item) Iter.t) =
    apply (fun coverage ->
        items (fun (file, item) -> update_file file 0 item coverage))

  let register_file_bulk (files : (string * code_item Iter.t) Iter.t) =
    apply (fun coverage ->
        files (fun (file, items) ->
            let file_hits = get_or_create_file_hits coverage file in
            items (update_file_hits file_hits 0)))

  let mark ~file item = apply (update_file file 1 item)

  let mark_branch side { file; line; branch_id } =
    apply
      (update_file file 1 (Conditional { line; branch_id; side = Some side }))

  let get_copy () : t =
    let copy_ref = ref (create ()) in
    apply (fun coverage -> copy_ref := copy coverage);
    !copy_ref
end
