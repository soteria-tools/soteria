open Soteria_std
module Hint = Hashtbl.Hint
module Hstring = Hashtbl.Hstring
module Config = Config
include Types
include Writers

type 'a with_coverage = { res : 'a; coverage : t }

let empty_file_hits meta =
  { lines = Hint.create 16; branches = Hstring.create 8; meta }

let create () = { per_file = Hstring.create 8; per_function = Hfun.create 8 }

let merge_function_meta m1 m2 =
  {
    hits = m1.hits + m2.hits;
    line = Option.either m1.line m2.line;
    end_line = Option.either m1.end_line m2.end_line;
  }

let copy_file_hits ~copy_meta (file_hits : 'a file_hits) : 'b file_hits =
  {
    lines = Hint.copy file_hits.lines;
    branches = Hstring.copy file_hits.branches;
    meta = copy_meta file_hits.meta;
  }

let copy (t : t) : t =
  let per_file = Hstring.create (Hstring.length t.per_file) in
  Hstring.iter
    (fun file file_hits ->
      Hstring.replace per_file file (copy_file_hits ~copy_meta:Fun.id file_hits))
    t.per_file;
  let per_function = Hfun.create (Hfun.length t.per_function) in
  Hfun.iter
    (fun fid file_hits ->
      Hfun.replace per_function fid (copy_file_hits ~copy_meta:Fun.id file_hits))
    t.per_function;
  { per_file; per_function }

let merge_hit_tables (dst : 'a file_hits) (src : 'b file_hits) =
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

let merge c1 c2 =
  let c = copy c1 in
  Hstring.iter
    (fun file src ->
      match Hstring.find_opt c.per_file file with
      | None ->
          Hstring.replace c.per_file file (copy_file_hits ~copy_meta:Fun.id src)
      | Some dst -> merge_hit_tables dst src)
    c2.per_file;
  Hfun.iter
    (fun fid src ->
      match Hfun.find_opt c.per_function fid with
      | None ->
          Hfun.replace c.per_function fid (copy_file_hits ~copy_meta:Fun.id src)
      | Some dst ->
          merge_hit_tables dst src;
          Hfun.replace c.per_function fid
            {
              lines = dst.lines;
              branches = dst.branches;
              meta = merge_function_meta dst.meta src.meta;
            })
    c2.per_function;
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

let[@inline] hint_update ~default h key f =
  match Hint.find_opt h key with
  | None -> Hint.replace h key (f default)
  | Some prev ->
      let res = f prev in
      if res <> prev then Hint.replace h key res

let[@inline] hstr_update ~default h key f =
  match Hstring.find_opt h key with
  | None -> Hstring.replace h key (f default)
  | Some prev ->
      let res = f prev in
      if res <> prev then Hstring.replace h key res

(* Types used by users (not internally) *)

type function_info = {
  file : string;
  name : string;
  line : int option;
  end_line : int option;
}

type location = File of string | Function of function_info
type branch_side = Then | Else
type branch_span = { loc : location; line : int; branch_id : string }

type code_item =
  | Line of int
  | Conditional of { line : int; branch_id : string; side : branch_side option }

let get_or_create_file_hits_per_file (coverage : t) file =
  match Hstring.find_opt coverage.per_file file with
  | Some file_hits -> file_hits
  | None ->
      let file_hits = empty_file_hits () in
      Hstring.replace coverage.per_file file file_hits;
      file_hits

let get_or_create_file_hits_per_function (coverage : t)
    { file; name; line; end_line } =
  let id = { file; name } in
  let fn_meta = make_function_meta ?line ?end_line () in
  match Hfun.find_opt coverage.per_function id with
  | Some file_hits ->
      { file_hits with meta = merge_function_meta fn_meta file_hits.meta }
  | None ->
      let hits = empty_file_hits fn_meta in
      Hfun.replace coverage.per_function id hits;
      hits

let update_file_hits (file_hits : 'a file_hits) incr (item : code_item) =
  match item with
  | Line line -> hint_update ~default:0 file_hits.lines line (( + ) incr)
  | Conditional { line; branch_id; side } ->
      hstr_update ~default:{ line; then_hits = 0; else_hits = 0 }
        file_hits.branches branch_id (fun prev ->
          match side with
          | Some Then -> { prev with then_hits = prev.then_hits + incr }
          | Some Else -> { prev with else_hits = prev.else_hits + incr }
          | None -> prev)

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

  let register loc item =
    apply (fun coverage ->
        match loc with
        | File file ->
            let target = get_or_create_file_hits_per_file coverage file in
            update_file_hits target 0 item
        | Function info ->
            let target = get_or_create_file_hits_per_function coverage info in
            update_file_hits target 0 item)

  let register_function info =
    apply (fun coverage ->
        ignore @@ get_or_create_file_hits_per_function coverage info)

  let register_bulk (files : (location * code_item Iter.t) Iter.t) =
    apply (fun coverage ->
        files (fun (loc, items) ->
            match loc with
            | File file ->
                let hits = get_or_create_file_hits_per_file coverage file in
                items (update_file_hits hits 0)
            | Function info ->
                let hits = get_or_create_file_hits_per_function coverage info in
                items (update_file_hits hits 0)))

  let mark loc item =
    apply (fun coverage ->
        match loc with
        | File file ->
            let target = get_or_create_file_hits_per_file coverage file in
            update_file_hits target 1 item
        | Function info ->
            let target = get_or_create_file_hits_per_function coverage info in
            update_file_hits target 1 item)

  let mark_function info =
    apply (fun coverage ->
        let target = get_or_create_file_hits_per_function coverage info in
        let meta = { target.meta with hits = target.meta.hits + 1 } in
        let id = { file = info.file; name = info.name } in
        Hfun.replace coverage.per_function id { target with meta })

  let mark_branch side { loc; line; branch_id } =
    mark loc (Conditional { line; branch_id; side = Some side })

  let get_copy () : t =
    let copy_ref = ref (create ()) in
    apply (fun coverage -> copy_ref := copy coverage);
    !copy_ref
end
