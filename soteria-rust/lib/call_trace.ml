module Meta = Charon.Meta

type element = { loc : Meta.span; msg : string }

let make_element ~loc ?(msg = "") () : element = { loc; msg }

let pp_span ft ({ span = { file; beg_loc; end_loc }; _ } : Meta.span) =
  let clean_filename name =
    let parts = String.split_on_char '/' name in
    if List.compare_length_with parts 3 <= 0 then name
    else
      let last_3 = List.rev (List.take 3 (List.rev parts)) in
      "../" ^ String.concat "/" last_3
  in
  let pp_filename ft ({ name; _ } : Meta.file) =
    match name with
    | Local name -> Fmt.string ft (clean_filename name)
    | Virtual name -> Fmt.pf ft "%s (virtual)" (clean_filename name)
  in
  if beg_loc.line = end_loc.line then
    Fmt.pf ft "%a:%d:%d-%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.col
  else
    Fmt.pf ft "%a:%d:%d-%d:%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.line end_loc.col

let pp_element ft { loc; msg } = Fmt.pf ft "• %s: %a" msg pp_span loc

type t = element list

let empty : t = []
let singleton ~loc ?(msg = "") () : t = [ make_element ~loc ~msg () ]
let pp : t Fmt.t = Fmt.(list ~sep:(Fmt.any "@\n") pp_element)
