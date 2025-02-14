type element = { loc : Charon.Meta.span; msg : string }

let make_element ~loc ?(msg = "") () : element = { loc; msg }

let pp_element ft { loc; msg } =
  (Fmt.Dump.pair Charon.Meta.pp_span Fmt.string) ft (loc, msg)

type t = element list

let empty : t = []
let singleton ~loc ?(msg = "") () : t = [ make_element ~loc ~msg () ]
let pp : t Fmt.t = Fmt.Dump.list pp_element
