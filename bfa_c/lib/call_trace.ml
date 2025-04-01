type element = { loc : Cerb_location.t; msg : string }

let make_element ~loc ?(msg = "") () : element = { loc; msg }

let pp_element ft { loc; msg } =
  (Fmt.Dump.pair Fmt_ail.pp_loc Fmt.string) ft (loc, msg)

type t = element list

let empty : t = []
let singleton ~loc ?(msg = "") () : t = [ make_element ~loc ~msg () ]
let pp : Format.formatter -> t -> unit = Fmt.Dump.list pp_element
