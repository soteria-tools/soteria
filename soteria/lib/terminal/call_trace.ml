type 'a element = { loc : 'a; msg : string }

let mk_element ~loc ?(msg = "") () : 'a element = { loc; msg }

let pp_element pp_loc ft { loc; msg } =
  if msg = "" then Fmt.pf ft "• %a" pp_loc loc
  else Fmt.pf ft "• %s: %a" msg pp_loc loc

type 'a t = 'a element list

let empty : 'a t = []
let singleton ~loc ?(msg = "") () : 'a t = [ mk_element ~loc ~msg () ]
let pp pp_loc = Fmt.Dump.list (pp_element pp_loc)
