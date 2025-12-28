include Stdlib.String

type t = string

let pp ft s = Format.pp_print_string ft s

let index_of ~sub_str s =
  let re = Str.regexp_string sub_str in
  try
    let pos = Str.search_forward re s 0 in
    Some pos
  with Not_found -> None

let contains ~sub_str s = Option.is_some (index_of ~sub_str s)
