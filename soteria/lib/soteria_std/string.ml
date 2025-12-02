include Stdlib.String

type t = string

let pp ft s = Format.pp_print_string ft s
