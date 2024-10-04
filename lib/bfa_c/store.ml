open Cerb_frontend

include Stdlib.Map.Make (struct
  type t = Symbol.sym

  let compare = Symbol.compare_sym
end)

let pp = Fmt.Dump.iter_bindings iter Fmt.nop Fmt_ail.pp_sym Svalue.pp
