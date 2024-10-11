open Cerb_frontend

include Stdlib.Map.Make (struct
  type t = Symbol.sym

  let compare = Symbol.compare_sym
end)

let pp_binding ft (v, ty) =
  Fmt.pf ft "%a : %a" (Fmt.Dump.option Svalue.pp) v Fmt_ail.pp_ty ty

let pp = Fmt.Dump.iter_bindings iter Fmt.nop Fmt_ail.pp_sym pp_binding
let find_type sym store = Option.map snd (find_opt sym store)
let find_value sym store = Option.map fst (find_opt sym store) |> Option.join
