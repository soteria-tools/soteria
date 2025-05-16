include Stdlib.Map.Make (Ail_helpers.Symbol_std)

type nonrec t = (Typed.T.sptr Typed.t option * Cerb_frontend.Ctype.ctype) t

let pp_binding ft (v, ty) =
  Fmt.pf ft "%a : %a" (Fmt.Dump.option Typed.ppa) v Fmt_ail.pp_ty ty

let pp : t Fmt.t = Fmt.Dump.iter_bindings iter Fmt.nop Fmt_ail.pp_sym pp_binding
let find_type sym store = Option.map snd (find_opt sym store)
let find_value sym store = Option.bind (find_opt sym store) fst
