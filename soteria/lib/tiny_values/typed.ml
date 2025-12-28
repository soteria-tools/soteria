open Hc
include Svalue

module T = struct
  type sint = [ `NonZero | `Zero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type any = [ `Bool | `NonZero | `Zero ]

  let pp_sint _ _ = ()
  let pp_nonzero _ _ = ()
  let pp_sbool _ _ = ()
  let pp_any _ _ = ()
end

type nonrec +'a t = t
type nonrec +'a ty = ty
type sbool = T.sbool

let[@inline] get_ty x = x.node.ty
let[@inline] untype_type x = x
let ppa = pp
let pp _ = pp
let ppa_ty = pp_ty
let pp_ty _ = pp_ty
let[@inline] cast x = x
let[@inline] untyped x = x
let[@inline] untyped_list l = l
let[@inline] type_ x = x
let type_checked x ty = if equal_ty x.node.ty ty then Some x else None
let cast_checked = type_checked

let cast_checked2 x y =
  if equal_ty x.node.ty y.node.ty then Some (x, y, x.node.ty) else None

module Syn = struct
  include Syn

  type 'a t = Svalue.t
end
