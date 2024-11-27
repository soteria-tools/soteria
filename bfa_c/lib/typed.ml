open Hashcons
include Svalue

module T = struct
  type sint = [ `NonZero | `MaybeZero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type 'a sopt = [ `Opt of 'a ]
  type cval = [ sint | sptr ]

  type any =
    [ `Bool | `Ptr | `Loc | `List of any | `Opt of any | `NonZero | `MaybeZero ]

  let pp_sint _ _ = ()
  let pp_nonzero _ _ = ()
  let pp_sbool _ _ = ()
  let pp_sptr _ _ = ()
  let pp_sloc _ _ = ()
  let pp_sseq _ _ _ = ()
  let pp_sopt _ _ _ = ()
  let pp_any _ _ = ()
  let pp_cval _ _ = ()
end

type nonrec +'a t = t
type nonrec +'a ty = ty

let get_ty x = x.node.ty
let ppa = pp
let pp _ = pp
let ppa_ty = pp_ty
let pp_ty _ = pp_ty
let cast x = x
let untyped x = x
let untyped_list l = l
let type_ x = x
let type_checked x ty = if equal_ty x.node.ty ty then Some x else None
let cast_checked = type_checked

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z")
  else Svalue.int_z z

let nonzero x = nonzero_z (Z.of_int x)

let check_nonzero t =
  let open Csymex.Syntax in
  if%sat Infix.(t #== zero) then Csymex.Result.error `NonZeroIsZero
  else Csymex.Result.ok t

let nondet = Csymex.nondet

module Syntax = struct
  module Symex_syntax = Csymex.SYMEX.Syntax.Symex_syntax

  module Sym_int_syntax = struct
    let mk_int = int
    let zero = zero
    let one = one
  end
end

module Result = struct
  type pc = T.sbool t

  include Csymex.Result
end
