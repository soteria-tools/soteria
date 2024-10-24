include Svalue

type sint = [ `NonZero | `MaybeZero ]
type nonzero = [ `NonZero ]
type sbool = [ `Bool ]
type sptr = [ `Ptr ]
type sloc = [ `Loc ]
type svoid = [ `Void ]
type 'a sseq = [ `List of 'a ]
type 'a sopt = [ `Opt of 'a ]

type any =
  [ `Bool
  | `Ptr
  | `Loc
  | `Void
  | `List of any
  | `Opt of any
  | `NonZero
  | `MaybeZero ]

type nonrec +'a t = t
type nonrec +'a ty = ty

let fresh = Z3solver.fresh
let cast x = x
let untyped x = x
let type_ x = x

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z")
  else Svalue.int_z z

let nonzero x = nonzero_z (Z.of_int x)

let check_nonzero t =
  let open Csymex.Syntax in
  if%sat Infix.(t #== zero) then Csymex.Result.error `NonZeroIsZero
  else Csymex.Result.ok t

module Syntax = struct
  module Symex_syntax = Csymex.SYMEX.Syntax.Symex_syntax
end
