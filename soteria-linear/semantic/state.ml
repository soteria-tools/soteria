open Aux

type err = [ `UseAfterFree | `Interp of string ]
[@@deriving show { with_path = false }]

module Excl_val = Soteria.Sym_states.Excl.Make (Symex) (S_val)
module Freeable_excl = Soteria.Sym_states.Freeable.Make (Symex) (Excl_val)
include Soteria.Sym_states.Pmap.Make (Symex) (S_int) (Freeable_excl)

module SM =
  Soteria.Sym_states.State_monad.Make
    (Symex)
    (struct
      type nonrec t = t option
    end)

let pp ft t = pp ft t
let empty : t option = None
let load addr = wrap addr (Freeable_excl.wrap (Excl_val.load ()))
let store addr value = wrap addr (Freeable_excl.wrap (Excl_val.store value))

let alloc () : (S_int.t, 'err, syn list) SM.Result.t =
  let v_false = (S_val.v_false :> S_val.t) in
  alloc ~new_codom:(Alive v_false)

let free addr = wrap addr (Freeable_excl.free ())
