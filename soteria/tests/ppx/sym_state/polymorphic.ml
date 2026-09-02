open Prelude

(* A state component that is polymorphic in the enclosing state: it stores a
   callback over that state, and as such cannot be serialised. *)
module Callback = struct
  type 'st t = 'st -> 'st Symex.t
  type _ syn = |

  let pp ft (_ : _ t) = Fmt.string ft "<callback>"
  let show x = (Fmt.to_to_string pp) x
  let pp_syn _ (syn : _ syn) = match syn with _ -> .
  let show_syn (syn : _ syn) = match syn with _ -> .
  let to_syn (_ : 'st t) : 'st syn list = []
  let ins_outs (syn : _ syn) = match syn with _ -> .
  let produce (syn : _ syn) _ = match syn with _ -> .
  let consume (syn : _ syn) _ = match syn with _ -> .
end

type t = { heap : Heap.t option; callback : t option Callback.t option }
[@@deriving sym_state { symex = Symex }]
