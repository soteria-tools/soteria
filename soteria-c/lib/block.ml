open Csymex
module Freeable_ctree_block = Freeable (Ctree_block)
include With_origin (Freeable_ctree_block)

let pp_pretty ft t =
  pp' ~inner:(Freeable_ctree_block.pp' ~inner:Ctree_block.pp_pretty) ft t

let is_freed (t : t) = match t.node with Freed -> true | _ -> false

let serialized_is_freed (s : serialized) =
  match s.node with Freed -> true | _ -> false

let alloc ?loc ~zeroed size =
  {
    node = Soteria.Sym_states.Freeable.Alive (Ctree_block.alloc ~zeroed size);
    info = loc;
  }

let free () = wrap (Freeable_ctree_block.free ())
