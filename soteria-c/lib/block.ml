open Csymex
module Freeable_ctree_block = Freeable (Ctree_block)
include With_origin (Freeable_ctree_block)

let pp_pretty ft t =
  pp' ~inner:(Freeable_ctree_block.pp' ~inner:Ctree_block.pp_pretty) ft t

let is_freed (t : (Freeable_ctree_block.t, 'b) with_info) =
  match t.node with Freed () -> true | _ -> false

let is_freed_syn (t : (Freeable_ctree_block.syn, 'b) with_info) =
  match t.node with Ser_Freed () -> true | _ -> false

let alloc ?loc ~zeroed size =
  {
    node = Freeable_ctree_block.Alive (Ctree_block.alloc ~zeroed size);
    info = loc;
  }

let free () = wrap (Freeable_ctree_block.free ())
