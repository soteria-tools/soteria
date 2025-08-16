open Csymex
open With_origin

(* TODO: This whole file could straightforwardly be:
  module Block = struct
    type t = Tree_block.t Freeable.t With_origin.t [@@derive symex_state]

    let is_freed ...
    let alloc ...
  end
*)

type t = Ctree_block.t Freeable.t With_origin.t
[@@deriving show { with_path = false }]

type serialized =
  Ctree_block.serialized Freeable.serialized With_origin.serialized
[@@deriving show { with_path = false }]

let serialize = With_origin.serialize (Freeable.serialize Ctree_block.serialize)

let subst_serialized =
  With_origin.subst_serialized
    (Freeable.subst_serialized Ctree_block.subst_serialized)

let iter_vars_serialized t f =
  let k =
    With_origin.iter_vars_serialized
      (Freeable.iter_vars_serialized Ctree_block.iter_vars_serialized)
  in
  k t f

let produce serialized t =
  (With_origin.produce (Freeable.produce Ctree_block.produce)) serialized t

let consume serialized t =
  (With_origin.consume (Freeable.consume Ctree_block.consume)) serialized t

let is_freed t = match t.node with Freeable.Freed -> true | _ -> false

let alloc ?loc ~zeroed size =
  With_origin.
    { node = Freeable.Alive (Ctree_block.alloc ~zeroed size); info = loc }

let free h =
  let k =
    With_origin.wrap
      (Freeable.free
         ~assert_exclusively_owned:Ctree_block.assert_exclusively_owned)
  in
  k h
