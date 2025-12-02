(** A bidirectional map implementation. *)

(** This functor builds a data structure that maintains a one-to-one
    correspondence between keys of type [KeyL] (left) and [KeyR] (right). It
    allows efficient lookup in both directions by internally maintaining two
    synchronized maps. *)
module Make (KeyL : Ordered_type.S) (KeyR : Ordered_type.S) = struct
  module M = Map.MakePp (KeyL)
  module M_rev = Map.Make (KeyR)

  type t = KeyR.t M.t * KeyL.t M_rev.t

  (** The empty map. *)
  let empty = (M.empty, M_rev.empty)

  (** Test whether the map is empty. *)
  let is_empty (m, _) = M.is_empty m

  (** The number of bindings in the map. *)
  let cardinal (m, _) = M.cardinal m

  (** The list of all bindings from left to right keys. *)
  let bindings (m, _) = M.bindings m

  (** The list of all bindings from right to left keys. *)
  let bindings_rev (_, m_rev) = M_rev.bindings m_rev

  (** Add a binding between a left key and a right key to the map. *)
  let add k1 k2 (m, m_rev) = (M.add k1 k2 m, M_rev.add k2 k1 m_rev)

  (** Remove the binding associated with the given left key from the map. *)
  let remove_l k1 (m, m_rev) =
    match M.find_opt k1 m with
    | None -> (m, m_rev)
    | Some k2 -> (M.remove k1 m, M_rev.remove k2 m_rev)

  (** Remove the binding associated with the given right key from the map. *)
  let remove_r k2 (m, m_rev) =
    match M_rev.find_opt k2 m_rev with
    | None -> (m, m_rev)
    | Some k1 -> (M.remove k1 m, M_rev.remove k2 m_rev)

  (** Find the right key associated with the given left key. *)
  let find_l k1 (m, _) = M.find_opt k1 m

  (** Find the left key associated with the given right key. *)
  let find_r k2 (_, m_rev) = M_rev.find_opt k2 m_rev

  (** Pretty-printer for the map. *)
  let pp ft (m, _) = M.pp KeyR.pp ft m
end
