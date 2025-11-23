(** A bidirectional map implementation.

    This functor builds a data structure that maintains a one-to-one correspondence
    between keys of type [KeyL] (left) and [KeyR] (right). It allows efficient lookup
    in both directions by internally maintaining two synchronized maps. *)

module Make (KeyL : Ordered_type.S) (KeyR : Ordered_type.S) = struct
  module M = Map.MakePp (KeyL)
  module M_rev = Map.Make (KeyR)

  type t = KeyR.t M.t * KeyL.t M_rev.t

  let empty = (M.empty, M_rev.empty)
  let is_empty (m, _) = M.is_empty m
  let cardinal (m, _) = M.cardinal m
  let bindings (m, _) = M.bindings m
  let bindings_rev (_, m_rev) = M_rev.bindings m_rev
  let add k1 k2 (m, m_rev) = (M.add k1 k2 m, M_rev.add k2 k1 m_rev)

  let remove_l k1 (m, m_rev) =
    match M.find_opt k1 m with
    | None -> (m, m_rev)
    | Some k2 -> (M.remove k1 m, M_rev.remove k2 m_rev)

  let remove_r k2 (m, m_rev) =
    match M_rev.find_opt k2 m_rev with
    | None -> (m, m_rev)
    | Some k1 -> (M.remove k1 m, M_rev.remove k2 m_rev)

  let find_l k1 (m, _) = M.find_opt k1 m
  let find_r k2 (_, m_rev) = M_rev.find_opt k2 m_rev
  let pp ft (m, _) = M.pp KeyR.pp ft m
end
