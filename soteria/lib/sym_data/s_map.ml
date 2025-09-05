module Key = struct
  module type S = sig
    include Stdlib.Map.OrderedType
    include S_elt.S with type t := t
    include S_eq.S with type t := t and module Symex := Symex
    include Soteria_std.Printable.S with type t := t
  end

  module Of_concrete
      (Symex : Symex.S)
      (K : sig
        include Soteria_std.Ordered_type.S

        val equal : t -> t -> bool
      end) =
  struct
    include K
    include S_elt.Of_concrete (Symex) (K)
    include S_eq.Of_concrete (Symex) (K)
  end
end

(* module Make (Symex : Symex.S) (Key : Key.S with module Symex = Symex) = struct
  open Symex.Syntax
  module Raw_map = Stdlib.Map.Make (Key)
end *)
