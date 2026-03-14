(** Separation logic for specifications and summaries *)

module Make (Symex : Symex.S) = struct
  module Asrt = Asrt.M (Symex)
  module Spec = Spec.M (Symex)
end
