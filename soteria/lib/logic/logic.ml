(** Separation logic for specifications and summaries *)

module Make (Symex : Symex.Base) = struct
  module Asrt = Asrt.M (Symex)
  module Spec = Spec.M (Symex)
end
