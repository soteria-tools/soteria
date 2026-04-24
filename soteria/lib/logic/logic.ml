(** Separation logic assertions *)

module Make (Symex : Symex.Base) = struct
  module Asrt = Asrt.M (Symex)
  module Util = Util.M (Symex)
end
