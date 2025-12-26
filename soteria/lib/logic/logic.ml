(** Entry point of the library for logic manipulation (such as function
    summaries). *)

(** A producer monad, that accumulates a substitution as it produces stuff,
    making operation transparent. *)
module Producer (Symex : Symex.Base) = struct
  open Symex.Value.Syn
  include Soteria_std.Monad.StateM (Subst)
end
