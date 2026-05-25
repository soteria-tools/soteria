(** Reversible mutable counter used internally by the bit-vector solver to
    allocate fresh save-points. The {{!Soteria_std.Reversible.Mutable}
    [Reversible.Mutable]} interface allows backing the counter out when
    symbolic execution backtracks across branches. *)

open Soteria_std
include Reversible.Mutable with type t = int ref
