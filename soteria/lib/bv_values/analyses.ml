open Soteria_symex

(* let log = Soteria.Logging.Logs.L.warn *)
let log _ = ()

module type S = sig
  include Soteria_std.Reversible.Mutable

  val simplify : t -> Svalue.t -> Svalue.t
  val add_constraint : t -> Svalue.t -> Svalue.t * Var.Set.t
  val encode : ?vars:Var.Hashset.t -> t -> Typed.sbool Typed.t Iter.t
end

module Merge (A1 : S) (A2 : S) : S = struct
  type t = A1.t * A2.t

  let init () = (A1.init (), A2.init ())

  let backtrack_n (a1, a2) n =
    A1.backtrack_n a1 n;
    A2.backtrack_n a2 n

  let save (a1, a2) =
    A1.save a1;
    A2.save a2

  let reset (a1, a2) =
    A1.reset a1;
    A2.reset a2

  let simplify (a1, a2) v = v |> A1.simplify a1 |> A2.simplify a2

  let add_constraint (a1, a2) v =
    let v', vars1 = A1.add_constraint a1 v in
    let v'', vars2 = A2.add_constraint a2 v' in
    (v'', Var.Set.union vars1 vars2)

  let encode ?vars (a1, a2) : Typed.sbool Typed.t Iter.t =
    Iter.append (A1.encode ?vars a1) (A2.encode ?vars a2)
end

module None : S = struct
  type t = unit

  let init () = ()
  let backtrack_n () _ = ()
  let save () = ()
  let reset () = ()
  let simplify () v = v
  let add_constraint () v = (v, Var.Set.empty)
  let encode ?vars:_ () = Iter.empty
end
