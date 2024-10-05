type 'a t = Freed | Substate of 'a [@@deriving eq, show]

module type P = sig
  type 'a symex
  type value
  type t

  val is_exclusively_owned : t -> value symex
end

module Make
    (Symex : Symex.S)
    (P : P with type 'a symex = 'a Symex.t and type value = Symex.Value.t) =
struct
  open Symex.Syntax

  let free freeable =
    match freeable with
    | Freed -> Symex.Result.error `DoubleFree
    | Substate s ->
        let* owned = P.is_exclusively_owned s in
        if%sat owned then Symex.Result.ok Freed
        else Symex.Result.error `MissingOwnership

  let wrap f x =
    match x with
    | Freed -> Symex.Result.error `UseAfterFree
    | Substate s ->
        let++ res = f s in
        Substate res
end
