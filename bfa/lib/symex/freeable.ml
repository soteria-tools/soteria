type 'a t = Freed | Alive of 'a

let pp ?(alive_prefix = "") pp_alive ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> Fmt.pf ft "%s%a" alive_prefix pp_alive a

module Make (Symex : Symex.S) = struct
  open Symex.Syntax

  type nonrec 'a t = 'a t = Freed | Alive of 'a

  let pp = pp

  let free ~is_exclusively_owned freeable =
    match freeable with
    | Freed -> Symex.Result.error `DoubleFree
    | Alive s ->
        let* owned = is_exclusively_owned s in
        if%sat owned then Symex.Result.ok ((), Freed)
        else Symex.Result.error `MissingOwnership

  (* [f] must be a "symex state monad" *)
  let wrap (f : 'a -> ('b * 'a, 'err) Symex.Result.t) (st : 'a t) =
    match st with
    | Freed -> Symex.Result.error `UseAfterFree
    | Alive st ->
        let++ res, st' = f st in
        (res, Alive st')

  (* [f] does not modify the state *)
  let wrap_read_only (f : 'a -> ('b, 'err) Symex.Result.t) (st : 'a t) =
    match st with Freed -> Symex.Result.error `UseAfterFree | Alive st -> f st
end
