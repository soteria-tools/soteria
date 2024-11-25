type 'a t = Freed | Alive of 'a

let pp ?(alive_prefix = "") pp_alive ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> Fmt.pf ft "%s%a" alive_prefix pp_alive a

module Make (Symex : Symex.S) = struct
  open Symex.Syntax

  type nonrec 'a t = 'a t = Freed | Alive of 'a

  let pp = pp

  let unwrap_alive = function
    | None -> Symex.Result.ok None
    | Some (Alive s) -> Symex.Result.ok (Some s)
    | Some Freed -> Symex.Result.error `UseAfterFree

  let free ~assert_exclusively_owned freeable =
    let** s = unwrap_alive freeable in
    let++ () = assert_exclusively_owned s in
    ((), Some Freed)

  (* [f] must be a "symex state monad" *)
  let wrap (f : 'a option -> ('b * 'a option, 'err) Symex.Result.t)
      (st : 'a t option) =
    match st with
    | None ->
        let++ res, st' = f None in
        (res, Option.map (fun x -> Alive x) st')
    | Some (Alive st) ->
        let++ res, st' = f (Some st) in
        (res, Option.map (fun x -> Alive x) st')
    | Some Freed -> Symex.Result.error `UseAfterFree

  (* [f] does not modify the state *)
  let wrap_read_only (f : 'a option -> ('b, 'err) Symex.Result.t)
      (st : 'a t option) =
    match st with
    | Some (Alive st) -> f (Some st)
    | None -> f None
    | Some Freed -> Symex.Result.error `UseAfterFree
end
