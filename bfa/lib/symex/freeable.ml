type 'a t = Freed | Alive of 'a

let pp ?(alive_prefix = "") pp_alive ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> Fmt.pf ft "%s%a" alive_prefix pp_alive a

module Make (Symex : Symex.S) = struct
  open Symex.Syntax

  type nonrec 'a t = 'a t = Freed | Alive of 'a
  type 'a cp = 'a t

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

  (* In the context of UX, using a non-matching spec will simply vanish *)
  let consume
      (cons :
        'inner_cp ->
        'inner_st option ->
        ('a * 'inner_st option, 'err) Symex.Result.t) (cp : 'inner_cp cp)
      (st : 'inner_st t option) =
    match cp with
    | Freed -> (
        match st with
        | None -> Symex.Result.error `MissingResource
        | Some Freed -> Symex.Result.ok ([], None)
        | Some (Alive _) -> Symex.vanish ())
    | Alive cp -> (
        match st with
        | None ->
            let++ res, st' = cons cp None in
            (res, Option.map (fun x -> Alive x) st')
        | Some Freed -> Symex.vanish ()
        | Some (Alive st) ->
            let++ res, st' = cons cp (Some st) in
            (res, Option.map (fun x -> Alive x) st'))

  let produce (prod : 'inner_cp -> 'inner_st option -> 'inner_st option Symex.t)
      (cp : 'inner_cp cp) (st : 'inner_st t option) : 'inner_st t option Symex.t
      =
    match cp with
    | Freed -> (
        match st with
        | None -> Symex.return (Some Freed)
        | Some _ -> Symex.vanish ())
    | Alive cp -> (
        match st with
        | None ->
            let+ st' = prod cp None in
            Option.map (fun s -> Alive s) st'
        | Some (Alive st) ->
            let+ st' = prod cp (Some st) in
            Option.map (fun s -> Alive s) st'
        | Some Freed -> Symex.vanish ())

  (* [f] does not modify the state *)
  let wrap_read_only (f : 'a option -> ('b, 'err) Symex.Result.t)
      (st : 'a t option) =
    match st with
    | Some (Alive st) -> f (Some st)
    | None -> f None
    | Some Freed -> Symex.Result.error `UseAfterFree
end
