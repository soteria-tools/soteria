(* TODO: Build this with the Sum transformer and `Excl(Freed)` *)

type 'a t = Freed | Alive of 'a

let pp ?(alive_prefix = "") pp_alive ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> Fmt.pf ft "%s%a" alive_prefix pp_alive a

module Make (Symex : Symex.S) = struct
  open Symex.Syntax

  type nonrec 'a t = 'a t = Freed | Alive of 'a
  type 'a serialized = 'a t

  let lift_fix fix = Alive fix
  let lift_fix_s r = Symex.Result.map_missing r lift_fix

  let serialize serialize_inner = function
    | Freed -> Freed
    | Alive a -> Alive (serialize_inner a)

  let subst_serialized subst_inner subst_var = function
    | Freed -> Freed
    | Alive a -> Alive (subst_inner subst_var a)

  let iter_vars_serialized iter_inner serialized f =
    match serialized with Freed -> () | Alive a -> iter_inner a f

  let pp = pp
  let pp_serialized = pp

  let unwrap_alive = function
    | None -> Symex.Result.ok None
    | Some (Alive s) -> Symex.Result.ok (Some s)
    | Some Freed -> Symex.Result.error `UseAfterFree

  let free ~assert_exclusively_owned freeable =
    let** s = unwrap_alive freeable in
    let++ () = assert_exclusively_owned s |> lift_fix_s in
    ((), Some Freed)

  (* [f] must be a "symex state monad" *)
  let wrap (f : 'a option -> ('b * 'a option, 'err, 'fix) Symex.Result.t)
      (st : 'a t option) :
      ('b * 'a t option, 'err, 'fix serialized) Symex.Result.t =
    match st with
    | None ->
        let++ res, st' = f None |> lift_fix_s in
        (res, Option.map (fun x -> Alive x) st')
    | Some (Alive st) ->
        let++ res, st' = f (Some st) |> lift_fix_s in
        (res, Option.map (fun x -> Alive x) st')
    | Some Freed -> Symex.Result.error `UseAfterFree

  (* In the context of UX, using a non-matching spec will simply vanish *)
  let consume
      (cons :
        'inner_ser ->
        'inner_st option ->
        ('inner_st option, 'err, 'inner_ser) Symex.Result.t)
      (serialized : 'inner_ser serialized) (st : 'inner_st t option) :
      ('inner_st t option, 'err, 'inner_ser serialized) Symex.Result.t =
    match serialized with
    | Freed -> (
        match st with
        | None -> Symex.Result.miss [ Freed ]
        | Some Freed -> Symex.Result.ok None
        | Some (Alive _) -> Symex.vanish ())
    | Alive ser -> (
        match st with
        | None ->
            let++ st' = cons ser None |> lift_fix_s in
            Option.map (fun x -> Alive x) st'
        | Some Freed -> Symex.vanish ()
        | Some (Alive st) ->
            let++ st' = cons ser (Some st) |> lift_fix_s in
            Option.map (fun x -> Alive x) st')

  let produce
      (prod : 'inner_ser -> 'inner_st option -> 'inner_st option Symex.t)
      (serialize : 'inner_ser serialized) (st : 'inner_st t option) :
      'inner_st t option Symex.t =
    match serialize with
    | Freed -> (
        match st with
        | None -> Symex.return (Some Freed)
        | Some _ -> Symex.vanish ())
    | Alive ser -> (
        match st with
        | None ->
            let+ st' = prod ser None in
            Option.map (fun s -> Alive s) st'
        | Some (Alive st) ->
            let+ st' = prod ser (Some st) in
            Option.map (fun s -> Alive s) st'
        | Some Freed -> Symex.vanish ())
end
