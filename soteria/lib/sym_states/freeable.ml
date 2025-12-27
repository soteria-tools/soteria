(* TODO: Build this with the Sum transformer and `Excl(Freed)` *)

type 'a t = Freed | Alive of 'a

let pp ?(alive_prefix = "") pp_alive ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> Fmt.pf ft "%s%a" alive_prefix pp_alive a

module Make (Symex : Symex.Base) = struct
  open Symex.Syntax

  type nonrec 'a t = 'a t = Freed | Alive of 'a
  type 'a syn = 'a t

  let lift_fix fix = Alive fix
  let lift_fix_s r = Symex.Result.map_missing r lift_fix

  let to_syn to_syn_inner = function
    | Freed -> Freed
    | Alive a -> Alive (to_syn_inner a)

  let subst subst_inner subst_vals = function
    | Freed -> Freed
    | Alive a -> Alive (subst_inner subst_vals a)

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
      (st : 'a t option) : ('b * 'a t option, 'err, 'fix syn) Symex.Result.t =
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
        'inner_syn ->
        'inner_st option ->
        ('inner_st option, 'inner_syn) Symex.Consumer.t) (syn : 'inner_syn syn)
      (st : 'inner_st t option) :
      ('inner_st t option, 'inner_syn syn) Symex.Consumer.t =
    let open Symex.Consumer in
    let open Syntax in
    let lift_fix_s s = map_missing s lift_fix in
    match syn with
    | Freed -> (
        match st with
        | None -> miss [ Freed ]
        | Some Freed -> ok None
        | Some (Alive _) -> lfail (Symex.Value.bool false))
    | Alive syn -> (
        match st with
        | None ->
            let+ st' = cons syn None |> lift_fix_s in
            Option.map (fun x -> Alive x) st'
        | Some Freed -> lfail (Symex.Value.bool false)
        | Some (Alive st) ->
            let+ st' = cons syn (Some st) |> lift_fix_s in
            Option.map (fun x -> Alive x) st')

  let produce
      (prod :
        'inner_syn -> 'inner_st option -> 'inner_st option Symex.Producer.t)
      (syn : 'inner_syn syn) (st : 'inner_st t option) :
      'inner_st t option Symex.Producer.t =
    let open Symex.Producer in
    let open Syntax in
    match syn with
    | Freed -> (
        match st with None -> return (Some Freed) | Some _ -> vanish ())
    | Alive syn -> (
        match st with
        | None ->
            let+ st' = prod syn None in
            Option.map (fun s -> Alive s) st'
        | Some (Alive st) ->
            let+ st' = prod syn (Some st) in
            Option.map (fun s -> Alive s) st'
        | Some Freed -> vanish ())
end
