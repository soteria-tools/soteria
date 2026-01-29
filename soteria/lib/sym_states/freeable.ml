(* TODO: Build this with the Sum transformer and `Excl(Freed)` *)

open Symex

type 'a freeable = Freed | Alive of 'a

let pp_freeable pp_a ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> pp_a ft a

module Make
    (Symex : Symex.Base)
    (I : sig
      include Base.M(Symex).S

      val assert_exclusively_owned :
        t option -> (unit, 'err, serialized list) Symex.Result.t
    end) =
struct
  type t = I.t freeable [@@deriving show { with_path = false }]

  type serialized = I.serialized freeable
  [@@deriving show { with_path = false }]

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  let lift_fix fix = Alive fix
  let lift_fix_r r = Compo_res.map_missing r (List.map lift_fix)

  let serialize = function
    | Freed -> [ Freed ]
    | Alive a -> List.map (fun x -> Alive x) (I.serialize a)

  let subst_serialized subst_var = function
    | Freed -> Freed
    | Alive a -> Alive (I.subst_serialized subst_var a)

  let iter_vars_serialized serialized f =
    match serialized with Freed -> () | Alive a -> I.iter_vars_serialized a f

  open SM
  open SM.Syntax

  let unwrap_alive () =
    let* st = SM.get_state () in
    match st with
    | None -> Result.ok None
    | Some (Alive s) -> Result.ok (Some s)
    | Some Freed -> Result.error `UseAfterFree

  (* [f] must be a "symex state monad" *)
  let wrap (f : ('a, 'err, I.serialized list) I.SM.Result.t) :
      ('b, 'err, serialized list) SM.Result.t =
    let** inner_state = unwrap_alive () in
    let*^ res, inner_state' = f inner_state in
    let* () = SM.set_state (Option.map (fun x -> Alive x) inner_state') in
    return (lift_fix_r res)

  let free () : (unit, 'err, serialized list) SM.Result.t =
    let** () =
      wrap (fun st ->
          let open Symex.Syntax in
          let+ res = I.assert_exclusively_owned st in
          (res, st))
    in
    SM.Result.set_state (Some Freed)

  (* In the context of UX, using a non-matching spec will simply vanish
  let consume
      (cons :
        'inner_ser ->
        'inner_st option ->
        ('inner_st option, [> Symex.lfail ], 'inner_ser) Symex.Result.t)
      (serialized : 'inner_ser serialized) (st : 'inner_st t option) :
      ( 'inner_st t option,
        [> Symex.lfail ],
        'inner_ser serialized )
      Symex.Result.t =
    match serialized with
    | Freed -> (
        match st with
        | None -> Symex.Result.miss [ Freed ]
        | Some Freed -> Symex.Result.ok None
        | Some (Alive _) -> Symex.consume_false ())
    | Alive ser -> (
        match st with
        | None ->
            let++ st' = cons ser None |> lift_fix_s in
            Option.map (fun x -> Alive x) st'
        | Some Freed -> Symex.vanish ()
        | Some (Alive st) ->
            let++ st' = cons ser (Some st) |> lift_fix_Us in
            Option.map (fun x -> Alive x) st') *)

  let produce (serialize : serialized) : unit SM.t =
    let* st = SM.get_state () in
    match serialize with
    | Freed -> (
        match st with
        | None -> SM.set_state (Some Freed)
        | Some _ -> SM.vanish ())
    | Alive ser ->
        let* ist =
          match st with
          | None -> SM.return None
          | Some (Alive s) -> SM.return (Some s)
          | Some Freed -> SM.vanish ()
        in
        let*^ (), ist' = I.produce ser ist in
        SM.set_state (Option.map (fun s -> Alive s) ist')
end
