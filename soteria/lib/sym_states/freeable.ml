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
        t option -> (unit, 'err, syn list) Symex.Result.t
    end) =
struct
  type t = I.t freeable [@@deriving show { with_path = false }]

  let pp' ?(inner = I.pp) = pp_freeable inner
  let pp ft t = pp' ft t

  type syn = I.syn freeable [@@deriving show { with_path = false }]

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  let lift_fix fix = Alive fix
  let lift_fix_r r = Compo_res.map_missing r (List.map lift_fix)

  let to_syn = function
    | Freed -> [ Freed ]
    | Alive a -> List.map (fun x -> Alive x) (I.to_syn a)

  type f = t

  open SM
  open SM.Syntax

  let unwrap_alive () =
    let* st = SM.get_state () in
    match st with
    | None -> Result.ok None
    | Some (Alive s) -> Result.ok (Some s)
    | Some Freed -> Result.error `UseAfterFree

  (* [f] must be a "symex state monad" *)
  let wrap (f : ('a, 'err, I.syn list) I.SM.Result.t) :
      ('b, 'err, syn list) SM.Result.t =
    let** inner_state = unwrap_alive () in
    let*^ res, inner_state' = f inner_state in
    let* () = SM.set_state (Option.map (fun x -> Alive x) inner_state') in
    return (lift_fix_r res)

  let free () : (unit, 'err, syn list) SM.Result.t =
    let** () =
      wrap (fun st ->
          let open Symex.Syntax in
          let+ res = I.assert_exclusively_owned st in
          (res, st))
    in
    SM.Result.set_state (Some Freed)

  (* In the context of UX, using a non-matching spec will simply vanish *)
  (* let consume
   *    (cons :
   *      'inner_ser ->
   *      'inner_st option ->
   *      ('inner_st option, [> Symex.lfail ], 'inner_ser) Symex.Result.t)
   *    (serialized : 'inner_ser serialized) (st : 'inner_st t option) :
   *    ( 'inner_st t option,
   *      [> Symex.lfail ],
   *      'inner_ser serialized )
   *    Symex.Result.t =
   *  match serialized with
   *  | Freed -> (
   *      match st with
   *      | None -> Symex.Result.miss [ Freed ]
   *      | Some Freed -> Symex.Result.ok None
   *      | Some (Alive _) -> Symex.consume_false ())
   *  | Alive ser -> (
   *      match st with
   *      | None ->
   *          let++ st' = cons ser None |> lift_fix_s in
   *          Option.map (fun x -> Alive x) st'
   *      | Some Freed -> Symex.vanish ()
   *      | Some (Alive st) ->
   *          let++ st' = cons ser (Some st) |> lift_fix_Us in
   *          Option.map (fun x -> Alive x) st') *)

  let produce (syn : syn) st : f option Symex.Producer.t =
    let open Symex.Producer in
    let open Symex.Producer.Syntax in
    match syn with
    | Freed -> (
        match st with None -> return (Some Freed) | Some _ -> vanish ())
    | Alive ser ->
        let* ist =
          match st with
          | None -> return None
          | Some (Alive s) -> return (Some s)
          | Some Freed -> vanish ()
        in
        let+ ist' = I.produce ser ist in
        Option.map (fun s -> Alive s) ist'
end
