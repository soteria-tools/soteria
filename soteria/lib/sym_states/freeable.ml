(* TODO: Build this with the Sum transformer and `Excl(Freed)` *)

open Soteria_std

type 'a freeable = Freed | Alive of 'a

let pp_freeable pp_a ft = function
  | Freed -> Fmt.pf ft "Freed"
  | Alive a -> pp_a ft a

module Make
    (Symex : Symex.Base)
    (I : sig
      include Base.M(Symex).S

      (** Checks this state is exclusively owned, returning if it is exclusively
          owned and otherwise missing with the fixes required to make this state
          exclusively owned.
          {b This function is expected to not be modify the state.} *)
      val assert_exclusively_owned : unit -> (unit, 'err, syn list) SM.Result.t
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
  let lift_fix_r x = Compo_res.map_missing (List.map lift_fix) x
  let lift_fix_c x = Symex.Consumer.map_missing (List.map lift_fix) x

  let to_syn = function
    | Freed -> [ Freed ]
    | Alive a -> List.map (fun x -> Alive x) (I.to_syn a)

  let ins_outs = function Freed -> ([], []) | Alive s -> I.ins_outs s

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
    let** () = wrap (I.assert_exclusively_owned ()) in
    SM.Result.set_state (Some Freed)

  let produce (syn : syn) st : st Symex.Producer.t =
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

  let consume (syn : syn) (st : st) : (st, syn list) Symex.Consumer.t =
    let open Symex.Consumer in
    let open Symex.Consumer.Syntax in
    match syn with
    | Freed -> (
        match st with
        | None -> miss [ [ Freed ] ]
        | Some Freed -> ok None
        | Some (Alive _) -> lfail (Value.of_bool false))
    | Alive ser -> (
        let* ist =
          match st with
          | None -> ok None
          | Some (Alive ist) -> ok (Some ist)
          | Some Freed -> lfail (Value.of_bool false)
        in
        let+ ist = lift_fix_c (I.consume ser ist) in
        match ist with Some ist -> Some (Alive ist) | None -> None)
end
