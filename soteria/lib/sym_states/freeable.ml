open Soteria_std

type 'a freeable = Ser_Freed of unit | Ser_Alive of 'a

let pp_freeable pp_a ft = function
  | Ser_Freed () -> Fmt.pf ft "Freed"
  | Ser_Alive a -> pp_a ft a

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
  module Freed_syn = struct
    type t = unit [@@deriving show { with_path = false }, eq]

    let fresh () = Symex.return ()
  end

  module Freed = Excl.Make_concrete (Symex) (Freed_syn)

  type t = Freed of Freed.t | Alive of I.t
  [@@deriving sym_state { inside_soteria; symex = Symex; syn = I.syn freeable }]

  let pp' ?(inner = I.pp) ft = function
    | Freed () -> Fmt.pf ft "Freed"
    | Alive a -> inner ft a

  let pp = pp' ?inner:None
  let pp_syn = pp_freeable I.pp_syn
  let lift_fix fix = Ser_Alive fix
  let lift_fix_r x = Compo_res.map_missing (List.map lift_fix) x
  let lift_fix_c x = Symex.Consumer.map_missing (List.map lift_fix) x

  open SM
  open SM.Syntax

  let unwrap_alive () =
    let* st = SM.get_state () in
    match st with
    | None -> Result.ok None
    | Some (Alive s) -> Result.ok (Some s)
    | Some (Freed ()) -> Result.error `UseAfterFree

  (* [f] must be a "symex state monad" *)
  let wrap (f : ('a, 'err, I.syn list) I.SM.Result.t) :
      ('b, 'err, syn list) SM.Result.t =
    let** inner_state = unwrap_alive () in
    let*^ res, inner_state' = f inner_state in
    let* () = SM.set_state (Option.map (fun x -> Alive x) inner_state') in
    return (lift_fix_r res)

  let free () : (unit, 'err, syn list) SM.Result.t =
    let** () = wrap (I.assert_exclusively_owned ()) in
    SM.Result.set_state (Some (Freed ()))
end
