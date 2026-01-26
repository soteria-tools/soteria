open Soteria_std
open Symex

module Make
    (Symex : Symex.Base)
    (Info : sig
      type t

      val pp : t Fmt.t
    end)
    (B : Base.M(Symex).S) =
struct
  (* TODO: this could probably all be generated using a deriver, instead of having a module in the library *)

  type ('a, 'info) with_info = { node : 'a; info : 'info option }
  [@@deriving show { with_path = false }]

  type t = (B.t, Info.t) with_info [@@deriving show { with_path = false }]

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  open SM.Syntax

  type serialized = (B.serialized, Info.t) with_info
  [@@deriving show { with_path = false }]

  let serialize (t : t) : serialized list =
    B.serialize t.node |> List.map (fun node -> { node; info = t.info })

  let subst_serialized subst_var serialized =
    let node = B.subst_serialized subst_var serialized.node in
    { node; info = serialized.info }

  let iter_vars_serialized s = B.iter_vars_serialized s.node

  let lower = function
    | None -> (None, None)
    | Some { node; info } -> (Some node, info)

  let lift ~info = function None -> None | Some node -> Some { node; info }

  let wrap (f : ('a, 'err, B.serialized list) B.SM.Result.t) :
      ('a, 'err, serialized list) SM.Result.t =
    let* t = SM.get_state () in
    let node, info = lower t in
    let* res, node' = SM.lift @@ f node in
    let+ () = SM.set_state (lift ~info node') in
    Compo_res.map_missing res (List.map (fun fix -> { node = fix; info }))

  let produce serialized : unit SM.t =
    let* t = SM.get_state () in
    let t_opt, t_orig = lower t in
    let info = Option.merge (fun a _ -> a) t_orig serialized.info in
    let* (), node = SM.lift @@ B.produce serialized.node t_opt in
    SM.set_state (lift ~info node)

  (* let consume consume_inner serialized t =
    let node, info = of_opt t in
    let+ res = consume_inner serialized.node node in
    match res with
    | Compo_res.Ok (Some node) -> Compo_res.Ok (Some { node; info })
    | Ok None -> Ok None
    | Error e -> Error e
    | Missing fixes ->
        Missing (List.map (fun fix -> { node = fix; info }) fixes) *)
end
