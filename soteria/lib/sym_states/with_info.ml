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
  (* TODO: this could probably all be generated using a deriver, instead of
     having a module in the library *)

  type ('a, 'info) with_info = { node : 'a; info : 'info option }
  [@@deriving show { with_path = false }]

  type t = (B.t, Info.t) with_info [@@deriving show { with_path = false }]

  let pp' ?(inner = B.pp) ?(info = Info.pp) ft t = pp_with_info inner info ft t
  let pp ft t = pp' ft t

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  open SM.Syntax

  type syn = (B.syn, Info.t) with_info [@@deriving show { with_path = false }]

  let to_syn (t : t) : syn list =
    B.to_syn t.node |> List.map (fun node -> { node; info = t.info })

  let lower = function
    | None -> (None, None)
    | Some { node; info } -> (Some node, info)

  let lift ~info = function None -> None | Some node -> Some { node; info }

  let wrap (f : ('a, 'err, B.syn list) B.SM.Result.t) :
      ('a, 'err, syn list) SM.Result.t =
    let* t = SM.get_state () in
    let node, info = lower t in
    let*^ res, node' = f node in
    let+ () = SM.set_state (lift ~info node') in
    Compo_res.map_missing res (List.map (fun fix -> { node = fix; info }))

  let produce syn : unit SM.t =
    let* t = SM.get_state () in
    let t_opt, t_orig = lower t in
    let info = Option.merge (fun a _ -> a) t_orig syn.info in
    let*^ (), node = B.produce syn.node t_opt in
    SM.set_state (lift ~info node)

  (* let consume consume_inner serialized t =
   *   let node, info = of_opt t in
   *   let+ res = consume_inner serialized.node node in
   *   match res with
   *   | Compo_res.Ok (Some node) -> Compo_res.Ok (Some { node; info })
   *   | Ok None -> Ok None
   *   | Error e -> Error e
   *   | Missing fixes ->
   *       Missing (List.map (fun fix -> { node = fix; info }) fixes) *)
end
