open Soteria_std

module Make
    (Symex : Symex.S)
    (Info : sig
      type t

      val pp : t Fmt.t
    end) =
struct
  (* TODO: this could probably all be generated using a deriver, instead of having a module in the library *)

  open Symex.Syntax

  type 'a t = { node : 'a; info : Info.t option }
  [@@deriving show { with_path = false }]

  type 'a serialized = 'a t [@@deriving show { with_path = false }]

  let serialize serialize_inner t =
    let node = serialize_inner t.node in
    { node; info = t.info }

  let subst_serialized subst_inner subst_var serialized =
    let node = subst_inner subst_var serialized.node in
    { node; info = serialized.info }

  let iter_vars_serialized iter_inner s = iter_inner s.node

  let of_opt = function
    | None -> (None, None)
    | Some { node; info } -> (Some node, info)

  let wrap f t =
    let node, info = of_opt t in
    let+ res = f node in
    match res with
    | Compo_res.Ok (res, Some node) -> Compo_res.Ok (res, Some { node; info })
    | Ok (res, None) -> Ok (res, None)
    | Error e -> Error e
    | Missing fixes ->
        Missing (List.map (fun fix -> { node = fix; info }) fixes)

  (* wraps a Tree_block operation *)

  let produce produce_inner serialized t : 'a t option Symex.t =
    let t_opt, t_orig = of_opt t in
    let info = Option.merge (fun a _ -> a) t_orig serialized.info in
    let+ node = produce_inner serialized.node t_opt in
    match node with Some node -> Some { node; info } | None -> None

  let consume consume_inner serialized t =
    let node, info = of_opt t in
    let+ res = consume_inner serialized.node node in
    match res with
    | Compo_res.Ok (Some node) -> Compo_res.Ok (Some { node; info })
    | Ok None -> Ok None
    | Error e -> Error e
    | Missing fixes ->
        Missing (List.map (fun fix -> { node = fix; info }) fixes)
end
