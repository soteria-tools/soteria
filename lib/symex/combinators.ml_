module type KeyS = sig
  type ty

  val ty : ty
end

module ParMap (Symex : Symex.S) (Key : KeyS with type ty = Symex.Value.ty) =
struct
  open Symex
  open Symex.Syntax

  let empty = Map.empty

  let alloc ~new_data t =
    let key = Symex.Value.fresh Key.ty in
    (key, Map.set t ~key ~data:new_data)

  let eval ~codom_eval i t =
    let with_binding key data =
      let+ res, new_data = codom_eval data in
      (new_data, Map.set t ~key:i ~data:new_data)
    in
    match Map.find t i with
    | Some v -> with_binding i v
    | None ->
        let rec find seq =
          match Sequence.next seq with
          | None -> Symex.vanish ()
          | Some ((this_key, this_data), next) ->
              if%sat Symex.value_eq this_key i then
                with_binding this_key this_data
              else find next
        in
        find (Map.to_sequence t)
end
