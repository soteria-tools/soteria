module Make (Symex : Symex.S) = struct
  type 'a t = 'a

  let pp pp_value = pp_value
  let owned x = x

  let load (st : 'a t option) =
    match st with
    | Some x -> Symex.Result.ok (x, st)
    | None -> Symex.Result.error `MissingValue

  let store x (st : 'a t option) =
    match st with
    | Some _ -> Symex.Result.ok ((), Some x)
    | None -> Symex.Result.error `MissingValue

  type 'a serialized = 'a

  let serialize serialize_val x = serialize_val x

  let iter_vars_serialized
      (i : 'a -> (Symex.Value.t * Symex.Value.ty -> unit) -> unit)
      (x : 'a serialized) (f : Symex.Value.t * Symex.Value.ty -> unit) : unit =
    i x f

  let consume ~sem_eq (serialized : 'a serialized) (t : 'a t option) =
    match t with
    | Some x -> Symex.Result.ok ~learned:[ sem_eq x serialized ] None
    | None -> Symex.Result.error `MissingValue

  let produce (serialized : 'a serialized) (t : 'a t option) =
    match t with
    | None -> Symex.return (Some serialized)
    | Some _ -> Symex.vanish ()
end
