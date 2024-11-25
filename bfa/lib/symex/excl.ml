module Make (Symex : Symex.S) = struct
  type 'a t = 'a

  let pp pp_value = pp_value
  let owned x = Some x

  let load st =
    match st with
    | Some x -> Symex.Result.ok (x, st)
    | None -> Symex.Result.error `MissingValue

  let store x st =
    match st with
    | Some _ -> Symex.Result.ok ((), Some x)
    | None -> Symex.Result.error `MissingValue
end
