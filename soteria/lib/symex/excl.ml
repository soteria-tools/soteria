module Make (Symex : Symex.S) = struct
  type 'a t = 'a
  type 'a serialized = 'a

  let pp pp_value = pp_value

  let load (st : 'a t option) =
    match st with
    | Some x -> Symex.Result.ok (x, st)
    | None -> Symex.Result.miss []

  let store x (st : 'a t option) =
    match st with
    | Some _ -> Symex.Result.ok ((), Some x)
    | None -> Symex.Result.miss []

  let serialize serialize_val x = serialize_val x
  let pp_serialized = pp

  let iter_vars_serialized (i : 'a -> 'b Symex.Value.ty Var.iter_vars)
      (x : 'a serialized) : 'b Symex.Value.ty Var.iter_vars =
   fun f -> i x f

  let subst_serialized subst_inner subst_var x = subst_inner subst_var x

  let consume ~sem_eq (serialized : 'a serialized) (t : 'a t option) :
      ('a t option, 'err, 'a serialized) Symex.Result.t =
    let open Symex.Syntax in
    match t with
    | Some x ->
        let+ () = Symex.assume [ sem_eq x serialized ] in
        Compo_res.Ok None
    | None -> Symex.Result.miss [ serialized ]

  let produce (serialized : 'a serialized) (t : 'a t option) =
    match t with
    | None -> Symex.return (Some serialized)
    | Some _ -> Symex.vanish ()
end
