module Key = struct
  module type S = sig
    include Stdlib.Map.OrderedType
    include S_elt.S with type t := t
    include S_eq.S with type t := t and module Symex := Symex
    include Soteria_std.Printable.S with type t := t
  end

  module Of_concrete (Symex : Symex.S) (K : Soteria_std.Ordered_type.S) = struct
    include K
    include S_elt.Of_concrete (Symex) (K)

    include
      S_eq.Of_concrete
        (Symex)
        (struct
          include K

          let equal x y = compare x y = 0
        end)
  end
end

module type S = sig
  module Symex : Symex.S

  type key

  module Raw_map : Stdlib.Map.S with type key = key

  type 'a t = 'a Raw_map.t

  (** [find_opt key map] returns a symbolic computation that yields
      - [(key, None)] if there is no key in the map that is semantically equal
        to [key]
      - [(key', Some v)] if there is a binding in the map for [(key', v)] and
        [key'] is semantically equal to [key].

      In both cases, the resulting [key'] can be used to insert or update the
      map directly without needing to search for it again. *)
  val find_opt : key -> 'a t -> (key * 'a option) Symex.t
end

module Make (Symex : Symex.S) (Key : Key.S with module Symex = Symex) :
  S with type key = Key.t and module Symex = Symex = struct
  module Symex = Symex
  open Symex.Syntax
  module Raw_map = Stdlib.Map.Make (Key)

  type key = Key.t
  type 'a t = 'a Raw_map.t

  let find_opt (key : Key.t) (m : 'a t) : (Key.t * 'a option) Symex.t =
    let rec find_sym s =
      match s () with
      | Seq.Nil -> Symex.return (key, None)
      | Seq.Cons ((k, v), tl) ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v) else find_sym tl
      (* TODO: Investigate: if this can be somehow made a tail-call? *)
    in
    match Raw_map.find_opt key m with
    | Some v ->
        (* We found it syntactically *)
        Symex.return (key, Some v)
    | None -> find_sym (Raw_map.to_seq m)
end

module Make_failfast (Symex : Symex.S) (Key : Key.S with module Symex = Symex) :
  S with type key = Key.t and module Symex = Symex = struct
  module Symex = Symex
  open Symex.Syntax
  module Raw_map = Stdlib.Map.Make (Key)

  type key = Key.t
  type 'a t = 'a Raw_map.t

  let find_opt (key : Key.t) (m : 'a t) : (Key.t * 'a option) Symex.t =
    let rec find_sym (fkey, fval) tl =
      (* At this point, we know that the element is in the map, so if tl is empty, it has to be first. *)
      match tl () with
      | Seq.Nil -> Symex.return (fkey, Some fval)
      | Seq.Cons ((skey, sval), ttl) ->
          if%sat Key.sem_eq key fkey then Symex.return (fkey, Some fval)
          else find_sym (skey, sval) ttl
      (* TODO: Investigate: if this can be somehow made a tail-call? *)
    in
    match Raw_map.find_opt key m with
    | Some v -> Symex.return (key, Some v)
    | None -> (
        let s = Raw_map.to_seq m in
        match s () with
        | Seq.Nil ->
            (* If the map is empty, we know for sure that the key is not in it *)
            Symex.return (key, None)
        | Seq.Cons (first, tl) ->
            let not_in_map =
              let all_keys_plus_key =
                let all_keys = s |> Seq.map fst |> List.of_seq in
                key :: all_keys
              in
              all_keys_plus_key |> Key.distinct
            in
            if%sat1 not_in_map then
              (* In the case we know all keys are different from [key], we can return immediately *)
              Symex.return (key, None)
            else find_sym first tl)
end

module Make_concrete (Symex : Symex.S) (K : Soteria_std.Ordered_type.S) :
  S with type key = K.t and module Symex = Symex = struct
  module Symex = Symex
  module Key = Key.Of_concrete (Symex) (K)
  module Raw_map = Stdlib.Map.Make (Key)

  type key = Key.t
  type 'a t = 'a Raw_map.t

  let find_opt (key : Key.t) (m : 'a t) =
    Symex.return (key, Raw_map.find_opt key m)
end
