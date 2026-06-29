module S = S_map_intf.M
module Key = S_map_intf.Key

module type MapS = S_map_intf.MapS

module Mk_concrete_key (Symex : Symex.Base) (K : Soteria_std.Ordered_type.S) =
struct
  include K

  let sem_eq x y = Symex.Value.of_bool (x = y)
  let simplify = Symex.return
  let distinct_seq _ = Symex.Value.of_bool true
end

module Build
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Map : MapS with type key = Key.t) =
struct
  module M = Map

  type 'a t = 'a M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let syntactic_bindings = M.to_seq
  let syntactic_mem = M.mem
  let syntactic_add = M.add
  let syntactic_add_opt k v = M.update k (fun _ -> v)

  let pp' ?(key = Key.pp) ?(ignore = fun _ -> false) codom =
    let open Fmt in
    let iter f = M.iter (fun k v -> f (k, v)) in
    let pp_binding ft (k, v) = pf ft "@[<2>%a ->@ %a@]" key k codom v in
    let iter_non_ignored f m =
      iter (fun (k, v) -> if ignore (k, v) then () else f (k, v)) m
    in
    braces (Fmt.iter ~sep:(any ";@\n") iter_non_ignored pp_binding)

  let pp pp_codom ft t = pp' pp_codom ft t
  let show show_codom = Fmt.to_to_string (pp (Fmt.of_to_string show_codom))
  let of_opt = function None -> M.empty | Some m -> m

  let fold (type acc) (f : acc -> Key.t * 'a -> acc Symex.t) (init : acc) st :
      acc Symex.t =
    let open Symex in
    let st = of_opt st in
    fold (module Soteria_std.Seq) (M.to_seq st) ~init ~f
end

module Build_base
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (M : MapS with type key = Key.t) =
struct
  include Build (Symex) (Key) (M)
  open Symex.Syntax

  let find_opt (key : Key.t) (st : 'a M.t) =
    let rec find_bindings binding =
      match binding () with
      | Seq.Nil -> Symex.return (key, None)
      | Seq.Cons ((k, v), tl) ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an
         if. *)
    in
    let* key = Key.simplify key in
    match M.find_opt key st with
    | Some v -> Symex.return (key, Some v)
    | None -> M.to_seq st |> find_bindings
end

module Make (Symex : Symex.Base) (Key : Key(Symex).S) =
  Build_base (Symex) (Key) (Stdlib.Map.Make (Key))

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
  Build_base (Symex) (Key) (PatriciaTree.MakeMap (Key))

(** Sound to use when the keys of the map may depend on symbolic variables *)

module Build_direct_or_lazy
    (Symex : Symex.Base)
    (B : sig
      open Symex

      (** [if_not_in_map outside_map ~then_ ~else_] decides how to handle
          branching when asking if the object is outside the map (captured by
          the [outside_map] symbolic boolean.)*)
      val if_not_in_map :
        Value.(sbool t) -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t
    end)
    (Key : Key(Symex).S)
    (M : MapS with type key = Key.t) =
struct
  include Build (Symex) (Key) (M)
  open Symex.Syntax

  let find_opt (key : Key.t) (st : 'a M.t) =
    let rec find_bindings binding =
      match binding () with
      | Seq.Nil -> Symex.vanish ()
      | Seq.Cons ((k, v), tl) ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v)
          else find_bindings tl
    in
    let* key = Key.simplify key in
    match M.find_opt key st with
    | Some v -> Symex.return (key, Some v)
    | None ->
        let not_in_map = M.to_seq st |> Seq.map fst |> Key.distinct_seq in
        B.if_not_in_map not_in_map
          ~then_:(fun () -> Symex.return (key, None))
          ~else_:(fun () -> M.to_seq st |> find_bindings)
end

module Build_direct_access
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (M : MapS with type key = Key.t) =
struct
  include
    Build_direct_or_lazy
      (Symex)
      (struct
        open Symex

        (** In the direct access map, we check if it's possible that the key is
            outside the map, and if it is, we only take that branch in UX.
            However, we keep the information that the value is not in the map,
            we have entirely dropped the other branch. *)
        let if_not_in_map guard ~then_ ~else_ =
          branch_on_take_one guard ~then_ ~else_
      end)
      (Key)
      (M)
end

(** An S_map where all keys are not guaranteed to be disjoint. *)
module Build_lazy
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (M : MapS with type key = Key.t) =
struct
  include
    Build_direct_or_lazy
      (Symex)
      (struct
        open Symex

        (** In the lazy map, we see if we know the value is inside the map. If
            it is, then we take that branch, knowing that fact. Otherwise, we
            consider a new binding. This changes the interpretation of a map:
            two different bindings are not necessarily semantically different
            keys!

            This is well modelled with the existing `if_sure` of Symex. *)
        let if_not_in_map not_in_map ~then_ ~else_ =
          let in_map = Value.not not_in_map in
          if_sure in_map ~then_:else_ ~else_:then_
      end)
      (Key)
      (M)
end

module Direct_access (Symex : Symex.Base) (Key : Key(Symex).S) =
  Build_direct_access (Symex) (Key) (Stdlib.Map.Make (Key))

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
  Build_direct_access (Symex) (Key) (PatriciaTree.MakeMap (Key))

module Lazy (Symex : Symex.Base) (Key : Key(Symex).S) =
  Build_lazy (Symex) (Key) (Stdlib.Map.Make (Key))

module Lazy_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
  Build_lazy (Symex) (Key) (PatriciaTree.MakeMap (Key))

module Concrete (Symex : Symex.Base) (Key : Key(Symex).S) = struct
  include Build (Symex) (Key) (Stdlib.Map.Make (Key))

  let find_opt (key : Key.t) (st : 'a M.t) =
    Symex.return (key, M.find_opt key st)
end
