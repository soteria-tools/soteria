module S = S_map_intf.M
module Key = S_map_intf.Key

module type MapS = S_map_intf.MapS

module Mk_concrete_key (Symex : Symex.Base) (K : Soteria_std.Ordered_type.S) =
struct
  include K

  let sem_eq x y = Symex.Value.of_bool (x = y)
  let simplify = Symex.return
  let distinct _ = Symex.Value.of_bool true
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
    fold_seq (M.to_seq st) ~init ~f
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

module Build_direct_access
    (Symex : Symex.Base)
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
        let not_in_map =
          M.to_seq st |> Seq.map fst |> List.of_seq |> Key.distinct
        in
        if%sat1 not_in_map then Symex.return (key, None)
        else M.to_seq st |> find_bindings
end

module Direct_access (Symex : Symex.Base) (Key : Key(Symex).S) =
  Build_direct_access (Symex) (Key) (Stdlib.Map.Make (Key))

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
  Build_direct_access (Symex) (Key) (PatriciaTree.MakeMap (Key))

module Concrete (Symex : Symex.Base) (Key : Key(Symex).S) = struct
  include Build (Symex) (Key) (Stdlib.Map.Make (Key))

  let find_opt (key : Key.t) (st : 'a M.t) =
    Symex.return (key, M.find_opt key st)
end
