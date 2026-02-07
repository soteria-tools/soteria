open Symex
open Compo_res
module S = Pmap_intf.M
module Key = Pmap_intf.Key

module type MapS = Pmap_intf.MapS

module Mk_concrete_key (Symex : Symex.Base) (K : Soteria_std.Ordered_type.S) :
  Key(Symex).S with type t = K.t = struct
  include K

  let sem_eq x y = Symex.Value.bool (K.compare x y = 0)
  let fresh () = failwith "Fresh not implemented for concrete keys"
  let simplify = Symex.return
  let distinct _ = Symex.Value.bool true
  let subst _ x = x
  let iter_vars _ = fun _ -> ()
end

module Build_from_find_opt_sym
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Map : MapS with type key = Key.t)
    (Find_opt_sym : sig
      val f : Key.t -> 'a Map.t -> (Key.t * 'a option) Symex.t
    end)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.syn = struct
  module M = Map

  type t = Codom.t M.t
  type syn = Key.t * Codom.syn

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  let empty = M.empty
  let syntactic_bindings (x : t) = M.to_seq x
  let syntactic_mem = M.mem

  let lift_fixes ~key (fix : Codom.syn list list) : syn list list =
    List.map (List.map (fun v_ser -> (key, v_ser))) fix

  open SM
  open SM.Syntax

  let pp_syn : Format.formatter -> syn -> unit =
    Fmt.Dump.(pair Key.pp Codom.pp_syn)

  let show_serialized = Fmt.to_to_string pp_syn

  let to_syn m =
    M.to_seq m
    |> Seq.concat_map (fun (k, v) ->
        List.to_seq (Codom.to_syn v) |> Seq.map (fun v_ser -> (k, v_ser)))
    |> List.of_seq

  let pp' ?(key = Key.pp) ?(codom = Codom.pp) ?(ignore = fun _ -> false) =
    let open Fmt in
    let iter f = M.iter (fun k v -> f (k, v)) in
    let pp_binding ft (k, v) = pf ft "@[<2>%a ->@ %a@]" key k codom v in
    let iter_non_ignored f m =
      iter (fun (k, v) -> if ignore (k, v) then () else f (k, v)) m
    in
    braces (Fmt.iter ~sep:(any ";@\n") iter_non_ignored pp_binding)

  let pp ft t = pp' ft t
  let show = Fmt.to_to_string (fun ft t -> pp ft t)
  let of_opt = function None -> M.empty | Some m -> m
  let to_opt m = if M.is_empty m then None else Some m
  let add_opt k v m = M.update k (fun _ -> v) m

  let alloc ~(new_codom : Codom.t) : (Key.t, 'err, syn list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key = lift @@ Key.fresh () in
    let* () =
      SM.assume
        [ Key.distinct (key :: (M.to_seq st |> Seq.map fst |> List.of_seq)) ]
    in
    let+ () = SM.set_state (to_opt (M.add key new_codom st)) in
    Ok key

  let allocs (type a k) ~(fn : a -> Key.t -> (k * Codom.t) Symex.t)
      ~(els : a list) : (k list, 'err, syn list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let*^ bindings, out_keys =
      Symex.fold_list els ~init:(Seq.empty, []) ~f:(fun (b, ks') e ->
          let open Symex.Syntax in
          let* k = Key.fresh () in
          let+ k', v = fn e k in
          (Seq.cons (k, v) b, k' :: ks'))
    in
    let out_keys = List.rev out_keys in
    let st = M.add_seq bindings st in
    let* () =
      SM.assume [ M.to_seq st |> Seq.map fst |> List.of_seq |> Key.distinct ]
    in
    let+ () = SM.set_state (to_opt st) in
    Ok out_keys

  let wrap (type a err) (key : Key.t)
      (f : (a, err, Codom.syn list) Codom.SM.Result.t) :
      (a, err, syn list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key, codom = lift @@ Find_opt_sym.f key st in
    let* res, codom = lift @@ f codom in
    match res with
    | Ok v ->
        (* Only update the state in case of success! *)
        let+ () = SM.set_state (to_opt (add_opt key codom st)) in
        Ok v
    | Error e -> SM.Result.error e
    | Missing fixes -> SM.Result.miss (lift_fixes ~key fixes)

  let produce (syn : syn) : unit SM.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let key, inner_ser = syn in
    let* key, codom = lift @@ Find_opt_sym.f key st in
    let* (), codom = lift @@ Codom.produce inner_ser codom in
    let st = add_opt key codom st in
    SM.set_state (to_opt st)

  (* let consume
   *    (cons :
   *      'inner_serialized ->
   *      'inner_st option ->
   *      ('inner_st option, [> Symex.lfail ], 'inner_serialized) Symex.Result.t)
   *    (serialized : 'inner_serialized serialized) (st : 'inner_st t option) :
   *    ( 'inner_st t option,
   *      [> Symex.lfail ],
   *      'inner_serialized serialized )
   *    Symex.Result.t =
   *  let st = of_opt st in
   *  let++ st =
   *    Result.fold_list serialized ~init:st ~f:(fun st (key, inner_ser) ->
   *        let* key, codom = Find_opt_sym.f key st in
   *        let++ codom = cons inner_ser codom |> lift_fix_s ~key in
   *        add_opt key codom st)
   *  in
   *  to_opt st *)

  let fold (type acc) (f : acc -> Key.t * Codom.t -> acc Symex.t) (init : acc)
      st : acc Symex.t =
    let open Symex in
    let st = of_opt st in
    fold_seq (M.to_seq st) ~init ~f
end

module Build_base
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (M : MapS with type key = Key.t)
    (Codom : Base.M(Symex).S) =
struct
  open Symex.Syntax

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (key : Key.t) (st : 'a M.t) =
    let rec find_bindings = function
      | [] -> Symex.return (key, None)
      | (k, v) :: tl ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an
         if. *)
    in
    match M.find_opt key st with
    | Some v -> Symex.return (key, Some v)
    | None -> M.to_seq st |> List.of_seq |> find_bindings

  include
    Build_from_find_opt_sym (Symex) (Key) (M)
      (struct
        let f = find_opt_sym
      end)
      (Codom)
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
    (M : MapS with type key = Key.t)
    (Codom : Base.M(Symex).S) =
struct
  open Symex.Syntax

  let find_opt_sym (key : Key.t) (st : 'a M.t) =
    let rec find_bindings = function
      | [] -> Symex.vanish ()
      | (k, v) :: tl ->
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
        else M.to_seq st |> List.of_seq |> find_bindings

  include
    Build_from_find_opt_sym (Symex) (Key) (M)
      (struct
        let f = find_opt_sym
      end)
      (Codom)
end

module Direct_access
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Codom : Base.M(Symex).S) =
struct
  include Build_direct_access (Symex) (Key) (Stdlib.Map.Make (Key)) (Codom)
end

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree)
    (Codom : Base.M(Symex).S) =
struct
  module M' = PatriciaTree.MakeMap (Key)
  include Build_direct_access (Symex) (Key) (M') (Codom)
end

(** Only sound to use if the keys of the map are invariant under interpretations
    of the symbolic variables *)
module Concrete
    (Symex : Symex.Base)
    (Key : Soteria_std.Ordered_type.S)
    (Codom : Base.M(Symex).S) =
struct
  module Key = Mk_concrete_key (Symex) (Key)
  module M' = Stdlib.Map.Make (Key)

  let find_opt_sym (key : Key.t) (st : 'a M'.t) =
    Symex.return (key, M'.find_opt key st)

  include
    Build_from_find_opt_sym (Symex) (Key) (M')
      (struct
        let f = find_opt_sym
      end)
      (Codom)
end
