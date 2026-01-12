open Symex
module Key = Pmap_intf.Key

module type Container_map_S = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_list : 'a t -> (key * 'a) list
end

(** @canonical *)
module S = Pmap_intf.M

module Mk_concrete_key (Symex : Symex.Base) (K : Soteria_std.Ordered_type.S) :
  Key(Symex).S with type t = K.t and type syn = K.t = struct
  include K

  type syn = K.t [@@deriving show { with_path = false }]

  let[@inline] to_syn x = x
  let sem_eq x y = Symex.Value.bool (K.compare x y = 0)
  let fresh () = failwith "Fresh not implemented for concrete keys"
  let simplify = Symex.return
  let distinct _ = Symex.Value.bool true
  let subst _ x = x
  let iter_vars _ = fun _ -> ()
  let exprs_syn _ = []
end

module Build_from_find_opt_sym
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (Map : Container_map_S with type key = Key.t)
    (Find_opt_sym : sig
      val f : Key.t -> 'a Map.t -> (Key.t * 'a option) Symex.t
    end) =
struct
  open Symex.Syntax
  open Symex
  module M = Map

  type 'a t = 'a M.t
  type 'a syn = Key.syn * 'a [@@deriving show { with_path = false }]

  let ins_outs ins_outs_codom (k, v) =
    let ins, outs = ins_outs_codom v in
    (Key.exprs_syn k @ ins, outs)

  let lift_fix ~key fix =
    let key = Key.to_syn key in
    List.map (fun v -> (key, v)) fix

  let empty = M.empty
  let syntactic_bindings = M.to_seq
  let syntactic_mem = M.mem

  let lift_fix_s ~key res =
    let+? fix = res in
    lift_fix ~key fix

  let pp_syn pp_inner : Format.formatter -> 'a syn -> unit =
    Fmt.brackets Fmt.Dump.(pair Key.pp_syn pp_inner)

  let to_syn (to_syn_inner : 'a -> 'syn list) (m : 'a t) : 'syn syn list =
    M.to_list m
    |> List.concat_map (fun (k, v) ->
           let k = Key.to_syn k in
           List.map (fun v -> (k, v)) (to_syn_inner v))

  let pp ?(ignore = fun _ -> false) pp_value =
    let open Fmt in
    let iter f = M.iter (fun k v -> f (k, v)) in
    let pp_binding ft (k, v) = pf ft "@[<2>%a ->@ %a@]" Key.pp k pp_value v in
    let iter_non_ignored f m =
      iter (fun (k, v) -> if ignore (k, v) then () else f (k, v)) m
    in
    braces (Fmt.iter ~sep:(any ";@\n") iter_non_ignored pp_binding)

  let of_opt = function None -> M.empty | Some m -> m
  let to_opt m = if M.is_empty m then None else Some m
  let add_opt k v m = M.update k (fun _ -> v) m

  let alloc (type a) ~(new_codom : a) (st : a t option) :
      (Key.t * a t option, 'err, 'fix list) Result.t =
    let st = of_opt st in
    let* key = Key.fresh () in
    let* () =
      Symex.assume
        [ Key.distinct (key :: (M.to_seq st |> Seq.map fst |> List.of_seq)) ]
    in
    Result.ok (key, to_opt (M.add key new_codom st))

  let allocs (type a b k) ~(fn : b -> Key.t -> (a * k) Symex.t) ~(els : b list)
      (st : a t option) : (k list * a t option, 'err, 'fix list) Result.t =
    let st = of_opt st in
    let* bindings, out_keys =
      Symex.fold_list els ~init:(Seq.empty, []) ~f:(fun (b, ks') e ->
          let* k = Key.fresh () in
          let+ v, k' = fn e k in
          (Seq.cons (k, v) b, k' :: ks'))
    in
    let out_keys = List.rev out_keys in
    let st = M.add_seq bindings st in
    let+ () =
      Symex.assume [ M.to_seq st |> Seq.map fst |> List.of_seq |> Key.distinct ]
    in
    Compo_res.Ok (out_keys, to_opt st)

  let wrap (f : 'a option -> ('b * 'a option, 'err, 'fix list) Symex.Result.t)
      (key : Key.t) (st : 'a t option) :
      ('b * 'a t option, 'err, 'fix syn list) Symex.Result.t =
    let st = of_opt st in
    let* key, codom = Find_opt_sym.f key st in
    let++ res, codom = f codom |> lift_fix_s ~key in
    (res, to_opt (add_opt key codom st))

  let produce
      (prod : 'inner_syn -> 'inner_st option -> 'inner_st option Producer.t)
      (syn : 'inner_syn syn) (st : 'inner_st t option) :
      'inner_st t option Producer.t =
    let open Symex in
    let open Producer.Syntax in
    let key, inner_syn = syn in
    let st = of_opt st in
    let* key = Producer.apply_subst Key.subst key in
    let* key, codom = Producer.lift (Find_opt_sym.f key st) in
    let+ codom = prod inner_syn codom in
    let st = add_opt key codom st in
    to_opt st

  let consume
      (cons :
        'inner_syn ->
        'inner_st option ->
        ('inner_st option, 'inner_syn list) Symex.Consumer.t)
      (syn : 'inner_syn syn) (st : 'inner_st t option) :
      ('inner_st t option, 'inner_syn syn list) Symex.Consumer.t =
    let open Symex in
    let open Consumer.Syntax in
    let st = of_opt st in
    let key, inner_syn = syn in
    let* key = Consumer.apply_subst Key.subst key in
    let* key, codom = Consumer.lift_symex (Find_opt_sym.f key st) in
    let+ codom = Consumer.map_missing (cons inner_syn codom) (lift_fix ~key) in
    let st = add_opt key codom st in
    to_opt st

  let fold (f : 'acc -> Key.t * 'a -> ('acc, 'err, 'fix) Symex.Result.t)
      (init : 'acc) (st : 'a t option) : ('acc, 'err, 'fix) Symex.Result.t =
    let st = of_opt st in
    Result.fold_seq (M.to_seq st) ~init ~f
end

module Build_base
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (M : Container_map_S with type key = Key.t) =
struct
  open Symex.Syntax

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (key : Key.t) (st : 'a M.t) =
    let rec find_bindings = function
      | [] -> Symex.return (key, None)
      | (k, v) :: tl ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an if. *)
    in
    match M.find_opt key st with
    | Some v -> Symex.return (key, Some v)
    | None -> M.to_seq st |> List.of_seq |> find_bindings

  include
    Build_from_find_opt_sym (Symex) (Key) (M)
      (struct
        let f = find_opt_sym
      end)
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
    (M : Container_map_S with type key = Key.t) =
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
end

module Direct_access (Symex : Symex.Base) (Key : Key(Symex).S) = struct
  include Build_direct_access (Symex) (Key) (Stdlib.Map.Make (Key))
end

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
struct
  module M' = PatriciaTree.MakeMap (Key)
  include Build_direct_access (Symex) (Key) (M')
end

(** Only sound to use if the keys of the map are invariant under interpretations
    of the symbolic variables *)
module Concrete (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) = struct
  module Key = Mk_concrete_key (Symex) (Key)
  module M' = Stdlib.Map.Make (Key)

  let find_opt_sym (key : Key.t) (st : 'a M'.t) =
    Symex.return (key, M'.find_opt key st)

  include
    Build_from_find_opt_sym (Symex) (Key) (M')
      (struct
        let f = find_opt_sym
      end)
end
