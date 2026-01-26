open Symex

module KeyS (Symex : Symex.Base) = struct
  module type S = sig
    type t

    include Stdlib.Map.OrderedType with type t := t

    type sbool_v := Symex.Value.(sbool t)

    val pp : Format.formatter -> t -> unit
    val sem_eq : t -> t -> sbool_v
    val fresh : unit -> t Symex.t
    val simplify : t -> t Symex.t
    val distinct : t list -> sbool_v
    val subst : (Var.t -> Var.t) -> t -> t
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end

  module type S_patricia_tree = sig
    include S

    val to_int : t -> int
  end
end

module type MapS = sig
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
end

module S
    (Symex : Symex.Base)
    (Key : sig
      type t
    end) =
struct
  module type S = sig
    type codom
    type t
    type codom_serialized
    type serialized = Key.t * codom_serialized

    module SM :
      State_monad.S
        with type 'a Symex.t = 'a Symex.t
         and type st = t option
         and module Value = Symex.Value

    type ('a, 'err) res := ('a, 'err, serialized list) SM.Result.t

    type ('a, 'err) codom_res :=
      codom option ->
      (('a, 'err, codom_serialized list) Compo_res.t * codom option) Symex.t

    val empty : t
    val syntactic_bindings : t -> (Key.t * codom) Seq.t
    val syntactic_mem : Key.t -> t -> bool
    val pp : ?ignore:(Key.t * codom -> bool) -> Format.formatter -> t -> unit
    val show : t -> string
    val pp_serialized : Format.formatter -> serialized -> unit
    val show_serialized : serialized -> string
    val serialize : t -> serialized list
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    val iter_vars_serialized :
      serialized -> (Var.t * 'b Symex.Value.ty -> unit) -> unit

    val of_opt : t option -> t
    val to_opt : t -> t option
    val alloc : new_codom:codom -> (Key.t, 'err) res

    val allocs :
      fn:('a -> Key.t -> ('k * codom) Symex.t) ->
      els:'a list ->
      ('k list, 'err) res

    val wrap : Key.t -> ('a, 'err) codom_res -> ('a, 'err) res

    val fold :
      ('acc -> Key.t * codom -> ('acc, 'err, serialized list) Symex.Result.t) ->
      'acc ->
      t option ->
      ('acc, 'err, serialized list) Symex.Result.t

    val produce : serialized -> t option -> (unit * t option) Symex.t

    (* val consume :
      ('inner_serialized ->
      'inner_st option ->
      ( 'inner_st option,
        ([> Symex.lfail ] as 'a),
        'inner_serialized )
      Symex.Result.t) ->
      'inner_serialized serialized ->
      'inner_st t option ->
      ('inner_st t option, 'a, 'inner_serialized serialized) Symex.Result.t *)
  end
end

module Mk_concrete_key (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) :
  KeyS(Symex).S with type t = Key.t = struct
  include Key

  let sem_eq x y = Symex.Value.bool (Key.compare x y = 0)
  let fresh () = failwith "Fresh not implemented for concrete keys"
  let simplify = Symex.return
  let distinct _ = Symex.Value.bool true
  let subst _ x = x
  let iter_vars _ = fun _ -> ()
end

module Build_from_find_opt_sym
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S)
    (Map : MapS with type key = Key.t)
    (Find_opt_sym : sig
      val f : Key.t -> 'a Map.t -> (Key.t * 'a option) Symex.t
    end)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized = struct
  module M = Map

  type t = Codom.t M.t
  type serialized = Key.t * Codom.serialized

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  let empty = M.empty
  let syntactic_bindings (x : t) = M.to_seq x
  let syntactic_mem = M.mem

  let lift_fix ~key (fix : Codom.serialized list) : serialized list =
    List.map (fun v_ser -> (key, v_ser)) fix

  let lift_fix_r ~key (res : ('a, 'err, Codom.serialized list) Compo_res.t) :
      ('a, 'err, serialized list) Compo_res.t =
    Compo_res.map_missing res (lift_fix ~key)

  open SM
  open SM.Syntax

  let pp_serialized : Format.formatter -> serialized -> unit =
    Fmt.Dump.(pair Key.pp Codom.pp_serialized)

  let show_serialized = Fmt.to_to_string pp_serialized

  let serialize m =
    M.to_seq m
    |> Seq.concat_map (fun (k, v) ->
        List.to_seq (Codom.serialize v) |> Seq.map (fun v_ser -> (k, v_ser)))
    |> List.of_seq

  let subst_serialized subst_var (key, v) =
    (Key.subst subst_var key, Codom.subst_serialized subst_var v)

  let iter_vars_serialized (key, v) f =
    Key.iter_vars key f;
    Codom.iter_vars_serialized v f

  let pp ?(ignore = fun _ -> false) =
    let open Fmt in
    let iter f = M.iter (fun k v -> f (k, v)) in
    let pp_binding ft (k, v) = pf ft "@[<2>%a ->@ %a@]" Key.pp k Codom.pp v in
    let iter_non_ignored f m =
      iter (fun (k, v) -> if ignore (k, v) then () else f (k, v)) m
    in
    braces (Fmt.iter ~sep:(any ";@\n") iter_non_ignored pp_binding)

  let show = Fmt.to_to_string (fun ft t -> pp ft t)
  let of_opt = function None -> M.empty | Some m -> m
  let to_opt m = if M.is_empty m then None else Some m
  let add_opt k v m = M.update k (fun _ -> v) m

  let alloc ~(new_codom : Codom.t) : (Key.t, 'err, serialized list) SM.Result.t
      =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key = lift @@ Key.fresh () in
    let* () =
      SM.assume
        [ Key.distinct (key :: (M.to_seq st |> Seq.map fst |> List.of_seq)) ]
    in
    let* () = SM.set_state (to_opt (M.add key new_codom st)) in
    Result.ok key

  let allocs (type a k) ~(fn : a -> Key.t -> (k * Codom.t) Symex.t)
      ~(els : a list) : (k list, 'err, serialized list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* bindings, out_keys =
      lift
      @@ Symex.fold_list els ~init:(Seq.empty, []) ~f:(fun (b, ks') e ->
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
    let* () = SM.set_state (to_opt st) in
    SM.Result.ok out_keys

  let wrap (type a err) (key : Key.t)
      (f : (a, err, Codom.serialized list) Codom.SM.Result.t) :
      (a, err, serialized list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key, codom = lift @@ Find_opt_sym.f key st in
    let* res, codom = lift @@ f codom in
    let+ () = SM.set_state (to_opt (add_opt key codom st)) in
    lift_fix_r ~key res

  let produce (serialized : serialized) : unit SM.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let key, inner_ser = serialized in
    let* key, codom = lift @@ Find_opt_sym.f key st in
    let* (), codom = lift @@ Codom.produce inner_ser codom in
    let st = add_opt key codom st in
    SM.set_state (to_opt st)

  (* let consume
      (cons :
        'inner_serialized ->
        'inner_st option ->
        ('inner_st option, [> Symex.lfail ], 'inner_serialized) Symex.Result.t)
      (serialized : 'inner_serialized serialized) (st : 'inner_st t option) :
      ( 'inner_st t option,
        [> Symex.lfail ],
        'inner_serialized serialized )
      Symex.Result.t =
    let st = of_opt st in
    let++ st =
      Result.fold_list serialized ~init:st ~f:(fun st (key, inner_ser) ->
          let* key, codom = Find_opt_sym.f key st in
          let++ codom = cons inner_ser codom |> lift_fix_s ~key in
          add_opt key codom st)
    in
    to_opt st *)

  (*     val fold :
      ('acc -> Key.t * codom -> ('acc, 'err, serialized) Symex.Result.t) ->
      'acc ->
      t option ->
      ('acc, 'err, serialized) Symex.Result.t *)

  let fold (type acc)
      (f :
        acc -> Key.t * Codom.t -> (acc, 'err, serialized list) Symex.Result.t)
      (init : acc) st : (acc, 'err, serialized list) Symex.Result.t =
    let open Symex in
    let st = of_opt st in
    Result.fold_seq (M.to_seq st) ~init ~f
end

module Build_base
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S)
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
      (Codom)
end

module Make (Symex : Symex.Base) (Key : KeyS(Symex).S) =
  Build_base (Symex) (Key) (Stdlib.Map.Make (Key))

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree) =
  Build_base (Symex) (Key) (PatriciaTree.MakeMap (Key))

(** Sound to use when the keys of the map may depend on symbolic variables *)

module Build_direct_access
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S)
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
    (Key : KeyS(Symex).S)
    (Codom : Base.M(Symex).S) =
struct
  include Build_direct_access (Symex) (Key) (Stdlib.Map.Make (Key)) (Codom)
end

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S_patricia_tree)
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
