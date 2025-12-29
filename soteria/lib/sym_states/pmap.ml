open Symex

module KeyS (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    type t
    type syn

    include Stdlib.Map.OrderedType with type t := t

    type sbool_v := Symex.Value.sbool Symex.Value.t

    val pp : Format.formatter -> t -> unit
    val sem_eq : t -> t -> sbool_v
    val fresh : unit -> t Symex.t
    val simplify : t -> t Symex.t
    val distinct : t list -> sbool_v
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
    val to_syn : t -> syn
    val subst : (Value.Syn.t -> 'a Value.t) -> syn -> t
    val exprs_syn : syn -> Symex.Value.Syn.t list
  end
end

module Mk_concrete_key (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) :
  KeyS(Symex).S with type t = Key.t = struct
  include Key

  type syn = Key.t

  let[@inline] to_syn x = x
  let sem_eq x y = Symex.Value.bool (Key.compare x y = 0)
  let fresh () = failwith "Fresh not implemented for concrete keys"
  let simplify = Symex.return
  let distinct _ = Symex.Value.bool true
  let subst _ x = x
  let iter_vars _ = fun _ -> ()
  let exprs_syn _ = []
end

module Build_from_find_opt_sym
    (Symex : Symex.Base)
    (Key : KeyS(Symex).S)
    (Find_opt_sym : sig
      val f : Key.t -> 'a Stdlib.Map.Make(Key).t -> (Key.t * 'a option) Symex.t
    end) =
struct
  open Symex.Syntax
  open Symex
  module M = Stdlib.Map.Make (Key)

  type 'a t = 'a M.t
  type 'a serialized = (Key.t * 'a) list
  type 'a syn = Key.syn * 'a

  let ins_outs ins_outs_codom (k, v) =
    let ins, outs = ins_outs_codom v in
    (Key.exprs_syn k @ ins, outs)

  let lift_fix_s ~key res =
    let key = Key.to_syn key in
    let+? fix = res in
    List.map (fun v -> (key, v)) fix

  let pp_serialized pp_inner : Format.formatter -> 'a serialized -> unit =
    Fmt.brackets
      (Fmt.iter ~sep:(Fmt.any ";@ ") List.iter Fmt.Dump.(pair Key.pp pp_inner))

  let serialize serialize_inner m =
    M.to_seq m |> Seq.map (fun (k, v) -> (k, serialize_inner v)) |> List.of_seq

  let to_syn to_syn_inner m =
    M.to_list m
    |> List.concat_map (fun (k, v) ->
           let k = Key.to_syn k in
           List.map (fun v -> (k, v)) (to_syn_inner v))

  let subst_serialized subst_inner subst_var l =
    List.map
      (fun (key, v) -> (Key.subst subst_var key, subst_inner subst_var v))
      l

  let iter_vars_serialized iter_inner l f =
    List.iter
      (fun (key, v) ->
        Key.iter_vars key f;
        iter_inner v f)
      l

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
      Symex.assume [ Key.distinct (key :: (M.bindings st |> List.map fst)) ]
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
    let+ () = Symex.assume [ Key.distinct @@ List.map fst @@ M.bindings st ] in
    Compo_res.Ok (out_keys, to_opt st)

  let wrap (f : 'a option -> ('b * 'a option, 'err, 'fix list) Symex.Result.t)
      (key : Key.t) (st : 'a t option) :
      ('b * 'a t option, 'err, 'fix syn list) Symex.Result.t =
    let st = of_opt st in
    let* key, codom = Find_opt_sym.f key st in
    let++ res, codom = f codom |> lift_fix_s ~key in
    (res, to_opt (add_opt key codom st))

  let produce
      (prod :
        'inner_syn -> 'inner_st option -> 'inner_st option Symex.Producer.t)
      (syn : 'inner_syn syn) (st : 'inner_st t option) :
      'inner_st t option Symex.Producer.t =
    let open Symex in
    let open Symex.Producer.Syntax in
    let key, inner_syn = syn in
    let st = of_opt st in
    let* key = Producer.apply_subst Key.subst key in
    let* key, codom = Producer.lift (Find_opt_sym.f key st) in
    let+ codom = prod inner_syn codom in
    let st = add_opt key codom st in
    to_opt st

  let fold
      (f : 'acc -> Key.t * 'a -> ('acc, 'err, 'fix serialized) Symex.Result.t)
      (init : 'acc) (st : 'a t option) :
      ('acc, 'err, 'fix serialized) Symex.Result.t =
    let st = of_opt st in
    Result.fold_seq (M.to_seq st) ~init ~f
end

module Make (Symex : Symex.Base) (Key : KeyS(Symex).S) = struct
  open Symex.Syntax
  module M' = Stdlib.Map.Make (Key)

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (key : Key.t) (st : 'a M'.t) =
    let rec find_bindings = function
      | [] -> Symex.return (key, None)
      | (k, v) :: tl ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an if. *)
    in
    match M'.find_opt key st with
    | Some v -> Symex.return (key, Some v)
    | None -> find_bindings (M'.bindings st)

  include
    Build_from_find_opt_sym (Symex) (Key)
      (struct
        let f = find_opt_sym
      end)
end

module Direct_access (Symex : Symex.Base) (Key : KeyS(Symex).S) = struct
  open Symex.Syntax
  module M' = Stdlib.Map.Make (Key)

  let find_opt_sym (key : Key.t) (st : 'a M'.t) =
    let rec find_bindings = function
      | [] -> Symex.vanish ()
      | (k, v) :: tl ->
          if%sat Key.sem_eq key k then Symex.return (k, Some v)
          else find_bindings tl
    in
    let* key = Key.simplify key in
    match M'.find_opt key st with
    | Some v -> Symex.return (key, Some v)
    | None ->
        let not_in_map =
          M'.to_seq st |> Seq.map fst |> List.of_seq |> Key.distinct
        in
        if%sat1 not_in_map then Symex.return (key, None)
        else find_bindings (M'.bindings st)

  include
    Build_from_find_opt_sym (Symex) (Key)
      (struct
        let f = find_opt_sym
      end)
end

(** Only sound to use if the keys of the map are invariant under interpretations
    of the symbolic variables *)
module Concrete (Symex : Symex.Base) (Key : Soteria_std.Ordered_type.S) = struct
  module Key = Mk_concrete_key (Symex) (Key)
  module M' = Stdlib.Map.Make (Key)

  let find_opt_sym (key : Key.t) (st : 'a M'.t) =
    Symex.return (key, M'.find_opt key st)

  include
    Build_from_find_opt_sym (Symex) (Key)
      (struct
        let f = find_opt_sym
      end)
end
