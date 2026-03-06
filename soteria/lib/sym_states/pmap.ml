open Symex
open Compo_res
open Data
module S = Pmap_intf.M
module Key = Pmap_intf.Key

module Build
    (Symex : Symex.Base)
    (Key : Key(Symex).S)
    (S_map : Data.S_map.S(Symex)(Key).S)
    (Codom : Base.M(Symex).S) :
  S(Symex)(Key).S
    with type codom := Codom.t
     and type codom_serialized := Codom.serialized = struct
  include S_map

  type t = Codom.t S_map.t
  type serialized = Key.t * Codom.serialized

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  open SM
  open SM.Syntax

  let lift_fixes ~key : Codom.serialized list list -> serialized list list =
    List.map (List.map (fun v_ser -> (key, v_ser)))

  let pp_serialized : Format.formatter -> serialized -> unit =
    Fmt.Dump.(pair Key.pp Codom.pp_serialized)

  let show_serialized = Fmt.to_to_string pp_serialized

  let serialize m =
    syntactic_bindings m
    |> Seq.concat_map (fun (k, v) ->
        List.to_seq (Codom.serialize v) |> Seq.map (fun v_ser -> (k, v_ser)))
    |> List.of_seq

  let subst_serialized subst_var (key, v) =
    (Key.subst subst_var key, Codom.subst_serialized subst_var v)

  let iter_vars_serialized (key, v) f =
    Key.iter_vars key f;
    Codom.iter_vars_serialized v f

  let pp' ?(codom = Codom.pp) ?key ?ignore = pp' ?key ?ignore codom
  let pp ft t = pp' ft t
  let show = Fmt.to_to_string pp
  let of_opt = function None -> empty | Some m -> m
  let to_opt m = if is_empty m then None else Some m

  let alloc ~(new_codom : Codom.t) : (Key.t, 'err, serialized list) SM.Result.t
      =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key = lift @@ Key.fresh () in
    let* () =
      SM.assume
        [
          Key.distinct
            (key :: (syntactic_bindings st |> Seq.map fst |> List.of_seq));
        ]
    in
    let+ () = SM.set_state (to_opt (syntactic_add key new_codom st)) in
    Ok key

  let allocs (type a k) ~(fn : a -> Key.t -> (k * Codom.t) Symex.t)
      ~(els : a list) : (k list, 'err, serialized list) SM.Result.t =
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
    let st =
      Seq.fold_left (fun st (k, v) -> syntactic_add k v st) st bindings
    in
    let* () =
      SM.assume
        [ syntactic_bindings st |> Seq.map fst |> List.of_seq |> Key.distinct ]
    in
    let+ () = SM.set_state (to_opt st) in
    Ok out_keys

  let wrap (type a err) (key : Key.t)
      (f : (a, err, Codom.serialized list) Codom.SM.Result.t) :
      (a, err, serialized list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key, codom = lift @@ find_opt key st in
    let* res, codom = lift @@ f codom in
    match res with
    | Ok v ->
        (* Only update the state in case of success! *)
        let+ () = SM.set_state (to_opt (syntactic_add_opt key codom st)) in
        Ok v
    | Error e -> SM.Result.error e
    | Missing fixes -> SM.Result.miss (lift_fixes ~key fixes)

  let produce (serialized : serialized) : unit SM.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let key, inner_ser = serialized in
    let* key, codom = lift @@ find_opt key st in
    let* (), codom = lift @@ Codom.produce inner_ser codom in
    let st = syntactic_add_opt key codom st in
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
end

module Make (Symex : Symex.Base) (Key : Key(Symex).S) =
  Build (Symex) (Key) (S_map.Make (Symex) (Key))

module Make_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
  Build (Symex) (Key) (S_map.Make_patricia_tree (Symex) (Key))

module Direct_access (Symex : Symex.Base) (Key : Key(Symex).S) =
  Build (Symex) (Key) (S_map.Direct_access (Symex) (Key))

module Direct_access_patricia_tree
    (Symex : Symex.Base)
    (Key : Key(Symex).S_patricia_tree) =
  Build (Symex) (Key) (S_map.Direct_access_patricia_tree (Symex) (Key))

module Concrete
    (Symex : Symex.Base)
    (Key : Soteria_std.Ordered_type.S)
    (Codom : Base.M(Symex).S) =
struct
  module Key' = struct
    include S_map.Mk_concrete_key (Symex) (Key)

    let fresh () = failwith "Fresh not implemented for concrete keys"
    let subst _ x = x
    let iter_vars _ _ = ()
  end

  include Build (Symex) (Key') (S_map.Concrete (Symex) (Key')) (Codom)
end
