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
  S(Symex)(Key).S with type codom := Codom.t and type codom_syn := Codom.syn =
struct
  include S_map

  type t = Codom.t S_map.t
  type syn = Key.syn * Codom.syn

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  open SM
  open SM.Syntax

  let lift_fix ~key (fix : Codom.syn list) : syn list =
    List.map (fun v_ser -> (Key.to_syn key, v_ser)) fix

  let lift_fixes ~key (fix : Codom.syn list list) : syn list list =
    List.map (lift_fix ~key) fix

  let pp_syn : Format.formatter -> syn -> unit =
    Fmt.Dump.(pair Key.pp_syn Codom.pp_syn)

  let show_syn = Fmt.to_to_string pp_syn

  let ins_outs ((k, c) : syn) =
    let k_ins = Key.exprs_syn k in
    let codom_ins, codom_outs = Codom.ins_outs c in
    (k_ins @ codom_ins, codom_outs)

  let to_syn m =
    syntactic_bindings m
    |> Seq.concat_map (fun (k, v) ->
        List.to_seq (Codom.to_syn v)
        |> Seq.map (fun v_ser -> (Key.to_syn k, v_ser)))
    |> List.of_seq

  let pp' ?(codom = Codom.pp) ?key ?ignore = pp' ?key ?ignore codom
  let pp ft t = pp' ft t
  let show = Fmt.to_to_string pp
  let of_opt = function None -> empty | Some m -> m
  let to_opt m = if is_empty m then None else Some m
  let show = Fmt.to_to_string pp
  let of_opt = function None -> empty | Some m -> m
  let to_opt m = if is_empty m then None else Some m

  let alloc ~(new_codom : Codom.t) : (Key.t, 'err, syn list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let*^ key = Key.fresh () in
    let* () =
      SM.assume
        [
          Key.distinct
            (key :: (syntactic_bindings st |> Seq.map fst |> List.of_seq));
        ]
        [
          Key.distinct
            (key :: (syntactic_bindings st |> Seq.map fst |> List.of_seq));
        ]
    in
    let+ () = SM.set_state (to_opt (syntactic_add key new_codom st)) in
    let+ () = SM.set_state (to_opt (syntactic_add key new_codom st)) in
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
    let st =
      Seq.fold_left (fun st (k, v) -> syntactic_add k v st) st bindings
    in
    let st =
      Seq.fold_left (fun st (k, v) -> syntactic_add k v st) st bindings
    in
    let* () =
      SM.assume
        [ syntactic_bindings st |> Seq.map fst |> List.of_seq |> Key.distinct ]
        SM.assume
        [ syntactic_bindings st |> Seq.map fst |> List.of_seq |> Key.distinct ]
    in
    let+ () = SM.set_state (to_opt st) in
    Ok out_keys

  let wrap (type a err) (key : Key.t)
      (f : (a, err, Codom.syn list) Codom.SM.Result.t) :
      (a, err, syn list) SM.Result.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let* key, codom = lift @@ find_opt key st in
    let* key, codom = lift @@ find_opt key st in
    let* res, codom = lift @@ f codom in
    match res with
    | Ok v ->
        (* Only update the state in case of success! *)
        let+ () = SM.set_state (to_opt (syntactic_add_opt key codom st)) in
        let+ () = SM.set_state (to_opt (syntactic_add_opt key codom st)) in
        Ok v
    | Error e -> SM.Result.error e
    | Missing fixes -> SM.Result.miss (lift_fixes ~key fixes)

  open Symex

  let produce (syn : syn) (st : Codom.t S_map.t option) :
      Codom.t S_map.t option Producer.t =
    let open Producer.Syntax in
    let st = of_opt st in
    let key, inner_ser = syn in
    let* key = Producer.apply_subst Key.subst key in
    let*^ key, codom = find_opt key st in
    let+ codom = Codom.produce inner_ser codom in
    to_opt (syntactic_add_opt key codom st)

  let consume (syn : syn) (st : st) : (st, syn list) Symex.Consumer.t =
    let open Consumer.Syntax in
    let st = of_opt st in
    let key, inner_ser = syn in
    let* key = Consumer.apply_subst Key.subst key in
    let*^ key, codom = find_opt key st in
    let+ codom =
      let+? fix = Codom.consume inner_ser codom in
      lift_fix ~key fix
    in
    to_opt @@ syntactic_add_opt key codom st
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

    type syn = t [@@deriving show { with_path = false }]

    let to_syn t = t
    let fresh () = failwith "Fresh not implemented for concrete keys"
    let subst _ x = x
    let exprs_syn _ = []

    let learn_eq syn t =
      if compare syn t = 0 then Symex.Consumer.ok ()
      else Symex.Consumer.lfail (Symex.Value.of_bool false)
  end

  include Build (Symex) (Key') (S_map.Concrete (Symex) (Key')) (Codom)
end
