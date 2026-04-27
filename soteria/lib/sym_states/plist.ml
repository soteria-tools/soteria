open Soteria_std.Compo_res

module S_int_sig (Symex : Symex.Base) = struct
  module Abstr = Data.Abstr.M (Symex)
  module S_int = Data.S_int.S (Symex)

  (** Symbolic integers *)
  module type S = [%mixins Abstr.S_with_syn + S_int.S + Stdlib.Map.OrderedType]
end

module Make
    (Symex : Symex.Base)
    (S_int : S_int_sig(Symex).S)
    (S_bool : Data.S_bool.S(Symex).S)
    (Elem : Base.M(Symex).S) =
struct
  open Symex.Syntax
  open Data.S_int.Make_syntax (Symex) (S_int)
  open Data.S_bool.Make_syntax (Symex) (S_bool)
  module M = Stdlib.Map.Make (S_int)

  type t = Elem.t M.t * S_int.t option

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  type syn = Ser_binding of (S_int.syn * Elem.syn) | Ser_bound of S_int.syn

  let ins_outs = function
    | Ser_binding (ofs, inner_ser) ->
        let is, os = Elem.ins_outs inner_ser in
        (S_int.exprs_syn ofs @ is, os)
    | Ser_bound b -> ([], S_int.exprs_syn b)

  let no_fix = []

  let[@inline] lift_fix ~ofs (fix : Elem.syn) =
    Ser_binding (S_int.to_syn ofs, fix)

  let[@inline] lift_fixes ~ofs = List.map @@ List.map (lift_fix ~ofs)
  let of_opt = function None -> (M.empty, None) | Some l -> l
  let[@inline] add_opt k v m = M.update k (fun _ -> v) m

  let to_opt (m, b) =
    if Option.is_none b && M.is_empty m then None else Some (m, b)

  let pp' ?(elem = Elem.pp) ft ((m, b) : t) =
    let pp_binding ft (k, v) = Fmt.pf ft "%a -> %a" S_int.pp k elem v in
    Fmt.pf ft "BOUND: %a ; {%a}" (Fmt.Dump.option S_int.pp) b
      (Fmt.list ~sep:(Fmt.any "; ") pp_binding)
      (M.bindings m)

  let pp ft t = pp' ~elem:Elem.pp ft t
  let show = Fmt.to_to_string pp

  let to_syn (m, b) : syn list =
    let bindings =
      M.to_seq m
      |> Seq.concat_map (fun (k, v) ->
          Elem.to_syn v
          |> List.to_seq
          |> Seq.map (fun v -> Ser_binding (S_int.to_syn k, v)))
      |> List.of_seq
    in
    match b with
    | None -> bindings
    | Some b -> Ser_bound (S_int.to_syn b) :: bindings

  let pp_syn : Format.formatter -> syn -> unit =
   fun ft ser ->
    match ser with
    | Ser_binding (k, v) ->
        Fmt.pf ft "Binding: %a -> %a" S_int.pp_syn k Elem.pp_syn v
    | Ser_bound b -> Fmt.pf ft "Bound: %a" S_int.pp_syn b

  let show_syn = Fmt.to_to_string pp_syn
  let empty = M.empty

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (ofs : S_int.t) (m : 'a M.t) =
    let rec find_bindings = function
      | [] -> Symex.return (ofs, None)
      | (k, v) :: tl ->
          if%sat ofs ==@ k then Symex.return (k, Some v) else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an
         if. *)
    in
    match M.find_opt ofs m with
    | Some v -> Symex.return (ofs, Some v)
    | None -> find_bindings (M.bindings m)

  let in_bounds_opt ofs b =
    match b with
    | None -> S_int.of_z Z.zero <=@ ofs
    | Some b -> S_int.of_z Z.zero <=@ ofs &&@ (ofs <@ b)

  let assert_in_bounds (ofs : S_int.t) (b : S_int.t option) =
    SM.assert_or_error (in_bounds_opt ofs b) `OutOfBounds

  let create (size : int) ~(new_codom : Elem.t) : t =
    if size <= 0 then
      raise (Invalid_argument "Plist.create: size must be strictly positive");
    let m =
      Seq.init size (fun i -> (S_int.of_z (Z.of_int i), new_codom)) |> M.of_seq
    in
    (m, Some (S_int.of_z (Z.of_int size)))

  open SM
  open SM.Syntax

  let assert_exclusively_owned () : (unit, 'err, syn list) SM.Result.t =
    let err = SM.Result.miss no_fix in
    let* b_opt = SM.get_state () in
    match of_opt b_opt with
    | _, None -> err
    | m, Some b ->
        if%sat b ==@ S_int.of_z (Z.of_int (M.cardinal m)) then Result.ok ()
        else err

  let wrap (ofs : S_int.t) (f : ('b, 'err, Elem.syn list) Elem.SM.Result.t) :
      ('b, 'err, syn list) SM.Result.t =
    let* t_opt = SM.get_state () in
    let m, b = of_opt t_opt in
    let** () = assert_in_bounds ofs b in
    let*^ ofs, sst = find_opt_sym ofs m in
    let*^ res, sst = f sst in
    match res with
    | Ok v ->
        (* Only update state in case of success *)
        let+ () = SM.set_state (to_opt (add_opt ofs sst m, b)) in
        Ok v
    | Error e -> SM.Result.error e
    | Missing fixes -> SM.Result.miss (lift_fixes ~ofs fixes)

  let consume (syn : syn) (st : SM.st) : (SM.st, syn list) Symex.Consumer.t =
    let open Symex.Consumer.Syntax in
    let m, b = of_opt st in
    match syn with
    | Ser_bound b_ser -> (
        match b with
        | None -> Symex.Consumer.miss [ [ syn ] ]
        | Some v ->
            let+ () = S_int.learn_eq b_ser v in
            to_opt (m, None))
    | Ser_binding (ofs, inner_ser) ->
        let* ofs = Symex.Consumer.apply_subst S_int.subst ofs in
        let* () = Symex.Consumer.assert_pure (in_bounds_opt ofs b) in
        let*^ ofs, codom = find_opt_sym ofs m in
        let+? fix =
          let+ codom = Elem.consume inner_ser codom in
          let m = add_opt ofs codom m in
          to_opt (m, b)
        in
        List.map (lift_fix ~ofs) fix

  let produce (syn : syn) (st : SM.st) : SM.st Symex.Producer.t =
    let open Symex.Producer in
    let open Symex.Producer.Syntax in
    let m, b = of_opt st in
    match syn with
    | Ser_bound b_ser -> (
        match b with
        | None ->
            let+ new_b = Symex.Producer.apply_subst S_int.subst b_ser in
            to_opt (m, Some new_b)
        | Some _ -> vanish ())
    | Ser_binding (ofs, inner_ser) ->
        let* ofs = Symex.Producer.apply_subst S_int.subst ofs in
        let*^ () = Symex.assume [ in_bounds_opt ofs b ] in
        let*^ ofs, codom = find_opt_sym ofs m in
        let+ codom = Elem.produce inner_ser codom in
        let m = add_opt ofs codom m in
        to_opt (m, b)
end
