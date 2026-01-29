open Symex
open Compo_res

module SInt_sig (Symex : Symex.Base) = struct
  module type S = sig
    (** Symbolic integers *)
    open Symex

    type t

    include Stdlib.Map.OrderedType with type t := t

    type sbool_v := Value.(sbool t)

    val pp : Format.formatter -> t -> unit
    val sem_eq : t -> t -> sbool_v
    val of_int : int -> t
    val in_range : t -> t * t -> sbool_v
    val greater_or_equal : t -> t -> sbool_v
    val subst : (Var.t -> Var.t) -> t -> t
    val iter_vars : t -> (Var.t * 'a Value.ty -> unit) -> unit
  end
end

module Make
    (Symex : Symex.Base)
    (SInt : SInt_sig(Symex).S)
    (Elem : Base.M(Symex).S) =
struct
  open Symex.Syntax
  module M = Stdlib.Map.Make (SInt)

  type t = Elem.t M.t * SInt.t option

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  type serialized =
    | Ser_binding of (SInt.t * Elem.serialized)
    | Ser_bound of SInt.t

  let no_fix = []
  let[@inline] lift_fix ~ofs (fix : Elem.serialized) = Ser_binding (ofs, fix)
  let[@inline] lift_fixes ~ofs = List.map @@ List.map (lift_fix ~ofs)
  let of_opt = function None -> (M.empty, None) | Some l -> l
  let[@inline] add_opt k v m = M.update k (fun _ -> v) m

  let to_opt (m, b) =
    if Option.is_none b && M.is_empty m then None else Some (m, b)

  let pp' ?(elem = Elem.pp) ft ((m, b) : t) =
    let pp_binding ft (k, v) = Fmt.pf ft "%a -> %a" SInt.pp k elem v in
    Fmt.pf ft "BOUND: %a ; {%a}" (Fmt.Dump.option SInt.pp) b
      (Fmt.list ~sep:(Fmt.any "; ") pp_binding)
      (M.bindings m)

  let pp ft t = pp' ~elem:Elem.pp ft t

  let serialize (m, b) : serialized list =
    let bindings =
      M.to_seq m
      |> Seq.concat_map (fun (k, v) ->
          Elem.serialize v
          |> List.to_seq
          |> Seq.map (fun v -> Ser_binding (k, v)))
      |> List.of_seq
    in
    match b with None -> bindings | Some b -> Ser_bound b :: bindings

  let pp_serialized : Format.formatter -> serialized -> unit =
   fun ft ser ->
    match ser with
    | Ser_binding (k, v) ->
        Fmt.pf ft "Binding: %a -> %a" SInt.pp k Elem.pp_serialized v
    | Ser_bound b -> Fmt.pf ft "Bound: %a" SInt.pp b

  let empty = M.empty

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (ofs : SInt.t) (m : 'a M.t) =
    let rec find_bindings = function
      | [] -> Symex.return (ofs, None)
      | (k, v) :: tl ->
          if%sat SInt.sem_eq ofs k then Symex.return (k, Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an if. *)
    in
    match M.find_opt ofs m with
    | Some v -> Symex.return (ofs, Some v)
    | None -> find_bindings (M.bindings m)

  let assert_in_bounds (ofs : SInt.t) (b : SInt.t option) =
    let cond =
      match b with
      | None -> SInt.greater_or_equal ofs (SInt.of_int 0)
      | Some b -> SInt.in_range ofs (SInt.of_int 0, b)
    in
    SM.lift @@ Symex.assert_or_error cond `OutOfBounds

  let create (size : int) ~(new_codom : Elem.t) : t =
    if size <= 0 then
      raise (Invalid_argument "Plist.create: size must be positive");
    let bindings = Seq.init size (fun i -> (SInt.of_int i, new_codom)) in
    let m = M.of_seq bindings in
    (m, Some (SInt.of_int size))

  open SM
  open SM.Syntax

  let assert_exclusively_owned () : (unit, 'err, serialized list) SM.Result.t =
    let err = SM.Result.miss no_fix in
    let* b_opt = SM.get_state () in
    match of_opt b_opt with
    | _, None -> err
    | m, Some b ->
        if%sat SInt.sem_eq b (SInt.of_int (M.cardinal m)) then Result.ok ()
        else err

  let wrap (ofs : SInt.t)
      (f : ('b, 'err, Elem.serialized list) Elem.SM.Result.t) :
      ('b, 'err, serialized list) SM.Result.t =
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
    let m, b = of_opt st in
    let l, b_ser = serialized in
    let** new_b =
      match (b, b_ser) with
      | None, None -> Result.ok None
      | (Some _ as x), None | None, (Some _ as x) -> Result.ok x
      | Some _, Some _ -> Symex.consume_false ()
    in
    let in_bounds_opt x =
      match new_b with
      | None -> SInt.greater_or_equal x (SInt.of_int 0)
      | Some b -> SInt.in_range x (SInt.of_int 0, b)
    in
    let++ m =
      Symex.Result.fold_list l ~init:m ~f:(fun m (ofs, inner_ser) ->
          let** () = Symex.consume_pure (in_bounds_opt ofs) in
          let* ofs, codom = find_opt_sym ofs m in
          let++ codom =
            let+? fix = cons inner_ser codom in
            lift_fix ~ofs fix
          in
          add_opt ofs codom m)
    in
    to_opt (m, new_b) *)

  let produce (serialized : serialized) : unit SM.t =
    let* st = SM.get_state () in
    let m, b = of_opt st in
    let in_bounds_opt x =
      match b with
      | None -> SInt.greater_or_equal x (SInt.of_int 0)
      | Some b -> SInt.in_range x (SInt.of_int 0, b)
    in
    match serialized with
    | Ser_bound b_ser -> (
        match b with
        | None -> SM.set_state (to_opt (m, Some b_ser))
        | Some _ -> SM.vanish ())
    | Ser_binding (ofs, inner_ser) ->
        let* () = SM.assume [ in_bounds_opt ofs ] in
        let*^ ofs, codom = find_opt_sym ofs m in
        let*^ (), codom = Elem.produce inner_ser codom in
        let m = add_opt ofs codom m in
        SM.set_state (to_opt (m, b))

  let subst_serialized subst_var (l, b) =
    ( List.map
        (fun (k, v) ->
          (SInt.subst subst_var k, Elem.subst_serialized subst_var v))
        l,
      Option.map (SInt.subst subst_var) b )

  let iter_vars_serialized (l, b) f =
    Option.iter (fun b -> SInt.iter_vars b f) b;
    List.iter
      (fun (k, v) ->
        SInt.iter_vars k f;
        Elem.iter_vars_serialized v f)
      l
end
