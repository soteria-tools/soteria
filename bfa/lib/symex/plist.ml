module type SInt_sig = sig
  (** Symbolic integers *)

  module Symex : Symex.S
  open Symex

  type t

  include Stdlib.Map.OrderedType with type t := t

  val pp : t Fmt.t
  val sem_eq : t -> t -> Value.t
  val of_int : int -> t
  val in_range : t -> t * t -> Value.t
  val greater_or_equal : t -> t -> Value.t
  val subst : (Var.t -> Var.t) -> t -> t
  val iter_vars : t -> (Var.t * Value.ty -> unit) -> unit
end

module Make (Symex : Symex.S) (SInt : SInt_sig with module Symex = Symex) =
struct
  open Symex.Syntax
  open Symex
  module M = Stdlib.Map.Make (SInt)

  type 'a t = 'a M.t * SInt.t option
  type 'a serialized = (SInt.t * 'a) list * SInt.t option

  let of_opt = function None -> (M.empty, None) | Some l -> l
  let add_opt k v m = M.update k (fun _ -> v) m

  let to_opt (m, b) =
    if Option.is_none b && M.is_empty m then None else Some (m, b)

  let pp pp_value ft ((m, b) : 'a t) =
    let pp_binding ft (k, v) = Fmt.pf ft "%a -> %a" SInt.pp k pp_value v in
    Fmt.pf ft "BOUND: %a ; {%a}" (Fmt.Dump.option SInt.pp) b
      (Fmt.list ~sep:(Fmt.any "; ") pp_binding)
      (M.bindings m)

  let serialize serialize_inner (m, b) =
    let bindings =
      M.to_seq m |> Seq.map (fun (k, v) -> (k, serialize_inner v))
    in
    (List.of_seq bindings, b)

  let pp_serialized pp_inner : 'a serialized Fmt.t =
   fun ft (l, b) ->
    let m = M.of_seq (List.to_seq l) in
    pp pp_inner ft (m, b)

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
    if%sat Symex.Value.not cond then Result.error `OutOfBounds else Result.ok ()

  let create (size : int) ~(new_codom : 'a) : 'a t =
    if size <= 0 then
      raise (Invalid_argument "Plist.create: size must be positive");
    let bindings = Seq.init size (fun i -> (SInt.of_int i, new_codom)) in
    let m = M.of_seq bindings in
    (m, Some (SInt.of_int size))

  let assert_exclusively_owned (b_opt : 'a t option) =
    let err = Result.error `MissingResource in
    match of_opt b_opt with
    | _, None -> err
    | m, Some b ->
        if%sat SInt.sem_eq b (SInt.of_int (M.cardinal m)) then Result.ok ()
        else err

  let wrap (f : 'a option -> ('b * 'a option, 'err) Symex.Result.t)
      (ofs : SInt.t) (t_opt : 'a t option) :
      ('b * 'a t option, 'err) Symex.Result.t =
    let m, b = of_opt t_opt in
    let** () = assert_in_bounds ofs b in
    let* ofs, sst = find_opt_sym ofs m in
    let++ res, sst' = f sst in
    (* Should I check for emptyness here? *)
    (res, to_opt (add_opt ofs sst' m, b))

  let consume
      (cons :
        'inner_serialized ->
        'inner_st option ->
        ('inner_st option, 'err) Symex.Result.t)
      (serialized : 'inner_serialized serialized) (st : 'inner_st t option) =
    let m, b = of_opt st in
    let l, b_ser = serialized in
    let* new_b =
      match (b, b_ser) with
      | None, None -> Symex.return None
      | (Some _ as x), None | None, (Some _ as x) -> Symex.return x
      | Some _, Some _ -> Symex.vanish ()
    in
    let in_bounds_opt x =
      match new_b with
      | None -> SInt.greater_or_equal x (SInt.of_int 0)
      | Some b -> SInt.in_range x (SInt.of_int 0, b)
    in
    let++ m =
      Symex.Result.fold_left l ~init:m ~f:(fun m (ofs, inner_ser) ->
          let* () = Symex.return ~learned:[ in_bounds_opt ofs ] () in
          let* ofs, codom = find_opt_sym ofs m in
          let++ codom = cons inner_ser codom in
          add_opt ofs codom m)
    in
    to_opt (m, new_b)

  let produce
      (prod : 'inner_serialized -> 'inner_st option -> 'inner_st option Symex.t)
      (serialized : 'inner_serialized serialized) (st : 'inner_st t option) =
    let m, b = of_opt st in
    let l, b_ser = serialized in
    let* new_b =
      match (b, b_ser) with
      | None, None -> Symex.return None
      | (Some _ as x), None | None, (Some _ as x) -> Symex.return x
      | Some _, Some _ -> Symex.vanish ()
    in
    let in_bounds_opt x =
      match new_b with
      | None -> SInt.greater_or_equal x (SInt.of_int 0)
      | Some b -> SInt.in_range x (SInt.of_int 0, b)
    in
    let+ m =
      Symex.fold_left l ~init:m ~f:(fun m (ofs, inner_ser) ->
          let* () = Symex.return ~learned:[ in_bounds_opt ofs ] () in
          let* ofs, codom = find_opt_sym ofs m in
          let+ codom = prod inner_ser codom in
          add_opt ofs codom m)
    in
    to_opt (m, new_b)

  let subst_serialized subst_inner subst_var (l, b) =
    ( List.map
        (fun (k, v) -> (SInt.subst subst_var k, subst_inner subst_var v))
        l,
      Option.map (SInt.subst subst_var) b )

  let iter_vars_serialized iter_inner (l, b) f =
    Option.iter (fun b -> SInt.iter_vars b f) b;
    List.iter
      (fun (k, v) ->
        SInt.iter_vars k f;
        iter_inner v f)
      l
end
