open Symex
open Soteria_std
open Svalue.Infix

(* let log = Logs.L.warn *)
let log _ = ()

module type S = sig
  include Soteria_std.Reversible.Mutable

  (** Simplifies a constraints using the current knowledge base, without
      updating it. *)
  val simplify : t -> Svalue.t -> Svalue.t

  (** Adds a constraint to the current analysis, updating the currently tracked
      data. *)
  val add_constraint : t -> Svalue.t -> Svalue.t * Var.Set.t

  (** Encode all the information relevant to the given variables and conjuncts
      them with the given accumulator. *)
  val encode : ?vars:Var.Hashset.t -> t -> Typed.sbool Typed.t Iter.t
end

module Merge (A1 : S) (A2 : S) : S = struct
  type t = A1.t * A2.t

  let init () = (A1.init (), A2.init ())

  let backtrack_n (a1, a2) n =
    A1.backtrack_n a1 n;
    A2.backtrack_n a2 n

  let save (a1, a2) =
    A1.save a1;
    A2.save a2

  let reset (a1, a2) =
    A1.reset a1;
    A2.reset a2

  let simplify (a1, a2) v = v |> A1.simplify a1 |> A2.simplify a2

  let add_constraint (a1, a2) v =
    let v', vars1 = A1.add_constraint a1 v in
    let v'', vars2 = A2.add_constraint a2 v' in
    (v'', Var.Set.union vars1 vars2)

  let encode ?vars (a1, a2) : Typed.sbool Typed.t Iter.t =
    Iter.append (A1.encode ?vars a1) (A2.encode ?vars a2)
end

module None : S = struct
  type t = unit

  let init () = ()
  let backtrack_n () _ = ()
  let save () = ()
  let reset () = ()
  let simplify () v = v
  let add_constraint () v = (v, Var.Set.empty)
  let encode ?vars:_ () = Iter.empty
end

module Interval : S = struct
  (* we only include stuff from Z we want  *)
  open struct
    let one = Z.one
    let zero = Z.zero
    let pred = Z.pred
    let succ = Z.succ
    let ( - ) = Z.sub
    let ( + ) = Z.add
    let ( < ) = Z.lt
    let ( <= ) = Z.leq
    let ( >= ) = Z.geq
    let ( > ) = Z.gt
    let pow2 n = Z.shift_left Z.one n
    let ( ~- ) size x = pow2 size - x
  end

  let mk_var n v : Svalue.t = Svalue.mk_var v (TBitVector n)
  let mk_var_ty n v : Typed.T.sint Typed.t = Typed.mk_var v (Typed.t_int n)
  let max_for n = Z.(pred (shift_left one n))

  type sign = Pos | Neg

  (** Sign multiplication *)
  let ( ** ) l r = if l = r then Pos else Neg

  let pp_sign fmt = function
    | Pos -> Fmt.string fmt "+"
    | Neg -> Fmt.string fmt "-"

  module Range = struct
    (** A range \[m, n\]; both sides are inclusive. Because we deal with
        bitvectors, we always have an upper and lower bound! *)
    type t = Z.t * Z.t

    let pp fmt (m, n) = Fmt.pf fmt "[%s; %s]" (Z.format "x" m) (Z.format "x" n)
    let pps fmt (sign, range) = Fmt.pf fmt "%a%a" pp_sign sign pp range
    let is_empty (m, n) : bool = m > n
    let default b = (zero, max_for b)

    (** The intersection of two ranges; always representable *)
    let intersect (m1, n1) (m2, n2) = (Z.max m1 m2, Z.min n1 n2)

    (** The union of two ranges; representable but OX *)
    let union (m1, n1) (m2, n2) = (Z.min m1 m2, Z.max n1 n2)

    let eq (m1, n1) (m2, n2) = Z.equal m1 m2 && Z.equal n1 n2

    let cmp (m1, n1) (m2, n2) =
      let c = Z.compare m1 m2 in
      if c <> 0 then c else Z.compare n1 n2
  end

  (** The interval data for a given variable; consists of one positive range,
      and a list of negative ranges that apply to it.

      See https://ceur-ws.org/Vol-1617/paper8.pdf *)
  module Data = struct
    type t = { pos : Range.t; negs : Range.t list; size : int }
    [@@deriving show { with_path = false }]

    let mk n = { pos = Range.default n; negs = []; size = n }

    (** Equality of two datas *)
    let equal d1 d2 =
      Range.eq d1.pos d2.pos
      && List.length d1.negs = List.length d2.negs
      (* this is ok because we sort negs *)
      && List.for_all2 Range.eq d1.negs d2.negs

    let apply_negs size pos negs =
      (* See [3.1] in https://ceur-ws.org/Vol-1617/paper8.pdf *)
      let pos, negs =
        List.fold_left
          (fun (((l, u) as pos), negs) ((a, b) as neg) ->
            if a > u || b < l then (pos, negs)
            else if a <= l then
              let neg_pos = (succ b, pow2 Stdlib.(size - 1)) in
              (Range.intersect pos neg_pos, negs)
            else if b >= u then
              let neg_pos = (zero, pred a) in
              (Range.intersect pos neg_pos, negs)
            else (pos, neg :: negs))
          (pos, []) negs
      in
      (pos, List.sort_uniq Range.cmp negs)

    (** Incorporates a positive range into this data, by using intersection.
        Returns the updated data, and whether the added range is redundant. *)
    let add_pos data r =
      let pos = Range.intersect data.pos r in
      if pos = data.pos then (data, true)
      else
        let pos, negs = apply_negs data.size pos data.negs in
        ({ data with pos; negs }, false)

    (** Incorporates a negative range into this data, by using set difference.
        Returns the updated data, and whether the added range is redundant. *)
    let add_neg data r =
      let pos, negs = apply_negs data.size data.pos (r :: data.negs) in
      if Range.eq pos data.pos && List.length negs = List.length data.negs then
        (data, true)
      else ({ data with pos; negs }, false)

    let add data (sign, range) =
      match sign with Pos -> add_pos data range | Neg -> add_neg data range

    (** Whether the interval data represents an empty set. *)
    let is_empty data =
      (* TODO: take into account the negative ranges! Maybe we can do a small optimisation
         where we automatically clear data and set a RangeData to be empty when we find it
         is after an add_pos/add_neg, rather than computing it here too. *)
      Range.is_empty data.pos

    (** Whether the data represents a singleton set. *)
    let is_singleton { pos = m, n; _ } = Z.equal m n

    (** The union of two data sets; overapproximates. *)
    let union d1 d2 =
      let pos = Range.union d1.pos d2.pos in
      (* we grossly overapproximate, and only keep negations that are in both *)
      let negs =
        List.filter (fun neg -> List.exists (Range.eq neg) d2.negs) d1.negs
      in
      { pos; negs; size = d1.size }

    (** [iter_sval_equivalent v d] returns an iterator over the set of symbolic
        values to encode the data [d] for variable [v]. *)
    let iter_sval_equivalent v { pos = m, n; negs; size } f =
      let open Typed.Infix in
      let bv = Typed.BitVec.mk size in
      let var = mk_var_ty size v in
      if Z.equal m n then f (var ==@ bv m)
      else (
        if not (Z.equal m Z.zero) then f (bv m <=@ var);
        if not (Z.equal n (max_for size)) then f (var <=@ bv n);

        negs
        |> List.iter @@ fun (m, n) ->
           if Z.equal m n then f (Typed.not (var ==@ bv m))
           else f (Typed.not (bv m <=@ var &&@ (var <=@ bv n))))
  end

  type st = Data.t Var.Map.t

  include Reversible.Make_mutable (struct
    type t = st

    let default = Var.Map.empty
  end)

  let get n v st =
    match Var.Map.find_opt v st with Some r -> r | None -> Data.mk n

  let st_equal = Var.Map.equal Data.equal
  let merge_states = Var.Map.merge (fun _ -> Option.merge Data.union)

  let pp ft st =
    Fmt.(iter_bindings Var.Map.iter (pair ~sep:(any " -> ") Var.pp Data.pp))
      ft st

  (** [update st var size (sign, range)] Updates state [st] for variable [var],
      with size [size], by adding to it the range [range] (this range is
      positive if [sign = Pos], negative otherwise).

      Returns [(learnt, dirty, st)]: [learnt] is the constraint to be added to
      the PC (e.g. [false] if this constraint is unfeasible), [dirty] is to mark
      variables whose range has changed and for which a new SAT check may be
      needed, and [st] is the new state. *)
  let update st var size new_range =
    let range = get size var st in
    let range', redundant = Data.add range new_range in
    if redundant then (
      log (fun m ->
          m "Useless range  %a: %a %a = %a" Var.pp var Data.pp range Range.pps
            new_range Data.pp range');
      let is_ok = not (Data.is_empty range) in
      (Svalue.Bool.bool is_ok, Var.Set.empty, st))
    else
      let st = Var.Map.add var range' st in
      log (fun m ->
          m "New range %a: %a %a@.  = %a" Var.pp var Data.pp range Range.pps
            new_range Data.pp range');
      if Data.is_singleton range' then
        (* We narrowed the range to one value! *)
        let const = Svalue.BitVec.mk size (fst range'.pos) in
        let var = mk_var size var in
        let eq = const ==@ var in
        (eq, Var.Set.empty, st)
      else if Data.is_empty range' then
        (* The range is empty, so this cannot be true *)
        (Svalue.Bool.v_false, Var.Set.empty, st)
      else
        (* We could cleanly absorb the range, so the PC doesn't need to store it -- however
           we must mark this variable as dirty, as maybe the modified range still renders
           the branch infeasible, e.g. because of some additional PC assertions. *)
        (Svalue.Bool.v_true, Var.Set.singleton var, st)

  let rec as_range (v : Svalue.t) =
    match v.node.kind with
    (* For the inequalities, see https://ceur-ws.org/Vol-1617/paper8.pdf *)
    (*
       Case 2: c1 <=u c2 + x
       • c1 < c2 => ~[ -c2; c1 - c2 - 1 ]
       • c1 >= c2 => [ c1 - c2; -c2 - 1 ]
    *)
    | Binop
        ( ((Lt false | Leq false) as bop),
          { node = { kind = BitVec c1; ty = TBitVector size }; _ },
          {
            node =
              {
                kind =
                  ( Var v
                  | Binop
                      ( Add _,
                        { node = { kind = Var v; _ }; _ },
                        { node = { kind = BitVec _; _ }; _ } )
                  | Binop
                      ( Add _,
                        { node = { kind = BitVec _; _ }; _ },
                        { node = { kind = Var v; _ }; _ } ) ) as rhs;
                _;
              };
            _;
          } ) ->
        let c1 = if bop = Lt false then Z.succ c1 else c1 in
        let c2 =
          match rhs with
          | Var _ -> Z.zero
          | Binop (Add _, { node = { kind = BitVec c2; _ }; _ }, _)
          | Binop (Add _, _, { node = { kind = BitVec c2; _ }; _ }) ->
              c2
          | _ -> failwith "unreachable"
        in
        if c1 < c2 then Some (v, size, (Neg, (~-size c2, c1 - c2 - one)))
        else Some (v, size, (Pos, (c1 - c2, ~-size c2 - one)))
    (*
       Case 3: c1 + x <=u c2
       • c1 <= c2 => ~[ c2 - c1 + 1; -c1 - 1 ]
       • c1 > c2 => [ -c1; -c1 + c2 ]
    *)
    | Binop
        ( ((Lt false | Leq false) as bop),
          {
            node =
              {
                kind =
                  ( Var v
                  | Binop
                      ( Add _,
                        { node = { kind = Var v; _ }; _ },
                        { node = { kind = BitVec _; _ }; _ } )
                  | Binop
                      ( Add _,
                        { node = { kind = BitVec _; _ }; _ },
                        { node = { kind = Var v; _ }; _ } ) ) as lhs;
                _;
              };
            _;
          },
          { node = { kind = BitVec c2; ty = TBitVector size }; _ } ) ->
        let c1 =
          match lhs with
          | Var _ -> Z.zero
          | Binop (Add _, { node = { kind = BitVec c1; _ }; _ }, _)
          | Binop (Add _, _, { node = { kind = BitVec c1; _ }; _ }) ->
              c1
          | _ -> failwith "unreachable"
        in
        let c2 = if bop = Lt false then Z.pred c2 else c2 in
        if c1 <= c2 then Some (v, size, (Neg, (c2 - c1 + one, ~-size one)))
        else Some (v, size, (Pos, (~-size c1, ~-size c1 + c2)))
    (*
       Case 4: x <=s c1
       • c1 < 2^{n-1} => ~[ c1 + 1; 2^{n-1} - 1 ]
       • c1 >= 2^{n-1} => [ 2^{n-1}; c1 ]
    *)
    | Binop
        ( ((Lt true | Leq true) as binop),
          { node = { kind = Var v; _ }; _ },
          { node = { kind = BitVec c1; ty = TBitVector size }; _ } ) ->
        let c1 = if binop = Lt true then Z.pred c1 else c1 in
        let mid = pow2 Stdlib.(size - 1) in
        if c1 < mid then Some (v, size, (Neg, (c1 + one, mid - one)))
        else Some (v, size, (Pos, (mid, c1)))
    (*
       Case 5: c1 <=s x
       • c1 < 2^{n-1} => [ c1; 2^{n-1} - 1 ]
       • c1 >= 2^{n-1} => ~[ 2^{n-1}; c1 - 1 ]
    *)
    | Binop
        ( ((Lt true | Leq true) as binop),
          { node = { kind = BitVec c1; ty = TBitVector size }; _ },
          { node = { kind = Var v; _ }; _ } ) ->
        let c1 = if binop = Lt true then Z.succ c1 else c1 in
        let mid = Z.shift_left Z.one Stdlib.(size - 1) in

        if c1 < mid then Some (v, size, (Pos, (c1, mid - one)))
        else Some (v, size, (Neg, (mid, c1 - one)))
    (* Simple equality *)
    | Binop
        ( Eq,
          { node = { kind = BitVec x; ty = TBitVector size }; _ },
          { node = { kind = Var v; _ }; _ } )
    | Binop
        ( Eq,
          { node = { kind = Var v; _ }; _ },
          { node = { kind = BitVec x; ty = TBitVector size }; _ } ) ->
        Some (v, size, (Pos, (x, x)))
    (* Custom rules *)
    | Binop
        ( MulOvf false,
          { node = { kind = Var v; ty = TBitVector size }; _ },
          { node = { kind = BitVec x; _ }; _ } )
    | Binop
        ( MulOvf false,
          { node = { kind = BitVec x; _ }; _ },
          { node = { kind = Var v; ty = TBitVector size }; _ } ) ->
        (* if x === 0, then this is always false *)
        if Z.equal x Z.zero then Some (v, size, (Neg, (Z.zero, max_for size)))
        else
          (* v *uovf x <=> v >= ceil(2^n / x) *)
          let pow2n = pow2 size in
          let bound =
            if Z.divisible pow2n x then Z.div pow2n x
            else Z.succ (Z.div pow2n x)
          in
          Some (v, size, (Pos, (bound, max_for size)))
    (* This only works for a single fact; we can't apply this to [!(A && B)], since that's
       a disjunction! *)
    | Unop (Not, v) ->
        Option.map
          (fun (v1, size, (sign, range)) -> (v1, size, (Neg ** sign, range)))
          (as_range v)
    | _ -> None

  (** [add_constraint ?sign v st] Adds a constraint [v] to the state [st].
      [sign] is the sign of the constraint (by default [Pos]; [Neg] indicates
      we're in a negation).

      Returns [(simp, learnt, dirty, st)]: [simp] is the simplified constraint
      (which may be [true] if the constraint was entirely absorbed, or [false]
      if it was deemed unfeasible), [learnt] is additional facts learnt from the
      simplified formula, [dirty] is the set of variables whose ranges changed,
      and [st] is the updated state. *)
  let rec add_constraint (v : Svalue.t) st :
      Svalue.t * Svalue.t * Var.Set.t * st =
    match (v.node.kind, lazy (as_range v)) with
    | Binop (And, v1, v2), _ ->
        let v1', learnt1, vars1, st' = add_constraint v1 st in
        let v2', learnt2, vars2, st'' = add_constraint v2 st' in

        log (fun m ->
            m "%a && %a => %a && %a" Svalue.pp v1 Svalue.pp v2 Svalue.pp v1'
              Svalue.pp v2');
        (v1' &&@ v2', learnt1 &&@ learnt2, Var.Set.union vars1 vars2, st'')
    | _, (lazy (Some (var, size, srange))) ->
        let learnt, vars, st' = update st var size srange in
        (Svalue.Bool.v_true, learnt, vars, st')
    | _, (lazy None) -> (v, Svalue.Bool.v_true, Var.Set.empty, st)

  let simplify (v : Svalue.t) st =
    match (v.node.kind, lazy (as_range v)) with
    | Binop (Or, v1, v2), _ ->
        let v1', _learnt1, vars1, st1 = add_constraint v1 st in
        let v2', _learnt2, vars2, st2 = add_constraint v2 st in
        let st' = merge_states st1 st2 in
        let vars = Var.Set.union vars1 vars2 in
        log (fun m ->
            m "checking %a / %a / %a /@.1. %a@.2. %a"
              Fmt.(list Var.pp)
              (Var.Set.to_list vars) Svalue.pp v1 Svalue.pp v2 pp st pp st');
        (* If the state is unchanged, then the conjunction covers all possible states and
           this is always true. (I think.) *)
        if
          Svalue.equal v1' Svalue.Bool.v_true
          && Svalue.equal v2' Svalue.Bool.v_true
          && st_equal st st'
        then Svalue.Bool.v_true
        else v
    | _, (lazy (Some (var, size, srange))) ->
        let range = get size var st in
        log (fun m ->
            m "Simplify range %a (curr %a) for %a" Range.pps srange Data.pp
              range pp st);
        let range', redundant = Data.add range srange in
        if redundant then Svalue.Bool.v_true
        else if Data.is_empty range' then Svalue.Bool.v_false
        else v
    | _, (lazy None) -> v

  let add_constraint v st =
    log (fun m -> m "Adding constraint: %a" Svalue.pp v);
    let v', learnt, vars, st' = add_constraint v st in
    if v <> v' || not (Var.Set.is_empty vars) then
      log (fun m ->
          m "Change: %a -> %a (%a)@." Svalue.pp v Svalue.pp v'
            Fmt.(list ~sep:(any ", ") Var.pp)
            (Var.Set.to_list vars))
    else log (fun m -> m "No change.@.");
    ((v' &&@ learnt, vars), st')

  let simplify st v = wrap_read (simplify v) st
  let add_constraint st v = wrap (add_constraint v) st

  let encode ?vars st : Typed.sbool Typed.t Iter.t =
    let to_check =
      Option.fold ~none:(fun _ -> true) ~some:Var.Hashset.mem vars
    in
    wrap_read
      (fun m f ->
        Var.Map.iter
          (fun v r -> if to_check v then Data.iter_sval_equivalent v r f)
          m)
      st
end
