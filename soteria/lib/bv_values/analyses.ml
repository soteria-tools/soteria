open Symex
open Soteria_std

(* let log = Soteria.Logging.Logs.L.warn *)
let log _ = ()

module type S = sig
  include Soteria_std.Reversible.Mutable

  val simplify : t -> Svalue.t -> Svalue.t
  val add_constraint : t -> Svalue.t -> Svalue.t * Var.Set.t
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

module Interval (Signed : sig
  val signed : bool
end) : S = struct
  open Svalue.Infix

  let mk_var n v : Svalue.t = Svalue.mk_var v (TBitVector n)
  let mk_var_ty n v : Typed.T.sint Typed.t = Typed.mk_var v (Typed.t_int n)

  module Range = struct
    (** A range \[m, n\] for a bitwidth; both sides are inclusive. Because we
        deal with bitvectors, we always have an upper and lower bound! *)
    type t = Z.t * Z.t * int

    let pp fmt (m, n, b) =
      Fmt.pf fmt "[%s; %s]"
        (Z.format ("0" ^ string_of_int (b / 4) ^ "x") m)
        (Z.format ("0" ^ string_of_int (b / 4) ^ "x") n)

    let max_for =
      if Signed.signed then fun n ->
        let n = n - 1 in
        Z.(pred (shift_left one n))
      else fun n -> Z.(pred (shift_left one n))

    let min_for =
      if Signed.signed then fun n -> Z.(neg (max_for n) - one)
      else fun _ -> Z.zero

    let overflowed (m, n, b) = Z.Compare.(m > max_for b || n < min_for b)
    let map2 f (m1, n1, b) (m2, n2, _) = (f m1 m2, f n1 n2, b)
    let is_empty (m, n, _) : bool = Z.gt m n
    let default b = (min_for b, max_for b, b)

    (** [iter_sval_equivalent v r] returns an iterator over the set of symbolic
        values that are represented by [v] spanning over the range [r] *)
    let iter_sval_equivalent v (m, n, b) =
      let open Typed.Infix in
      fun f ->
        if Z.equal m n then f (mk_var_ty b v ==@ Typed.BitVec.mk_masked b m)
        else (
          if not (Z.equal m (min_for b)) then
            f (Typed.BitVec.mk_masked b m <=@ mk_var_ty b v);
          if not (Z.equal n (max_for b)) then
            f (mk_var_ty b v <=@ Typed.BitVec.mk_masked b n))

    (** The intersection of two ranges; always representable *)
    let intersect ((m1, n1, b) : t) ((m2, n2, _) : t) : t =
      (Z.max m1 m2, Z.min n1 n2, b)

    (** The union of two ranges; representable but OX *)
    let union ((m1, n1, b) : t) ((m2, n2, _) : t) : t =
      (Z.min m1 m2, Z.max n1 n2, b)

    (** The difference [r1 / r2] of two ranges; this is tricky: if [r2] is
        somewhere inside the [r1] without touching its edges (e.g.
        [[0, 5] / [2, 3]]), we cannot compute an appropriate range and must
        overapproximate to [r1].

        This is ok here, as long as we don't simplify the assertion, as the
        solver will still have all information. FIXME: is the above true? *)
    let diff ((m1, n1, b) as range : t) ((m2, n2, _) : t) : t option =
      (* [m, n] \ {m} = [m+1, n] *)
      if Z.equal m2 n2 && Z.equal m1 m2 then Some (Z.succ m1, n1, b)
        (* [m, n] \ {n} = [m, n-1] *)
      else if Z.equal m2 n2 && Z.equal n1 m2 then Some (m1, Z.pred n1, b)
        (* l \ r = l; nothing to do *)
      else if Z.gt m1 n2 || Z.lt n1 m2 then Some range
        (* can't do anything, so we stay OX and don't modify the range *)
      else None
  end

  include Reversible.Make_mutable (struct
    type t = Range.t Var.Map.t

    let default : t = Var.Map.empty
  end)

  let pp_binding ft (k, v) = Fmt.pf ft "%a: %a" Var.pp k Range.pp v
  let pp = Fmt.(iter_bindings Var.Map.iter pp_binding)

  (** Union of two interval mappings, doing the union of the intervals *)
  let st_union = Var.Map.merge (fun _ -> Option.map2 Range.union)

  let get n v st =
    match Var.Map.find_opt v st with Some r -> r | None -> Range.default n

  (** [to_range n v st] Calculates the range of an Svalue, overapproximating to
      the full range of values over [n] bits if needed. Assumes [v] is of type
      [TBitVec]. *)
  let rec to_range n (v : Svalue.t) st : Range.t =
    let to_range v = to_range n v st in
    match v.node.kind with
    | Var var -> get n var st
    | BitVec bv ->
        let z = Svalue.BitVec.bv_to_z Signed.signed n bv in
        (z, z, n)
    | Binop (Add, l, r) ->
        let lr = to_range l in
        let rr = to_range r in
        let res = Range.map2 Z.add lr rr in
        if Range.overflowed res then Range.default n else res
    | Binop (Sub, l, r) ->
        let lr = to_range l in
        let rr = to_range r in
        let res = Range.map2 Z.sub lr rr in
        if Range.overflowed res then Range.default n else res
    | Binop (Rem false, _, r) ->
        (* assuming the rhs is not zero; *)
        let nl, nr, _ = to_range r in
        let max = Z.(pred @@ max (abs nl) (abs nr)) in
        (Z.zero, max, n)
    | Binop (Rem true, _, r) | Binop (Mod, _, r) ->
        (* assuming the rhs is not zero; *)
        let nl, nr, _ = to_range r in
        let max = Z.(pred @@ max (abs nl) (abs nr)) in
        (Z.neg max, max, n)
    | Ite (_, l, r) ->
        let lr = to_range l in
        let rr = to_range r in
        Range.union lr rr
    | _ -> Range.default n

  (** [simplify st v ] simplifies the constraint [v], without learning anything,
      using the current knowledge base. *)
  let rec simplify (v : Svalue.t) st : Svalue.t =
    let simplify v = simplify v st in
    match v.node.kind with
    | Binop (Eq, l, r) -> (
        let l = simplify l in
        let r = simplify r in
        match l.node.ty with
        | TBitVector n ->
            let lr = to_range n l st in
            let rr = to_range n r st in
            if Range.is_empty (Range.intersect lr rr) then Svalue.Bool.v_false
            else Eval.eval_binop Eq l r
        (* defaults *)
        | _ -> Eval.eval_binop Eq l r)
    | Binop (((Lt s | Leq s) as bop), l, r) when s = Signed.signed -> (
        let n = Svalue.size_of l.node.ty in
        let l = simplify l in
        let r = simplify r in
        let lr = to_range n l st in
        let rr = to_range n r st in
        match (bop, lr, rr) with
        | Lt _, (ml, _, _), (_, nr, _) when Z.geq ml nr -> Svalue.Bool.v_false
        | Leq _, (ml, _, _), (_, nr, _) when Z.gt ml nr -> Svalue.Bool.v_false
        | Lt _, (_, nl, _), (mr, _, _) when Z.lt nl mr -> Svalue.Bool.v_true
        | Leq _, (_, nl, _), (mr, _, _) when Z.leq nl mr -> Svalue.Bool.v_true
        (* defaults *)
        | _ -> Eval.eval_binop bop l r)
    | Binop (op, l, r) -> Eval.eval_binop op (simplify l) (simplify r)
    | Unop (op, v) -> Eval.eval_unop op (simplify v)
    | Ite (b, t, e) -> Svalue.Bool.ite (simplify b) (simplify t) (simplify e)
    | _ -> v

  (** [add_constraint ?neg ?absorb v st] Adds a constraint [v] to the state
      [st]. [absorb=false] indicates that we cannot return [true] to indicate
      the assertion now lives in the analysis. [neg=false] indicates we are
      adding a negated constraint, so rather than doing set intersection, we
      need to do set difference. *)
  let rec add_constraint ?(neg = false) ?(absorb = true) (v : Svalue.t) st :
      (Svalue.t * Var.Set.t) * Range.t Var.Map.t =
    let update var ((_, _, b) as range') =
      let range = get b var st in
      let new_range =
        if neg then Range.diff range range'
        else Some (Range.intersect range range')
      in
      match new_range with
      (* We couldn't compute anything from this update *)
      | None -> ((v, Var.Set.empty), st)
      (* We found an inequality, but we learnt nothing from it; we can discard it *)
      | Some new_range when range = new_range ->
          log (fun m ->
              m "Useless range  %a: %a %s %a = %a" Var.pp var Range.pp range
                (if neg then "/" else "∩")
                Range.pp range' Range.pp new_range);
          let is_ok = not (Range.is_empty range) in
          ((Svalue.Bool.bool (is_ok <> neg), Var.Set.empty), st)
      (* We got a new range, but this is a negation, meaning we can' be sure we didn't lose
         some information; to be safe, we let the PC keep the value.
         Also take this case if we do not absorb this information (e.g. in a disjunction),
         as in that case the PC must keep track of the assertion.  *)
      | Some _ when not absorb -> ((v, Var.Set.empty), st)
      | Some new_range -> (
          let st' = Var.Map.add var new_range st in
          log (fun m ->
              m "New range %a: %a %s %a = %a" Var.pp var Range.pp range
                (if neg then "/" else "∩")
                Range.pp range' Range.pp new_range);
          match new_range with
          (* We narrowed the range to one value! *)
          | m, n, _ when Z.equal m n ->
              let const = Svalue.BitVec.mk_masked b m in
              let var = mk_var b var in
              let eq = const ==@ var in
              (* this is hacky; we found the exact value, but we can't return the equality
                 if we're negating, since that equality will otherwise be negated. *)
              (((if neg then Svalue.Bool.not eq else eq), Var.Set.empty), st')
          (* The range is empty, so this cannot be true *)
          | _ when Range.is_empty new_range ->
              ((Svalue.Bool.v_false, Var.Set.empty), st')
          (* We could cleanly absorb the range, so the PC doesn't need to store it -- however
             we must mark this variable as dirty, as maybe the modified range still renders
             the branch infeasible, e.g. because of some additional PC assertions. *)
          | _ -> ((Svalue.Bool.bool (not neg), Var.Set.singleton var), st'))
    in
    match v.node.kind with
    | Binop
        ( ((Lt s | Leq s) as bop),
          { node = { kind = Var v; _ }; _ },
          { node = { kind = BitVec max; ty = TBitVector b }; _ } )
      when s = Signed.signed ->
        let max = Svalue.BitVec.bv_to_z s b max in
        let max = match bop with Lt _ -> Z.pred max | _ -> max in
        update v (Range.min_for b, max, b)
    | Binop
        ( ((Lt s | Leq s) as bop),
          { node = { kind = BitVec min; ty = TBitVector b }; _ },
          { node = { kind = Var v; _ }; _ } )
      when s = Signed.signed ->
        let min = Svalue.BitVec.bv_to_z s b min in
        let min = match bop with Lt _ -> Z.succ min | _ -> min in
        update v (min, Range.max_for b, b)
    | Binop
        ( Eq,
          { node = { kind = BitVec x; ty = TBitVector b }; _ },
          { node = { kind = Var var; _ }; _ } )
    | Binop
        ( Eq,
          { node = { kind = Var var; _ }; _ },
          { node = { kind = BitVec x; ty = TBitVector b }; _ } ) ->
        let x = Svalue.BitVec.bv_to_z Signed.signed b x in
        update var (x, x, b)
    (* We conservatively explore negations; we're only interested in simple (in)equalities *)
    | Unop (Not, nv) ->
        let (nv', vars), st' = add_constraint ~absorb ~neg:(not neg) nv st in
        ((Svalue.Bool.not nv', vars), st')
    (* We don't explore && and || within negations, because that becomes messy. *)
    | Binop (And, v1, v2) when not neg ->
        let (v1', vars1), st' = add_constraint ~absorb v1 st in
        let (v2', vars2), st'' = add_constraint ~absorb v2 st' in
        log (fun m ->
            m "%a && %a => %a && %a" Svalue.pp v1 Svalue.pp v2 Svalue.pp v1'
              Svalue.pp v2');
        if v1' == v && v2' == v then ((v, Var.Set.empty), st)
        else ((v1' &&@ v2', Var.Set.union vars1 vars2), st'')
    | Binop (Or, v1, v2) when not neg ->
        let (v1', vars1), st1 = add_constraint ~absorb:false v1 st in
        let (v2', vars2), st2 = add_constraint ~absorb:false v2 st in
        log (fun m ->
            m "%a || %a => %a || %a" Svalue.pp v1 Svalue.pp v2 Svalue.pp v1'
              Svalue.pp v2');
        ((v1' ||@ v2', Var.Set.union vars1 vars2), st_union st1 st2)
    | _ -> ((v, Var.Set.empty), st)

  (** Simplifies a constraints using the current knowledge base, without
      updating it. *)
  let simplify st v = wrap_read (simplify v) st

  (** Adds a constraint to the current interval analysis, updating the currently
      tracked intervals. *)
  let add_constraint st v =
    wrap
      (fun s ->
        let res, s = add_constraint v s in
        Logs.L.warn (fun m ->
            m "After adding constraint %a, got %a (state %a)" Svalue.pp v
              Svalue.pp (fst res) pp s);
        (res, s))
      st

  (** Encode all the information relevant to the given variables and conjuncts
      them with the given accumulator. *)
  let encode ?vars st : Typed.sbool Typed.t Iter.t =
    let to_check =
      Option.fold ~none:(fun _ -> true) ~some:Var.Hashset.mem vars
    in
    wrap_read
      (fun m ->
        fun f ->
         Var.Map.iter
           (fun v r -> if to_check v then Range.iter_sval_equivalent v r f)
           m)
      st
end

module UInterval = Interval (struct
  let signed = false
end)

module SInterval = Interval (struct
  let signed = true
end)

module Intervals = Merge (UInterval) (SInterval)
