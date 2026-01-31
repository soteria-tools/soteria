open Soteria_std
open Symex
open Svalue.Infix

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

module Interval : S = struct
  let mk_var v : Svalue.t = Svalue.mk_var v TInt
  let mk_var_ty v : Typed.T.sint Typed.t = Typed.mk_var v Typed.t_int

  module Range = struct
    (** A range \[m, n\]; both sides are inclusive. A [None] in the range
        represents ±∞. *)
    type t = Z.t option * Z.t option

    let pp fmt (m, n) =
      Fmt.pf fmt "[%a; %a]"
        Fmt.(option ~none:(any "∞") Z.pp_print)
        m
        Fmt.(option ~none:(any "∞") Z.pp_print)
        n

    let is_empty : t -> bool = function
      | Some m, Some n -> Z.gt m n
      | _ -> false

    (** [iter_sval_equivalent v r] returns an iterator over the set of symbolic
        values that are represented by [v] spanning over the range [r] *)
    let iter_sval_equivalent v r =
      let open Typed.Infix in
      fun f ->
        match r with
        | Some m, Some n when Z.equal m n -> f (mk_var_ty v ==@ Typed.int_z m)
        | Some m, Some n ->
            let var = mk_var_ty v in
            f (Typed.int_z m <=@ var);
            f (var <=@ Typed.int_z n)
        | Some m, None -> f (Typed.int_z m <=@ mk_var_ty v)
        | None, Some n -> f (mk_var_ty v <=@ Typed.int_z n)
        | None, None -> ()

    (** The intersection of two ranges; always representable *)
    let intersect ((m1, n1) : t) ((m2, n2) : t) : t =
      (Option.merge Z.max m1 m2, Option.merge Z.min n1 n2)

    (** The union of two ranges; representable but OX *)
    let union ((m1, n1) : t) ((m2, n2) : t) : t =
      (Option.map2 Z.min m1 m2, Option.map2 Z.max n1 n2)

    (** The difference [r1 / r2] of two ranges; this is tricky: if [r2] is
        somewhere inside the [r1] without touching its edges (e.g.
        [[0, 5] / [2, 3]]), we cannot compute an appropriate range and must
        overapproximate to [r1].

        This is ok here, as long as we don't simplify the assertion, as the
        solver will still have all information. FIXME: is the above true? *)
    let diff (r1 : t) (r2 : t) : t option =
      match (r1, r2) with
      (* [m, n] \ {m} = [m+1, n] *)
      | (Some m1, n1), (Some m2, Some n2) when Z.equal m2 n2 && Z.equal m1 m2 ->
          Some (Some (Z.succ m1), n1)
      (* [m, n] \ {n} = [m, n-1] *)
      | (m1, Some n1), (Some m2, Some n2) when Z.equal m2 n2 && Z.equal n1 m2 ->
          Some (m1, Some (Z.pred n1))
      | (Some m1, n1), (_, Some n2) when Z.gt m1 n2 -> Some (Some m1, n1)
      | (m1, Some n1), (Some m2, _) when Z.lt n1 m2 -> Some (m1, Some n1)
      | _ -> None
  end

  include Reversible.Make_mutable (struct
    type t = Range.t Var.Map.t

    let default : t = Var.Map.empty
  end)

  (** Union of two interval mappings, doing the union of the intervals *)
  let st_inter = Var.Map.idempotent_inter (fun _ -> Range.union)

  let get v st = Var.Map.find_opt v st |> Option.value ~default:(None, None)

  (** [simplify st v ] simplifies the constraint [v], without learning anything,
      using the current knowledge base. *)
  let rec simplify (v : Svalue.t) st : Svalue.t =
    let simplify v = simplify v st in
    match v.node.kind with
    | Binop (((Eq | Lt | Leq) as bop), l, r) -> (
        let rec to_range (v : Svalue.t) : Range.t =
          match v.node.kind with
          | Var v -> get v st
          | Int i -> (Some i, Some i)
          | Binop (Plus, l, r) ->
              let ml, nl = to_range l in
              let mr, nr = to_range r in
              (Option.map2 Z.add ml mr, Option.map2 Z.add nl nr)
          | Binop (Minus, l, r) ->
              let ml, nl = to_range l in
              let mr, nr = to_range r in
              (Option.map2 Z.sub ml nr, Option.map2 Z.sub nl mr)
          | Binop (Mod, _, r) ->
              (* assuming the rhs is not zero; *)
              let nl, nr = to_range r in
              let max =
                Option.map2 (fun x y -> Z.(pred @@ max (abs x) (abs y))) nl nr
              in
              (Some Z.zero, max)
          | Binop (Rem, _, r) ->
              (* assuming the rhs is not zero; *)
              let nl, nr = to_range r in
              let max =
                Option.map2 (fun x y -> Z.(pred @@ max (abs x) (abs y))) nl nr
              in
              (Option.map Z.neg max, max)
          | _ -> (None, None)
        in
        let l = simplify l in
        let r = simplify r in
        let lr = to_range l in
        let rr = to_range r in
        match (bop, lr, rr) with
        | Eq, _, _ when Range.is_empty (Range.intersect lr rr) -> Svalue.v_false
        | Lt, (Some ml, _), (_, Some nr) when Z.geq ml nr -> Svalue.v_false
        | Leq, (Some ml, _), (_, Some nr) when Z.gt ml nr -> Svalue.v_false
        | Lt, (_, Some nl), (Some mr, _) when Z.lt nl mr -> Svalue.v_true
        | Leq, (_, Some nl), (Some mr, _) when Z.leq nl mr -> Svalue.v_true
        | Leq, (Some ml, Some mr), (Some nr, _)
          when Z.equal ml mr && Z.leq mr nr ->
            Svalue.v_true
        (* defaults *)
        | _ -> Eval.eval_binop bop l r)
    | Binop (op, l, r) -> Eval.eval_binop op (simplify l) (simplify r)
    | Unop (op, v) -> Eval.eval_unop op (simplify v)
    | Ite (b, t, e) -> Svalue.ite (simplify b) (simplify t) (simplify e)
    | _ -> v

  (** [add_constraint ?neg ?absorb v st] Adds a constraint [v] to the state
      [st]. [absorb=false] indicates that we cannot return [true] to indicate
      the assertion now lives in the analysis. [neg=false] indicates we are
      adding a negated constraint, so rather than doing set intersection, we
      need to do set difference. *)
  let rec add_constraint ?(neg = false) ?(absorb = true) (v : Svalue.t) st :
      (Svalue.t * Var.Set.t) * Range.t Var.Map.t =
    let update var range' =
      let range = get var st in
      let new_range =
        if neg then Range.diff range range'
        else Some (Range.intersect range range')
      in
      match new_range with
      (* We couldn't compute anything from this update *)
      | None -> ((v, Var.Set.empty), st)
      (* We found an inequality, but we learnt nothing from it; we can discard
         it *)
      | Some new_range when range = new_range ->
          log (fun m ->
              m "Useless range  %a: %a %s %a = %a" Var.pp var Range.pp range
                (if neg then "/" else "∩")
                Range.pp range' Range.pp new_range);
          let is_ok = not (Range.is_empty range) in
          ((Svalue.bool (is_ok <> neg), Var.Set.empty), st)
      | Some new_range -> (
          let st' = Var.Map.add var new_range st in
          log (fun m ->
              m "New range %a: %a %s %a = %a" Var.pp var Range.pp range
                (if neg then "/" else "∩")
                Range.pp range' Range.pp new_range);
          match new_range with
          (* We narrowed the range to one value! *)
          | Some m, Some n when Z.equal m n ->
              let eq = Svalue.int_z m ==@ mk_var var in
              (* this is hacky; we found the exact value, but we can't return
                 the equality if we're negating, since that equality will
                 otherwise be negated. *)
              (((if neg then Svalue.not eq else eq), Var.Set.empty), st')
          (* The range is empty, so this cannot be true *)
          | _ when Range.is_empty new_range ->
              ((Svalue.v_false, Var.Set.empty), st')
          (* We got a new range, but this is a negation, meaning we can' be sure
             we didn't lose some information; to be safe, we let the PC keep the
             value. Also take this case if we do not absorb this information
             (e.g. in a disjunction), as in that case the PC must keep track of
             the assertion. *)
          | _ when not absorb -> ((v, Var.Set.empty), st')
          (* We could cleanly absorb the range, so the PC doesn't need to store
             it -- however we must mark this variable as dirty, as maybe the
             modified range still renders the branch infeasible, e.g. because of
             some additional PC assertions. *)
          | _ -> ((Svalue.bool (not neg), Var.Set.singleton var), st'))
    in
    match v.node.kind with
    | Binop
        ( ((Lt | Leq) as bop),
          { node = { kind = Var v; _ }; _ },
          { node = { kind = Int max; _ }; _ } ) ->
        let max = if bop = Lt then Z.pred max else max in
        update v (None, Some max)
    | Binop
        ( ((Lt | Leq) as bop),
          { node = { kind = Int min; _ }; _ },
          { node = { kind = Var v; _ }; _ } ) ->
        let min = if bop = Lt then Z.succ min else min in
        update v (Some min, None)
    | Binop
        ( Eq,
          { node = { kind = Int x; _ }; _ },
          { node = { kind = Var var; _ }; _ } )
    | Binop
        ( Eq,
          { node = { kind = Var var; _ }; _ },
          { node = { kind = Int x; _ }; _ } ) ->
        update var (Some x, Some x)
    (* We conservatively explore negations; we're only interested in simple
       (in)equalities *)
    | Unop (Not, nv) ->
        let (nv', vars), st' = add_constraint ~absorb ~neg:(not neg) nv st in
        ((Svalue.not nv', vars), st')
    (* We don't explore && and || within negations, because that becomes
       messy. *)
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
        ((v1' ||@ v2', Var.Set.union vars1 vars2), st_inter st1 st2)
    | _ -> ((v, Var.Set.empty), st)

  (** Simplifies a constraints using the current knowledge base, without
      updating it. *)
  let simplify st v = wrap_read (simplify v) st

  (** Adds a constraint to the current interval analysis, updating the currently
      tracked intervals. *)
  let add_constraint st v = wrap (add_constraint v) st

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
