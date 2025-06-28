open Soteria_std
open Soteria_symex
open Svalue.Infix

(* let log = Soteria_logs.Logs.L.warn *)
let log _ = ()

module type S = sig
  include Soteria_std.Reversible.Mutable

  val add_constraint : t -> Svalue.t -> Svalue.t * Var.Set.t

  val encode :
    ?vars:Var.Hashset.t -> Typed.sbool Typed.t -> t -> Typed.sbool Typed.t
end

module None : S = struct
  type t = unit

  let init () = ()
  let backtrack_n () _ = ()
  let save () = ()
  let reset () = ()
  let add_constraint () v = (v, Var.Set.empty)
  let encode ?vars:_ acc () = acc
end

module Interval : S = struct
  let mk_var v : Svalue.t = Svalue.mk_var v TInt

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

    let to_sval v r : Svalue.t =
      match r with
      | Some m, Some n when Z.equal m n -> mk_var v ==@ Svalue.int_z m
      | Some m, Some n ->
          let var = mk_var v in
          Svalue.int_z m <=@ var &&@ (var <=@ Svalue.int_z n)
      | Some m, None -> Svalue.int_z m <=@ mk_var v
      | None, Some n -> mk_var v <=@ Svalue.int_z n
      | None, None -> Svalue.v_true

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
      | _ -> None
  end

  include Reversible.Make_mutable (struct
    type t = Range.t Var.Map.t

    let default : t = Var.Map.empty
  end)

  (** Union of two interval mappings, doing the union of the intervals *)
  let st_union = Var.Map.merge (fun _ -> Option.map2 Range.union)

  let get v st = Var.Map.find_opt v st |> Option.value ~default:(None, None)

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
      (* We found an inequality, but we learnt nothing from it; we can discard it *)
      | Some new_range when range = new_range ->
          log (fun m ->
              m "Useless range  %a: %a %s %a = %a" Var.pp var Range.pp range
                (if neg then "/" else "∩")
                Range.pp range' Range.pp new_range);
          ((Svalue.bool (not (Range.is_empty range)), Var.Set.empty), st)
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
              (* this is hacky; we found the exact value, but we can't return the equality
                 if we're negating, since that equality will otherwise be negated. *)
              (((if neg then Svalue.not eq else eq), Var.Set.empty), st')
          (* The range is empty, so this cannot be true *)
          | _ when Range.is_empty new_range ->
              ((Svalue.v_false, Var.Set.empty), st')
          (* We got a new range, but this is a negation, meaning we can' be sure we didn't lose
             some information; to be safe, we let the PC keep the value.
             Also take this case if we do not absorb this information (e.g. in a disjunction),
             as in that case the PC must keep track of the assertion.  *)
          | _ when neg || not absorb -> ((v, Var.Set.empty), st')
          (* We could cleanly absorb the range, so the PC doesn't need to store it -- however
             we must mark this variable as dirty, as maybe the modified range still renders
             the branch infeasible, e.g. because of some additional PC assertions. *)
          | _ -> ((Svalue.v_true, Var.Set.singleton var), st'))
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
    (* We conservatively explore negations; we're only interested in simple (in)equalities *)
    | Unop (Not, nv) ->
        let (nv', vars), st' = add_constraint ~absorb ~neg:(not neg) nv st in
        ((Svalue.not nv', vars), st')
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
    (* We can try computing ranges of expressions and guessing truthiness *)
    | Binop (((Eq | Lt | Leq) as bop), l, r) ->
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
          | _ -> (None, None)
        in
        let lr = to_range l in
        let rr = to_range r in
        let res =
          match (bop, lr, rr) with
          | Eq, _, _ when Range.is_empty (Range.intersect lr rr) -> Some false
          | Lt, (Some ml, _), (_, Some nr) when Z.geq ml nr -> Some false
          | Leq, (Some ml, _), (_, Some nr) when Z.gt ml nr -> Some false
          | _ -> None
        in
        ((Option.fold res ~some:Svalue.bool ~none:v, Var.Set.empty), st)
    | _ -> ((v, Var.Set.empty), st)

  (** Adds a constraint to the current interval analysis, updating the currently
      tracked intervals. *)
  let add_constraint st v = wrap (add_constraint v) st

  (** Encode all the information relevant to the given variables and conjuncts
      them with the given accumulator. *)
  let encode ?vars (acc : Typed.sbool Typed.t) st : Typed.sbool Typed.t =
    let to_check =
      Option.fold ~none:(fun _ -> true) ~some:Var.Hashset.mem vars
    in
    wrap_read
      (fun m ->
        Var.Map.fold
          (fun v r acc -> if to_check v then acc &&@ Range.to_sval v r else acc)
          m (Typed.untyped acc))
      st
    |> Typed.type_
end
