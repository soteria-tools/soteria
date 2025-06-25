open Soteria_std
open Soteria_symex

(* let log = Soteria_logs.Logs.L.warn *)
let log _ = ()

module Interval = struct
  (** A range \[m, n\]; both sides are inclusive. A [None] in the range
      represents ±∞. *)
  type range = Z.t option * Z.t option

  let pp_range fmt (m, n) =
    Fmt.pf fmt "[%a; %a]"
      Fmt.(option ~none:(any "∞") Z.pp_print)
      m
      Fmt.(option ~none:(any "∞") Z.pp_print)
      n

  let pp_binding fmt (var, range) =
    Fmt.pf fmt "%a: %a" Var.pp var pp_range range

  let pp_map ft m =
    Fmt.pf ft "{ %a }"
      Fmt.(iter_bindings ~sep:(any ", ") Var.Map.iter pp_binding)
      m

  include Reversible.Make_mutable (struct
    type t = range Var.Map.t

    let default : t = Var.Map.empty
  end)

  let is_empty : range -> bool = function
    | Some m, Some n -> Z.gt m n
    | _ -> false

  (** The intersection of two ranges; always representable *)
  let intersect ((m1, n1) : range) ((m2, n2) : range) : range option =
    Some (Option.merge Z.max m1 m2, Option.merge Z.min n1 n2)

  (** The union of two ranges; representable but OX *)
  let union ((m1, n1) : range) ((m2, n2) : range) : range option =
    Some (Option.map2 Z.min m1 m2, Option.map2 Z.max n1 n2)

  (** Intersection of two interval mappings, doing the intersection of the
      intervals *)
  let st_intersect = Var.Map.merge (fun _ -> Option.bind2 intersect)

  (** Union of two interval mappings, doing the union of the intervals *)
  let st_union = Var.Map.merge (fun _ -> Option.bind2 union)

  (** The difference [r1 / r2] of two ranges; this is tricky: if [r2] is
      somewhere inside the [r1] without touching its edges (e.g.
      [[0, 5] / [2, 3]]), we cannot compute an appropriate range and must
      overapproximate to [r1].

      This is ok here, as long as we don't simplify the assertion, as the solver
      will still have all information. FIXME: is the above true? *)
  let diff (r1 : range) (r2 : range) : range option =
    match (r1, r2) with
    (* [m, n] \ {m} = [m+1, n] *)
    | (Some m1, n1), (Some m2, Some n2) when Z.equal m2 n2 && Z.equal m1 m2 ->
        Some (Some (Z.succ m1), n1)
    (* [m, n] \ {n} = [m, n-1] *)
    | (m1, Some n1), (Some m2, Some n2) when Z.equal m2 n2 && Z.equal n1 m2 ->
        Some (m1, Some (Z.pred n1))
    | _ -> None

  let get v st = Var.Map.find_opt v st |> Option.value ~default:(None, None)

  let rec add_constraint ?(neg = false) (v : Svalue.t) st =
    let open Svalue.Infix in
    let update var range' =
      let range = get var st in
      let new_range =
        if neg then diff range range' else intersect range range'
      in
      match new_range with
      (* We couldn't compute anything from this update *)
      | None -> (v, st)
      (* We found an inequality, but we learnt nothing from it; we can discard it *)
      | Some new_range when range = new_range ->
          log (fun m ->
              m "Useless range  %a: %a %s %a = %a" Var.pp var pp_range range
                (if neg then "/" else "∩")
                pp_range range' pp_range new_range);
          (Svalue.bool (not (is_empty range)), st)
      | Some new_range -> (
          let st' = Var.Map.add var new_range st in
          log (fun m ->
              m "New range %a: %a %s %a = %a" Var.pp var pp_range range
                (if neg then "/" else "∩")
                pp_range range' pp_range new_range);
          match new_range with
          (* We narrowed the range to one value! *)
          | Some m, Some n when Z.equal m n ->
              let eq = Svalue.int_z m ==@ Svalue.mk_var var TInt in
              (* this is hacky; we found the exact value, but we can't return the equality
                 if we're negating, since that equality will otherwise be negated. *)
              ((if neg then Svalue.not eq else eq), st')
          (* The range is empty, so this cannot be true *)
          | _ when is_empty new_range -> (Svalue.v_false, st')
          (* We got a range we can't deduce anything from *)
          | _ -> (v, st'))
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
        let nv', st' = add_constraint ~neg:(not neg) nv st in
        (Svalue.not nv', st')
    (* We don't explore && and || within negations, because that becomes messy. *)
    | Binop (And, v1, v2) when not neg ->
        let v1', st' = add_constraint v1 st in
        let v2', st'' = add_constraint v2 st' in
        log (fun m ->
            m "%a && %a => %a && %a" Svalue.pp v1 Svalue.pp v2 Svalue.pp v1'
              Svalue.pp v2');
        if v1' == v && v2' == v then (v, st) else (v1' &&@ v2', st'')
    | Binop (Or, v1, v2) when not neg ->
        let v1', st1 = add_constraint v1 st in
        let v2', st2 = add_constraint v2 st in
        log (fun m ->
            m "%a || %a => %a || %a" Svalue.pp v1 Svalue.pp v2 Svalue.pp v1'
              Svalue.pp v2');
        (v1' ||@ v2', st_union st1 st2)
    | _ -> (v, st)

  let add_constraint v st =
    let v', st' = add_constraint v st in
    log (fun m ->
        if st <> st' || v <> v' then
          let list_diff l1 l2 = List.filter (fun x -> not (List.mem x l1)) l2 in
          let diff = list_diff (Var.Map.bindings st) (Var.Map.bindings st') in
          m "Int: %a -> %a with diff (%a)" Svalue.pp v Svalue.pp v'
            Fmt.(list ~sep:(any ", ") pp_binding)
            diff);
    (v', st')

  (** Adds a constraint to the current interval analysis, updating the currently
      tracked intervals. *)
  let add_constraint st v = wrap (add_constraint v) st

  let save st =
    log (fun m -> m "Save: %a" (Dynarray.pp pp_map) st);
    save st

  let backtrack_n st n =
    log (fun m -> m "Backtrack %d; before: %a" n (Dynarray.pp pp_map) st);
    backtrack_n st n;
    log (fun m -> m "After: %a" (Dynarray.pp pp_map) st)
end
