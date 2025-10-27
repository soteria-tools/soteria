open Symex
open Soteria_std
open Svalue.Infix

(* let log = Soteria.Logging.Logs.L.warn *)
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
  let mk_var n v : Svalue.t = Svalue.mk_var v (TBitVector n)
  let mk_var_ty n v : Typed.T.sint Typed.t = Typed.mk_var v (Typed.t_int n)
  let max_for n = Z.(pred (shift_left one n))

  type sign = Pos | Neg

  let pp_sign fmt = function
    | Pos -> Fmt.string fmt "+"
    | Neg -> Fmt.string fmt "-"

  module Range = struct
    (** A range \[m, n\]; both sides are inclusive. Because we deal with
        bitvectors, we always have an upper and lower bound! *)
    type t = Z.t * Z.t

    let pp fmt (m, n) = Fmt.pf fmt "[%s; %s]" (Z.format "x" m) (Z.format "x" n)
    let is_empty (m, n) : bool = Z.gt m n
    let default b = (Z.zero, max_for b)

    (** The intersection of two ranges; always representable *)
    let intersect (m1, n1) (m2, n2) = (Z.max m1 m2, Z.min n1 n2)
  end

  (** The interval data for a given variable; consists of one positive range,
      and a list of negative ranges that apply to it.

      See https://ceur-ws.org/Vol-1617/paper8.pdf *)
  module Data = struct
    type t = { pos : Range.t; negs : Range.t list; size : int }
    [@@deriving show { with_path = false }]

    let mk n = { pos = Range.default n; negs = []; size = n }

    let apply_negs sz pos negs =
      let open Z in
      let open Z.Compare in
      (* See [3.1] in https://ceur-ws.org/Vol-1617/paper8.pdf *)
      List.fold_left
        (fun (((l, u) as pos), negs) ((a, b) as neg) ->
          if a > u || b < l then (pos, negs)
          else if a <= l then
            let neg_pos = (succ b, shift_left one Stdlib.(sz - 1)) in
            (Range.intersect pos neg_pos, negs)
          else if b >= u then
            let neg_pos = (zero, pred a) in
            (Range.intersect pos neg_pos, negs)
          else (pos, neg :: negs))
        (pos, []) negs

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
      if pos = data.pos && List.length negs = List.length data.negs then
        (data, true)
      else ({ data with pos; negs }, false)

    (** Whether the interval data represents an empty set. *)
    let is_empty data =
      (* TODO: take into account the negative ranges! Maybe we can do a small optimisation
         where we automatically clear data and set a RangeData to be empty when we find it
         is after an add_pos/add_neg, rather than computing it here too. *)
      Range.is_empty data.pos

    (** Whether the data represents a singleton set. *)
    let is_singleton { pos = m, n; _ } = Z.equal m n

    (** [iter_sval_equivalent v d] returns an iterator over the set of symbolic
        values to encode the data [d] for variable [v]. *)
    let iter_sval_equivalent v { pos = m, n; negs; size } f =
      let bv = Typed.BitVec.mk size in
      let var = mk_var_ty size v in
      let open Typed.Infix in
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

  (** [simplify st v] simplifies the constraint [v], without learning anything,
      using the current knowledge base. *)
  let rec simplify (v : Svalue.t) st : Svalue.t =
    let simplify v = simplify v st in
    match v.node.kind with
    (* TODO: simplify equalities and inequalities *)
    | Binop (op, l, r) ->
        let l' = simplify l in
        let r' = simplify r in
        if l' == l && r' == r then v else Eval.eval_binop op l' r'
    | Unop (op, v) ->
        let v' = simplify v in
        if v' == v then v else Eval.eval_unop op v'
    | Ite (i, t, e) ->
        let i' = simplify i in
        let t' = simplify t in
        let e' = simplify e in
        if i' == i && t' == t && e' == e then v else Svalue.Bool.ite i' t' e'
    | _ -> v

  let update st var size sign new_range =
    let range = get size var st in
    let range', redundant =
      match sign with
      | Pos -> Data.add_pos range new_range
      | Neg -> Data.add_neg range new_range
    in
    if redundant then (
      log (fun m ->
          m "Useless range  %a: %a %a %a = %a" Var.pp var Data.pp range pp_sign
            sign Range.pp new_range Data.pp range');
      let is_ok = not (Data.is_empty range) in
      ((Svalue.Bool.bool is_ok, Var.Set.empty), st))
    else
      let st = Var.Map.add var range' st in
      log (fun m ->
          m "New range %a: %a %a %a = %a" Var.pp var Data.pp range pp_sign sign
            Range.pp new_range Data.pp range');
      if Data.is_singleton range' then
        (* We narrowed the range to one value! *)
        let const = Svalue.BitVec.mk size (fst range'.pos) in
        let var = mk_var size var in
        let eq = const ==@ var in
        ((eq, Var.Set.empty), st)
      else if Data.is_empty range' then
        (* The range is empty, so this cannot be true *)
        ((Svalue.Bool.v_false, Var.Set.empty), st)
      else
        (* We could cleanly absorb the range, so the PC doesn't need to store it -- however
           we must mark this variable as dirty, as maybe the modified range still renders
           the branch infeasible, e.g. because of some additional PC assertions. *)
        ((Svalue.Bool.v_true, Var.Set.singleton var), st)

  (** [add_constraint v st] Adds a constraint [v] to the state [st]. *)
  let rec add_constraint (v : Svalue.t) st : (Svalue.t * Var.Set.t) * st =
    let open Z in
    (* -c = 2^w - c *)
    let ( ~- ) size x = shift_left one size - x in
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
        if Z.lt c1 c2 then update st v size Neg (~-size c2, c1 - c2 - one)
        else update st v size Pos (c1 - c2, ~-size c2 - one)
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
        if Z.leq c1 c2 then update st v size Neg (c2 - c1 + one, ~-size one)
        else update st v size Pos (~-size c1, ~-size c1 + c2)
    (*
       Case 4: x <=s c1
       • c1 < 2^{n-1} => ~[ c1 + 1; 2^{n-1} - 1 ]
       • c1 >= 2^{n-1} => [ 2^{n-1}; c1 ]
    *)
    | Binop
        ( ((Lt true | Leq true) as binop),
          { node = { kind = Var var; _ }; _ },
          { node = { kind = BitVec c1; ty = TBitVector size }; _ } ) ->
        let c1 = if binop = Lt true then Z.pred c1 else c1 in
        let mid = Z.shift_left Z.one Stdlib.(size - 1) in
        if Z.lt c1 mid then update st var size Neg (c1 + one, mid - one)
        else update st var size Pos (mid, c1)
    (*
       Case 5: c1 <=s x
       • c1 < 2^{n-1} => [ c1; 2^{n-1} - 1 ]
       • c1 >= 2^{n-1} => ~[ 2^{n-1}; c1 - 1 ]
    *)
    | Binop
        ( ((Lt true | Leq true) as binop),
          { node = { kind = BitVec c1; ty = TBitVector size }; _ },
          { node = { kind = Var var; _ }; _ } ) ->
        let c1 = if binop = Lt true then Z.succ c1 else c1 in
        let mid = Z.shift_left Z.one Stdlib.(size - 1) in
        if Z.lt c1 mid then update st var size Pos (c1, mid - one)
        else update st var size Neg (mid, c1 - one)
    (* Simple equality *)
    | Binop
        ( Eq,
          { node = { kind = BitVec x; ty = TBitVector b }; _ },
          { node = { kind = Var var; _ }; _ } )
    | Binop
        ( Eq,
          { node = { kind = Var var; _ }; _ },
          { node = { kind = BitVec x; ty = TBitVector b }; _ } ) ->
        update st var b Pos (x, x)
    | Binop (And, v1, v2) ->
        let (v1', vars1), st' = add_constraint v1 st in
        let (v2', vars2), st'' = add_constraint v2 st' in
        log (fun m ->
            m "%a && %a => %a && %a" Svalue.pp v1 Svalue.pp v2 Svalue.pp v1'
              Svalue.pp v2');
        if v1' == v && v2' == v then ((v, Var.Set.empty), st)
        else ((v1' &&@ v2', Var.Set.union vars1 vars2), st'')
    | _ -> ((v, Var.Set.empty), st)

  let add_constraint v st =
    let (v', vars), st' = add_constraint v st in
    if v <> v' then
      log (fun m ->
          m "Change: %a -> %a (%a)" Svalue.pp v Svalue.pp v'
            Fmt.(list ~sep:(any ", ") Var.pp)
            (Var.Set.to_list vars));
    ((v', vars), st')

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
