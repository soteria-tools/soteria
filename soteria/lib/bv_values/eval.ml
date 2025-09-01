open Svalue

let eval_binop : Binop.t -> t -> t -> t = function
  | And -> Bool.and_
  | Or -> Bool.or_
  | Eq -> Bool.sem_eq
  | FEq -> Float.eq
  | FLeq -> Float.leq
  | FLt -> Float.lt
  | FPlus -> Float.plus
  | FMinus -> Float.minus
  | FTimes -> Float.times
  | FDiv -> Float.div
  | FRem -> Float.rem
  | BvPlus -> BitVec.plus
  | BvMinus -> BitVec.minus
  | BvTimes -> BitVec.times
  | BvDiv signed -> BitVec.div ~signed
  | BvRem signed -> BitVec.rem ~signed
  | BvMod -> BitVec.mod_
  | BvPlusOvf signed -> BitVec.plus_overflows ~signed
  | BvTimesOvf signed -> BitVec.times_overflows ~signed
  | BvLt signed -> BitVec.lt ~signed
  | BvLeq signed -> BitVec.leq ~signed
  | BvConcat -> BitVec.concat
  | BitAnd -> BitVec.and_
  | BitOr -> BitVec.or_
  | BitXor -> BitVec.xor
  | BitShl -> BitVec.shl
  | BitLShr -> BitVec.lshr
  | BitAShr -> BitVec.ashr

let eval_unop : Unop.t -> t -> t = function
  | Not -> Bool.not
  | FAbs -> Float.abs
  | GetPtrLoc -> Ptr.loc
  | GetPtrOfs -> Ptr.ofs
  | BvOfBool n -> BitVec.of_bool n
  | BvOfFloat (signed, _) -> BitVec.of_float ~signed
  | FloatOfBv (signed, _) -> BitVec.to_float ~signed
  | BvExtract (from, to_) -> BitVec.extract from to_
  | BvExtend (signed, by) -> BitVec.extend ~signed by
  | BvNot -> BitVec.not
  | BvNeg -> BitVec.neg
  | BvNegOvf -> BitVec.neg_overflows
  | FIs fc -> Float.is_floatclass fc
  | FRound rm -> Float.round rm

let rec eval ?(force = false) ~eval_var ~eval_bool (x : t) : t =
  let eval = eval ~force ~eval_var ~eval_bool in
  let x = if x.node.ty = TBool then eval_bool x else x in
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Float _ | BitVec _ -> x
  | Ptr (l, o) ->
      let nl = eval l in
      let no = eval o in
      if l == nl && o == no then x else Ptr.mk (eval l) (eval o)
  | Unop (unop, v) ->
      let nv = eval v in
      if v == nv && Stdlib.not force then x else eval_unop unop nv
  | Binop (binop, v1, v2) ->
      (* TODO: for binops that may short-circuit such as || or &&,
    we could do this without evaluating both sides, and deciding if any
      of either side evaluates properly to e.g. true/false *)
      let nv1 = eval v1 in
      let nv2 = eval v2 in
      if v1 == nv1 && v2 == nv2 && Stdlib.not force then x
      else eval_binop binop nv1 nv2
  | Nop (nop, l) -> (
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed && Stdlib.not force then x
      else match nop with Distinct -> Bool.distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard Bool.v_true then eval then_
      else if equal guard Bool.v_false then eval else_
      else Bool.ite guard (eval then_) (eval else_)
  | Seq l ->
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else Svalue.SSeq.mk ~seq_ty:x.node.ty l

(** Evaluates an expression; will call [eval_var] on each [Var] encountered, and
    will call [eval_bool] on every node of type [TBool] encountered. [force]
    indicates that expressions should be re-evaluated even if their components
    hasn't changed. If evaluation errors (e.g. from a division by zero), gives
    up and returns the original expression. *)
let eval ?force ?(eval_var : Var.t -> Svalue.ty -> t = Svalue.mk_var)
    ?(eval_bool : t -> t = Fun.id) (x : t) : t =
  try eval ~eval_var ~eval_bool ?force x with Division_by_zero -> x
