open Svalue
open Soteria_std

let eval_binop : Binop.t -> t -> t -> t = function
  | And -> Bool.and_
  | Or -> Bool.or_
  | Eq -> Bool.sem_eq
  | FEq -> Float.eq
  | FLeq -> Float.leq
  | FLt -> Float.lt
  | FAdd -> Float.add
  | FSub -> Float.sub
  | FMul -> Float.mul
  | FDiv -> Float.div
  | FRem -> Float.rem
  | Add checked -> BitVec.add ~checked
  | Sub checked -> BitVec.sub ~checked
  | Mul { checked } -> BitVec.mul ~checked
  | Div signed -> BitVec.div ~signed
  | Rem signed -> BitVec.rem ~signed
  | Mod -> BitVec.mod_
  | MulOvf signed -> BitVec.mul_overflows ~signed
  | Lt signed -> BitVec.lt ~signed
  | Leq signed -> BitVec.leq ~signed
  | BvConcat -> BitVec.concat
  | BitAnd -> BitVec.and_
  | BitOr -> BitVec.or_
  | BitXor -> BitVec.xor
  | Shl -> BitVec.shl
  | LShr -> BitVec.lshr
  | AShr -> BitVec.ashr

let eval_unop : Unop.t -> t -> t = function
  | Not -> Bool.not
  | FAbs -> Float.abs
  | GetPtrLoc -> Ptr.loc
  | GetPtrOfs -> Ptr.ofs
  | BvOfBool n -> BitVec.of_bool n
  | BvOfFloat (rounding, signed, size) ->
      BitVec.of_float ~rounding ~signed ~size
  | FloatOfBv (rounding, signed, fp) -> BitVec.to_float ~rounding ~signed ~fp
  | BvExtract (from, to_) -> BitVec.extract from to_
  | BvExtend (signed, by) -> BitVec.extend ~signed by
  | BvNot -> BitVec.not
  | Neg -> BitVec.neg
  | FIs fc -> Float.is_floatclass fc
  | FRound rm -> Float.round rm

let rec eval ~eval_var (x : t) : t =
  let eval = eval ~eval_var in
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Float _ | BitVec _ -> x
  | Ptr (l, o) ->
      let nl = eval l in
      let no = eval o in
      if l == nl && o == no then x else Ptr.mk (eval l) (eval o)
  | Unop (unop, v) ->
      let nv = eval v in
      if v == nv then x else eval_unop unop nv
  | Binop (binop, v1, v2) ->
      (* TODO: for binops that may short-circuit such as || or &&,
    we could do this without evaluating both sides, and deciding if any
      of either side evaluates properly to e.g. true/false *)
      let nv1 = eval v1 in
      let nv2 = eval v2 in
      if v1 == nv1 && v2 == nv2 then x else eval_binop binop nv1 nv2
  | Nop (nop, l) -> (
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x
      else match nop with Distinct -> Bool.distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard Bool.v_true then eval then_
      else if equal guard Bool.v_false then eval else_
      else Bool.ite guard (eval then_) (eval else_)
  | Seq l ->
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else Svalue.SSeq.mk ~seq_ty:x.node.ty l

(** Evaluates an expression; will call [eval_var] on each [Var] encountered. If
    evaluation errors (e.g. from a division by zero), gives up and returns the
    original expression. *)
let eval ?(eval_var : Var.t -> Svalue.ty -> t = Svalue.mk_var) (x : t) : t =
  try eval ~eval_var x with Division_by_zero -> x
