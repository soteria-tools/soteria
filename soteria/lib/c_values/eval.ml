open Soteria_std
open Svalue

type _ Effect.t += Eval_var : Var.t * Svalue.ty -> t Effect.t

let eval_var (v : Var.t) (ty : Svalue.ty) : t =
  Effect.perform (Eval_var (v, ty))

let eval_binop : Binop.t -> t -> t -> t = function
  | And -> S_bool.and_
  | Or -> S_bool.or_
  | Eq -> sem_eq
  | Leq -> leq
  | Lt -> lt
  | Plus -> plus
  | Minus -> minus
  | Times -> times
  | Div -> div
  | Rem -> rem
  | Mod -> mod_
  | FEq -> Float.eq
  | FLeq -> Float.leq
  | FLt -> Float.lt
  | FPlus -> Float.plus
  | FMinus -> Float.minus
  | FTimes -> Float.times
  | FDiv -> Float.div
  | FRem -> Float.rem
  | BvPlus -> BitVec.Raw.plus
  | BvMinus -> BitVec.Raw.minus
  | BvTimes -> BitVec.Raw.times
  | BvDiv s -> BitVec.Raw.div s
  | BvRem s -> BitVec.Raw.rem s
  | BvMod -> BitVec.Raw.mod_
  | BvPlusOvf s -> BitVec.Raw.plus_overflows s
  | BvTimesOvf s -> BitVec.Raw.times_overflows s
  | BvLt s -> BitVec.Raw.lt s
  | BvLeq s -> BitVec.Raw.leq s
  | BvConcat -> BitVec.Raw.concat
  | BitAnd -> BitVec.Raw.and_
  | BitOr -> BitVec.Raw.or_
  | BitXor -> BitVec.Raw.xor
  | BitShl -> BitVec.Raw.shl
  | BitLShr -> BitVec.Raw.lshr
  | BitAShr -> BitVec.Raw.ashr

let eval_unop : Unop.t -> t -> t = function
  | Not -> S_bool.not
  | FAbs -> Float.abs
  | GetPtrLoc -> Ptr.loc
  | GetPtrOfs -> Ptr.ofs
  | IntOfBool -> int_of_bool
  | BvOfFloat (rounding, signed, n) -> BitVec.of_float rounding signed n
  | BvOfInt (signed, n) -> BitVec.of_int signed n
  | IntOfBv signed -> BitVec.to_int signed
  | FloatOfBv (rounding, signed, _) -> BitVec.to_float rounding signed
  | BvExtract (from, to_) -> BitVec.Raw.extract from to_
  | BvExtend (signed, by) -> BitVec.Raw.extend signed by
  | BvNot -> BitVec.Raw.not
  | BvNegOvf -> BitVec.Raw.neg_overflows
  | FIs fc -> Float.is_floatclass fc
  | FRound rm -> Float.round rm

let rec eval (x : t) : t =
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Int _ | Float _ | BitVec _ -> x
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
      if Stdlib.not changed then x else match nop with Distinct -> distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard v_true then eval then_
      else if equal guard v_false then eval else_
      else ite guard (eval then_) (eval else_)
  | Seq l ->
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else Svalue.SSeq.mk ~seq_ty:x.node.ty l

let eval ~(eval_var : Var.t -> Svalue.ty -> t option) (x : t) : t option =
  try Some (eval x) with
  | Division_by_zero -> None
  | effect Eval_var (v, ty), k -> (
      match eval_var v ty with
      | Some v -> Effect.Deep.continue k v
      | None -> None)
