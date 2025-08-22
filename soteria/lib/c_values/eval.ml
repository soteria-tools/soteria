open Svalue

type _ Effect.t += Eval_var : Var.t * Svalue.ty -> t Effect.t

let eval_var (v : Var.t) (ty : Svalue.ty) : t =
  Effect.perform (Eval_var (v, ty))

let rec eval ?(force = false) (x : t) : t =
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Int _ | Float _ | BitVec _ -> x
  | Ptr (l, o) ->
      let nl = eval l in
      let no = eval o in
      if l == nl && o == no then x else Ptr.mk (eval l) (eval o)
  | Unop (unop, v) -> (
      let nv = eval v in
      if v == nv && Stdlib.not force then x
      else
        match unop with
        | Unop.Not -> not nv
        | FAbs -> Float.abs nv
        | GetPtrLoc -> Ptr.loc nv
        | GetPtrOfs -> Ptr.ofs nv
        | IntOfBool -> int_of_bool nv
        | BvOfFloat n -> BitVec.of_float n nv
        | BvOfInt ->
            let signed, size = shape_of_bv x.node.ty in
            BitVec.of_int signed size nv
        | IntOfBv signed -> BitVec.to_int signed nv
        | FloatOfBv -> BitVec.to_float nv
        | BvExtract (from, to_) -> bv_extract from to_ nv
        | BvExtend by ->
            let n = size_of_bv x.node.ty in
            bv_extend (n + by) x
        | FIs fc -> Float.is_floatclass fc nv
        | FRound rm -> Float.round rm nv)
  | Binop (binop, v1, v2) -> (
      (* TODO: for binops that may short-circuit such as || or &&,
    we could do this without evaluating both sides, and deciding if any
      of either side evaluates properly to e.g. true/false *)
      let nv1 = eval v1 in
      let nv2 = eval v2 in
      if v1 == nv1 && v2 == nv2 && Stdlib.not force then x
      else
        match binop with
        | Binop.And -> and_ nv1 nv2
        | Or -> or_ nv1 nv2
        | Eq -> sem_eq nv1 nv2
        | Leq -> leq nv1 nv2
        | Lt -> lt nv1 nv2
        | Plus -> plus nv1 nv2
        | Minus -> minus nv1 nv2
        | Times -> times nv1 nv2
        | Div -> div nv1 nv2
        | Rem -> rem nv1 nv2
        | Mod -> mod_ nv1 nv2
        | FEq -> Float.eq nv1 nv2
        | FLeq -> Float.leq nv1 nv2
        | FLt -> Float.lt nv1 nv2
        | FPlus -> Float.plus nv1 nv2
        | FMinus -> Float.minus nv1 nv2
        | FTimes -> Float.times nv1 nv2
        | FDiv -> Float.div nv1 nv2
        | FRem -> Float.rem nv1 nv2
        | BvPlus -> BitVec.Raw.plus nv1 nv2
        | BvMinus -> BitVec.Raw.minus nv1 nv2
        | BvTimes -> BitVec.Raw.times nv1 nv2
        | BvDiv s -> BitVec.Raw.div s nv1 nv2
        | BvRem s -> BitVec.Raw.rem s nv1 nv2
        | BvLt s -> BitVec.Raw.lt s nv1 nv2
        | BvLeq s -> BitVec.Raw.leq s nv1 nv2
        | BitAnd -> BitVec.Raw.and_ nv1 nv2
        | BitOr -> BitVec.Raw.or_ nv1 nv2
        | BitXor -> BitVec.Raw.xor nv1 nv2
        | BitShl -> BitVec.Raw.shl nv1 nv2
        | BitShr -> BitVec.Raw.shr nv1 nv2)
  | Nop (nop, l) -> (
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed && Stdlib.not force then x
      else match nop with Distinct -> distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard v_true then eval then_
      else if equal guard v_false then eval else_
      else ite guard (eval then_) (eval else_)
  | Seq l ->
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else Svalue.SSeq.mk ~seq_ty:x.node.ty l

let eval ?force ~(eval_var : Var.t -> Svalue.ty -> t option) (x : t) : t option
    =
  try Some (eval ?force x) with
  | Division_by_zero -> None
  | effect Eval_var (v, ty), k -> (
      match eval_var v ty with
      | Some v -> Effect.Deep.continue k v
      | None -> None)
