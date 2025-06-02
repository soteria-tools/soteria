open Svalue

type _ Effect.t += Eval_var : Var.t * Svalue.ty -> t Effect.t

let eval_var (v : Var.t) (ty : Svalue.ty) : t =
  Effect.perform (Eval_var (v, ty))

(* TODO: add equality checks when reforming values to avoid re-hashconsing if not required. *)
let rec eval (x : t) : t =
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Int _ | Float _ | BitVec _ -> x
  | Ptr (l, o) -> Ptr.mk (eval l) (eval o)
  | Unop (unop, v) -> (
      let v = eval v in
      match unop with
      | Unop.Not -> not v
      | FAbs -> abs_f v
      | GetPtrLoc -> Ptr.loc v
      | GetPtrOfs -> Ptr.ofs v
      | IntOfBool -> int_of_bool v
      | BvOfFloat -> bv_of_float v
      | BvOfInt ->
          let n = size_of_bv x.node.ty in
          bv_of_int n v
      | IntOfBv signed -> int_of_bv signed v
      | FloatOfBv -> float_of_bv v
      | BvExtract (from, to_) -> bv_extract from to_ v
      | FIs fc -> is_floatclass fc v
      | FRound rm -> float_round rm v)
  | Binop (binop, v1, v2) -> (
      (* TODO: for binops that may short-circuit such as || or &&,
    we could do this without evaluating both sides, and deciding if any
      of either side evaluates properly to e.g. true/false *)
      let v1 = eval v1 in
      let v2 = eval v2 in
      match binop with
      | Binop.And -> and_ v1 v2
      | Or -> or_ v1 v2
      | Eq -> sem_eq v1 v2
      | Leq -> leq v1 v2
      | Lt -> lt v1 v2
      | Plus -> plus v1 v2
      | Minus -> minus v1 v2
      | Times -> times v1 v2
      | Div -> (
          match x.node.kind with
          | Int z when Z.equal z Z.zero -> v1 (* Returning anything is fine *)
          | _ -> div v1 v2)
      | Rem -> rem v1 v2
      | Mod -> mod_ v1 v2
      | FEq -> eq_f v1 v2
      | FLeq -> leq_f v1 v2
      | FLt -> lt_f v1 v2
      | FPlus -> plus_f v1 v2
      | FMinus -> minus_f v1 v2
      | FTimes -> times_f v1 v2
      | FDiv -> div_f v1 v2
      | FRem -> rem_f v1 v2
      | BvPlus ->
          let n = size_of_bv x.node.ty in
          raw_bv_plus n v1 v2
      | BvMinus ->
          let n = size_of_bv x.node.ty in
          raw_bv_minus n v1 v2
      | BitAnd ->
          let n = size_of_bv x.node.ty in
          raw_bit_and n v1 v2
      | BitOr ->
          let n = size_of_bv x.node.ty in
          raw_bit_or n v1 v2
      | BitXor ->
          let n = size_of_bv x.node.ty in
          raw_bit_xor n v1 v2
      | BitShl ->
          let n = size_of_bv x.node.ty in
          raw_bit_shl n v1 v2
      | BitShr ->
          let n = size_of_bv x.node.ty in
          raw_bit_shr n v1 v2)
  | Nop (nop, l) -> (
      let l = List.map eval l in
      match nop with Distinct -> distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard v_true then eval then_
      else if equal guard v_false then eval else_
      else ite guard (eval then_) (eval else_)
  | Seq l ->
      let l = List.map eval l in
      Svalue.SSeq.mk ~seq_ty:x.node.ty l

let eval ~(eval_var : Var.t -> Svalue.ty -> t option) (x : t) : t option =
  try Some (eval x)
  with effect Eval_var (v, ty), k -> (
    match eval_var v ty with Some v -> Effect.Deep.continue k v | None -> None)
