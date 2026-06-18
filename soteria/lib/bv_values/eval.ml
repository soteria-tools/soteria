open Svalue
open Soteria_std

let eval_arith : Binop.arith -> sbv t -> sbv t -> sbv t = function
  | Add checked -> BitVec.add ~checked
  | Sub checked -> BitVec.sub ~checked
  | Mul checked -> BitVec.mul ~checked
  | Div signed -> BitVec.div ~signed
  | Rem signed -> BitVec.rem ~signed
  | Mod -> BitVec.mod_
  | BvConcat -> BitVec.concat
  | BitAnd -> BitVec.and_
  | BitOr -> BitVec.or_
  | BitXor -> BitVec.xor
  | Shl -> BitVec.shl
  | LShr -> BitVec.lshr
  | AShr -> BitVec.ashr

let eval_cmp : Binop.cmp -> sbv t -> sbv t -> sbool t = function
  | Lt signed -> BitVec.lt ~signed
  | Leq signed -> BitVec.leq ~signed
  | AddOvf signed -> BitVec.add_overflows ~signed
  | SubOvf signed -> BitVec.sub_overflows ~signed
  | MulOvf signed -> BitVec.mul_overflows ~signed

let eval_farith : Binop.farith -> sfloat t -> sfloat t -> sfloat t = function
  | FAdd -> Float.add
  | FSub -> Float.sub
  | FMul -> Float.mul
  | FDiv -> Float.div
  | FRem -> Float.rem

let eval_fcmp : Binop.fcmp -> sfloat t -> sfloat t -> sbool t = function
  | FEq -> Float.eq
  | FLeq -> Float.leq
  | FLt -> Float.lt

let eval_boolean : Binop.boolean -> sbool t -> sbool t -> sbool t = function
  | And -> Bool.and_
  | Or -> Bool.or_

let eval_on_bv : type o. o Unop.on_bv -> sbv t -> o t = function
  | FloatOfBv (rounding, signed, fp) -> BitVec.to_float ~rounding ~signed ~fp
  | FloatOfBvRaw _ -> BitVec.to_float_raw
  | BvExtract (from, to_) -> BitVec.extract from to_
  | BvExtend (signed, by) -> BitVec.extend ~signed by
  | BvNot -> BitVec.not
  | Neg _ -> fun v -> BitVec.neg v

let eval_on_bool : type o. o Unop.on_bool -> sbool t -> o t = function
  | Not -> Bool.not
  | BvOfBool n -> BitVec.of_bool n

let eval_on_float : type o. o Unop.on_float -> sfloat t -> o t = function
  | BvOfFloat (rounding, signed, size) ->
      BitVec.of_float ~rounding ~signed ~size
  | FAbs -> Float.abs
  | FIs fc -> Float.is_floatclass fc
  | FRound rm -> Float.round rm

let eval_on_ptr : type o. o Unop.on_ptr -> sptr t -> o t = function
  | GetPtrLoc -> Ptr.loc
  | GetPtrOfs -> Ptr.ofs

(* A variable-evaluation callback. It is polymorphic in the variable's kind, so
   it is wrapped in a record (rank-2 polymorphism). *)
type eval_var = { ev : 'a. 'a t -> Var.t -> 'a ty -> 'a t }

let rec eval : type a. force:bool -> eval_var:eval_var -> a t -> a t =
 fun ~force ~eval_var x ->
  let ev : type b. b t -> b t = fun v -> eval ~force ~eval_var v in
  match x.node.kind with
  | Var v -> eval_var.ev x v x.node.ty
  | Bool _ | Float _ | BitVec _ | LocLit _ -> x
  | Ptr (l, o) ->
      let nl = ev l in
      let no = ev o in
      if (not force) && l == nl && o == no then x else Ptr.mk nl no
  | UnBv (op, v) ->
      let nv = ev v in
      if (not force) && v == nv then x else eval_on_bv op nv
  | UnBool (op, v) ->
      let nv = ev v in
      if (not force) && v == nv then x else eval_on_bool op nv
  | UnFloat (op, v) ->
      let nv = ev v in
      if (not force) && v == nv then x else eval_on_float op nv
  | UnPtr (op, v) ->
      let nv = ev v in
      if (not force) && v == nv then x else eval_on_ptr op nv
  | BvArith (op, v1, v2) ->
      let nv1 = ev v1 in
      let nv2 = ev v2 in
      if (not force) && v1 == nv1 && v2 == nv2 then x else eval_arith op nv1 nv2
  | BvCmp (op, v1, v2) ->
      let nv1 = ev v1 in
      let nv2 = ev v2 in
      if (not force) && v1 == nv1 && v2 == nv2 then x else eval_cmp op nv1 nv2
  | FArith (op, v1, v2) ->
      let nv1 = ev v1 in
      let nv2 = ev v2 in
      if (not force) && v1 == nv1 && v2 == nv2 then x
      else eval_farith op nv1 nv2
  | FCmp (op, v1, v2) ->
      let nv1 = ev v1 in
      let nv2 = ev v2 in
      if (not force) && v1 == nv1 && v2 == nv2 then x else eval_fcmp op nv1 nv2
  | BoolBin (op, v1, v2) ->
      let nv1 = ev v1 in
      let nv2 = ev v2 in
      if (not force) && v1 == nv1 && v2 == nv2 then x
      else eval_boolean op nv1 nv2
  | Eq (v1, v2) ->
      let nv1 = ev v1 in
      let nv2 = ev v2 in
      if (not force) && v1 == nv1 && v2 == nv2 then x else Bool.sem_eq nv1 nv2
  | Nop (Distinct, l) ->
      let l, changed = List.map_changed ev l in
      if (not force) && not changed then x else Bool.distinct l
  | Ite (guard, then_, else_) ->
      let guard = ev guard in
      if equal guard Bool.v_true then ev then_
      else if equal guard Bool.v_false then ev else_
      else
        let nthen = ev then_ in
        let nelse = ev else_ in
        if (not force) && then_ == nthen && else_ == nelse then x
        else Bool.ite guard nthen nelse
  | Exists (vs, sv) ->
      let eval_var' =
        {
          ev =
            (fun sv v ty ->
              if List.exists (fun (v', _) -> Var.equal v v') vs then sv
              else eval_var.ev sv v ty);
        }
      in
      let nsv = eval ~force ~eval_var:eval_var' sv in
      if (not force) && sv == nsv then x else Bool.mk_exists vs sv
  | Seq l ->
      let l, changed = List.map_changed ev l in
      if (not force) && not changed then x
      else Svalue.SSeq.mk ~seq_ty:x.node.ty l

(** Evaluates an expression; will call [eval_var] on each [Var] encountered. If
    evaluation errors (e.g. from a division by zero), gives up and returns the
    original expression. The [force] flag forces evaluation to proceed even if
    no sub-expressions changed (required if evaluating an expression that has
    not been constructed using smart constructors). *)
let eval ?(force = false) ?(eval_var = { ev = (fun x _ _ -> x) }) (x : 'a t) :
    'a t =
  try eval ~force ~eval_var x with Division_by_zero -> x
