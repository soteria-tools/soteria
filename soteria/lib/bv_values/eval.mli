(** {1 Symbolic Value Evaluation}

    This module provides evaluation (simplification) of symbolic values.
    It reduces expressions by applying the semantics of operations when
    operands are concrete or can be simplified.

    {2 Overview}

    The evaluator recursively traverses a symbolic value and:
    - Applies concrete operations when operands are known
    - Simplifies conditionals when guards are concrete
    - Propagates constants through expressions
    - Optionally substitutes variables via a custom [eval_var] function

    {2 Usage Example}

    {[
      (* Simple evaluation *)
      let simplified = Eval.eval (BitVec.add x (BitVec.mk 32 Z.zero)) in
      (* Result: x (adding zero is identity) *)

      (* With variable substitution *)
      let eval_var _ var _ = Svalue.BitVec.mk 32 (Z.of_int 42) in
      let result = Eval.eval ~eval_var some_expr in
    ]}
*)

(** [eval_binop op v1 v2] applies binary operation [op] to values [v1] and [v2].

    This dispatches to the appropriate {!Svalue} module function based on
    the operation type (boolean, bitvector, floating-point, etc.). *)
val eval_binop : Svalue.Binop.t -> Svalue.t -> Svalue.t -> Svalue.t

(** [eval_unop op v] applies unary operation [op] to value [v].

    This dispatches to the appropriate {!Svalue} module function based on
    the operation type. *)
val eval_unop : Svalue.Unop.t -> Svalue.t -> Svalue.t

(** [eval ?eval_var x] evaluates (simplifies) the symbolic value [x].

    @param eval_var Optional function called for each variable encountered.
           Signature: [original_expr -> var -> ty -> replacement_value].
           The default returns the original expression unchanged.

    The evaluator:
    - Recursively evaluates subexpressions
    - Applies operations when operands become concrete
    - Short-circuits ITE when guards are [true] or [false]
    - Catches division-by-zero and returns the original expression

    @return The simplified value, or the original if no simplification possible. *)
val eval : ?eval_var:(Svalue.t -> Var.t -> Svalue.ty -> Svalue.t) -> Svalue.t -> Svalue.t
