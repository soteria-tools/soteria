(** {1 SMT Encoding of Symbolic Values}

    This module provides the translation from symbolic values ({!Svalue.t})
    to SMT-LIB S-expressions for use with SMT solvers.

    {2 Overview}

    The encoding handles:
    - Booleans, bitvectors, and floating-point values
    - Unary and binary operations
    - Sequences and conditionals (ITE)
    - Variables (encoded as SMT atoms)

    {b Note}: Pointer values are not supported in this encoding module.
    Attempting to encode pointers will raise a failure.

    {2 Memoization}

    The encoding is memoized using hash-consing tags from {!Svalue}, avoiding
    redundant encoding of shared subexpressions.

    {2 Usage}

    {[
      let smt_expr = Encoding.encode_value some_svalue in
      (* smt_expr is a Simple_smt.sexp ready for the solver *)
    ]}
*)

(** The type of symbolic values being encoded. *)
type t = Svalue.t

(** The type of value types. *)
type ty = Svalue.ty

(** [sort_of_ty ty] returns the SMT-LIB sort corresponding to type [ty].

    @raise Failure if [ty] is a pointer type. *)
val sort_of_ty : ty -> Simple_smt.sexp

(** [encode_value v] encodes symbolic value [v] as an SMT-LIB S-expression.

    Conjunctions (And) are split and encoded as a conjunction of assertions
    for efficiency.

    @raise Failure if [v] contains pointer values. *)
val encode_value : t -> Simple_smt.sexp

(** Initial SMT commands to set up the solver (currently empty). *)
val init_commands : Simple_smt.sexp list
