(** Extensible bit-vector based values, with floating point support. Suitable
    for low-level languages.

    {1 Overview}

    The core is {!Svalue}: an untyped, hash-consed expression grammar (booleans,
    bit-vectors, floats, pointers, sequences, quantifiers) equipped with
    simplifying smart constructors. On top of it {!Typed} adds a phantom "ghost"
    type tracking the {e kind} of each value (integer, boolean, pointer, ...)
    for extra static safety. {!Encoding} lowers values to SMT, {!Bv_solver}
    drives Z3, and {!Analyses} are the abstract domain analyses used along the
    way.

    {1 Using the functors}

    Everything is parameterised by {{!Svalue.Value_ext}[Value_ext]}, a
    {e domain-specific extension} that adds extra value/type leaves to the
    grammar. Tools that need no extra leaves pass {!Svalue.Dummy_ext}.

    A tool instantiates the typed layer {b once} and reuses it everywhere:
    {@ocaml[
    module Typed = Bv_values.Typed.Make (Bv_values.Svalue.Dummy_ext) ()
    ]}
    From {!Typed} one then gets {{!Typed.Make.Svalue}[Typed.Svalue]} (the
    untyped layer and its constructors), {{!Typed.Make.Eval}[Typed.Eval]}
    (normalisation) and {{!Typed.Make.Expr}[Typed.Expr]} (substitution). To
    discharge path conditions, feed [Typed] to a solver from {!Bv_solver}. *)

module Analyses = Analyses
module Encoding = Encoding
module Eval = Eval
module Save_counter = Save_counter
module Svalue = Svalue
module Bv_solver = Bv_solver
module Typed = Typed
