(** Low-level solver manipulation and utilities.

    The main interface is {!Solver_interface.S}, which defines the interface
    that SMT solver backends must implement to be used with the symbolic
    execution engine. This signature assumes the solver is mutable.

    A solver relies on a value encoding, which is defined by the {!Value.S}
    signature as a way to encode values and types into SMT-LIB.

    Soteria then provides an implementation of this interface for Z3 in {!Z3};
    more backends are planned for the future. *)

module Solver_interface = Solver_interface
module Value = Value
module Z3 = Z3
module Config = Config
module Smt_utils = Smt_utils
