(** Auxiliary SMT-LIB declarations required by value encodings.

    A {{!Value.S}solver value} (or in some cases, a
    {{!Soteria.Bv_values.Svalue.Value_ext}value extension} may need global
    SMT-LIB declarations to support its encoding, e.g. declaring an algebraic
    datatype for a recursive value type. Encoders cannot communicate with the
    solver directly, so we instead provide a function, {!declare}, to allow
    doing it.

    Solver implementations (like {!Z3}) handle the raised effect, {!Declare}, by
    de-duplicating declarations (by {!field:key}) and sending them to the
    underlying solver process before the command that is about to be run.
    {b There is no guarantee the declaration will still be in scope for whatever
       command follows:} encoders must {e always} re-declare definitions they
    may need. *)

type t = {
  key : string;
      (** Canonical identifier for the declaration. Two {!declare}s with the
          same [key] are assumed to produce identical [commands], and are
          deduplicated. *)
  commands : Smt.sexp Iter.t;
      (** Lazily produces the SMT-LIB commands declaring the object(s); only
          invoked if [key] is not already declared, so that the declaration is
          never built otherwise. *)
}

(** [equal d1 d2] is [true] if [d1] and [d2] have the same {!field:key}. *)
let equal (d1 : t) (d2 : t) = String.equal d1.key d2.key

type _ Effect.t += Declare : t -> unit Effect.t

(** [declare ~key commands] ensures the commands produced by [commands] have
    been sent to the solver before the encoding being produced is used.
    Producing the commands may itself [declare] the declarations it depends on:
    they are sent before the commands produced. *)
let declare ~key commands = Effect.perform (Declare { key; commands })
