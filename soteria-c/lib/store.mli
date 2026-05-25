(** Local variable store for the C interpreter.

    The store maps each C local variable (identified by its
    {!Ail_tys.sym} symbol) to a {!binding}: its C type plus its current
    contents. A binding is either a stack pointer (for variables whose
    address has been taken), a regular {{!Aggregate_val.t} aggregate value},
    or uninitialised memory. The store is purely functional; mutation is
    expressed by returning an updated store. *)

(** The contents of a stored variable. *)
type binding_kind =
  | Stackptr of Typed.T.sptr Typed.t
      (** A stack pointer: the variable has been allocated on the heap because
          its address is taken somewhere. *)
  | Value of Aggregate_val.t
      (** A direct value held in the store. *)
  | Uninit
      (** Variable has been declared but not yet assigned. Reads are UB. *)
[@@deriving show]

(** A single store entry: a {!binding_kind} together with its C type. *)
type binding = { kind : binding_kind; ty : Ail_tys.ctype } [@@deriving show]

(** A whole store. *)
type t

val pp : t Fmt.t

(** The empty store. *)
val empty : t

(** [add_value x v ty s] binds [x : ty] to the value [v] in [s]. *)
val add_value : Ail_tys.sym -> Aggregate_val.t -> Ail_tys.ctype -> t -> t

(** [add_stackptr x p ty s] binds [x : ty] to the heap-allocated pointer [p]
    in [s]. Used for locals whose address is taken. *)
val add_stackptr :
  Ail_tys.sym -> Typed.T.sptr Typed.t -> Ail_tys.ctype -> t -> t

(** [reserve x ty s] declares [x : ty] in [s] with an {!Uninit} binding. *)
val reserve : Ail_tys.sym -> Ail_tys.ctype -> t -> t

(** [remove x s] removes the binding for [x] from [s]. *)
val remove : Ail_tys.sym -> t -> t

(** All bindings in the store, in unspecified order. *)
val bindings : t -> (Ail_tys.sym * binding) list

val is_empty : t -> bool
val mem : Ail_tys.sym -> t -> bool
val find_opt : Ail_tys.sym -> t -> binding option

(** [declare_value x v s] updates the binding for [x] (which must already
    exist) to hold the value [v], preserving its declared C type. *)
val declare_value : Ail_tys.sym -> Aggregate_val.t -> t -> t

(** Returns the declared C type of [x]; raises if [x] is not in the store. *)
val get_ty : Ail_tys.sym -> t -> Ail_tys.ctype
