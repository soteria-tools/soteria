type binding_kind =
  | Stackptr of Typed.T.sptr Typed.t
  | Value of Aggregate_val.t
  | Uninit
[@@deriving show]

type binding = { kind : binding_kind; ty : Ail_tys.ctype } [@@deriving show]
type t

val pp : t Fmt.t
val empty : t
val add_value : Ail_tys.sym -> Aggregate_val.t -> Ail_tys.ctype -> t -> t

val add_stackptr :
  Ail_tys.sym -> Typed.T.sptr Typed.t -> Ail_tys.ctype -> t -> t

val reserve : Ail_tys.sym -> Ail_tys.ctype -> t -> t
val remove : Ail_tys.sym -> t -> t
val bindings : t -> (Ail_tys.sym * binding) list
val is_empty : t -> bool
val mem : Ail_tys.sym -> t -> bool
val find_opt : Ail_tys.sym -> t -> binding option
val declare_value : Ail_tys.sym -> Aggregate_val.t -> t -> t
val get_ty : Ail_tys.sym -> t -> Ail_tys.ctype
