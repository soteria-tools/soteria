open Rustsymex

module type S = sig
  type tag
  and access = Read | Write

  and state =
    | Reserved of bool
    | Unique
    | Frozen
    | ReservedIM
    | Cell
    | Disabled
    | UB

  and protector = Strong | Weak
  and t
  and tb_state

  val fresh_tag : unit -> tag
  val zero : tag
  val pp : Format.formatter -> t -> unit
  val pp_tag : Format.formatter -> tag -> unit
  val pp_state : Format.formatter -> state -> unit
  val pp_tb_state : Format.formatter -> tb_state -> unit
  val init : state:state -> unit -> t * tag
  val ub_state : t

  val add_child :
    parent:tag -> ?protector:protector -> state:state -> t -> t * tag

  val unprotect : tag -> t -> t
  val strong_protector_exists : t -> bool
  val empty_state : tb_state
  val is_empty_state : tb_state -> bool
  val equal_state : tb_state -> tb_state -> bool
  val set_protector : protected:bool -> tag -> t -> tb_state -> tb_state

  (** [access root accessed e state]: Update all nodes in the mapping [state]
      for the tree rooted at [root] with an event [e], that happened at
      [accessed]. *)
  val access :
    tag ->
    access ->
    t ->
    tb_state ->
    (tb_state, [> `AliasingError ], 'm) Result.t

  val merge : tb_state -> tb_state -> tb_state

  (* Compositionality *)

  type serialized

  val pp_serialized : Format.formatter -> serialized -> unit
  val serialize : t -> serialized Seq.t

  val subst_serialized :
    (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

  val iter_vars_serialized :
    serialized -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit

  type serialized_state

  val pp_serialized_state : Format.formatter -> serialized_state -> unit
  val serialize_state : tb_state -> serialized_state Seq.t

  val subst_serialized_state :
    (Svalue.Var.t -> Svalue.Var.t) -> serialized_state -> serialized_state

  val iter_vars_serialized_state :
    serialized_state -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit

  val consume_state :
    serialized_state ->
    tb_state ->
    (tb_state, 'err, serialized_state) Rustsymex.Result.t

  val produce_state : serialized_state -> tb_state -> tb_state Rustsymex.t
end
