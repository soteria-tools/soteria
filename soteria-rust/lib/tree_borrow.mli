open Rustsymex

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
val add_child : parent:tag -> ?protector:bool -> state:state -> t -> t * tag
val unprotect : tag -> t -> t
val empty_state : tb_state
val set_protector : protected:bool -> tag -> t -> tb_state -> tb_state

(** [access root accessed e state]: Update all nodes in the mapping [state] for
    the tree rooted at [root] with an event [e], that happened at [accessed]. *)
val access :
  tag -> access -> t -> tb_state -> (tb_state, [> `AliasingError ], 'm) Result.t

val merge : tb_state -> tb_state -> tb_state
