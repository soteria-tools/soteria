open Rustsymex

val is_disabled : unit -> bool
val set_enabled : bool -> unit

type tag
and access = Read | Write
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB

and t = {
  tag : tag;
  protector : bool;
  children : t list;
  initial_state : state;
}

and tb_state

val fresh_tag : unit -> tag
val zero : tag
val pp : t Fmt.t
val pp_tag : tag Fmt.t
val pp_state : state Fmt.t
val init : ?protector:bool -> state:state -> unit -> t
val equal : t -> t -> bool
val update : t -> (t -> t) -> tag -> t
val add_child : parent:tag -> root:t -> t -> t
val empty_state : tb_state
val set_protector : protected:bool -> t -> tag -> tb_state -> tb_state

(** [access root accessed e state]: Update all nodes in the mapping [state] for
    the tree rooted at [root] with an event [e], that happened at [accessed]. *)
val access :
  t -> tag -> access -> tb_state -> (tb_state, [> `AliasingError ], 'm) Result.t

val merge : tb_state -> tb_state -> tb_state
