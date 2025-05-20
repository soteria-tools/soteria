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

(** [access root accessed im e state]: Update all nodes in the mapping [state]
    for the tree rooted at [root] with an event [e], that happened at
    [accessed]. [im] indicates whether this location is considered interiorly
    mutable, which affects the default state of tags (Reserved vs ReservedIM).
    Returns the new state and a boolean indicating whether an undefined behavior
    was encountered *)
val access : t -> tag -> bool -> access -> tb_state -> tb_state * bool

val merge : tb_state -> tb_state -> tb_state
