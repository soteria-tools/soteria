type tag
and access = Read | Write
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB
and t = { tag : tag; tag_protected : bool; children : t list }
and tb_state

val fresh_tag : unit -> tag
val zero : tag
val pp : t Fmt.t
val pp_tag : tag Fmt.t
val init : ?protected:bool -> unit -> t
val equal : t -> t -> bool
val empty_state : tb_state

(** [access root tag e state]: Update all nodes in the mapping [state] for the
    tree rooted at [root] with an event [e], that happened at [tag]. Returns the
    new state and a boolean indicating whether an undefined behavior was
    encountered *)
val access : t -> tag -> access -> tb_state -> tb_state * bool

val merge : tb_state -> tb_state -> tb_state
