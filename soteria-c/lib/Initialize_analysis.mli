val init_once : unit -> unit
val register_once_initialiser : (unit -> unit) -> unit
val reinit : Ail_tys.sigma -> unit
val register_before_each_initialiser : (Ail_tys.sigma -> unit) -> unit
