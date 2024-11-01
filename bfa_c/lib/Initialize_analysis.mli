val reset : unit -> unit
val register_resetter : (unit -> unit) -> unit
val init : Ail_tys.sigma -> unit
val register_initializer : (Ail_tys.sigma -> unit) -> unit
val reinit : Ail_tys.sigma -> unit
