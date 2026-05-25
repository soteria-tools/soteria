(** The concrete C heap model used by Soteria-C.

    Just an alias for {{!State_intf.S} [State_intf.S]} — load/store, alloc,
    free, plus global handling. See {!State_intf} for the operations. *)

include State_intf.S
