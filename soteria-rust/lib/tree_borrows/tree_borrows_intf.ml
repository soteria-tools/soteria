module type Rust_symex = Soteria.Symex.Base with module Value = Rustsymex.Value

module M (Symex : Rust_symex) = struct
  module type S = sig
    (** {2 Tree Borrows trees (the general structure)} *)

    type tag
    type access = Read | Write

    type state =
      | Reserved of bool
      | Unique
      | Frozen
      | ReservedIM
      | Cell
      | Disabled
      | UB

    type protector = Strong | Weak
    type t

    val pp : Format.formatter -> t -> unit
    val pp_tag : Format.formatter -> tag -> unit
    val pp_state : Format.formatter -> state -> unit

    module SM :
      Soteria.Sym_states.State_monad.S
        with type 'a Symex.t = 'a Symex.t
         and type st = t option

    (** {2 Tree Borrows state (the per-byte information)} *)

    type tb_state

    val pp_tb_state : Format.formatter -> tb_state -> unit

    (* Compositionality *)

    type serialized

    val pp_serialized : Format.formatter -> serialized -> unit
    val serialize : t -> serialized list

    val subst_serialized :
      (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

    val iter_vars_serialized :
      serialized -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit

    val consume : serialized -> t -> (t, 'err, serialized list) Symex.Result.t
    val produce : serialized -> unit SM.t

    type serialized_state

    val pp_serialized_state : Format.formatter -> serialized_state -> unit
    val serialize_state : tb_state -> serialized_state list

    val subst_serialized_state :
      (Svalue.Var.t -> Svalue.Var.t) -> serialized_state -> serialized_state

    val iter_vars_serialized_state :
      serialized_state -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit

    val consume_state :
      serialized_state ->
      tb_state option ->
      (tb_state option, 'err, serialized_state list) Symex.Result.t

    val produce_state :
      serialized_state -> tb_state option -> tb_state option Symex.t

    type full_serialized = Structure of serialized | State of serialized_state

    (** {2 Operations on the structure} *)

    (** Generates a nondeterministic tag, for a nondeterministic pointer. May
        return [None] if this tree borrows implementation doesn't support
        symbolic tags. *)
    val nondet_tag : unit -> tag option Symex.t

    val init : unit -> (t * tag) Symex.t

    val borrow :
      ?protector:protector ->
      tag ->
      state:state ->
      (tag, 'e, serialized list) SM.Result.t

    val unprotect : tag -> (unit, 'e, serialized list) SM.Result.t
    val strong_protector_exists : t option -> bool

    (** {2 Operations on the state} *)

    val fix_empty_state : unit -> serialized_state list
    val init_st : unit -> tb_state Symex.t
    val equal_state : tb_state option -> tb_state option -> bool

    val set_protector :
      protected:bool ->
      tag ->
      t option ->
      tb_state option ->
      (tb_state option, 'e, full_serialized list) Symex.Result.t

    (** [access root accessed e state]: Update all nodes in the mapping [state]
        for the tree rooted at [root] with an event [e], that happened at
        [accessed]. *)
    val access :
      tag ->
      access ->
      t option ->
      tb_state option ->
      ( tb_state option,
        [> `AliasingError ],
        full_serialized list )
      Symex.Result.t

    val merge : tb_state -> tb_state -> tb_state Symex.t
  end
end

module type T = (Symex : Rust_symex) -> M(Symex).S
