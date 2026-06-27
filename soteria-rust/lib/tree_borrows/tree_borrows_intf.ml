open Common
module Ptr_tag = Svalue.Ptr_tag

module type Rust_symex = Soteria.Symex.Base with module Value = Rustsymex.Value

module M (Symex : Rust_symex) = struct
  module D_abstr = Soteria.Data.Abstr.M (Symex)
  module Base = Soteria.Sym_states.Base.M (Symex)

  module type S = sig
    module Tree : sig
      (** {2 Tree Borrows trees (the general structure)} *)
      type t [@@mixins Base.S]

      val init : unit -> (Ptr_tag.t * t) Symex.t

      val borrow :
        ?protector:protector ->
        Ptr_tag.t ->
        state:state ->
        (Ptr_tag.t, 'e, syn list) SM.Result.t

      val unprotect : Ptr_tag.t -> (unit, 'e, syn list) SM.Result.t
      val strong_protector_exists : t option -> bool
      val assert_exclusively_owned : unit -> (unit, 'e, syn list) SM.Result.t
    end

    module State : sig
      (** {2 Tree Borrows state (the per-byte information)} *)

      type t [@@mixins Base.S]
      type syn_full := [ `Structure of Tree.syn | `State of syn ]

      (* TODO: I really want to remove this but idk how *)

      (** Makes a fix to create an empty TB state *)
      val fix_empty : unit -> syn list

      val init : unit -> t Symex.t
      val equal : t option -> t option -> bool

      (** {3 Operations on the state} *)

      val set_protector :
        protected:bool ->
        Ptr_tag.t ->
        Tree.t option ->
        t option ->
        (t option, 'e, syn_full list) Symex.Result.t

      (** [access root accessed e state]: Update all nodes in the mapping
          [state] for the tree rooted at [root] with an event [e], that happened
          at [accessed]. *)
      val access :
        Ptr_tag.t ->
        access ->
        Tree.t option ->
        t option ->
        (t option, [> `AliasingError ], syn_full list) Symex.Result.t

      val merge : t -> t -> t Symex.t

      val assert_exclusively_owned :
        t option -> (unit, 'e, syn list) Symex.Result.t
    end
  end
end

module type T = (Symex : Rust_symex) -> M(Symex).S
