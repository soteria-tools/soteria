open Soteria_std

module Config : sig
  type t = {
    flamegraphs : string option;
        (** Specify the folder in which the flamegraphs will be saved. *)
  }

  val cmdliner_term : unit -> t Cmdliner.Term.t
  val set_and_lock : t -> unit
  val get : unit -> t
end

module Flamegraph : sig
  (** Module for creating flamegraph files ([.folded], using the format
      described in
      {{:https://github.com/brendangregg/flamegraph}Brendan Gregg's Flamegraph
       repository}). This system is integrated with the {!Symex} module, making
      it easy and convenient to use. *)

  module Make (M : Monad.Base) : sig
    include Effects.Bookkeeping with type arg := string

    (* FIXME: unfortunately we can't just reuse [Reversible.Effectful] here as
       we don't want to expose a [wrap] or [run] *)

    (** Checkpoints the current execution time, by increasing the total time of
        the current frame with the time elapsed since the last checkpoint. This
        is automatically called by [backtrack_n], but can also be called
        manually to checkpoint at specific points in the execution. See also
        {!Symex.Core.with_frame}. *)
    val checkpoint : unit -> unit

    (** [with_frame name f] runs function [f] inside a new frame with the given
        name [name], and adds the time spent executing [f] to that frame. This
        implicitly checkpoints before and after executing [f]. *)
    val with_frame : string -> (unit -> 'a M.t) -> 'a M.t

    (** Backtracks the current flamegraph state; this also checkpoints (see
        {!checkpoint}) *)
    val backtrack_n : int -> unit

    (** Saves the current flamegraph state. *)
    val save : unit -> unit
  end
end
