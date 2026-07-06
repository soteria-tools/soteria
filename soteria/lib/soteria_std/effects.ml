(** Effect used for bookkeeping, which keeps tracks of some information that can
    then be dumped "somewhere" (typically a file, configured externally), or
    ignored. *)

module Bookkeeping = struct
  (** Module type of a bookkeeping*)
  module type S = sig
    (** The type of the information tracked by the effect. *)
    type t

    (** The type of the argument to the effect, if it needs to be tracked. *)
    type arg

    (** [with_ arg f] runs function [f] and handles effects raised by the
        functions of this module, and returns a pair of the result of [f] and
        the data tracked by the effect. This is to be used when the user wants
        to directly manage the data tracked by the effect. *)
    val with_ : arg -> (unit -> 'a) -> 'a * t

    (** [with_dumped arg f] runs function [f] and handles effects raised by the
        functions of this module, and dumps the resulting data to the file
        specified by the current (externally-defined) configuration if it is
        set, or ignores them otherwise (if configured to do so). *)
    val with_dumped : arg -> (unit -> 'a) -> 'a

    (** [with_ignored () f] runs function [f] and handles effects raised by the
        functions of this module, but ignores their effect. This is to be used
        when the user does not wish to pay the performance cost of tracking the
        effect. *)
    val with_ignored : unit -> (unit -> 'a) -> 'a
  end

  (** The different ways a managed effect can be handled. *)
  type 'a mode =
    | Ignore
        (** Handle the effect by ignoring it, so don't collect any information.
        *)
    | Dump of 'a
        (** Handle the effect, and dump the information according to the
            relevant configuration. The value is the parameter passed to the
            feature's dump handler. *)
    | Caller
        (** The effect is already handled by the caller, nothing needs to be
            done. *)

  let manage (module M : S) (x : M.arg mode) =
    match x with
    | Ignore -> M.with_ignored ()
    | Dump arg -> M.with_dumped arg
    | Caller -> fun f -> f ()
end
