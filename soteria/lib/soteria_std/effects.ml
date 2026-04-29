(** Module type for an effect that can be managed, by dumping its data
    "somewhere" (typically a file, configured externally), or by ignoring it. *)
module type Managed_effect = sig
  (** The type of the argument to the effect, if it needs to be tracked. *)
  type arg

  (** [with_dumped () f] runs function [f] and handles effects raised by the
      functions of this module, and dumps the resulting data to the file
      specified by the current (externally-defined) configuration if it is set,
      or ignores them otherwise (if configured to do so). *)
  val with_dumped : arg -> (unit -> 'a) -> 'a

  (** [with_ignored () f] runs function [f] and handles effects raised by the
      functions of this module, but ignores their effect. This is to be used
      when the user does not wish to pay the performance cost of tracking the
      effect. *)
  val with_ignored : unit -> (unit -> 'a) -> 'a
end
