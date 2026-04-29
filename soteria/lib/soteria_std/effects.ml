(** Module type for an effect that can be managed, by dumping its data
    "somewhere" (typically a file, configured externally), or by ignoring it. *)
module type Managed_effect = sig
  (** The type of the information tracked by the effect. *)
  type t

  (** The type of the argument to the effect, if it needs to be tracked. *)
  type arg

  (** [with_ arg f] runs function [f] and handles effects raised by the
      functions of this module, and returns a pair of the result of [f] and the
      data tracked by the effect. This is to be used when the user wants to
      directly manage the data tracked by the effect. *)
  val with_ : arg -> (unit -> 'a) -> 'a * t

  (** [with_dumped arg f] runs function [f] and handles effects raised by the
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
