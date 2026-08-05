(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Charon
open Svalue
open Common

module M (StateM : State.StateM.S) = struct
  open StateM

  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> Typed.(T.any t) list -> Typed.(T.any t) ret

  module type S = sig
    val cleanup : payload:Typed.([< T.sptr_f ] t) -> Typed.([> T.tuple ] t) ret
  end
end
