open Svalue

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM

  let new_ ~(fun_sig : Charon.Types.fun_sig) =
    map Typed.cast_any_adt @@ Value_codec.nondet_valid fun_sig.output
end
