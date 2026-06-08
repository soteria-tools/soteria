open Svalue

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM

  let new_ ~(fun_sig : Charon.Types.fun_sig) =
    map Typed.cast @@ Encoder.nondet_valid fun_sig.output
end
