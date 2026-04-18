open Prelude

module Steps = struct
  type t = { v : int }

  let pp ft { v } = Fmt.pf ft "Steps(%d)" v
  let is_empty { v } = v = 0
end

type t = {
  heap : Heap.t option;
  steps : Steps.t;
      [@sym_state.ignore
        { empty = { Steps.v = 0 }; is_empty = Steps.is_empty; pp = Steps.pp }]
}
[@@deriving sym_state { symex = Symex }]
