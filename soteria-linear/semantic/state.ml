open Aux

type err = [ `UseAfterFree | `Interp of string | Symex.cons_fail ]
[@@deriving show { with_path = false }]

type t = Excl_val.t Freeable.t PMap.t [@@deriving show { with_path = false }]

type syn = Excl_val.syn Freeable.syn PMap.syn
[@@deriving show { with_path = false }]

let ins_outs (syn : syn) =
  PMap.ins_outs (Freeable.ins_outs Excl_val.ins_outs) syn

let to_syn (st : t option) : syn list =
  match st with
  | None -> []
  | Some pmap -> PMap.to_syn (Freeable.to_syn Excl_val.to_syn) pmap

let empty : t option = None
let load addr st = PMap.wrap (Freeable.wrap Excl_val.load) addr st

let store addr value st =
  PMap.wrap (Freeable.wrap (Excl_val.store value)) addr st

let alloc st =
  let v_false = (S_val.v_false :> S_val.t) in
  PMap.alloc ~new_codom:(Freeable.Alive v_false) st

let free addr st =
  PMap.wrap
    (Freeable.free ~assert_exclusively_owned:Excl_val.assert_exclusively_owned)
    addr st

let error msg _state = `Interp msg
let consume s t = PMap.consume (Freeable.consume Excl_val.consume) s t

let produce (s : syn) (t : t option) =
  PMap.produce (Freeable.produce Excl_val.produce) s t
