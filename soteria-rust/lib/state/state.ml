module type S = State_intf.S
module type State_M = Rust_state_m.S

module Tree_state : S = Tree_state
module Make_monad = Rust_state_m.Make
