module type S = State_intf.S

module Tree_state = Tree_state.Make (Tree_borrows.Concrete)
module StateM = Rust_state_m
