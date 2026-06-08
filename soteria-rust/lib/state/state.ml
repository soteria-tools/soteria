module type S = State_intf.S

module Tree_state = Tree_state
module StateM = Rust_state_m

(* check the interface matches *)
module _ : S = Tree_state.Make (Tree_borrows.Concrete.Make)
