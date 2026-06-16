module type S = State_intf.S

module Tree_state = Tree_state
module StateM = Rust_state_m

(* we use this to assert the signature of the module without erasing its
   types *)

module _ : functor (_ : Tree_borrows.T) -> S = Tree_state.Make
