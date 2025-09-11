open Rust_val

module M (State : State_intf.S) = struct
  open State_monad.Make (State)

  let alloc_id args =
    match args with
    | [ Ptr (ptr, _) ] -> ok (Base (Sptr.as_id ptr :> T.cval Typed.t))
    | _ -> failwith "alloc_id: invalid arguments"
end
