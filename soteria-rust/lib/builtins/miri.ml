open Rust_val

module M (State : State_intf.S) = struct
  open State_monad.Make (State)
  open Syntax

  let alloc_id args =
    match args with
    | [ Ptr (ptr, _) ] -> ok (Base (Sptr.as_id ptr :> T.cval Typed.t))
    | _ -> not_impl "alloc_id: invalid arguments"

  let promise_alignement args =
    match args with
    | [ Ptr (ptr, _); Base align ] ->
        let align = Typed.cast @@ Typed.cast_i Usize align in
        let is_aligned = Sptr.is_aligned align ptr in
        let+ () = assume [ is_aligned ] in
        Tuple []
    | _ -> not_impl "miri_promise_symbolic_alignment: invalid arguments"
end
