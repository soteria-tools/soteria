(** Miri-related intrinsics.

    See https://github.com/rust-lang/miri/blob/master/tests/utils/miri_extern.rs
*)

open Rust_val

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let alloc_id args =
    match args with
    | [ Ptr (ptr, _) ] -> ok (Int (Sptr.as_id ptr))
    | _ -> not_impl "alloc_id: invalid arguments"

  let promise_alignement args =
    match args with
    | [ Ptr (ptr, _); Int align ] ->
        let align = Typed.cast @@ Typed.cast_i Usize align in
        let is_aligned = Sptr.is_aligned align ptr in
        let+ () = assume [ is_aligned ] in
        Tuple []
    | _ -> not_impl "miri_promise_symbolic_alignment: invalid arguments"
end
