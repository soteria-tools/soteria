(** Miri-related intrinsics.

    See https://github.com/rust-lang/miri/blob/master/tests/utils/miri_extern.rs
*)

open Rust_val
open Typed.Infix
open Typed.Syntax

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax
  module Alloc = Alloc.M (StateM)

  let alloc_id args =
    match args with
    | [ Ptr (ptr, _) ] -> ok (Int (Sptr.as_id ptr))
    | _ -> not_impl "alloc_id: invalid arguments"

  let promise_alignement args =
    match args with
    | [ Ptr (ptr, _); Int align ] ->
        let align = Typed.cast @@ Typed.cast_i Usize align in
        let* addr = Sptr.decay ptr in
        let+ () = assume [ (addr %@ align ==@ Usize.(0s)) ] in
        Tuple []
    | _ -> not_impl "miri_promise_symbolic_alignment: invalid arguments"

  let alloc args = Alloc.alloc ~zeroed:false args
  let dealloc args = Alloc.dealloc args
end
