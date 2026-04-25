(** Miri-related builtins.

    See https://github.com/rust-lang/miri/blob/master/tests/utils/miri_extern.rs
*)

open Rust_val

type fn = Alloc | AllocId | Dealloc | Nop | PromiseAlignment

let fn_pats =
  [
    ("miri_get_alloc_id", AllocId);
    ("miri_pointer_name", Nop);
    ("miri_print_borrow_state", Nop);
    ("miri_run_provenance_gc", Nop);
    ("miri_write_to_stdout", Nop);
    ("miri_alloc", Alloc);
    ("miri_dealloc", Dealloc);
    ("miri_promise_symbolic_alignment", PromiseAlignment);
  ]

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
        let is_aligned, _err = Sptr.is_aligned align ptr in
        let+ () = assume [ is_aligned ] in
        Tuple []
    | _ -> not_impl "miri_promise_symbolic_alignment: invalid arguments"

  let alloc args = Alloc.alloc ~zeroed:false args
  let dealloc args = Alloc.dealloc args
  let nop _ = ok (Tuple [])

  let[@inline] fn_to_stub = function
    | Nop -> nop
    | Alloc -> alloc
    | Dealloc -> dealloc
    | AllocId -> alloc_id
    | PromiseAlignment -> promise_alignement
end
