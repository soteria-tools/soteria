open Rust_val
open Typed.Infix
open Typed.Syntax

module M (State : State_intf.S) = struct
  open State_monad.Make (State)
  open Syntax

  let alloc ?(zeroed = false) args =
    let size, align =
      match args with
      | [ Int size; Int align ] -> (size, align)
      | _ -> failwith "alloc: invalid arguments"
    in
    let align = Typed.cast_i Usize align in
    let size = Typed.cast_i Usize size in
    let max_size = Layout.max_value_z (TInt Isize) in
    let max_size = Typed.BitVec.usize max_size in
    let* () =
      State.assert_ (Usize.(1s) <=@ align &&@ (size <@ max_size)) `InvalidAlloc
    in
    let align = Typed.cast align in
    let+ ptr = State.alloc_untyped ~zeroed ~size ~align () in
    Ptr ptr

  let dealloc args =
    let ((ptr_in, _) as ptr), size, align =
      match args with
      | [ Ptr ptr; Int size; Int align ] -> (ptr, size, align)
      | [ Int ptr; Int size; Int align ] ->
          let ptr = Typed.cast_i Usize ptr in
          let ptr = Sptr.null_ptr_of ptr in
          ((ptr, Thin), size, align)
      | _ -> failwith "dealloc: invalid arguments"
    in
    let alloc_size, alloc_align = Sptr.allocation_info ptr_in in
    let* () =
      State.assert_
        (alloc_align ==?@ align &&@ (alloc_size ==?@ size))
        `InvalidFree
    in
    let+ () = State.free ptr in
    Tuple []

  let realloc args =
    let ptr, old_size, align, size =
      match args with
      | [ Ptr ptr; Int old_size; Int align; Int size ] ->
          (ptr, old_size, align, size)
      | _ -> failwith "realloc: invalid arguments"
    in
    let ptr_in, _ = ptr in
    let prev_size, prev_align = Sptr.allocation_info ptr_in in
    let* () =
      State.assert_
        (prev_align ==?@ align &&@ (prev_size ==?@ old_size))
        `InvalidAlloc
    in
    let align = Typed.cast align in
    let size = Typed.cast_i Usize size in
    let* new_ptr = State.alloc_untyped ~zeroed:false ~size ~align () in
    let* () =
      if%sat size >=@ prev_size then
        State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size:prev_size
      else State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size
    in
    let+ () = State.free ptr in
    Ptr new_ptr

  let no_alloc_shim_is_unstable _ = ok (Tuple [])
end
