open Rust_val
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix

module M (State : State_intf.S) = struct
  let alloc ?(zeroed = false) ~args state =
    let size, align =
      match args with
      | [ Base size; Base align ] -> (size, align)
      | _ -> failwith "alloc: invalid arguments"
    in
    let align = Typed.cast_i Usize align in
    let size = Typed.cast_i Usize size in
    let max_size = Layout.max_value_z (TInt Isize) in
    let max_size = Typed.BitVec.usize max_size in
    let min_align = Typed.BitVec.usizei 1 in
    if%sat align >=@ min_align &&@ (size <@ max_size) then
      let align = Typed.cast align in
      let++ ptr, state = State.alloc_untyped ~zeroed ~size ~align state in
      (Ptr ptr, state)
    else State.error `InvalidAlloc state

  let dealloc ~args state =
    let ((ptr_in, _) as ptr), size, align =
      match args with
      | [ Ptr ptr; Base size; Base align ] -> (ptr, size, align)
      | _ -> failwith "dealloc: invalid arguments"
    in
    let alloc_size, alloc_align = State.Sptr.allocation_info ptr_in in
    if%sat alloc_align ==?@ align &&@ (alloc_size ==?@ size) then
      let++ (), state = State.free ptr state in
      (Tuple [], state)
    else State.error `InvalidFree state

  let realloc ~args state =
    let ptr, old_size, align, size =
      match args with
      | [ Ptr ptr; Base old_size; Base align; Base size ] ->
          (ptr, old_size, align, size)
      | _ -> failwith "realloc: invalid arguments"
    in
    let ptr_in, _ = ptr in
    let prev_size, prev_align = State.Sptr.allocation_info ptr_in in
    if%sat prev_align ==?@ align &&@ (prev_size ==?@ old_size) then
      let align = Typed.cast align in
      let size = Typed.cast_i Usize size in
      let** new_ptr, state = State.alloc_untyped ~size ~align state in
      let** (), state =
        if%sat size >=@ prev_size then
          State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size:prev_size state
        else not_impl "Can't realloc to smaller size"
      in
      let++ (), state = State.free ptr state in
      (Ptr new_ptr, state)
    else State.error `InvalidAlloc state

  let no_alloc_shim_is_unstable ~args:_ state = Result.ok (Tuple [], state)
end
