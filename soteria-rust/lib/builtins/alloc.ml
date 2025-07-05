open Charon_util
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix

module M (State : State_intf.S) = struct
  let alloc ?(zeroed = false) ~args state =
    let size, align =
      match args with
      | [ Base align; Base size ] -> (align, size)
      | _ -> failwith "alloc: invalid arguments"
    in
    let* align = cast_checked ~ty:Typed.t_int align in
    let* size = cast_checked ~ty:Typed.t_int size in
    let max_size = Layout.max_value Isize in
    if%sat align >=@ 1s &&@ (size <@ max_size) then
      let align = Typed.cast align in
      let++ ptr, state = State.alloc_untyped ~zeroed ~size ~align state in
      (Ptr ptr, state)
    else State.error `InvalidLayout state

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
    let ptr, old_size, size, align =
      match args with
      | [ Ptr ptr; Base old_size; Base size; Base align ] ->
          (ptr, old_size, size, align)
      | _ -> failwith "realloc: invalid arguments"
    in
    let ptr_in, _ = ptr in
    let prev_size, prev_align = State.Sptr.allocation_info ptr_in in
    if%sat prev_align ==?@ align &&@ (prev_size ==?@ old_size) then
      let align = Typed.cast align in
      let* size = cast_checked ~ty:Typed.t_int size in
      let** new_ptr, state = State.alloc_untyped ~size ~align state in
      let** (), state =
        if%sat size >=@ size then
          State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size:prev_size state
        else not_impl "Can't realloc to smaller size"
      in
      let++ (), state = State.free ptr state in
      (Ptr new_ptr, state)
    else State.error `InvalidLayout state

  let no_alloc_shim_is_unstable ~args:_ state = Result.ok (Tuple [], state)
end
