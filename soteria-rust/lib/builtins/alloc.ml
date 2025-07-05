open Charon_util
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix

module M (State : State_intf.S) = struct
  let alloc ~args state =
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
      let++ ptr, state = State.alloc_untyped ~size ~align state in
      (Ptr ptr, state)
    else State.error `InvalidLayout state

  let dealloc ~args state =
    let ptr =
      match args with
      | Ptr ptr :: _ -> ptr
      | _ -> failwith "dealloc: invalid arguments"
    in
    let++ (), state = State.free ptr state in
    (Tuple [], state)

  let realloc ~args:_ _ = not_impl "Not impl: alloc::realloc"
  let alloc_zeroed ~args:_ _ = not_impl "Not impl: alloc::alloc_zeroed"
  let no_alloc_shim_is_unstable ~args:_ state = Result.ok (Tuple [], state)
end
