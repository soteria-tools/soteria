open Csymex
open Csymex.Syntax
open Typed.Syntax
module T = Typed.T

let malloc ~prog:_ ~(args : T.cval Typed.t list) ~state =
  let* sz =
    match args with
    | [ sz ] -> return sz
    | _ -> not_impl "malloc with non-one arguments"
  in
  let sz = Typed.cast sz in
  Csymex.branches
    [
      (fun () -> Heap.alloc sz state);
      (fun () -> Result.ok (Typed.Ptr.null, state));
    ]

let free ~prog:_ ~(args : T.cval Typed.t list) ~state =
  let* ptr =
    match args with
    | [ ptr ] -> return ptr
    | _ -> not_impl "free with non-one arguments"
  in
  match Typed.get_ty ptr with
  | TPointer ->
      let++ (), state =
        if%sat Typed.Ptr.is_null (Typed.cast ptr) then Result.ok ((), state)
        else Heap.free (Typed.cast ptr) state
      in
      (0s, state)
  | TInt -> Fmt.kstr not_impl "free with int argument: %a" Typed.ppa ptr
  | _ -> Fmt.kstr not_impl "free with non-pointer argument: %a" Typed.ppa ptr

let memcpy ~prog:_ ~(args : T.cval Typed.t list) ~state =
  let* dst, src, size =
    match args with
    | [ dst; src; size ] -> return (dst, src, size)
    | _ -> not_impl "memcpy with non-three arguments"
  in
  let dst = Typed.cast dst in
  let src = Typed.cast src in
  let size = Typed.cast size in
  let++ (), state = Heap.copy_nonoverlapping ~dst ~src ~size state in
  (dst, state)
