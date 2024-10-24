open Csymex
open Csymex.Syntax
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
