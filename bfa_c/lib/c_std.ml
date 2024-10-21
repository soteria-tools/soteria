open Csymex
open Csymex.Syntax

let malloc ~prog:_ ~args ~state =
  let* sz =
    match args with
    | [ sz ] -> return sz
    | _ -> not_impl "malloc with non-one arguments"
  in
  Csymex.branches
    [
      (fun () -> Heap.alloc sz state);
      (fun () -> Result.ok (Svalue.Ptr.null, state));
    ]
