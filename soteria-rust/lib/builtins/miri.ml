open Rustsymex
open Typed
open Charon_util

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr

  type nonrec rust_val = Sptr.t rust_val

  let alloc_id ~args state =
    let loc =
      match args with
      | [ Ptr (ptr, _) ] -> Sptr.as_id ptr
      | _ -> failwith "alloc_id: invalid arguments"
    in
    Result.ok (Base (loc :> T.cval Typed.t), state)
end
