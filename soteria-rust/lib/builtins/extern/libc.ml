open Rust_val
open Typed.Syntax
open Typed.Infix

type fn = Exit | Sysconf | Malloc | Free

let fn_pats =
  [ ("exit", Exit); ("free", Free); ("malloc", Malloc); ("sysconf", Sysconf) ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let exit args =
    match args with
    | [ Int code ] ->
        if%sat code ==@ U32.(0s) then error `OkExit else error (`Exit code)
    | _ -> L.failwith "exit: invalid arguments"

  (* [malloc]/[free] return memory aligned for any fundamental type; we model
     them as always succeeding, like the Rust allocator shims in {!Alloc}. *)
  let malloc args =
    let size =
      match args with
      | [ Int size ] -> Typed.cast_i Usize size
      | _ -> failwith "malloc: invalid arguments"
    in
    let max_size = Typed.BitVec.usize (Layout.max_value_z (TInt Isize)) in
    let* () = assert_ (size <=@ max_size) `InvalidAlloc in
    (* we under-approximate here: the alignment can be smaller (its min size is
       the first power of 2 greater than or equal to the requested size) *)
    let align = Typed.BitVec.usizeinz (2 * Crate.pointer_size ()) in
    let+ ptr = State.alloc_untyped ~zeroed:false ~size ~align () in
    Ptr ptr

  let free args =
    match args with
    | [ Ptr ((ptr_in, _) as ptr) ] ->
        (* [free(NULL)] is a no-op. *)
        if%sat Sptr.is_null ptr_in then ok (Tuple [])
        else
          let+ () = State.free ptr in
          Tuple []
    | _ -> failwith "free: invalid arguments"

  let sysconf args =
    match args with
    | [ Int _ ] ->
        (* https://man7.org/linux/man-pages/man3/sysconf.3.html
         * It is basically ok to always return the i64 `-1` saying "I don't know"
         *)
        let ret = Typed.BitVec.u64 Z.minus_one in
        ok (Int ret)
    | _ -> L.failwith "sysconf: invalid arguments"

  let[@inline] fn_to_stub = function
    | Exit -> exit
    | Free -> free
    | Malloc -> malloc
    | Sysconf -> sysconf
end
