open Svalue
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
    | [ code ] ->
        let code = Typed.cast_i U32 code in
        if%sat code ==@ U32.(0s) then error `OkExit else error (`Exit code)
    | _ -> L.failwith "exit: invalid arguments"

  (* [malloc]/[free] return memory aligned for any fundamental type; we model
     them as always succeeding, like the Rust allocator shims in {!Alloc}. *)
  let malloc args =
    let size =
      match args with
      | [ size ] -> Typed.cast_i Usize size
      | _ -> L.failwith "malloc: invalid arguments"
    in
    let max_size = Typed.BitVec.usize (Layout.max_value_z (TInt Isize)) in
    let* () = assert_ (size <=@ max_size) `InvalidAlloc in
    (* we under-approximate here: the alignment can be smaller (its min size is
       the first power of 2 greater than or equal to the requested size) *)
    let align = Typed.BitVec.usizeinz (2 * Crate.pointer_size ()) in
    State.alloc_untyped ~zeroed:false ~size ~align ()

  let free args =
    match args with
    | [ ptr ] ->
        (* [free(NULL)] is a no-op. *)
        let ptr = Typed.cast_ptr_f ptr in
        let ptr_in = Typed.Ptr.ptr_of ptr in
        if%sat Sptr.is_null ptr_in then ok (Typed.Adt.mk_tuple [])
        else
          let+ () = State.free ptr in
          Typed.Adt.mk_tuple []
    | _ -> L.failwith "free: invalid arguments"

  let sysconf _args =
    (* https://man7.org/linux/man-pages/man3/sysconf.3.html
     * It is basically ok to always return the i64 `-1` saying "I don't know"
     *)
    ok (Typed.BitVec.u64i (-1))

  let[@inline] fn_to_stub = function
    | Exit -> exit
    | Free -> free
    | Malloc -> malloc
    | Sysconf -> sysconf
end
