open Rust_val
open Typed.Syntax
open Typed.Infix

type fn = Exit | Sysconf | TlvAtexit | Malloc | Calloc | Free | Realloc

let fn_pats =
  [
    ("exit", Exit);
    ("sysconf", Sysconf);
    ("_tlv_atexit", TlvAtexit);
    ("malloc", Malloc);
    ("calloc", Calloc);
    ("free", Free);
    ("realloc", Realloc);
  ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let exit args =
    match args with
    | [ Int code ] ->
        if%sat code ==@ U32.(0s) then error `OkExit else error (`Exit code)
    | _ -> failwith "exit: invalid arguments"

  let sysconf args =
    match args with
    | [ Int _ ] ->
        (* https://man7.org/linux/man-pages/man3/sysconf.3.html
         * It is basically ok to always return the i64 `-1` saying "I don't know"
         *)
        let ret = Typed.BitVec.u64 Z.minus_one in
        ok (Int ret)
    | _ -> failwith "sysconf: invalid arguments"

  let malloc_alloc ~zeroed size =
    (* [max_align_t] is platform-defined; 16 covers x86_64 / arm64. *)
    let align = Typed.BitVec.usize (Z.of_int 16) in
    let size = Typed.cast_i Usize size in
    let max_size = Layout.max_value_z (TInt Isize) in
    let max_size = Typed.BitVec.usize max_size in
    if%sat size <=@ max_size then
      let+ ptr =
        State.alloc_untyped ~zeroed ~size ~align:(Typed.cast align) ()
      in
      Ptr ptr
    else
      (* malloc returns NULL on failure; under-approximate by saying allocation
         always succeeds for valid sizes. *)
      ok (Ptr (Sptr.null (), Thin))

  let malloc args =
    match args with
    | [ Int size ] -> malloc_alloc ~zeroed:false size
    | _ -> failwith "malloc: invalid arguments"

  let calloc args =
    match args with
    | [ Int nmemb; Int size ] ->
        let nmemb = Typed.cast_i Usize nmemb in
        let size = Typed.cast_i Usize size in
        let total = Typed.BitVec.(no_ovf_unsafe (mul nmemb size)) in
        malloc_alloc ~zeroed:true total
    | _ -> failwith "calloc: invalid arguments"

  let free args =
    match args with
    | [ Ptr ((sptr, _) as ptr) ] ->
        (* free(NULL) is a no-op per C99. *)
        if%sat Sptr.is_null sptr then ok (Tuple [])
        else
          let+ () = State.free ptr in
          Tuple []
    | _ -> failwith "free: invalid arguments"

  let realloc args =
    match args with
    | [ Ptr ((sptr, _) as ptr); Int size ] ->
        let size = Typed.cast_i Usize size in
        if%sat Sptr.is_null sptr then malloc_alloc ~zeroed:false size
        else
          let align = Typed.BitVec.usize (Z.of_int 16) in
          let prev_size, _prev_align = Sptr.allocation_info sptr in
          let* new_ptr =
            State.alloc_untyped ~zeroed:false ~size
              ~align:(Typed.cast align) ()
          in
          let copy_size = Typed.BitVec.min ~signed:false size prev_size in
          let* () =
            State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size:copy_size
          in
          let+ () = State.free ptr in
          Ptr new_ptr
    | _ -> failwith "realloc: invalid arguments"

  (** macOS thread-local destructor registration. Same behaviour as the Rust
      wrapper in [system/impl.ml] (which targets the Rust-side function), but
      invoked here when called as an [extern "C"] symbol. *)
  let tlv_atexit fun_exec args =
    match args with
    | [ Ptr dtor; arg ] ->
        let+ () =
          State.register_thread_exit (fun () ->
              let* fn = State.lookup_fn dtor in
              let* _ = fun_exec fn [ arg ] in
              ok ())
        in
        Tuple []
    | _ ->
        Fmt.kstr not_impl "_tlv_atexit: unexpected arguments: %a"
          Fmt.(list pp_rust_val)
          args

  let[@inline] fn_to_stub fun_exec = function
    | Exit -> exit
    | Sysconf -> sysconf
    | TlvAtexit -> tlv_atexit fun_exec
    | Malloc -> malloc
    | Calloc -> calloc
    | Free -> free
    | Realloc -> realloc
end
