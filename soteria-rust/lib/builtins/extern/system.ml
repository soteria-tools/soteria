open Charon
open Svalue
open Typed.Syntax
open Typed.Infix
open Common.Charon_util

type fn =
  | TlvAtexit
  | CxaThreadAtexitImpl
  | DsoHandle
  | PthreadKeyCreate
  | PthreadKeyNoop

let fn_pats =
  [
    ("_tlv_atexit", TlvAtexit);
    (* Linux weak `extern` symbols read by the thread-local destructor
       registration. We don't link against the C runtime, so these weak symbols
       resolve to null. *)
    ("__cxa_thread_atexit_impl", CxaThreadAtexitImpl);
    ("__dso_handle", DsoHandle);
    (* Linux falls back to a pthread TLS key whose destructor drains the
       thread-local destructor list at thread exit. *)
    ("pthread_key_create", PthreadKeyCreate);
    ("pthread_setspecific", PthreadKeyNoop);
    ("pthread_key_delete", PthreadKeyNoop);
  ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  (** Used on macOS to register thread local destructors; receives a function
      pointer and an argument. Should call the destructor with the argument at
      the end of the thread.

      [_tlv_atexit(dtor: unsafe extern "C" fn( *mut u8), arg: *mut u8)] *)
  let tlv_atexit ~fun_exec args =
    let* dtor, arg =
      match args with
      | [ dtor_ptr; arg_ptr ] -> ok (Typed.cast_ptr_f dtor_ptr, arg_ptr)
      | _ ->
          not_impl "tlv_atexit: unexpected arguments: %a"
            Fmt.(list Typed.ppa)
            args
    in
    let+ () =
      State.register_thread_exit (fun () ->
          let* fn = State.lookup_fn dtor in
          map ignore @@ fun_exec fn [ arg ])
    in
    Typed.Adt.mk_tuple []

  (** [__cxa_thread_atexit_impl: Option<extern "C" fn(...)>] is a weak symbol we
      don't link against, so it reads as [None]; the destructor registration
      then takes its fallback (`register_dtor_fallback`) path. *)
  let cxa_thread_atexit_impl ~(fun_sig : Types.fun_sig) _args =
    let adt = ty_as_adt fun_sig.output in
    ok (Typed.Adt.Checked.mk_enum adt "None" [])

  (** [__dso_handle: *mut u8] is likewise an unlinked weak symbol, i.e. null. *)
  let dso_handle _args = ok (Typed.Ptr.mk_ptr_f (Sptr.null ()) None)

  (** [pthread_key_create] takes a key out-pointer and an optional destructor.
      We don't model pthread TLS storage, so any non-sentinel key works; std
      asserts the created key isn't [KEY_SENTVAL] (0), so we write [1]. The
      key's destructor (which ignores its argument and just drains the
      thread-local destructor list) is registered to run at thread exit. *)
  let pthread_key_create ~(fun_sig : Types.fun_sig) ~fun_exec args =
    let key, dtor =
      match args with
      | [ key; dtor ] -> (Typed.cast_ptr_f key, dtor)
      | _ -> L.failwith "pthread_key_create: unexpected arguments"
    in
    let* () = State.store key (TLiteral (TUInt U32)) U32.(1s) in
    let* dtor =
      State.transmute ~from:(List.nth fun_sig.inputs 1) ~to_:unit_ptr dtor
    in
    let dtor = Typed.cast_ptr_f dtor in
    let dtor_inner = Typed.Ptr.ptr_of dtor in
    if%sat Sptr.is_null dtor_inner then ok U32.(0s)
    else
      let+ () =
        State.register_thread_exit (fun () ->
            let* fn = State.lookup_fn dtor in
            map ignore @@ fun_exec fn [ Typed.Ptr.mk_ptr_f (Sptr.null ()) None ])
      in
      U32.(0s)

  (** [pthread_setspecific]/[pthread_key_delete] only manage the (unmodelled)
      TLS storage and marker; returning success is enough. *)
  let pthread_key_noop _args = ok U32.(0s)

  let[@inline] fn_to_stub fun_sig fun_exec = function
    | TlvAtexit -> tlv_atexit ~fun_exec
    | CxaThreadAtexitImpl -> cxa_thread_atexit_impl ~fun_sig
    | DsoHandle -> dso_handle
    | PthreadKeyCreate -> pthread_key_create ~fun_sig ~fun_exec
    | PthreadKeyNoop -> pthread_key_noop
end
