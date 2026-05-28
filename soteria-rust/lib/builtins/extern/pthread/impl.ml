(** POSIX pthread synchronization primitives.

    Rust's standard library on Unix targets uses [pthread_mutex_*],
    [pthread_rwlock_*] and [pthread_cond_*] (plus their attribute counterparts)
    to back [std::sync::Mutex], [RwLock] and [Condvar]. Implementations here
    take inspiration from Miri's
    {{:https://doc.rust-lang.org/beta/nightly-rustc/src/miri/shims/unix/sync.rs.html#466-964}
     [shims/unix/sync.rs]}, with the simplification that Soteria currently
    explores a single thread: locks have no notion of "another thread", so
    locking an already-locked non-recursive mutex deadlocks immediately. *)

open Charon
open Rust_val
open Typed.Infix

(** [PTHREAD_MUTEX_KIND_UNCHANGED] sentinel from Miri (see [sync.rs]). Written
    to a freshly-initialised mutex attribute to distinguish it from a
    user-chosen kind. *)
let pthread_mutex_kind_unchanged = Z.of_int 0x8000000

(* Hardcoded libc constants (Linux glibc values). These should eventually be
   sourced from FFI, since e.g. macOS uses different errno numbers. *)
let pthread_mutex_normal = Z.zero
let pthread_mutex_default = Z.zero
let pthread_mutex_recursive = Z.one
let pthread_mutex_errorcheck = Z.of_int 2
let errno_ebusy = Z.of_int 16
let errno_einval = Z.of_int 22
let errno_edeadlk = Z.of_int 35
let errno_eperm = Z.of_int 1

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  let i32_ty : Types.ty = TLiteral (TInt I32)

  let as_ptr_arg name = function
    | Ptr p -> ok p
    | _ -> Fmt.kstr not_impl "%s: expected a pointer argument" name

  let ret_i32 z = Int (Typed.BitVec.u32 z)
  let zero = ret_i32 Z.zero

  (** Offsets a [full_ptr] by [n] bytes (unchecked). *)
  let offset_bytes (ptr, meta) n =
    let n = Typed.BitVec.usize (Z.of_int n) in
    let+ ptr' = Sptr.offset ~check:false ~signed:false n ptr in
    (ptr', meta)

  let load_i32 ptr =
    let+ v = State.load ptr i32_ty in
    Rust_val.as_base_i Charon.Values.U32 v

  let store_i32 ptr v = State.store ptr i32_ty (Int (Typed.BitVec.u32 v))

  (* -- mutex attributes -- *)

  (** [pthread_mutexattr_init(attr)]: stores [PTHREAD_MUTEX_KIND_UNCHANGED] in
      the attribute's kind slot (we use offset 0 since [pthread_mutexattr_t] is
      opaque in libc). Returns 0. *)
  let pthread_mutexattr_init args =
    match args with
    | [ attr ] ->
        let* attr_ptr = as_ptr_arg "pthread_mutexattr_init" attr in
        let kind = Int (Typed.BitVec.u32 pthread_mutex_kind_unchanged) in
        let* () = State.store attr_ptr i32_ty kind in
        ok zero
    | _ -> not_impl "pthread_mutexattr_init: expected 1 argument"

  (** [pthread_mutexattr_destroy(attr)]: reads the [kind] field to ensure the
      attribute was initialised (an uninit read traps as UB), then uninitialises
      it so a subsequent destroy errors out. Returns 0. *)
  let pthread_mutexattr_destroy args =
    match args with
    | [ attr ] ->
        let* attr_ptr = as_ptr_arg "pthread_mutexattr_destroy" attr in
        let* _ = State.load attr_ptr i32_ty in
        let* () = State.uninit attr_ptr i32_ty in
        ok zero
    | _ -> not_impl "pthread_mutexattr_destroy: expected 1 argument"

  (** [pthread_mutexattr_settype(attr, kind)]: stores [kind] into the attribute.
      Returns 0 if [kind] is one of the recognised values, [EINVAL] otherwise.
  *)
  let pthread_mutexattr_settype args =
    match args with
    | [ attr; kind ] ->
        let* attr_ptr = as_ptr_arg "pthread_mutexattr_settype" attr in
        let kind = Rust_val.as_base_i Charon.Values.U32 kind in
        let is_valid =
          Typed.BitVec.u32 pthread_mutex_normal
          ==@ kind
          ||@ (Typed.BitVec.u32 pthread_mutex_default ==@ kind)
          ||@ (Typed.BitVec.u32 pthread_mutex_recursive ==@ kind)
          ||@ (Typed.BitVec.u32 pthread_mutex_errorcheck ==@ kind)
        in
        if%sat is_valid then
          let* () = State.store attr_ptr i32_ty (Int kind) in
          ok zero
        else ok (ret_i32 errno_einval)
    | _ -> not_impl "pthread_mutexattr_settype: expected 2 arguments"

  (* -- mutex -- *)

  (** Reads the kind from an attribute pointer (or returns [DEFAULT] for null);
      translates [UNCHANGED] to [DEFAULT]. *)
  let mutex_kind_from_attr attr =
    match attr with
    | Ptr ((sptr, _) as attr_ptr) ->
        if%sat Sptr.is_null sptr then
          ok (Typed.BitVec.u32 pthread_mutex_default)
        else
          let* k = load_i32 attr_ptr in
          if%sat k ==@ Typed.BitVec.u32 pthread_mutex_kind_unchanged then
            ok (Typed.BitVec.u32 pthread_mutex_default)
          else ok k
    | _ -> not_impl "pthread_mutex_init: expected a pointer for attr"

  (** [pthread_mutex_init(mutex, attr)]: stores the kind at offset 0 and the
      lock count (0) at offset 4. *)
  let pthread_mutex_init args =
    match args with
    | [ mutex; attr ] ->
        let* mutex_ptr = as_ptr_arg "pthread_mutex_init" mutex in
        let* kind = mutex_kind_from_attr attr in
        let* () = State.store mutex_ptr i32_ty (Int kind) in
        let* count_ptr = offset_bytes mutex_ptr 4 in
        let* () = store_i32 count_ptr Z.zero in
        ok zero
    | _ -> not_impl "pthread_mutex_init: expected 2 arguments"

  (** Common helper: reads (kind, count) from a mutex. *)
  let mutex_load_state mutex_ptr =
    let* kind = load_i32 mutex_ptr in
    let* count_ptr = offset_bytes mutex_ptr 4 in
    let+ count = load_i32 count_ptr in
    (kind, count, count_ptr)

  let is_recursive kind = kind ==@ Typed.BitVec.u32 pthread_mutex_recursive
  let is_errorcheck kind = kind ==@ Typed.BitVec.u32 pthread_mutex_errorcheck

  (** [pthread_mutex_lock(mutex)]: in our sequential model, locking a held mutex
      deadlocks (Normal/Default), returns [EDEADLK] (ErrorCheck), or increments
      the recursion count (Recursive). *)
  let pthread_mutex_lock args =
    match args with
    | [ mutex ] ->
        let* mutex_ptr = as_ptr_arg "pthread_mutex_lock" mutex in
        let* kind, count, count_ptr = mutex_load_state mutex_ptr in
        if%sat count ==@ Typed.BitVec.u32 Z.zero then
          let* () = store_i32 count_ptr Z.one in
          ok zero
        else if%sat is_recursive kind then
          let new_count =
            Typed.BitVec.(no_ovf_unsafe (add count (u32 Z.one)))
          in
          let* () = State.store count_ptr i32_ty (Int new_count) in
          ok zero
        else if%sat is_errorcheck kind then ok (ret_i32 errno_edeadlk)
        else
          error (`StdErr "pthread_mutex_lock: deadlock on already-locked mutex")
    | _ -> not_impl "pthread_mutex_lock: expected 1 argument"

  (** [pthread_mutex_trylock(mutex)]: like [lock] but returns [EBUSY] instead of
      blocking when the mutex is already held. *)
  let pthread_mutex_trylock args =
    match args with
    | [ mutex ] ->
        let* mutex_ptr = as_ptr_arg "pthread_mutex_trylock" mutex in
        let* kind, count, count_ptr = mutex_load_state mutex_ptr in
        if%sat count ==@ Typed.BitVec.u32 Z.zero then
          let* () = store_i32 count_ptr Z.one in
          ok zero
        else if%sat is_recursive kind then
          let new_count =
            Typed.BitVec.(no_ovf_unsafe (add count (u32 Z.one)))
          in
          let* () = State.store count_ptr i32_ty (Int new_count) in
          ok zero
        else ok (ret_i32 errno_ebusy)
    | _ -> not_impl "pthread_mutex_trylock: expected 1 argument"

  (** [pthread_mutex_unlock(mutex)]: decrements the recursion count; for
      Recursive a held mutex stays held until count drops to 0. Unlocking an
      already-unlocked mutex is UB for Normal/Default (we report a "std error"
      to match the user-visible behavior) and [EPERM] for ErrorCheck/Recursive.
  *)
  let pthread_mutex_unlock args =
    match args with
    | [ mutex ] ->
        let* mutex_ptr = as_ptr_arg "pthread_mutex_unlock" mutex in
        let* kind, count, count_ptr = mutex_load_state mutex_ptr in
        if%sat count ==@ Typed.BitVec.u32 Z.zero then
          if%sat is_recursive kind ||@ is_errorcheck kind then
            ok (ret_i32 errno_eperm)
          else
            error
              (`StdErr
                 "pthread_mutex_unlock: unlocking an already-unlocked mutex")
        else
          let new_count =
            Typed.BitVec.(no_ovf_unsafe (sub count (u32 Z.one)))
          in
          let* () = State.store count_ptr i32_ty (Int new_count) in
          ok zero
    | _ -> not_impl "pthread_mutex_unlock: expected 1 argument"

  (** [pthread_mutex_destroy(mutex)]: errors if still locked, otherwise
      uninitialises both fields. *)
  let pthread_mutex_destroy args =
    match args with
    | [ mutex ] ->
        let* mutex_ptr = as_ptr_arg "pthread_mutex_destroy" mutex in
        let* _kind, count, count_ptr = mutex_load_state mutex_ptr in
        if%sat Typed.not (count ==@ Typed.BitVec.u32 Z.zero) then
          error (`StdErr "pthread_mutex_destroy: destroying a locked mutex")
        else
          let* () = State.uninit mutex_ptr i32_ty in
          let* () = State.uninit count_ptr i32_ty in
          ok zero
    | _ -> not_impl "pthread_mutex_destroy: expected 1 argument"

  (* -- rwlock (not yet handled) -- *)

  let unsupported name _args =
    Fmt.kstr not_impl "Extern function %s is not yet handled" name

  let pthread_rwlock_rdlock = unsupported "pthread_rwlock_rdlock"
  let pthread_rwlock_tryrdlock = unsupported "pthread_rwlock_tryrdlock"
  let pthread_rwlock_wrlock = unsupported "pthread_rwlock_wrlock"
  let pthread_rwlock_trywrlock = unsupported "pthread_rwlock_trywrlock"
  let pthread_rwlock_unlock = unsupported "pthread_rwlock_unlock"
  let pthread_rwlock_destroy = unsupported "pthread_rwlock_destroy"

  (* -- condvar (not yet handled) -- *)

  let pthread_condattr_init = unsupported "pthread_condattr_init"
  let pthread_condattr_setclock = unsupported "pthread_condattr_setclock"
  let pthread_condattr_getclock = unsupported "pthread_condattr_getclock"
  let pthread_condattr_destroy = unsupported "pthread_condattr_destroy"
  let pthread_cond_init = unsupported "pthread_cond_init"
  let pthread_cond_signal = unsupported "pthread_cond_signal"
  let pthread_cond_broadcast = unsupported "pthread_cond_broadcast"
  let pthread_cond_wait = unsupported "pthread_cond_wait"
  let pthread_cond_timedwait = unsupported "pthread_cond_timedwait"
  let pthread_cond_destroy = unsupported "pthread_cond_destroy"
end
