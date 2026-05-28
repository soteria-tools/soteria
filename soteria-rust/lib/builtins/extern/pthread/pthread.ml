(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common
open Rust_val

type fn =
  | PthreadCondBroadcast
  | PthreadCondDestroy
  | PthreadCondInit
  | PthreadCondSignal
  | PthreadCondTimedwait
  | PthreadCondWait
  | PthreadCondattrDestroy
  | PthreadCondattrGetclock
  | PthreadCondattrInit
  | PthreadCondattrSetclock
  | PthreadMutexDestroy
  | PthreadMutexInit
  | PthreadMutexLock
  | PthreadMutexTrylock
  | PthreadMutexUnlock
  | PthreadMutexattrDestroy
  | PthreadMutexattrInit
  | PthreadMutexattrSettype
  | PthreadRwlockDestroy
  | PthreadRwlockRdlock
  | PthreadRwlockTryrdlock
  | PthreadRwlockTrywrlock
  | PthreadRwlockUnlock
  | PthreadRwlockWrlock

let fn_pats : (string * fn) list =
  [
    ("pthread_cond_broadcast", PthreadCondBroadcast);
    ("pthread_cond_destroy", PthreadCondDestroy);
    ("pthread_cond_init", PthreadCondInit);
    ("pthread_cond_signal", PthreadCondSignal);
    ("pthread_cond_timedwait", PthreadCondTimedwait);
    ("pthread_cond_wait", PthreadCondWait);
    ("pthread_condattr_destroy", PthreadCondattrDestroy);
    ("pthread_condattr_getclock", PthreadCondattrGetclock);
    ("pthread_condattr_init", PthreadCondattrInit);
    ("pthread_condattr_setclock", PthreadCondattrSetclock);
    ("pthread_mutex_destroy", PthreadMutexDestroy);
    ("pthread_mutex_init", PthreadMutexInit);
    ("pthread_mutex_lock", PthreadMutexLock);
    ("pthread_mutex_trylock", PthreadMutexTrylock);
    ("pthread_mutex_unlock", PthreadMutexUnlock);
    ("pthread_mutexattr_destroy", PthreadMutexattrDestroy);
    ("pthread_mutexattr_init", PthreadMutexattrInit);
    ("pthread_mutexattr_settype", PthreadMutexattrSettype);
    ("pthread_rwlock_destroy", PthreadRwlockDestroy);
    ("pthread_rwlock_rdlock", PthreadRwlockRdlock);
    ("pthread_rwlock_tryrdlock", PthreadRwlockTryrdlock);
    ("pthread_rwlock_trywrlock", PthreadRwlockTrywrlock);
    ("pthread_rwlock_unlock", PthreadRwlockUnlock);
    ("pthread_rwlock_wrlock", PthreadRwlockWrlock);
  ]

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  let[@inline] as_ptr (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Int v ->
        let v = Typed.cast_i Usize v in
        let ptr = Sptr.of_address v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  include Impl.M (StateM)

  let[@inline] fn_to_stub _fun_exec = function
    | PthreadCondBroadcast -> pthread_cond_broadcast
    | PthreadCondDestroy -> pthread_cond_destroy
    | PthreadCondInit -> pthread_cond_init
    | PthreadCondSignal -> pthread_cond_signal
    | PthreadCondTimedwait -> pthread_cond_timedwait
    | PthreadCondWait -> pthread_cond_wait
    | PthreadCondattrDestroy -> pthread_condattr_destroy
    | PthreadCondattrGetclock -> pthread_condattr_getclock
    | PthreadCondattrInit -> pthread_condattr_init
    | PthreadCondattrSetclock -> pthread_condattr_setclock
    | PthreadMutexDestroy -> pthread_mutex_destroy
    | PthreadMutexInit -> pthread_mutex_init
    | PthreadMutexLock -> pthread_mutex_lock
    | PthreadMutexTrylock -> pthread_mutex_trylock
    | PthreadMutexUnlock -> pthread_mutex_unlock
    | PthreadMutexattrDestroy -> pthread_mutexattr_destroy
    | PthreadMutexattrInit -> pthread_mutexattr_init
    | PthreadMutexattrSettype -> pthread_mutexattr_settype
    | PthreadRwlockDestroy -> pthread_rwlock_destroy
    | PthreadRwlockRdlock -> pthread_rwlock_rdlock
    | PthreadRwlockTryrdlock -> pthread_rwlock_tryrdlock
    | PthreadRwlockTrywrlock -> pthread_rwlock_trywrlock
    | PthreadRwlockUnlock -> pthread_rwlock_unlock
    | PthreadRwlockWrlock -> pthread_rwlock_wrlock
end
