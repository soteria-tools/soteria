(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Common

module M (StateM : State.StateM.S) = struct
  open StateM

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  module type S = sig
    val pthread_cond_broadcast : rust_val list -> rust_val ret
    val pthread_cond_destroy : rust_val list -> rust_val ret
    val pthread_cond_init : rust_val list -> rust_val ret
    val pthread_cond_signal : rust_val list -> rust_val ret
    val pthread_cond_timedwait : rust_val list -> rust_val ret
    val pthread_cond_wait : rust_val list -> rust_val ret
    val pthread_condattr_destroy : rust_val list -> rust_val ret
    val pthread_condattr_getclock : rust_val list -> rust_val ret
    val pthread_condattr_init : rust_val list -> rust_val ret
    val pthread_condattr_setclock : rust_val list -> rust_val ret
    val pthread_mutex_destroy : rust_val list -> rust_val ret
    val pthread_mutex_init : rust_val list -> rust_val ret
    val pthread_mutex_lock : rust_val list -> rust_val ret
    val pthread_mutex_trylock : rust_val list -> rust_val ret
    val pthread_mutex_unlock : rust_val list -> rust_val ret
    val pthread_mutexattr_destroy : rust_val list -> rust_val ret
    val pthread_mutexattr_init : rust_val list -> rust_val ret
    val pthread_mutexattr_settype : rust_val list -> rust_val ret
    val pthread_rwlock_destroy : rust_val list -> rust_val ret
    val pthread_rwlock_rdlock : rust_val list -> rust_val ret
    val pthread_rwlock_tryrdlock : rust_val list -> rust_val ret
    val pthread_rwlock_trywrlock : rust_val list -> rust_val ret
    val pthread_rwlock_unlock : rust_val list -> rust_val ret
    val pthread_rwlock_wrlock : rust_val list -> rust_val ret
  end
end
