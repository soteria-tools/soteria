(** Minimal thread-safe hash table: a {!Stdlib.Hashtbl} guarded by a mutex.

    Intended for memoisation caches {e shared} across domains (e.g. keyed by a
    hash-cons tag), so that expensive results — in particular ones that avoid a
    solver call — are reused across all parallel analyses.

    Each operation takes the lock independently and is short; callers compute
    values {e outside} any table operation (the usual [find_opt]/[add] memo
    pattern). Two domains missing the same key concurrently may both compute the
    value: that is harmless for a pure memo (same result, last write wins). *)

type ('k, 'v) t = { tbl : ('k, 'v) Stdlib.Hashtbl.t; mutex : Mutex.t }

let create n = { tbl = Stdlib.Hashtbl.create n; mutex = Mutex.create () }

let find_opt t k =
  Mutex.protect t.mutex (fun () -> Stdlib.Hashtbl.find_opt t.tbl k)

(** Insert (or overwrite) a binding. Memo keys map to a stable value, so
    overwriting is fine and avoids shadowed-binding accumulation. *)
let add t k v =
  Mutex.protect t.mutex (fun () -> Stdlib.Hashtbl.replace t.tbl k v)

let clear t = Mutex.protect t.mutex (fun () -> Stdlib.Hashtbl.clear t.tbl)
let length t = Mutex.protect t.mutex (fun () -> Stdlib.Hashtbl.length t.tbl)
