(** Domain-local storage helpers.

    Thin wrapper over {!Stdlib.Domain.DLS} for per-domain mutable state. Each
    domain gets its own independent copy, created lazily from the [init] thunk
    on first access in that domain.

    Use this for caches, counters and display state that only need to be
    consistent within a single analysis (the intended model is one domain per
    analysis): per-domain copies are sound and avoid lock contention entirely.
    Do {e not} use it for state whose identity must be shared across domains
    (e.g. hash-consing tables). *)

type 'a t = 'a Domain.DLS.key

(** [make init] creates a domain-local cell. [init ()] is called at most once
    per domain, the first time {!get} is called from that domain. *)
let make (init : unit -> 'a) : 'a t = Domain.DLS.new_key init

(** [get k] returns the calling domain's copy of [k], initialising it if
    necessary. *)
let get (k : 'a t) : 'a = Domain.DLS.get k

(** [set k v] replaces the calling domain's copy of [k] with [v]. *)
let set (k : 'a t) (v : 'a) : unit = Domain.DLS.set k v
