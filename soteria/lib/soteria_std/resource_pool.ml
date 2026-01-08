(** Thread-safe resource pool for managing reusable resources. *)

(** Raised when an invalid auth token is provided to [release]. *)
exception Pool_invalid_auth

module type Resource = sig
  type t

  val create : handle:int -> unit -> t
end

module type S = sig
  type resource
  type t
  type pool
  type auth

  (** Create a new empty pool. *)
  val create_pool : unit -> pool

  (** Acquire a resource from the pool. If an unused resource is available, it
      is returned. Otherwise, a new resource is created. Returns the pooled
      resource and an auth token that must be used for subsequent operations. *)
  val acquire : pool -> t * auth

  (** Release a resource back to the pool. The auth token must match the one
      returned by [acquire]. Raises [Pool_invalid_auth] if the auth token is
      invalid. *)
  val release : pool -> auth -> t -> unit

  (** Get the handle of the pooled resource. It is a unique identifier for the
      resource within the pool. *)
  val handle : t -> int

  (** Access the underlying resource. The auth token must match the current
      valid token for this resource. Raises [Pool_invalid_auth] if the resource
      has been released. *)
  val wrap : auth:auth -> t -> (resource -> 'a) -> 'a
end

module Make (R : Resource) : S with type resource = R.t = struct
  type handle = int
  type auth = int
  type resource = R.t
  type t = { handle : handle; res : resource; mutable auth : auth option }

  type pool = {
    mutex : Mutex.t;
    resources : t Dynarray.t;
    available : handle Dynarray.t;
    mutable next_auth : auth;
  }

  let create_pool () =
    {
      mutex = Mutex.create ();
      resources = Dynarray.create ();
      available = Dynarray.create ();
      next_auth = 0;
    }

  let acquire pool =
    Mutex.protect pool.mutex @@ fun () ->
    if Dynarray.is_empty pool.available then (
      let handle = Dynarray.length pool.resources in
      let res = R.create ~handle () in
      let auth = pool.next_auth in
      pool.next_auth <- pool.next_auth + 1;
      let pooled = { handle; res; auth = Some auth } in
      Dynarray.add_last pool.resources pooled;
      (pooled, auth))
    else
      let h = Dynarray.pop_last pool.available in
      let pooled = Dynarray.get pool.resources h in
      let auth = pool.next_auth in
      pool.next_auth <- pool.next_auth + 1;
      pooled.auth <- Some auth;
      (pooled, auth)

  let release pool auth pooled =
    Mutex.protect pool.mutex @@ fun () ->
    match pooled.auth with
    | Some current_auth when current_auth = auth ->
        pooled.auth <- None;
        Dynarray.add_last pool.available pooled.handle
    | _ -> raise Pool_invalid_auth

  let handle pooled = pooled.handle

  let wrap ~auth pooled f =
    match pooled.auth with
    | Some current_auth when current_auth = auth -> f pooled.res
    | _ -> raise Pool_invalid_auth
end
