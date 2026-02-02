(** Thread-safe resource pool for managing reusable resources.

    This module provides a generic, thread-safe resource pool that manages
    reusable resources such as solver instances. Resources are created on
    demand and can be acquired, used, and released back to the pool.

    {1 Overview}

    The pool uses an authentication token system to ensure safe resource
    access:
    - When you {!S.acquire} a resource, you get both the resource and an auth token
    - The auth token must be provided when {!S.apply}ing operations to the resource
    - When you {!S.release} the resource, the token is invalidated
    - Subsequent attempts to use the old token will raise {!Pool_invalid_auth}

    This prevents use-after-release bugs in concurrent code.

    {1 Example Usage}

    {[
      (* Define a resource type *)
      module SolverResource = struct
        type t = solver_handle
        let init () = create_solver ()
      end

      (* Create the pool module *)
      module SolverPool = Resource_pool.Make(SolverResource)

      (* Use the pool *)
      let pool = SolverPool.create_pool ()

      let use_solver () =
        let pooled, auth = SolverPool.acquire pool in
        let result =
          SolverPool.apply ~auth pooled (fun solver ->
            run_query solver
          )
        in
        SolverPool.release pool auth pooled;
        result
    ]}

    {1 Thread Safety}

    All pool operations ({!S.acquire}, {!S.release}) are protected by a mutex,
    making them safe to call from multiple threads concurrently. However,
    operations on the underlying resource ({!S.apply}) are not synchronized -
    it is the caller's responsibility to ensure proper synchronization when
    the same resource might be accessed concurrently. *)

(** Raised when an invalid authentication token is provided to
    {!S.release} or {!S.apply}.

    This typically indicates:
    - The resource was already released (token invalidated)
    - A stale token from a previous acquisition is being used *)
exception Pool_invalid_auth

(** {1 Resource Signature} *)

(** Signature for poolable resources.

    Resources must be initializable with no arguments. The [init] function
    is called lazily when the pool needs more resources. *)
module type Resource = sig
  (** The type of the resource. *)
  type t

  (** [init ()] creates a new resource instance.

      This is called by the pool when no existing resources are available. *)
  val init : unit -> t
end

(** {1 Pool Signature} *)

(** Signature for a resource pool.

    Provides thread-safe acquire/release semantics with authentication
    tokens to prevent use-after-release bugs. *)
module type S = sig
  (** The type of the underlying resource. *)
  type resource

  (** The type of a pooled resource wrapper.

      Contains metadata for pool management (handle, auth state). *)
  type t

  (** The type of the resource pool. *)
  type pool

  (** Authentication token for resource access.

      Tokens are unique per acquisition and are invalidated on release. *)
  type auth

  (** [create_pool ()] creates a new empty resource pool.

      Resources will be created on demand when {!acquire} is called. *)
  val create_pool : unit -> pool

  (** [acquire pool] obtains a resource from the pool.

      If an unused resource is available, it is returned. Otherwise,
      a new resource is created via {!Resource.init}.

      @return A tuple [(pooled, auth)] where [pooled] is the resource wrapper
              and [auth] is the authentication token for this acquisition.

      Thread-safe: multiple threads can call [acquire] concurrently. *)
  val acquire : pool -> t * auth

  (** [release pool auth pooled] returns a resource to the pool.

      After release, the authentication token is invalidated. Any attempt
      to use [auth] with {!apply} will raise {!Pool_invalid_auth}.

      @raise Pool_invalid_auth if [auth] does not match the current valid
             token for [pooled] (e.g., already released or wrong token).

      Thread-safe: multiple threads can call [release] concurrently. *)
  val release : pool -> auth -> t -> unit

  (** [handle pooled] returns the unique handle for this pooled resource.

      Handles are assigned sequentially starting from 0 and uniquely
      identify resources within the pool. *)
  val handle : t -> int

  (** [apply ~auth pooled f] applies function [f] to the underlying resource.

      @param auth The authentication token from {!acquire}
      @param pooled The pooled resource wrapper
      @param f Function to apply to the underlying resource
      @return The result of [f resource]
      @raise Pool_invalid_auth if [auth] is invalid (resource was released) *)
  val apply : auth:auth -> t -> (resource -> 'a) -> 'a

  (** [total_resources pool] returns the total number of resources created.

      This includes both available and currently-acquired resources. *)
  val total_resources : pool -> int

  (** [available_resources pool] returns the number of resources ready for reuse.

      These are resources that have been released and are waiting in the pool. *)
  val available_resources : pool -> int
end

(** {1 Pool Functor} *)

(** Functor to create a resource pool for a given resource type.

    @param R The resource module satisfying {!Resource} *)
module Make (R : Resource) : S with type resource = R.t
