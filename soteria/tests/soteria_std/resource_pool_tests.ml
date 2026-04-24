open Test_register

let register = register "Resource_pool"

(* ============================================================================
   Test Resource Modules
   ============================================================================ *)

(* Simple counter resource for testing resource identity *)
module Counter_resource = struct
  type t = { id : int }

  let counter = ref 0

  let init () =
    let id = !counter in
    counter := id + 1;
    { id }

  let reset () = counter := 0
end

(* Mutable resource for testing state persistence *)
module Mutable_resource = struct
  type t = { mutable value : int }

  let init () = { value = 0 }
end

(* Atomic counter resource for concurrent tests *)
module Atomic_resource = struct
  type t = { id : int }

  let counter = Atomic.make 0

  let init () =
    let id = Atomic.fetch_and_add counter 1 in
    { id }

  let reset () = Atomic.set counter 0
  let count () = Atomic.get counter
end

module Counter_pool = Resource_pool.Make (Counter_resource)
module Mutable_pool = Resource_pool.Make (Mutable_resource)
module Atomic_pool = Resource_pool.Make (Atomic_resource)

(* ============================================================================
   Basic Functionality Tests
   ============================================================================ *)

let acquire_creates_resource =
  let@ () = register "acquire_creates_resource" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t, auth = Counter_pool.acquire pool in
  let id = Counter_pool.apply ~auth t (fun r -> r.id) in
  Alcotest.(check int) "resource id is 0" 0 id;
  Alcotest.(check int) "one resource created" 1 !Counter_resource.counter

let acquire_returns_valid_auth =
  let@ () = register "acquire_returns_valid_auth" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t, auth = Counter_pool.acquire pool in
  (* Should not raise *)
  let _ = Counter_pool.apply ~auth t (fun r -> r.id) in
  ()

let wrap_returns_function_result =
  let@ () = register "wrap_returns_function_result" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t, auth = Counter_pool.acquire pool in
  let result = Counter_pool.apply ~auth t (fun r -> r.id + 42) in
  Alcotest.(check int) "wrap returns correct result" 42 result

let release_makes_resource_available =
  let@ () = register "release_makes_resource_available" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t1, auth1 = Counter_pool.acquire pool in
  let id1 = Counter_pool.apply ~auth:auth1 t1 (fun r -> r.id) in
  Counter_pool.release pool auth1 t1;
  let t2, auth2 = Counter_pool.acquire pool in
  let id2 = Counter_pool.apply ~auth:auth2 t2 (fun r -> r.id) in
  Alcotest.(check int) "same resource reused" id1 id2;
  Alcotest.(check int) "only one resource created" 1 !Counter_resource.counter

(* ============================================================================
   Resource Reuse Tests
   ============================================================================ *)

let reuse_after_release =
  let@ () = register "reuse_after_release" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t1, auth1 = Counter_pool.acquire pool in
  let id1 = Counter_pool.apply ~auth:auth1 t1 (fun r -> r.id) in
  Counter_pool.release pool auth1 t1;
  let t2, auth2 = Counter_pool.acquire pool in
  let id2 = Counter_pool.apply ~auth:auth2 t2 (fun r -> r.id) in
  Alcotest.(check int) "resource reused has same id" id1 id2

let multiple_resources_created =
  let@ () = register "multiple_resources_created" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t1, auth1 = Counter_pool.acquire pool in
  let t2, auth2 = Counter_pool.acquire pool in
  let t3, auth3 = Counter_pool.acquire pool in
  let id1 = Counter_pool.apply ~auth:auth1 t1 (fun r -> r.id) in
  let id2 = Counter_pool.apply ~auth:auth2 t2 (fun r -> r.id) in
  let id3 = Counter_pool.apply ~auth:auth3 t3 (fun r -> r.id) in
  Alcotest.(check int) "three resources created" 3 !Counter_resource.counter;
  Alcotest.(check bool)
    "all ids distinct" true
    (id1 <> id2 && id2 <> id3 && id1 <> id3)

let lifo_reuse =
  let@ () = register "lifo_reuse" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t1, auth1 = Counter_pool.acquire pool in
  let t2, auth2 = Counter_pool.acquire pool in
  let _id1 = Counter_pool.apply ~auth:auth1 t1 (fun r -> r.id) in
  let id2 = Counter_pool.apply ~auth:auth2 t2 (fun r -> r.id) in
  Counter_pool.release pool auth1 t1;
  Counter_pool.release pool auth2 t2;
  (* LIFO: t2 was released last, so it should be acquired first *)
  let t3, auth3 = Counter_pool.acquire pool in
  let id3 = Counter_pool.apply ~auth:auth3 t3 (fun r -> r.id) in
  Alcotest.(check int) "LIFO reuse order" id2 id3

(* ============================================================================
   Auth Token Validation Tests
   ============================================================================ *)

let wrap_with_wrong_auth_raises =
  let@ () = register "wrap_with_wrong_auth_raises" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t1, _auth1 = Counter_pool.acquire pool in
  let _t2, wrong_auth = Counter_pool.acquire pool in
  (* Use auth from t2 to access t1 - should fail *)
  Alcotest.check_raises "wrong auth raises" Resource_pool.Pool_invalid_auth
    (fun () -> ignore (Counter_pool.apply ~auth:wrong_auth t1 (fun r -> r.id)))

let wrap_after_release_raises =
  let@ () = register "wrap_after_release_raises" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t, auth = Counter_pool.acquire pool in
  Counter_pool.release pool auth t;
  Alcotest.check_raises "wrap after release raises"
    Resource_pool.Pool_invalid_auth (fun () ->
      ignore (Counter_pool.apply ~auth t (fun r -> r.id)))

let release_with_wrong_auth_raises =
  let@ () = register "release_with_wrong_auth_raises" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t1, _auth1 = Counter_pool.acquire pool in
  let _t2, wrong_auth = Counter_pool.acquire pool in
  (* Use auth from t2 to release t1 - should fail *)
  Alcotest.check_raises "wrong auth raises" Resource_pool.Pool_invalid_auth
    (fun () -> Counter_pool.release pool wrong_auth t1)

let double_release_raises =
  let@ () = register "double_release_raises" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t, auth = Counter_pool.acquire pool in
  Counter_pool.release pool auth t;
  Alcotest.check_raises "double release raises" Resource_pool.Pool_invalid_auth
    (fun () -> Counter_pool.release pool auth t)

let old_auth_invalid_after_reacquire =
  let@ () = register "old_auth_invalid_after_reacquire" in
  Counter_resource.reset ();
  let pool = Counter_pool.create_pool () in
  let t, auth1 = Counter_pool.acquire pool in
  Counter_pool.release pool auth1 t;
  let _t2, _auth2 = Counter_pool.acquire pool in
  (* t and _t2 point to the same underlying resource, but auth1 is now
     invalid *)
  Alcotest.check_raises "old auth invalid" Resource_pool.Pool_invalid_auth
    (fun () -> ignore (Counter_pool.apply ~auth:auth1 t (fun r -> r.id)))

(* ============================================================================
   Mutable Resource Tests
   ============================================================================ *)

let mutable_resource_state_persists =
  let@ () = register "mutable_resource_state_persists" in
  let pool = Mutable_pool.create_pool () in
  let t, auth = Mutable_pool.acquire pool in
  Mutable_pool.apply ~auth t (fun r -> r.value <- 42);
  let v1 = Mutable_pool.apply ~auth t (fun r -> r.value) in
  Alcotest.(check int) "value persists within session" 42 v1;
  Mutable_pool.release pool auth t;
  let t2, auth2 = Mutable_pool.acquire pool in
  let v2 = Mutable_pool.apply ~auth:auth2 t2 (fun r -> r.value) in
  Alcotest.(check int) "value persists across sessions" 42 v2

let mutable_resource_isolated =
  let@ () = register "mutable_resource_isolated" in
  let pool = Mutable_pool.create_pool () in
  let t1, auth1 = Mutable_pool.acquire pool in
  let t2, auth2 = Mutable_pool.acquire pool in
  Mutable_pool.apply ~auth:auth1 t1 (fun r -> r.value <- 100);
  Mutable_pool.apply ~auth:auth2 t2 (fun r -> r.value <- 200);
  let v1 = Mutable_pool.apply ~auth:auth1 t1 (fun r -> r.value) in
  let v2 = Mutable_pool.apply ~auth:auth2 t2 (fun r -> r.value) in
  Alcotest.(check int) "resource 1 has correct value" 100 v1;
  Alcotest.(check int) "resource 2 has correct value" 200 v2

(* ============================================================================
   Multiple Pool Tests
   ============================================================================ *)

let separate_pools_independent =
  let@ () = register "separate_pools_independent" in
  Counter_resource.reset ();
  let pool1 = Counter_pool.create_pool () in
  let pool2 = Counter_pool.create_pool () in
  let t1, auth1 = Counter_pool.acquire pool1 in
  let t2, auth2 = Counter_pool.acquire pool2 in
  (* Resources are different (different ids from the shared counter) *)
  let id1 = Counter_pool.apply ~auth:auth1 t1 (fun r -> r.id) in
  let id2 = Counter_pool.apply ~auth:auth2 t2 (fun r -> r.id) in
  Alcotest.(check bool) "different resources" true (id1 <> id2);
  (* Release from pool1 doesn't affect pool2 *)
  Counter_pool.release pool1 auth1 t1;
  let v2 = Counter_pool.apply ~auth:auth2 t2 (fun r -> r.id) in
  Alcotest.(check int) "pool2 resource still valid" id2 v2

(* ============================================================================
   Concurrent Tests
   ============================================================================ *)

let num_domains = 4
let iterations_per_domain = 100

let concurrent_acquires =
  let@ () = register "concurrent_acquires" in
  Atomic_resource.reset ();
  let pool = Atomic_pool.create_pool () in
  let barrier = Atomic.make 0 in
  let domains =
    List.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            (* Wait at barrier for all domains to be ready *)
            Atomic.incr barrier;
            while Atomic.get barrier < num_domains do
              Domain.cpu_relax ()
            done;
            (* Each domain acquires many resources *)
            List.init iterations_per_domain (fun _ -> Atomic_pool.acquire pool)))
  in
  let results = List.map Domain.join domains in
  let all_acquired = List.concat results in
  let total = List.length all_acquired in
  Alcotest.(check int)
    "total resources acquired"
    (num_domains * iterations_per_domain)
    total;
  Alcotest.(check int)
    "total resources created"
    (num_domains * iterations_per_domain)
    (Atomic_resource.count ())

let concurrent_acquire_release =
  let@ () = register "concurrent_acquire_release" in
  Atomic_resource.reset ();
  let pool = Atomic_pool.create_pool () in
  let barrier = Atomic.make 0 in
  let successful_ops = Atomic.make 0 in
  let domains =
    List.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            (* Wait at barrier *)
            Atomic.incr barrier;
            while Atomic.get barrier < num_domains do
              Domain.cpu_relax ()
            done;
            (* Each domain does many acquire/release cycles *)
            for _ = 1 to iterations_per_domain do
              let t, auth = Atomic_pool.acquire pool in
              let _ = Atomic_pool.apply ~auth t (fun r -> r.id) in
              Atomic_pool.release pool auth t;
              Atomic.incr successful_ops
            done))
  in
  List.iter Domain.join domains;
  let total_ops = Atomic.get successful_ops in
  Alcotest.(check int)
    "all operations succeeded"
    (num_domains * iterations_per_domain)
    total_ops;
  (* With acquire/release cycles, resources should be reused *)
  let resources_created = Atomic_resource.count () in
  Alcotest.(check bool)
    "resources reused (created <= num_domains)" true
    (resources_created <= num_domains)

let concurrent_wrap_is_safe =
  let@ () = register "concurrent_wrap_is_safe" in
  let pool = Mutable_pool.create_pool () in
  let barrier = Atomic.make 0 in
  let increments_per_domain = 1000 in
  let domains =
    List.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            (* Acquire before barrier so each domain has its own resource *)
            let t, auth = Mutable_pool.acquire pool in
            (* Wait at barrier for all domains to be ready *)
            Atomic.incr barrier;
            while Atomic.get barrier < num_domains do
              Domain.cpu_relax ()
            done;
            (* Increment many times *)
            for _ = 1 to increments_per_domain do
              Mutable_pool.apply ~auth t (fun r -> r.value <- r.value + 1)
            done;
            let final_value = Mutable_pool.apply ~auth t (fun r -> r.value) in
            Mutable_pool.release pool auth t;
            final_value))
  in
  let results = List.map Domain.join domains in
  (* Each domain should have incremented its own resource correctly *)
  List.iter
    (fun v ->
      Alcotest.(check int)
        "each resource incremented correctly" increments_per_domain v)
    results

let concurrent_no_use_after_release =
  let@ () = register "concurrent_no_use_after_release" in
  Atomic_resource.reset ();
  let pool = Atomic_pool.create_pool () in
  let acquired = Atomic.make false in
  let released = Atomic.make false in
  let stale_auth : Atomic_pool.auth option ref = ref None in
  let stale_t : Atomic_pool.t option ref = ref None in
  let got_exception = Atomic.make false in
  let domain1 =
    Domain.spawn (fun () ->
        let t, auth = Atomic_pool.acquire pool in
        stale_auth := Some auth;
        stale_t := Some t;
        Atomic.set acquired true;
        (* Wait a bit then release *)
        for _ = 1 to 1000 do
          Domain.cpu_relax ()
        done;
        Atomic_pool.release pool auth t;
        Atomic.set released true)
  in
  let domain2 =
    Domain.spawn (fun () ->
        (* Wait for domain1 to acquire *)
        while not (Atomic.get acquired) do
          Domain.cpu_relax ()
        done;
        (* Wait for domain1 to release *)
        while not (Atomic.get released) do
          Domain.cpu_relax ()
        done;
        (* Now try to use the stale auth *)
        match (!stale_t, !stale_auth) with
        | Some t, Some auth -> (
            try ignore (Atomic_pool.apply ~auth t (fun r -> r.id))
            with Resource_pool.Pool_invalid_auth ->
              Atomic.set got_exception true)
        | _ -> ())
  in
  Domain.join domain1;
  Domain.join domain2;
  Alcotest.(check bool) "got Pool_invalid_auth" true (Atomic.get got_exception)
