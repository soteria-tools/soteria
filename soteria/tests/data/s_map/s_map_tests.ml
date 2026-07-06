module Typed = Soteria.Tiny_values.Typed
module Tiny_solver = Soteria.Tiny_values.Tiny_solver
module Symex = Soteria.Symex.Make (Tiny_solver.Z3_solver)
module S_map = Soteria.Data.S_map

(* A symbolic key over symbolic integers, with unit values, used to instantiate
   the "lazy" symbolic map (the one whose [find_opt] first looks for a syntactic
   match and otherwise branches on semantic equality with every existing
   key). *)
module S_int = struct
  type t = Typed.T.sint Typed.t

  let pp = Typed.ppa
  let show = Fmt.to_to_string pp
  let compare = Typed.compare
  let sem_eq = Typed.sem_eq
  let simplify = Symex.simplify
  let distinct_seq = Typed.distinct_seq
end

module LazyMap = S_map.Lazy (Symex) (S_int)
open Symex
open Syntax
open Typed.Infix

(* ============================================================================
   Test Processes
   ============================================================================ *)

(* Looking a key up in the empty map never finds a value, and hands back a key
   usable for future syntactic operations. *)
let empty_lookup () =
  let* x = nondet Typed.t_int in
  let* _key, res = LazyMap.find_opt x LazyMap.empty in
  Alcotest.(check bool) "lookup in empty map is None" true (Option.is_none res);
  return (Option.is_some res)

(* After a syntactic insertion, looking the very same value up resolves
   syntactically: it returns [Some] without branching at all. *)
let syntactic_hit () =
  let* x = nondet Typed.t_int in
  let* key, res = LazyMap.find_opt x LazyMap.empty in
  Alcotest.(check bool) "fresh lookup is None" true (Option.is_none res);
  let map = LazyMap.syntactic_add key () LazyMap.empty in
  let* key, res = LazyMap.find_opt x map in
  Alcotest.(check bool)
    "retrieveing syntactic hit gives the same key" true (Typed.equal key x);
  Alcotest.(check bool) "lookup after add is Some" true (Option.is_some res);
  return 0

(* The core "lazy" behaviour: a value that is not syntactically present is
   matched against existing keys by branching on semantic equality, and the key
   handed back is the {i existing} key it matched, not the query itself. *)
let lazy_matching_key () =
  let map = LazyMap.empty in
  let* x = nondet Typed.t_int in
  let* y = nondet Typed.t_int in
  let* kx, res = LazyMap.find_opt x map in
  Alcotest.(check bool) "fresh x lookup is None" true (Option.is_none res);
  let map = LazyMap.syntactic_add kx () map in
  (* [y] is syntactically distinct from [x], so this lookup branches on whether
     [y] is semantically equal to [x]; [ky] is the matched key on each
     branch. *)
  let* ky, _res = LazyMap.find_opt y map in
  Alcotest.(check bool)
    "looking up fresh y is None, even though it could equal x" true
    (Option.is_none _res);
  let map = LazyMap.syntactic_add ky () map in
  if%sat x ==@ y then return 0
  else
    let* z = nondet Typed.t_int in
    let* () = assume [ z ==@ x ||@ (z ==@ y) ] in
    let* kz, _res = LazyMap.find_opt z map in
    if Typed.equal kz kx then return 1
    else if Typed.equal kz ky then return 2
    else return (-1)

(* ============================================================================
   Helper
   ============================================================================ *)

let get_results process =
  List.map fst (run ~mode:OX (process ())) |> List.sort Stdlib.compare

(* ============================================================================
   Tests
   ============================================================================ *)

let empty_lookup_test () =
  Alcotest.(check (list bool))
    "no value found" [ false ] (get_results empty_lookup)

let syntactic_hit_test () =
  Alcotest.(check (list int))
    "single non-branching path" [ 0 ]
    (get_results syntactic_hit)

(* The [x = y] branch returns 0; otherwise [z] semantically matches either [x]'s
   key (1) or [y]'s key (2). The catch-all -1 is unreachable given the
   assumption. *)
let lazy_matching_key_test () =
  Alcotest.(check (list int))
    "matched key per branch" [ 0; 1; 2 ]
    (get_results lazy_matching_key)

(* ============================================================================
   Test Runner
   ============================================================================ *)

let () =
  Alcotest.run "S_map.Lazy"
    [
      ( "lazy",
        [
          Alcotest.test_case "empty lookup is None" `Quick empty_lookup_test;
          Alcotest.test_case "syntactic hit does not branch" `Quick
            syntactic_hit_test;
          Alcotest.test_case "lazy lookup returns matching key" `Quick
            lazy_matching_key_test;
        ] );
    ]
