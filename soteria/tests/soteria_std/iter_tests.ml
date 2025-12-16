open Test_register

let register = register "Iter"

open Iter

let iter_testable : int Iter.t Alcotest.testable =
  (module struct
    type t = int Iter.t

    let pp = Fmt.(iter ~sep:comma Iter.iter int)

    let equal l r =
      let l = to_list l in
      let r = to_list r in
      List.equal Int.equal l r
  end)

let check_iter = Alcotest.(check iter_testable) "iterators are equal"

let of_list_combine_test =
  let@ () = register "of_list_combine" in
  let l1 = [ 1; 2; 3; 4 ] in
  let l2 = [ 10; 20; 30; 40 ] in
  let expected = Iter.of_list [ 11; 22; 33; 44 ] in
  let result = Iter.of_list_combine l1 l2 |> Iter.map (fun (a, b) -> a + b) in
  check_iter result expected

let join_list_test =
  let@ () = register "join_list" in
  let i = Iter.of_list [ 1; 2; 3; 4 ] in
  let l = [ 10; 20; 30; 40 ] in
  let expected = Iter.of_list [ 11; 22; 33; 44 ] in
  let result = Iter.join_list i l |> Iter.map (fun (a, b) -> a + b) in
  check_iter result expected
