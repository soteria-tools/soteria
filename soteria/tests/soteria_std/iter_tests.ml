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

let of_list_combine_empty_test =
  let@ () = register "of_list_combine_empty" in
  let l1 = [] in
  let l2 = [] in
  let expected = Iter.of_list [] in
  let result = Iter.of_list_combine l1 l2 |> Iter.map (fun (a, b) -> a + b) in
  check_iter result expected

let of_list_combine_one_elem_test =
  let@ () = register "of_list_combine_one_elem" in
  let l1 = [ 7 ] in
  let l2 = [ 3 ] in
  let expected = Iter.of_list [ 10 ] in
  let result = Iter.of_list_combine l1 l2 |> Iter.map (fun (a, b) -> a + b) in
  check_iter result expected

let of_list_combine_mismatched_length_test =
  let@ () = register "of_list_combine_mismatched_length" in
  let l1 = [ 1; 2 ] in
  let l2 = [ 10 ] in
  Alcotest.check_raises "lists of mismatched length"
    (Invalid_argument "Iter.of_list_combine") (fun () ->
      ignore
        (Iter.of_list_combine l1 l2
        |> Iter.map (fun (a, b) -> a + b)
        |> Iter.to_list))

let join_list_n_test =
  let@ () = register "join_list_n" in
  let i = Iter.of_list [ 1; 2; 3; 4 ] in
  let l = [ 10; 20; 30; 40 ] in
  let expected = Iter.of_list [ 11; 22; 33; 44 ] in
  let result = Iter.combine_list i l |> Iter.map (fun (a, b) -> a + b) in
  check_iter result expected

let join_list_0_test =
  let@ () = register "join_list_0" in
  let i0 = Iter.of_list [] in
  let l0 = [] in
  let expected0 = Iter.of_list [] in
  let result0 = Iter.combine_list i0 l0 |> Iter.map (fun (a, b) -> a + b) in
  check_iter result0 expected0

let join_list_1_test =
  let@ () = register "join_list_1" in
  let i1 = Iter.of_list [ 5 ] in
  let l1 = [ 100 ] in
  let expected1 = Iter.of_list [ 105 ] in
  let result1 = Iter.combine_list i1 l1 |> Iter.map (fun (a, b) -> a + b) in
  check_iter result1 expected1

let join_list_shorter_list_test =
  let@ () = register "join_list_shorter_list_test" in
  let i = Iter.of_list [ 1; 2; 3 ] in
  let l = [ 10; 20 ] in
  Alcotest.check_raises "list shorter than iterator"
    (Invalid_argument "Iter.combine_list") (fun () ->
      ignore
        (Iter.combine_list i l |> Iter.map (fun (a, b) -> a + b) |> Iter.to_list))
