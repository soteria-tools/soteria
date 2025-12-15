include Iter

let rec of_list_combine l1 l2 k =
  match (l1, l2) with
  | [], [] -> ()
  | [ a1 ], [ a2 ] -> k (a1, a2)
  | a1 :: b1 :: l1, a2 :: b2 :: l2 ->
      k (a1, a2);
      k (b1, b2);
      of_list_combine l1 l2 k
  | _, _ -> invalid_arg "Iter.of_list_combine"
