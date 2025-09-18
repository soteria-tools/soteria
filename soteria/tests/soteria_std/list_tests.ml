open Test_register

let register f = register "List" f

open List

let take_count =
  let@ () = register "take count" in
  let check_take_count = Alcotest.(check @@ pair (list int) int) in
  let l = [ 1; 2; 3; 4; 5 ] in
  check_take_count "3 firsts" (take_count 3 l) ([ 1; 2; 3 ], 3);
  check_take_count "0 firsts" (take_count 0 l) ([], 0);
  check_take_count "5 firsts" (take_count 5 l) (l, 5);
  check_take_count "6 firsts" (take_count 6 l) (l, 5);
  check_take_count "10 firsts" (take_count 10 l) (l, 5)
