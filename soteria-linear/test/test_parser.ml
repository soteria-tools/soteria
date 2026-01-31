open Soteria_linear_lib.Parser

let test_parse_program s =
  let _ = parse_program s in
  print_endline ("Parsed program successfully: " ^ s)

(* let test_parse_assertion s =
 *   let _ = parse_assertion s in
 *   print_endline ("Parsed assertion successfully: " ^ s) *)

let () =
  test_parse_program "let main arg = [arg] <- 1; 0";
  test_parse_program "let f x = if x then 1 else 2; 3";
  test_parse_program "let g y = let z = y in z; z";
  test_parse_program "let h a = (1)";
  test_parse_program "let i b = (let x = 1 in x)"
(* test_parse_assertion "take x = RW(a); take Freed(x); x==1" *)
