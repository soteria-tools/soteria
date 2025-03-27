open Bfa_c_lib

let process_one = Csymex.assume [ Typed.v_false ]
let process_two = Csymex.assume [ Typed.v_true ]
let count_outcomes process = Csymex.run process |> List.length

let () =
  Printf.printf "Number of outcomes for process_one: %d\n"
    (count_outcomes process_one);
  Printf.printf "Number of outcomes for process_two: %d\n"
    (count_outcomes process_two)
