open Soteria_c_values
open Soteria_c_lib
open Csymex
open Syntax

let process_one = assume [ Typed.v_false ]
let process_two = assume [ Typed.v_true ]
let count_outcomes process = run process |> List.length

let () =
  Printf.printf "Number of outcomes for process_one: %d\n"
    (count_outcomes process_one);
  Printf.printf "Number of outcomes for process_two: %d\n"
    (count_outcomes process_two);
  Printf.printf "\n\n"

let fn () =
  let open Typed.Infix in
  let* v = nondet ~constrs:(fun x -> [ Typed.zero ==@ x ]) Typed.t_int in
  if%sat Typed.not (v ==@ Typed.one) then return true else return false

let do_test () =
  let pp =
    let open Fmt in
    list ~sep:semi (braces (pair ~sep:comma bool (Dump.list Typed.ppa)))
  in
  let b = run (fn ()) in
  Fmt.pr "Branches: %a@\n" pp b;
  let b = run (fn ()) in
  Fmt.pr "Branches: %a@\n" pp b

let () = do_test ()
