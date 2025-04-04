open Bfa_c_lib
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax

let fn () =
  let* v = nondet ~constrs:(fun x -> [ 0s ==@ x ]) Typed.t_int in
  if%sat Typed.not (v ==@ 1s) then return true else return false

let () =
  let a = run (fn ()) in
  let b = run (fn ()) in
  (* Expected: true / true *)
  Fmt.pr "Branches: %a / %a"
    Fmt.(list ~sep:comma bool)
    (List.map fst a)
    Fmt.(list ~sep:comma bool)
    (List.map fst b);
  exit 0
