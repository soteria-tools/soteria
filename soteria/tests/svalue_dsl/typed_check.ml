(* Compilation IS the test: the DSL-derived [Svalue_dsl.Typed] must enforce
   the same phantom-sort discipline as the hand-written [Typed]/[typed.mli].
   If the sorts were wrong, these annotations would not typecheck. *)

module T = Soteria.Tiny_values.Svalue_dsl.Typed
open T.T

let a : [> sint ] T.t = T.int_z (Z.of_int 3)
let b : [> sint ] T.t = T.int_z (Z.of_int 4)

(* arithmetic stays in [sint] *)
let _sum : [> sint ] T.t = T.add a b
let _prod : [> sint ] T.t = T.mul a (T.sub a b)

(* comparisons land in [sbool] *)
let cmp : [> sbool ] T.t = T.lt a b
let _eq : [> sbool ] T.t = T.sem_eq a b

(* boolean algebra needs [sbool] *)
let _conj : [> sbool ] T.t = T.and_ cmp (T.not cmp)
let tt : [> sbool ] T.t = T.v_true

(* [ite]'s guard must be [sbool]; its branches are polymorphic *)
let _i : [> sint ] T.t = T.ite tt a b
let _b : [> sbool ] T.t = T.ite tt cmp tt

(* a function that only accepts strictly-boolean values; passing a [sint]
   here would be a type error, proving the sorts are real *)
let only_bool (x : [< sbool ] T.t) = T.not x
let _ = only_bool cmp

let () =
  Printf.printf "Svalue_dsl.Typed phantom typing OK: %s\n"
    (Format.asprintf "%a / %a" T.ppa _sum T.ppa _conj)
