  $ ../test.sh err_non_t.ml
  File "err_non_t.ml", line 3, characters 17-20:
  3 | type t = { foo : int; bar : Bar.t } [@@deriving reversible]
                       ^^^
  Error: [@deriving reversible] expects fields/components to have type <Module>.t
  [1]
