  $ ../test.sh err_all_ignored.ml
  File "err_all_ignored.ml", lines 3-4, characters 0-23:
  3 | type t = { foo : Foo.t; [@reversible.ignore] bar : Bar.t [@reversible.ignore] }
  4 | [@@deriving reversible]
  Error: [@deriving reversible] cannot be used on a record where all fields are ignored
  [1]
