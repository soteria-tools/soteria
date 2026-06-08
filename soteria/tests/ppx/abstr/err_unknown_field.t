  $ ../test.sh err_unknown_field.ml
  File "err_unknown_field.ml", line 3, characters 26-36:
  3 | type t = { off : S_int.t; size : int } [@@deriving abstr { symex = Symex }]
                                ^^^^^^^^^^
  Error: [@deriving abstr] field "size" must have type <Module>.t or <phantom> Typed.t, or be annotated [@concrete]
  [1]
