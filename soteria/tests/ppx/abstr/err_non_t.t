  $ ../test.sh err_non_t.ml
  File "err_non_t.ml", line 3, characters 5-6:
  3 | type u = { off : S_int.t } [@@deriving abstr { symex = Symex }]
           ^
  Error: [@deriving abstr] only supports a type named 't'
  [1]
