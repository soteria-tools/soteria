  $ ../test.sh err_non_record.ml
  File "err_non_record.ml", line 3, characters 0-53:
  3 | type t = S_int.t [@@deriving abstr { symex = Symex }]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [@deriving abstr] only supports record types
  [1]
