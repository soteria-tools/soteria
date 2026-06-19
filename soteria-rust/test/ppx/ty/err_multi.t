  $ ../test.sh err_multi.ml
  File "err_multi.ml", line 7, characters 23-60:
  7 |   match%ty (x, c) with TBitVector _, CInt | TFloat _, CFloat -> x | _, _ -> x
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: match%ty: this or-pattern mixes branches with different runtime types; split them into separate cases
  [1]
