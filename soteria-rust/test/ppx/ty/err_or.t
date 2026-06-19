  $ ../test.sh err_or.ml
  File "err_or.ml", line 5, characters 18-41:
  5 |   match%ty x with TBitVector _ | TFloat _ -> x | _ -> x
                        ^^^^^^^^^^^^^^^^^^^^^^^
  Error: match%ty: this or-pattern mixes branches with different runtime types; split them into separate cases
  [1]
