  $ ../test.sh err_unknown.ml
  File "err_unknown.ml", line 6, characters 18-26:
  6 |   match%ty x with TBogus _ -> x | _ -> x
                        ^^^^^^^^
  Error: match%ty: unknown runtime-type constructor [TBogus]
  [1]
