  $ ../test.sh err_ext_unknown.ml
  File "err_ext_unknown.ml", line 5, characters 29-35:
  5 |   match%ty x with TExtension TBogus -> x | _ -> x
                                   ^^^^^^
  Error: match%ty: unknown extension constructor [TBogus]
  [1]
