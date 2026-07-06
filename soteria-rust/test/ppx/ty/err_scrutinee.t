  $ ../test.sh err_scrutinee.ml
  File "err_scrutinee.ml", line 5, characters 11-23:
  5 |   match%ty Typed.cast x with TBool -> () | _ -> ()
                 ^^^^^^^^^^^^
  Error: match%ty: the scrutinee must be a simple identifier (bind it with [let] first)
  [1]
