  $ ../test.sh or_ok.ml
  open Prelude
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
  
  let test (x : [< Typed.T.any ] Typed.t) =
    match Typed.get_ty x with
    | TBitVector 8 | TBitVector 16 ->
        let x =
          (Typed.cast x : [< Typed.T.sint ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sint x
    | _ -> ()
  Success ✅
