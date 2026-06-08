  $ ../test.sh payload.ml
  open Prelude
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = 0
  
  let test (x : [< Typed.T.any ] Typed.t) =
    match Typed.get_ty x with
    | TBitVector n when n > 8 ->
        let x = (Typed.cast x : [< Typed.T.sint ] Typed.t) in
        n + use_sint x
    | TBitVector size ->
        let x = (Typed.cast x : [< Typed.T.sint ] Typed.t) in
        size + use_sint x
    | TExtension (Adt a) ->
        let x = (Typed.cast x : [< Typed.T.adt ] Typed.t) in
        a
    | _ -> -1
  Success ✅
