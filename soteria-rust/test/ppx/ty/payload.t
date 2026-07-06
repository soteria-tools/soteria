  $ ../test.sh payload.ml
  open Prelude
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = 0
  
  let test (x : [< Typed.T.any ] Typed.t) =
    match Typed.get_ty x with
    | TBitVector n when n > 8 ->
        let x =
          (Typed.cast x : [< Typed.T.sint ] Typed.t)
            [@@warning "-unused-var"]
        in
        n + use_sint x
    | TBitVector size ->
        let x =
          (Typed.cast x : [< Typed.T.sint ] Typed.t)
            [@@warning "-unused-var"]
        in
        size + use_sint x
    | TExtension (TEnum a) ->
        let x =
          (Typed.cast x : [< Typed.T.enum ] Typed.t)
            [@@warning "-unused-var"]
        in
        a
    | _ -> -1
  Success ✅
