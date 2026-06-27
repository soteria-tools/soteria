  $ ../test.sh multi.ml
  open Prelude
  
  type cty = CInt | CFloat | CPtr | COther
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
  let use_sfloat (_ : [< Typed.T.sfloat ] Typed.t) = ()
  let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()
  
  let test (x : [< Typed.T.any ] Typed.t) (c : cty) =
    match (Typed.get_ty x, c) with
    | TBitVector _, CInt ->
        let x =
          (Typed.cast x : [< Typed.T.sint ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sint x
    | TFloat _, (CFloat | COther) ->
        let x =
          (Typed.cast x : [< Typed.T.sfloat ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sfloat x
    | TExtension TFullPtr, CPtr ->
        let x =
          (Typed.cast x : [< Typed.T.sptr_f ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sptr_f x
    | _, _ -> ()
  Success ✅
