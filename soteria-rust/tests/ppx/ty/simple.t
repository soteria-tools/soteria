  $ ../test.sh simple.ml
  open Prelude
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
  let use_sfloat (_ : [< Typed.T.sfloat ] Typed.t) = ()
  let use_sbool (_ : [< Typed.T.sbool ] Typed.t) = ()
  let use_sptr (_ : [< Typed.T.sptr ] Typed.t) = ()
  let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()
  let use_sptr_t (_ : [< Typed.T.sptr_t ] Typed.t) = ()
  let use_adt (_ : [< Typed.T.adt ] Typed.t) = ()
  
  let test (x : [< Typed.T.any ] Typed.t) =
    match Typed.get_ty x with
    | TBool ->
        let x = (Typed.cast x : [< Typed.T.sbool ] Typed.t) in
        use_sbool x
    | TBitVector _ ->
        let x = (Typed.cast x : [< Typed.T.sint ] Typed.t) in
        use_sint x
    | TFloat _ ->
        let x = (Typed.cast x : [< Typed.T.sfloat ] Typed.t) in
        use_sfloat x
    | TPointer _ ->
        let x = (Typed.cast x : [< Typed.T.sptr ] Typed.t) in
        use_sptr x
    | TExtension FullPtr ->
        let x = (Typed.cast x : [< Typed.T.sptr_f ] Typed.t) in
        use_sptr_f x
    | TExtension ThinPtr ->
        let x = (Typed.cast x : [< Typed.T.sptr_t ] Typed.t) in
        use_sptr_t x
    | TExtension (Adt _) ->
        let x = (Typed.cast x : [< Typed.T.adt ] Typed.t) in
        use_adt x
    | _ -> ()
  Success ✅
