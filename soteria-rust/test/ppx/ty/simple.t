  $ ../test.sh simple.ml
  open Prelude
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
  let use_sfloat (_ : [< Typed.T.sfloat ] Typed.t) = ()
  let use_sbool (_ : [< Typed.T.sbool ] Typed.t) = ()
  let use_sptr (_ : [< Typed.T.sptr ] Typed.t) = ()
  let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()
  let use_sptr_t (_ : [< Typed.T.sptr_t ] Typed.t) = ()
  let use_adt (_ : [< Typed.T.enum ] Typed.t) = ()
  
  let test (x : [< Typed.T.any ] Typed.t) =
    match Typed.get_ty x with
    | TBool ->
        let x =
          (Typed.cast x : [< Typed.T.sbool ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sbool x
    | TBitVector _ ->
        let x =
          (Typed.cast x : [< Typed.T.sint ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sint x
    | TFloat _ ->
        let x =
          (Typed.cast x : [< Typed.T.sfloat ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sfloat x
    | TPointer _ ->
        let x =
          (Typed.cast x : [< Typed.T.sptr ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sptr x
    | TExtension TFullPtr ->
        let x =
          (Typed.cast x : [< Typed.T.sptr_f ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sptr_f x
    | TExtension TThinPtr ->
        let x =
          (Typed.cast x : [< Typed.T.sptr_t ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sptr_t x
    | TExtension (TEnum _) ->
        let x =
          (Typed.cast x : [< Typed.T.enum ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_adt x
    | _ -> ()
  Success ✅
