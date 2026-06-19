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
    | TExtension FullPtr -> use_sptr_f x
    | TExtension ThinPtr -> use_sptr_t x
    | TExtension (Adt _) -> use_adt x
    | _ -> ()
  File "out.ml", line 28, characters 39-40:
  28 |   | TExtension (ThinPtr) -> use_sptr_t x
                                              ^
  Error: The value x has type [< Prelude.Typed.T.sptr_f ] Prelude.Typed.t
         but an expression was expected of type
           [< Prelude.Typed.T.sptr_t ] Prelude.Typed.t
         Type [< Prelude.Typed.T.sptr_f ] = [< `FullPtr ]
         is not compatible with type
           [< Prelude.Typed.T.sptr_t ] = [< `ThinPtr ]
         These two variant types have no intersection
  [1]
