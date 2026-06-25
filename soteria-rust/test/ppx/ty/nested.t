  $ ../test.sh nested.ml
  open Prelude
  
  let use_sint (_ : [< Typed.T.sint ] Typed.t) = ()
  let use_sptr (_ : [< Typed.T.sptr ] Typed.t) = ()
  let use_sptr_f (_ : [< Typed.T.sptr_f ] Typed.t) = ()
  
  let test (x : [< Typed.T.any ] Typed.t) (y : [< Typed.T.any ] Typed.t) =
    match Typed.get_ty x with
    | TExtension TFullPtr -> (
        let x =
          (Typed.cast x : [< Typed.T.sptr_f ] Typed.t)
            [@@warning "-unused-var"]
        in
        use_sptr_f x;
        match Typed.get_ty y with
        | TPointer _ ->
            let y =
              (Typed.cast y : [< Typed.T.sptr ] Typed.t)
                [@@warning "-unused-var"]
            in
            use_sptr y
        | TBitVector _ ->
            let y =
              (Typed.cast y : [< Typed.T.sint ] Typed.t)
                [@@warning "-unused-var"]
            in
            use_sint y
        | _ -> ())
    | _ -> ()
  Success ✅
