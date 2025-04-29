open Charon_util
open Charon
open Typed
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Rustsymex.Syntax

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr

  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  let cmp_of_int v =
    if%sat v <@ 0s then return (-1s)
    else if%sat v ==@ 0s then return 0s else return 1s

  let rec equality_check (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) st =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt | TPointer, TPointer | TFloat _, TFloat _ ->
        Result.ok (v1 ==@ v2 |> Typed.int_of_bool)
    | TPointer, TInt ->
        let v2 : T.sint Typed.t = Typed.cast v2 in
        if%sat Typed.(v2 ==@ zero) then
          Result.ok (v1 ==@ Typed.Ptr.null |> Typed.int_of_bool)
        else Heap.error `UBPointerComparison st
    | TInt, TPointer -> equality_check v2 v1 st
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  (** Evaluates a binary operator on values of the given type; this operation
      checks for division by zero, but doesn't check for overflow (to allow
      wrapping behaviour.) *)
  let safe_binop (bop : Expressions.binop) (l : [< T.cval ] Typed.t)
      (r : [< T.cval ] Typed.t) st : ([> T.cval ] Typed.t, 'e, 'm) Result.t =
    let* l, r, ty = cast_checked2 l r in
    match untype_type ty with
    | TInt ->
        let** res =
          match bop with
          | Add | CheckedAdd -> Result.ok (l +@ r)
          | Sub | CheckedSub -> Result.ok (l -@ r)
          | Mul | CheckedMul -> Result.ok (l *@ r)
          | Div ->
              if%sat r ==@ 0s then Heap.error `DivisionByZero st
              else Result.ok (l /@ cast r)
          | Rem ->
              if%sat r ==@ 0s then Heap.error `DivisionByZero st
              else Result.ok (rem l (cast r))
          | _ -> not_impl "Invalid binop in eval_lit_binop"
        in
        Result.ok (res :> T.cval Typed.t)
    | TFloat _ ->
        let l, r = (cast l, cast r) in
        let** res =
          match bop with
          | Add -> Result.ok (l +.@ r)
          | Sub -> Result.ok (l -.@ r)
          | Mul -> Result.ok (l *.@ r)
          | Div ->
              if%sat r ==@ float_like r 0.0 then Heap.error `DivisionByZero st
              else Result.ok (l /.@ cast r)
          | Rem ->
              if%sat r ==@ float_like r 0.0 then Heap.error `DivisionByZero st
              else Result.ok (rem l (cast r))
          | _ -> not_impl "Invalid binop in eval_lit_binop"
        in
        Result.ok (res :> T.cval Typed.t)
    | TPointer -> Heap.error `UBPointerArithmetic st
    | _ -> not_impl "Unexpected type in eval_lit_binop"

  (** Evaluates a binary operator of {+,-,/,*,rem}, and ensures the result is within the type's
      constraints, else errors *)
  let eval_lit_binop bop lit_ty l r st =
    let** res = safe_binop bop l r st in
    let** () =
      if bop <> Rem then Result.ok ()
      else
        match lit_ty with
        | Values.TInteger inty ->
            let min = Layout.min_value inty in
            if%sat l ==@ min &&@ (r ==@ -1s) then Heap.error `Overflow st
            else Result.ok ()
        | _ -> Result.ok ()
    in
    let constrs = Layout.constraints lit_ty in
    if%sat conj (constrs res) then Result.ok (res :> T.cval Typed.t)
    else Heap.error `Overflow st

  (** Wraps a given value to make it fit within the constraints of the given
      type *)
  let wrap_value ty v =
    let* v = cast_checked ~ty:Typed.t_int v in
    let size = Layout.size_of_int_ty ty in
    let unsigned_max = nonzero_z (Z.shift_left Z.one (8 * size)) in
    let max = Layout.max_value ty in
    let signed = Layout.is_signed ty in
    let res = v %@ unsigned_max in
    if Stdlib.not signed then return res
    else if%sat res <=@ max then return res else return (res -@ unsigned_max)

  (** Evaluates the checked operation, returning (wrapped value, overflowed). *)
  let eval_checked_lit_binop op lit_ty l r st =
    let ty =
      match lit_ty with
      | Values.TInteger ity -> ity
      | _ -> failwith "Non-integer in checked binary operation"
    in
    let** v = safe_binop op l r st in
    let* wrapped = wrap_value ty v in
    let overflowed = Typed.(int_of_bool (not (v ==@ wrapped))) in
    Result.ok (Tuple [ Base wrapped; Base overflowed ])

  let rec eval_ptr_binop (bop : Expressions.binop) l r st :
      ([> T.cval ] Typed.t, 'e, 'm) Result.t =
    match (bop, l, r) with
    | Ne, _, _ ->
        let++ res = eval_ptr_binop Eq l r st in
        not_int_bool (cast res)
    | Eq, Ptr (l, None), Ptr (r, None) ->
        Result.ok (int_of_bool (Sptr.sem_eq l r))
    | Eq, Ptr (l, Some ml), Ptr (r, Some mr) ->
        Result.ok (int_of_bool (Sptr.sem_eq l r &&@ (ml ==@ mr)))
    | Eq, Ptr (_, Some _), Ptr (_, None) | Eq, Ptr (_, None), Ptr (_, Some _) ->
        Result.ok (int_of_bool Typed.v_false)
    | Eq, Ptr (p, _), Base v | Eq, Base v, Ptr (p, _) ->
        if%sat v ==@ 0s then Result.ok (int_of_bool (Sptr.is_at_null_loc p))
        else
          Fmt.kstr not_impl "Don't know how to eval %a == %a" Sptr.pp p
            Typed.ppa v
    | (Lt | Le | Gt | Ge), Ptr (l, ml), Ptr (r, mr) ->
        if%sat Sptr.is_same_loc l r then
          let dist = Sptr.distance l r in
          let bop =
            match bop with
            | Lt -> ( <@ )
            | Le -> ( <=@ )
            | Gt -> ( >@ )
            | Ge -> ( >=@ )
            | _ -> assert false
          in
          let v = bop dist 0s in
          match (ml, mr) with
          | Some ml, Some mr ->
              if%sat dist ==@ 0s then
                let* ml, mr, mty = cast_checked2 ml mr in
                match untype_type mty with
                | TInt -> Result.ok (int_of_bool (bop ml mr))
                | mty ->
                    Fmt.kstr not_impl
                      "Don't know how to compare metadata of type %a"
                      Svalue.pp_ty mty
              else Result.ok (int_of_bool v)
          (* is this correct? *)
          | Some _, None | None, Some _ -> Result.ok (int_of_bool v)
          | None, None -> Result.ok (int_of_bool v)
        else Heap.error `UBPointerComparison st
    | Cmp, Ptr (l, _), Ptr (r, _) ->
        if%sat Sptr.is_same_loc l r then
          let v = Sptr.distance l r in
          let* cmp = cmp_of_int v in
          Result.ok cmp
        else Heap.error `UBPointerComparison st
    | Cmp, Ptr (p, _), Base v | Cmp, Base v, Ptr (p, _) ->
        if%sat v ==@ 0s then
          if%sat Sptr.is_at_null_loc p then Result.ok 0s
          else if l = Base v then Result.ok 1s
          else Result.ok (-1s)
        else
          Fmt.kstr not_impl "Don't know how to eval %a cmp %a" Sptr.pp p
            Typed.ppa v
    | op, l, r ->
        Fmt.kstr not_impl
          "Unexpected operation or value in eval_ptr_binop: %a, %a, %a"
          Expressions.pp_binop op pp_rust_val l pp_rust_val r
end
