open Charon
open Typed
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Rustsymex
open Rustsymex.Syntax
open Rust_val

module M (State : State_intf.S) = struct
  module Sptr = State.Sptr

  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  let cmp ~signed l r =
    let ( < ) = if signed then ( <$@ ) else ( <@ ) in
    if%sat l < r then return U8.(-1s)
    else if%sat l ==@ r then return U8.(0s) else return U8.(1s)

  let cmp_of_int v =
    let zero = BV.zero (size_of_int v) in
    if%sat v <$@ zero then return U8.(-1s)
    else if%sat v ==@ zero then return U8.(0s) else return U8.(1s)

  let rec equality_check (v1 : [< T.cval ] Typed.t) (v2 : [< T.cval ] Typed.t) =
    match (get_ty v1, get_ty v2) with
    | TBitVector _, TBitVector _ | TPointer _, TPointer _ ->
        Result.ok (BV.of_bool (v1 ==@ v2))
    | TFloat _, TFloat _ -> Result.ok (BV.of_bool (v1 ==.@ v2))
    | TPointer _, TBitVector _ ->
        let v2 : T.sint Typed.t = cast v2 in
        if%sat v2 ==@ Usize.(0s) then
          let res = cast v1 ==@ Ptr.null () in
          Result.ok (BV.of_bool res)
        else Result.error `UBPointerComparison
    | TBitVector _, TPointer _ -> equality_check v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a" ppa v1
          ppa v2

  let binop_fn (bop : Expressions.binop) signed =
    match bop with
    | Add _ | AddChecked -> ( +!@ )
    | Sub _ | SubChecked -> ( -!@ )
    | Mul _ | MulChecked -> ( *!@ )
    | Div _ -> BV.div ~signed
    | Rem _ -> BV.rem ~signed
    | Shl _ -> ( <<@ )
    | Shr _ -> if signed then BV.ashr else BV.lshr
    | _ -> failwith "Invalid binop in binop_fn"

  (** Rust allows shift operations on integers of differents sizes, which isn't
      possible in SMT-Lib, so we normalise the righthand side to match the left
      hand side. *)
  let normalise_shift_r (bop : Expressions.binop) l r =
    match bop with
    | Shl _ | Shr _ ->
        let l, r = (cast l, cast r) in
        let l_size = Typed.size_of_int l in
        let r_size = Typed.size_of_int r in
        if l_size > r_size then BV.extend ~signed:false (l_size - r_size) r
        else if l_size < r_size then BV.extract 0 (l_size - 1) r
        else r
    | _ -> r

  (** Evaluates a binary operator of [+,-,/,*,rem], and ensures the result is
      within the type's constraints, else errors *)
  let eval_lit_binop (bop : Expressions.binop) ty (l : T.cval Typed.t)
      (r : T.cval Typed.t) : ([> T.sfloat | T.sint ] Typed.t, 'e, 'f) Result.t =
    (* do overflow/arithmetic checks *)
    let signed = Layout.is_signed ty in
    let is_integer = match ty with TUInt _ | TInt _ -> true | _ -> false in
    let** () =
      match bop with
      | _ when Stdlib.not is_integer -> Result.ok ()
      | Add (OUB | OPanic) ->
          let l = cast_lit ty l in
          let r = cast_lit ty r in
          let overflows = BV.add_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Sub (OUB | OPanic) ->
          let l = cast_lit ty l in
          let r = cast_lit ty r in
          let overflows = BV.sub_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Mul (OUB | OPanic) ->
          let l = cast_lit ty l in
          let r = cast_lit ty r in
          let overflows = BV.mul_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Div om | Rem om ->
          if%sat r ==@ BV.mki_lit ty 0 then Result.error `DivisionByZero
          else if signed then
            let min = Layout.min_value_z ty in
            let min = BV.mk_lit ty min in
            let m_one = BV.mki_lit ty (-1) in
            if%sat bool (om <> OWrap) &&@ (l ==@ min) &&@ (r ==@ m_one) then
              Result.error `Overflow
            else Result.ok ()
          else Result.ok ()
      | Shl (OUB | OPanic) | Shr (OUB | OPanic) ->
          (* at this point, the size of the right-hand side might not match the given literal
             type, so we must be careful. *)
          let size = 8 * Layout.size_of_literal_ty ty in
          let r, size_r = cast_int r in
          if%sat r <$@ BV.mki size_r 0 ||@ (r >=$@ BV.mki size_r size) then
            Result.error `InvalidShift
          else Result.ok ()
      | _ -> Result.ok ()
    in

    match ty with
    | TInt _ | TUInt _ ->
        (* normalise both sides to be the same size; this is usually always the case,
           except for shift operations, so we do it manually *)
        let r = normalise_shift_r bop l r in
        let l = cast_lit ty l in
        let r = cast_lit ty r in
        let op = binop_fn bop signed in
        Result.ok (cast (op l (cast r)))
    | TFloat _ -> (
        let l, r = (cast l, cast r) in
        match bop with
        | Add _ -> Result.ok (l +.@ r)
        | Sub _ -> Result.ok (l -.@ r)
        | Mul _ -> Result.ok (l *.@ r)
        | Div _ -> Result.ok (l /.@ r)
        | Rem _ -> Result.ok (Float.rem l (cast r))
        | _ -> not_impl "Invalid binop for float in eval_lit_binop")
    | _ -> not_impl "Unexpected type in eval_lit_binop"

  (** Applies the given binary operator using wrapping semantics; in other
      words, any overflow is ignored and unchecked for. *)
  let wrapping_binop (bop : Expressions.binop) ty l r :
      ([> T.sint ] Typed.t, 'e, 'f) Result.t =
    let++ () =
      match bop with
      | Div _ | Rem _ ->
          if%sat r ==@ BV.mki_lit ty 0 then Result.error `DivisionByZero
          else Result.ok ()
      | _ -> Result.ok ()
    in
    let r = normalise_shift_r bop l r in
    let l = cast_lit ty l in
    let r = cast_lit ty r in
    let signed = Layout.is_signed ty in
    let op = binop_fn bop signed in
    cast (op l (cast r))

  (** Evaluates the checked operation, returning (wrapped value, overflowed). *)
  let eval_checked_lit_binop (op : Expressions.binop) ty l r =
    let l = cast_lit ty l in
    let r = cast_lit ty r in
    let wrapped, overflowed =
      match op with
      | AddChecked -> l +?@ r
      | SubChecked -> l -?@ r
      | MulChecked -> l *?@ r
      | _ -> failwith "Invalid checked op"
    in
    Result.ok (Tuple [ Base wrapped; Base (BV.of_bool overflowed) ])

  let rec eval_ptr_binop (bop : Expressions.binop) l r :
      ([> T.cval ] Typed.t, 'e, 'm) Result.t =
    match (bop, l, r) with
    | Ne, _, _ ->
        let++ res = eval_ptr_binop Eq l r in
        BV.not_bool (cast res)
    | Eq, Ptr (l, None), Ptr (r, None) ->
        Result.ok (BV.of_bool (Sptr.sem_eq l r))
    | Eq, Ptr (l, Some ml), Ptr (r, Some mr) ->
        Result.ok (BV.of_bool (Sptr.sem_eq l r &&@ (ml ==@ mr)))
    | Eq, Ptr (_, Some _), Ptr (_, None) | Eq, Ptr (_, None), Ptr (_, Some _) ->
        Result.ok (BV.of_bool v_false)
    | Eq, Ptr (p, _), Base v | Eq, Base v, Ptr (p, _) ->
        let v = cast_i Usize v in
        if%sat v ==@ Usize.(0s) then
          Result.ok (BV.of_bool (Sptr.is_at_null_loc p))
        else Fmt.kstr not_impl "Don't know how to eval %a == %a" Sptr.pp p ppa v
    | Eq, Base v1, Base v2 -> Result.ok (BV.of_bool (v1 ==@ v2))
    | (Lt | Le | Gt | Ge), Ptr (l, ml), Ptr (r, mr) ->
        if%sat Sptr.is_same_loc l r then
          let* dist = Sptr.distance l r in
          let bop =
            match bop with
            | Lt -> ( <$@ )
            | Le -> ( <=$@ )
            | Gt -> ( >$@ )
            | Ge -> ( >=$@ )
            | _ -> assert false
          in
          let v = bop dist Usize.(0s) in
          match (ml, mr) with
          | Some ml, Some mr ->
              if%sat dist ==@ Usize.(0s) then
                let ml, mr, mty = cast_checked2 ml mr in
                match untype_type mty with
                | TBitVector _ -> Result.ok (BV.of_bool (bop ml mr))
                | mty ->
                    Fmt.kstr not_impl
                      "Don't know how to compare metadata of type %a"
                      Svalue.pp_ty mty
              else Result.ok (BV.of_bool v)
          (* is this correct? *)
          | Some _, None | None, Some _ -> Result.ok (BV.of_bool v)
          | None, None -> Result.ok (BV.of_bool v)
        else Result.error `UBPointerComparison
    | Cmp, Ptr (l, _), Ptr (r, _) ->
        if%sat Sptr.is_same_loc l r then
          let* v = Sptr.distance l r in
          let* cmp = cmp_of_int v in
          Result.ok cmp
        else Result.error `UBPointerComparison
    | Cmp, Ptr (p, _), Base v | Cmp, Base v, Ptr (p, _) ->
        if%sat v ==@ BV.usizei (Layout.size_of_uint_ty Usize) then
          if%sat Sptr.is_at_null_loc p then Result.ok U8.(0s)
          else if l = Base v then Result.ok U8.(1s)
          else Result.ok U8.(-1s)
        else
          Fmt.kstr not_impl "Don't know how to eval %a cmp %a" Sptr.pp p ppa v
    | op, l, r ->
        Fmt.kstr not_impl
          "Unexpected operation or value in eval_ptr_binop: %a, %a, %a"
          Expressions.pp_binop op pp_rust_val l pp_rust_val r
end
