open Charon
open Typed
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
    if%sat l < r then return (BitVec.u8i (-1))
    else if%sat l ==@ r then return (BitVec.u8i 0) else return (BitVec.u8i 1)

  let cmp_of_int v =
    let zero = BitVec.zero (size_of_int v) in
    if%sat v <$@ zero then return (BitVec.u8i (-1))
    else if%sat v ==@ zero then return (BitVec.u8i 0) else return (BitVec.u8i 1)

  let rec equality_check (v1 : [< T.cval ] Typed.t) (v2 : [< T.cval ] Typed.t) =
    match (get_ty v1, get_ty v2) with
    | TBitVector _, TBitVector _ | TPointer _, TPointer _ ->
        Result.ok (v1 ==@ v2 |> BitVec.of_bool)
    | TFloat _, TFloat _ -> Result.ok (v1 ==.@ v2 |> BitVec.of_bool)
    | TPointer _, TBitVector _ ->
        let v2 : T.sint Typed.t = cast v2 in
        if%sat v2 ==@ BitVec.usizei 0 then
          let res = cast v1 ==@ Ptr.null () |> BitVec.of_bool in
          Result.ok res
        else Result.error `UBPointerComparison
    | TBitVector _, TPointer _ -> equality_check v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a" ppa v1
          ppa v2

  let binop_fn (bop : Expressions.binop) signed =
    match bop with
    | Add _ | AddChecked -> ( +@ )
    | Sub _ | SubChecked -> ( -@ )
    | Mul _ | MulChecked -> ( *@ )
    | Div _ -> BitVec.div ~signed
    | Rem _ -> BitVec.rem ~signed
    | Shl _ -> ( <<@ )
    | Shr _ -> if signed then BitVec.ashr else BitVec.lshr
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
        if l_size > r_size then
          Typed.BitVec.extend ~signed:false (l_size - r_size) r
        else if l_size < r_size then Typed.BitVec.extract 0 (l_size - 1) r
        else r
    | _ -> r

  (** Evaluates a binary operator of [+,-,/,*,rem], and ensures the result is
      within the type's constraints, else errors *)
  let eval_lit_binop (bop : Expressions.binop) ty (l : T.cval Typed.t)
      (r : T.cval Typed.t) =
    (* do overflow/arithmetic checks *)
    let signed = Layout.is_signed ty in
    let is_integer = match ty with TUInt _ | TInt _ -> true | _ -> false in
    let** () =
      match bop with
      | _ when Stdlib.not is_integer -> Result.ok ()
      | Add (OUB | OPanic) ->
          let l = cast_lit ty l in
          let r = cast_lit ty r in
          let overflows = BitVec.add_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Sub (OUB | OPanic) ->
          let l = cast_lit ty l in
          let r = cast_lit ty r in
          let overflows = BitVec.sub_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Mul (OUB | OPanic) ->
          let l = cast_lit ty l in
          let r = cast_lit ty r in
          let overflows = BitVec.mul_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Div (OUB | OPanic) | Rem (OUB | OPanic) ->
          if%sat r ==@ BitVec.mki_lit ty 0 then Result.error `DivisionByZero
          else if signed then
            let min = Layout.min_value_z ty in
            let min = BitVec.mk_lit ty min in
            let m_one = BitVec.mki_lit ty (-1) in
            if%sat l ==@ min &&@ (r ==@ m_one) then Result.error `Overflow
            else Result.ok ()
          else Result.ok ()
      | Shl (OUB | OPanic) | Shr (OUB | OPanic) ->
          (* at this point, the size of the right-hand side might not match the given literal
             type, so we must be careful. *)
          let size = 8 * Layout.size_of_literal_ty ty in
          let r, size_r = cast_int r in
          if%sat r <$@ BitVec.mki size_r 0 ||@ (r >=$@ BitVec.mki size_r size)
          then Result.error `InvalidShift
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
        Result.ok (op l r)
    | TFloat _ -> (
        let l, r = (cast l, cast r) in
        match bop with
        | Add _ -> Result.ok (l +.@ r)
        | Sub _ -> Result.ok (l -.@ r)
        | Mul _ -> Result.ok (l *.@ r)
        | Div _ -> Result.ok (l /.@ cast r)
        | Rem _ -> Result.ok (Float.rem l (cast r))
        | _ -> not_impl "Invalid binop for float in eval_lit_binop")
    | _ -> not_impl "Unexpected type in eval_lit_binop"

  (** Wraps a given value to make it fit within the constraints of the given
      type *)
  let wrapping_binop (bop : Expressions.binop) ty l r =
    let r = normalise_shift_r bop l r in
    let l = cast_lit ty l in
    let r = cast_lit ty r in
    let signed = Layout.is_signed ty in
    let op = binop_fn bop signed in
    op l r

  (** Evaluates the checked operation, returning (wrapped value, overflowed). *)
  let eval_checked_lit_binop op ty l r =
    let wrapped = wrapping_binop op ty l r in
    let signed = Layout.is_signed ty in
    let l = cast_lit ty l in
    let r = cast_lit ty r in
    let overflows_fn =
      match op with
      | AddChecked -> BitVec.add_overflows
      | SubChecked -> BitVec.sub_overflows
      | MulChecked -> BitVec.mul_overflows
      | _ -> failwith "Invalid checked op"
    in
    let overflowed = BitVec.of_bool @@ overflows_fn ~signed l r in
    Result.ok (Tuple [ Base wrapped; Base overflowed ])

  let rec eval_ptr_binop (bop : Expressions.binop) l r :
      ([> T.cval ] Typed.t, 'e, 'm) Result.t =
    match (bop, l, r) with
    | Ne, _, _ ->
        let++ res = eval_ptr_binop Eq l r in
        BitVec.not_bool (cast res)
    | Eq, Ptr (l, None), Ptr (r, None) ->
        Result.ok (BitVec.of_bool (Sptr.sem_eq l r))
    | Eq, Ptr (l, Some ml), Ptr (r, Some mr) ->
        Result.ok (BitVec.of_bool (Sptr.sem_eq l r &&@ (ml ==@ mr)))
    | Eq, Ptr (_, Some _), Ptr (_, None) | Eq, Ptr (_, None), Ptr (_, Some _) ->
        Result.ok (BitVec.of_bool v_false)
    | Eq, Ptr (p, _), Base v | Eq, Base v, Ptr (p, _) ->
        let v = cast_i Usize v in
        if%sat v ==@ BitVec.usizei 0 then
          Result.ok (BitVec.of_bool (Sptr.is_at_null_loc p))
        else Fmt.kstr not_impl "Don't know how to eval %a == %a" Sptr.pp p ppa v
    | Eq, Base v1, Base v2 -> Result.ok (BitVec.of_bool (v1 ==@ v2))
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
          let v = bop dist (BitVec.usizei 0) in
          match (ml, mr) with
          | Some ml, Some mr ->
              if%sat dist ==@ BitVec.usizei 0 then
                let ml, mr, mty = cast_checked2 ml mr in
                match untype_type mty with
                | TBitVector _ -> Result.ok (BitVec.of_bool (bop ml mr))
                | mty ->
                    Fmt.kstr not_impl
                      "Don't know how to compare metadata of type %a"
                      Svalue.pp_ty mty
              else Result.ok (BitVec.of_bool v)
          (* is this correct? *)
          | Some _, None | None, Some _ -> Result.ok (BitVec.of_bool v)
          | None, None -> Result.ok (BitVec.of_bool v)
        else Result.error `UBPointerComparison
    | Cmp, Ptr (l, _), Ptr (r, _) ->
        if%sat Sptr.is_same_loc l r then
          let* v = Sptr.distance l r in
          let* cmp = cmp_of_int v in
          Result.ok cmp
        else Result.error `UBPointerComparison
    | Cmp, Ptr (p, _), Base v | Cmp, Base v, Ptr (p, _) ->
        if%sat v ==@ BitVec.usizei (Layout.size_of_uint_ty Usize) then
          if%sat Sptr.is_at_null_loc p then Result.ok (BitVec.u8i 0)
          else if l = Base v then Result.ok (BitVec.u8i 1)
          else Result.ok (BitVec.u8i (-1))
        else
          Fmt.kstr not_impl "Don't know how to eval %a cmp %a" Sptr.pp p ppa v
    | op, l, r ->
        Fmt.kstr not_impl
          "Unexpected operation or value in eval_ptr_binop: %a, %a, %a"
          Expressions.pp_binop op pp_rust_val l pp_rust_val r
end
