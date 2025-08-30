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

  let cmp_of_int v =
    let size = size_of_int v in
    let zero = BitVec.zero size in
    if%sat v <$@ zero then return (BitVec.mki size (-1))
    else if%sat v ==@ zero then return zero else return (BitVec.one size)

  let rec equality_check (v1 : [< T.cval ] Typed.t) (v2 : [< T.cval ] Typed.t) =
    match (get_ty v1, get_ty v2) with
    | TBitVector _, TBitVector _ | TPointer _, TPointer _ ->
        Result.ok (v1 ==@ v2 |> BitVec.of_bool 8)
    | TFloat _, TFloat _ -> Result.ok (v1 ==.@ v2 |> BitVec.of_bool 8)
    | TPointer _, TBitVector n ->
        let v2 : T.sint Typed.t = cast v2 in
        if%sat v2 ==@ BitVec.zero n then
          let res = cast v1 ==@ Ptr.null n |> BitVec.of_bool 8 in
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

  (** Evaluates a binary operator of [+,-,/,*,rem], and ensures the result is
      within the type's constraints, else errors *)
  let eval_lit_binop (bop : Expressions.binop) ty l r =
    (* do overflow/arithmetic checks *)
    let signed = Layout.is_signed ty in
    let* l, r, ty_ = cast_checked2 l r in
    let is_integer =
      match untype_type ty_ with TBitVector _ -> true | _ -> false
    in
    let** () =
      match bop with
      | _ when Stdlib.not is_integer -> Result.ok ()
      | Add (OUB | OPanic) ->
          let overflows = BitVec.plus_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Sub (OUB | OPanic) ->
          let overflows = BitVec.minus_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Mul (OUB | OPanic) ->
          let overflows = BitVec.times_overflows ~signed l r in
          if%sat overflows then Result.error `Overflow else Result.ok ()
      | Div (OUB | OPanic) | Rem (OUB | OPanic) ->
          let size = 8 * Layout.size_of_literal_ty ty in
          if%sat r ==@ BitVec.zero size then Result.error `DivisionByZero
          else if signed then
            let min = Layout.min_value_z ty in
            let min = BitVec.mk_masked size min in
            let m_one = BitVec.mki_masked size (-1) in
            if%sat l ==@ min &&@ (r ==@ m_one) then Result.error `Overflow
            else Result.ok ()
          else Result.ok ()
      | Shl (OUB | OPanic) | Shr (OUB | OPanic) ->
          let size = 8 * Layout.size_of_literal_ty ty in
          let r = cast r in
          if%sat r <$@ BitVec.zero size ||@ (r >=$@ BitVec.mki size size) then
            Result.error `InvalidShift
          else Result.ok ()
      | _ -> Result.ok ()
    in

    match untype_type ty_ with
    | TBitVector _ ->
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
    | TPointer _ -> Result.error `UBPointerArithmetic
    | _ -> not_impl "Unexpected type in eval_lit_binop"

  (** Wraps a given value to make it fit within the constraints of the given
      type *)
  let wrapping_binop (bop : Expressions.binop) ty l r =
    let size = 8 * Layout.size_of_literal_ty ty in
    let* l = cast_checked ~ty:(t_int size) l in
    let+ r = cast_checked ~ty:(t_int size) r in
    let signed = Layout.is_signed ty in
    let op = binop_fn bop signed in
    op l r

  (** Evaluates the checked operation, returning (wrapped value, overflowed). *)
  let eval_checked_lit_binop op ty l r =
    let* wrapped = wrapping_binop op ty l r in
    let signed = Layout.is_signed ty in
    let size = 8 * Layout.size_of_literal_ty ty in
    let* l = cast_checked ~ty:(t_int size) l in
    let* r = cast_checked ~ty:(t_int size) r in
    let overflows_fn =
      match op with
      | AddChecked -> BitVec.plus_overflows
      | SubChecked -> BitVec.minus_overflows
      | MulChecked -> BitVec.times_overflows
      | _ -> failwith "Invalid checked op"
    in
    let overflowed = BitVec.of_bool 8 @@ overflows_fn ~signed l r in
    Result.ok (Tuple [ Base wrapped; Base overflowed ])

  let rec eval_ptr_binop (bop : Expressions.binop) l r :
      ([> T.cval ] Typed.t, 'e, 'm) Result.t =
    match (bop, l, r) with
    | Ne, _, _ ->
        let++ res = eval_ptr_binop Eq l r in
        BitVec.not_bool (cast res)
    | Eq, Ptr (l, None), Ptr (r, None) ->
        Result.ok (BitVec.of_bool 8 (Sptr.sem_eq l r))
    | Eq, Ptr (l, Some ml), Ptr (r, Some mr) ->
        Result.ok (BitVec.of_bool 8 (Sptr.sem_eq l r &&@ (ml ==@ mr)))
    | Eq, Ptr (_, Some _), Ptr (_, None) | Eq, Ptr (_, None), Ptr (_, Some _) ->
        Result.ok (BitVec.of_bool 8 v_false)
    | Eq, Ptr (p, _), Base v | Eq, Base v, Ptr (p, _) ->
        let* v = cast_int v in
        let bits = size_of_int v in
        if%sat v ==@ BitVec.zero bits then
          Result.ok (BitVec.of_bool 8 (Sptr.is_at_null_loc p))
        else Fmt.kstr not_impl "Don't know how to eval %a == %a" Sptr.pp p ppa v
    | Eq, Base v1, Base v2 -> Result.ok (BitVec.of_bool 8 (v1 ==@ v2))
    | (Lt | Le | Gt | Ge), Ptr (l, ml), Ptr (r, mr) ->
        if%sat Sptr.is_same_loc l r then
          let* dist = Sptr.distance l r in
          let bits = size_of_int dist in
          let bop =
            match bop with
            | Lt -> ( <$@ )
            | Le -> ( <=$@ )
            | Gt -> ( >$@ )
            | Ge -> ( >=$@ )
            | _ -> assert false
          in
          let v = bop dist (BitVec.zero bits) in
          match (ml, mr) with
          | Some ml, Some mr ->
              if%sat dist ==@ BitVec.zero bits then
                let* ml, mr, mty = cast_checked2 ml mr in
                match untype_type mty with
                | TBitVector _ -> Result.ok (BitVec.of_bool 8 (bop ml mr))
                | mty ->
                    Fmt.kstr not_impl
                      "Don't know how to compare metadata of type %a"
                      Svalue.pp_ty mty
              else Result.ok (BitVec.of_bool 8 v)
          (* is this correct? *)
          | Some _, None | None, Some _ -> Result.ok (BitVec.of_bool 8 v)
          | None, None -> Result.ok (BitVec.of_bool 8 v)
        else Result.error `UBPointerComparison
    | Cmp, Ptr (l, _), Ptr (r, _) ->
        if%sat Sptr.is_same_loc l r then
          let* v = Sptr.distance l r in
          let* cmp = cmp_of_int v in
          Result.ok cmp
        else Result.error `UBPointerComparison
    | Cmp, Ptr (p, _), Base v | Cmp, Base v, Ptr (p, _) ->
        let size = size_of_int (cast v) in
        if%sat v ==@ BitVec.zero size then
          if%sat Sptr.is_at_null_loc p then Result.ok (BitVec.zero 8)
          else if l = Base v then Result.ok (BitVec.mki 8 1)
          else Result.ok (BitVec.mki 8 (-1))
        else
          Fmt.kstr not_impl "Don't know how to eval %a cmp %a" Sptr.pp p ppa v
    | op, l, r ->
        Fmt.kstr not_impl
          "Unexpected operation or value in eval_ptr_binop: %a, %a, %a"
          Expressions.pp_binop op pp_rust_val l pp_rust_val r
end
