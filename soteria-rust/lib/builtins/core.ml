(** Core operations, that may be used in several parts of the interpreter, so we
    share them here for convenience. *)

open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open Rust_val
open Syntaxes.FunctionWrap

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let cmp ~signed l r =
    let ( < ) = if signed then ( <$@ ) else ( <@ ) in
    let discr =
      Typed.ite (l < r) U8.(-1s) (Typed.ite (l ==@ r) U8.(0s) U8.(1s))
    in
    Enum (discr, [])

  let rec equality_check (v1 : [< T.sint | T.sptr ] Typed.t)
      (v2 : [< T.sint | T.sptr ] Typed.t) =
    match (get_ty v1, get_ty v2) with
    | TBitVector _, TBitVector _ | TPointer _, TPointer _ ->
        ok (BV.of_bool (v1 ==@ v2))
    | TPointer _, TBitVector _ ->
        let v2 : T.sint Typed.t = cast v2 in
        if%sat v2 ==@ Usize.(0s) then
          let res = cast v1 ==@ Ptr.null () in
          ok (BV.of_bool res)
        else error `UBPointerComparison
    | TBitVector _, TPointer _ -> equality_check v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a" ppa v1
          ppa v2

  (** Rust allows shift operations on integers of differents sizes, which isn't
      possible in SMT-Lib, so we normalise the righthand side to match the left
      hand side. *)
  let[@inline] normalise_shift_r (bop : Expressions.binop) l r =
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
  let eval_lit_binop (bop : Expressions.binop) ty (l : [< T.sint ] Typed.t)
      (r : [< T.sint ] Typed.t) : ([> T.sint ] Typed.t, 'e) StateM.t =
    (* do overflow/arithmetic checks *)
    let signed = Layout.is_signed ty in
    let r = normalise_shift_r bop l r in
    let* () =
      match bop with
      | Add (OUB | OPanic) ->
          let overflows = BV.add_overflows ~signed l r in
          assert_not overflows `Overflow
      | Sub (OUB | OPanic) ->
          let overflows = BV.sub_overflows ~signed l r in
          assert_not overflows `Overflow
      | Mul (OUB | OPanic) ->
          let overflows = BV.mul_overflows ~signed l r in
          assert_not overflows `Overflow
      | Div _ | Rem _ ->
          let* () = assert_not (r ==@ BV.mki_lit ty 0) `DivisionByZero in
          if signed then
            (* overflow on rem/div is UB even when wrapping *)
            let min = Layout.min_value_z ty in
            let min = BV.mk_lit ty min in
            let m_one = BV.mki_lit ty (-1) in
            assert_ (not (l ==@ min &&@ (r ==@ m_one))) `Overflow
          else ok ()
      | Shl (OUB | OPanic) | Shr (OUB | OPanic) ->
          (* at this point, the size of the right-hand side might not match the
             given literal type, so we must be careful. *)
          let size = 8 * Layout.size_of_literal_ty ty in
          assert_
            (BV.mki_lit ty 0 <=$@ r &&@ (r <$@ BV.mki_lit ty size))
            `InvalidShift
      | _ -> ok ()
    in
    match bop with
    | Add om -> ok (BV.add ~checked:(om <> OWrap) l r)
    | Sub om -> ok (BV.sub ~checked:(om <> OWrap) l r)
    | Mul om -> ok (BV.mul ~checked:(om <> OWrap) l r)
    | Div _ -> ok (BV.div ~signed l (cast r))
    | Rem _ -> ok (BV.rem ~signed l (cast r))
    | Shl _ -> ok (BV.shl l r)
    | Shr _ -> ok (if signed then BV.ashr l r else BV.lshr l r)
    | _ -> failwith "Invalid binop in binop_fn"

  (** Evaluates the checked operation, returning (wrapped value, overflowed). *)
  let eval_checked_lit_binop (op : Expressions.binop) ty l r =
    let l = cast_lit ty l in
    let r = cast_lit ty r in
    let signed = Layout.is_signed ty in
    let wrapped, overflowed =
      match (op, signed) with
      | AddChecked, false -> l +?@ r
      | AddChecked, true -> l +$?@ r
      | SubChecked, false -> l -?@ r
      | SubChecked, true -> l -$?@ r
      | MulChecked, false -> l *?@ r
      | MulChecked, true -> l *$?@ r
      | _ -> failwith "Invalid checked op"
    in
    ok (Tuple [ Int wrapped; Int (BV.of_bool overflowed) ])

  let meta_as_int = function
    | Len l -> ok (Some l)
    | VTable ptr ->
        let+ ptr = Sptr.decay ptr in
        Some ptr
    | Thin -> ok None

  let eval_meta_eq l r =
    let* meta_l = meta_as_int l in
    let+ meta_r = meta_as_int r in
    match (meta_l, meta_r) with
    | Some l, Some r -> l ==@ r
    | None, None -> v_true
    | _, _ -> v_false

  let rec eval_ptr_binop (bop : Expressions.binop) l r =
    match (bop, l, r) with
    | Ne, _, _ ->
        let+ res = eval_ptr_binop Eq l r in
        BV.not_bool (cast res)
    | Eq, Ptr (l, meta_l), Ptr (r, meta_r) ->
        let* meta_eq = eval_meta_eq meta_l meta_r in
        ok (BV.of_bool (meta_eq &&@ Sptr.sem_eq l r))
    | Eq, Ptr (p, _), Int v | Eq, Int v, Ptr (p, _) ->
        let v = cast_i Usize v in
        if%sat v ==@ Usize.(0s) then ok (BV.of_bool (Sptr.is_at_null_loc p))
        else Fmt.kstr not_impl "Don't know how to eval %a == %a" Sptr.pp p ppa v
    | Eq, Int v1, Int v2 -> ok (BV.of_bool (v1 ==@ v2))
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
          | Thin, Thin -> ok (BV.of_bool v)
          (* is the below line correct? *)
          | Thin, _ | _, Thin -> ok (BV.of_bool v)
          | _, _ ->
              if%sat dist ==@ Usize.(0s) then
                let* ml = meta_as_int ml in
                let* mr = meta_as_int mr in
                let ml = Option.get ml in
                let mr = Option.get mr in
                ok (BV.of_bool (bop ml mr))
              else ok (BV.of_bool v)
        else error `UBPointerComparison
    | op, l, r ->
        Fmt.kstr not_impl
          "Unexpected operation or value in eval_ptr_binop: %a, %a, %a"
          Expressions.pp_binop op pp_rust_val l pp_rust_val r

  let transmute ~from_ty ~to_ty v =
    (* Some fun details:
     *  - we need to use [get_state] and re-use the current state, rather than
     *    using an empty state, because if the [load] does any reference
     *    validity checks we need the current state to have these addresses!
     *  - we need to take the max of either types for the alignment, to ensure
     *    that transmuting e.g. from [u16; 2] to (u32) works. *)
    L.debug (fun m ->
        m "Transmuting %a: %a -> %a" pp_rust_val v Common.Charon_util.pp_ty
          from_ty Common.Charon_util.pp_ty to_ty);
    let* { size; align; _ } = Layout.layout_of from_ty in
    let* { align = align_2; _ } = Layout.layout_of to_ty in
    let align = BV.max ~signed:false align align_2 in
    let* ptr = State.alloc_untyped ~zeroed:false ~size ~align () in
    let* () = State.store ptr from_ty v in
    let* v = State.load ptr to_ty in
    let+ () = State.free ptr in
    v

  let zero_valid ~ty =
    let+^ res =
      let@ () = run ~env:() ~state:State.empty in
      let* { size; align; _ } = Layout.layout_of ty in
      let* ptr = State.alloc_untyped ~zeroed:true ~size ~align () in
      State.load ptr ty
    in
    Soteria.Symex.Compo_res.is_ok res
end
