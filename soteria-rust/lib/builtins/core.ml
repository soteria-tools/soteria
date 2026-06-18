(** Core operations, that may be used in several parts of the interpreter, so we
    share them here for convenience. *)

open Charon
open Svalue
open Typed
open Typed.Syntax
open Typed.Infix
open Syntaxes.FunctionWrap

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let cmp ~signed l r =
    let ordering = Crate.get_adt_lang_item_ref "Ordering" in
    let ( < ) = if signed then ( <$@ ) else ( <@ ) in
    let discr =
      Typed.ite (l < r) U8.(-1s) (Typed.ite (l ==@ r) U8.(0s) U8.(1s))
    in
    Typed.Adt.mk_enum ordering discr []

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
        not_impl "Unexpected types in cval equality: %a and %a" Typed.ppa v1
          Typed.ppa v2

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
    let checked_of : Expressions.overflow_mode -> Typed.checked = function
      | OWrap -> unchecked
      | OPanic | OUB -> checked_of_signed signed
    in
    let res =
      match bop with
      | Add om -> BV.add ~checked:(checked_of om) l r
      | Sub om -> BV.sub ~checked:(checked_of om) l r
      | Mul om -> BV.mul ~checked:(checked_of om) l r
      | Div _ -> BV.div ~signed l (cast r)
      | Rem _ -> BV.rem ~signed l (cast r)
      | Shl _ -> BV.shl l r
      | Shr _ -> if signed then BV.ashr l r else BV.lshr l r
      | _ -> L.failwith "Invalid binop in binop_fn"
    in
    (* SAFETY: Overflows were either already checked for, or it was expected to
       properly wrap. *)
    ok (BV.no_ovf_unsafe res)

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
      | _ ->
          L.failwith "Invalid checked op: (%a, %b)" Expressions.pp_binop op
            signed
    in
    ok (Typed.Adt.mk_tuple [ wrapped; BV.of_bool overflowed ])

  let meta_as_int meta =
    match%ty meta with
    | TBitVector _ -> ok meta
    | TExtension TThinPtr -> Sptr.decay meta
    | _ -> failwith "invalid metadata type"

  let opt_meta_as_int = function
    | None -> ok None
    | Some meta -> map Option.some (meta_as_int meta)

  let eval_meta_eq (l : [< T.ptr_meta ] Typed.t option)
      (r : [< T.ptr_meta ] Typed.t option) =
    let* meta_l = opt_meta_as_int l in
    let+ meta_r = opt_meta_as_int r in
    match (meta_l, meta_r) with
    | Some l, Some r -> l ==@ r
    | None, None -> v_true
    | _, _ -> v_false (* TODO: is this branch even feasible? *)

  let rec eval_ptr_binop (bop : Expressions.binop) (l : Typed.([< T.sptr_f ] t))
      (r : Typed.([< T.sptr_f ] t)) =
    match bop with
    | Ne ->
        let+ res = eval_ptr_binop Eq l r in
        BV.not_bool (cast res)
    | Eq ->
        let null_or_in_bound p = Sptr.is_null p ||@ Sptr.in_bound p in
        let l, meta_l = Typed.Ptr.split l in
        let r, meta_r = Typed.Ptr.split r in
        let same_provenance = Sptr.have_same_provenance l r in
        if%sure same_provenance then
          (* Fast path: if two pointer have the same provenance, it's enough to
             compare their offsets *)
          let+ meta_eq = eval_meta_eq meta_l meta_r in
          BV.of_bool (meta_eq &&@ (Typed.Ptr.ofs' l ==@ Typed.Ptr.ofs' r))
        else if%sure
          (not same_provenance) &&@ null_or_in_bound l &&@ null_or_in_bound r
        then
          (* Fast path: case where two pointers have different provenances. If
             they are both in bound, then they can't compare equal since two
             distinct allocations cannot overlap. Similarly, if one of the is
             null, and the other one has a valid provance and is in bound, then
             they can't compare equal either because an allocation cannot be at
             address 0.

             Note: pointers that have no provenance have size 0, so they are
             always out of bound, so testing equality of a pointer with valid
             provenance and a non-0 address with no provenance will bypass this
             path, as it should. *)
          ok (BV.of_bool v_false)
        else
          let* meta_eq = eval_meta_eq meta_l meta_r in
          let+ distance = Sptr.distance l r in
          let ptr_eq = distance ==@ Usize.(0s) in
          BV.of_bool (meta_eq &&@ ptr_eq)
    | Lt | Le | Gt | Ge -> (
        let l, ml = Typed.Ptr.split l in
        let r, mr = Typed.Ptr.split r in
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
        | None, None -> ok (BV.of_bool v)
        (* is the below line correct? *)
        | None, _ | _, None -> ok (BV.of_bool v)
        | Some ml, Some mr ->
            if%sat dist ==@ Usize.(0s) then
              let* ml = meta_as_int ml in
              let* mr = meta_as_int mr in
              ok (BV.of_bool (bop ml mr))
            else ok (BV.of_bool v))
    | op ->
        not_impl "Unexpected operation or value in eval_ptr_binop: %a, %a, %a"
          Expressions.pp_binop op Typed.ppa l Typed.ppa r

  let zero_valid ~ty =
    let+^ res =
      let@ () = run ~env:() ~state:State.empty in
      let* { size; align; _ } = Layout.layout_of ty in
      let* ptr = State.alloc_untyped ~zeroed:true ~size ~align () in
      State.load ptr ty
    in
    Compo_res.is_ok res

  let parse_string ptr =
    let str_ty : Charon.Types.ty =
      TAdt
        { id = TBuiltin TStr; generics = Charon.TypesUtils.empty_generic_args }
    in
    let+ str_data = State.load ptr str_ty in
    Typed.Adt.as_tuple @@ Typed.cast_any_adt str_data
    |> (Monad.OptionM.all @@ fun b -> Typed.BitVec.to_z @@ Typed.cast_i U8 b)
    |> Option.map (fun cs ->
        let cs = List.map (fun z -> Char.chr (Z.to_int z)) cs in
        let str = String.of_seq @@ List.to_seq cs in
        if
          String.starts_with ~prefix:"\"" str
          && String.ends_with ~suffix:"\"" str
        then
          let unquoted = String.sub str 1 (String.length str - 2) in
          try Scanf.unescaped unquoted with _ -> unquoted
        else str)

  let string_to_ptr str =
    let* ptr_res = State.load_str_global str in
    match ptr_res with
    | Some ptr -> ok ptr
    | None ->
        let len = String.length str in
        let chars =
          String.to_bytes str
          |> Bytes.fold_left (fun l c -> BV.u8i (Char.code c) :: l) []
          |> List.rev
        in
        let char_arr = Typed.Adt.mk_tuple chars in
        let str_ty : Types.ty =
          Common.Charon_util.mk_array_ty (TLiteral (TUInt U8)) (Z.of_int len)
        in
        let@ () = with_alloc_kind ~kind:StaticString in
        let* ptr = State.alloc_ty str_ty in
        let ptr, _ = Typed.Ptr.split ptr in
        let ptr = Typed.Ptr.mk_ptr_f ptr (Some (BV.usizei len)) in
        let* () = State.store ptr str_ty char_arr in
        let+ () = State.store_str_global str ptr in
        ptr
end
