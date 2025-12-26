module Make (Mappings : Smtml.Mappings_intf.M) = struct
  open Mappings

  type t = solver
  type value = Svalue.t
  type ty = Svalue.ty

  let mk_ptr, loc, ofs, ptr_ty =
    let ptr =
      Adt.make "Ptr"
        [
          Adt.Cons.make "mk-ptr"
            ~fields:[ ("loc", Some Types.int); ("ofs", Some Types.int) ];
        ]
    in
    let ptr_ty = Adt.ty ptr in
    let loc, ofs =
      ( Adt.selector "loc" ptr |> Option.get,
        Adt.selector "ofs" ptr |> Option.get )
    in
    let mk_ptr = Adt.constructor "mk-ptr" ptr |> Option.get in
    (mk_ptr, loc, ofs, ptr_ty)

  let encode_ty (ty : Svalue.ty) =
    match ty with
    | TBool -> Types.bool
    | TInt -> Types.int
    | TLoc -> Types.int
    | TFloat fp -> (
        match fp with
        | F32 -> Types.float 8 24
        | F64 -> Types.float 11 53
        | F16 | F128 -> assert false)
    | TSeq _ -> assert false
    | TPointer -> ptr_ty
    | TBitVector m -> Types.bitv m

  let encode_unop (op : Svalue.Unop.t) term =
    match op with
    | Not -> not_ term
    | FAbs -> Float.abs term
    | GetPtrLoc -> Func.apply loc [ term ]
    | GetPtrOfs -> Func.apply ofs [ term ]
    | IntOfBool -> ite term (int 1) (int 0)
    | BvOfFloat _ -> assert false
    | BvOfInt (_, m) -> Int.to_bv m term
    | FloatOfBv (_, _, _) -> assert false
    | IntOfBv signed -> Bitv.to_int ~signed term
    | BvExtract (high, low) -> Bitv.extract term ~high ~low
    | BvExtend (true, n) -> Bitv.sign_extend n term
    | BvExtend (false, n) -> Bitv.zero_extend n term
    | BvNot -> Bitv.lognot term
    | BvNegOvf -> Bitv.nego term
    | FIs Normal -> Float.is_normal term
    | FIs Subnormal -> Float.is_subnormal term
    | FIs Zero -> Float.is_zero term
    | FIs Infinite -> Float.is_infinite term
    | FIs NaN -> Float.is_nan term
    | FRound _ -> assert false

  let encode_binop (op : Svalue.Binop.t) : term -> term -> term =
    match op with
    | And -> and_
    | Or -> or_
    | Eq -> eq
    | Leq -> Int.le
    | Lt -> Int.lt
    | Plus -> Int.add
    | Minus -> Int.sub
    | Times -> Int.mul
    | Div -> Int.div
    | Rem -> Int.rem
    | Mod -> Int.mod_
    | FEq -> Float.eq
    | FLeq -> Float.le
    | FLt -> Float.lt
    | FPlus -> Float.add ~rm:Float.Rounding_mode.rna
    | FMinus -> Float.sub ~rm:Float.Rounding_mode.rna
    | FTimes -> Float.mul ~rm:Float.Rounding_mode.rna
    | FDiv -> Float.div ~rm:Float.Rounding_mode.rna
    | FRem -> Float.rem
    | BvPlus -> Bitv.add
    | BvMinus -> Bitv.sub
    | BvTimes -> Bitv.mul
    | BvDiv true -> Bitv.div
    | BvDiv false -> Bitv.div_u
    | BvRem true -> Bitv.rem
    | BvRem false -> Bitv.rem_u
    | BvMod -> Bitv.smod
    | BvPlusOvf signed -> Bitv.addo ~signed
    | BvTimesOvf signed -> Bitv.mulo ~signed
    | BvLt true -> Bitv.lt
    | BvLt false -> Bitv.lt_u
    | BvLeq true -> Bitv.le
    | BvLeq false -> Bitv.le_u
    | BvConcat -> Bitv.concat
    | BitAnd -> Bitv.logand
    | BitOr -> Bitv.logor
    | BitXor -> Bitv.logxor
    | BitShl -> Bitv.shl
    | BitLShr -> Bitv.lshr
    | BitAShr -> Bitv.ashr

  let encode_nop _op _v_list = assert false

  let rec encode_value (v : Svalue.t) : term =
    match Svalue.kind v with
    | Var var ->
        Mappings.const (Svalue.Var.to_string var) (encode_ty (Svalue.ty v))
    | Bool b -> if b then Mappings.true_ else Mappings.false_
    | Int z ->
        (* FIXME: It probably makes sense for smtml to also use zarith *)
        let i = Z.to_int z in
        Mappings.int i
    | Float _ -> assert false
    | Ptr (loc, ofs) -> Func.apply mk_ptr [ encode_value loc; encode_value ofs ]
    | BitVec z ->
        let bv_len =
          match Svalue.ty v with
          | Svalue.TBitVector _m -> assert false
          | _ -> Fmt.failwith "cannot infer the lenght of the bitvector value"
        in
        (* Create a smtml bitvector to print it correctly to the mappings *)
        let bv = Smtml.Bitvector.make z bv_len in
        Bitv.v (Smtml.Bitvector.to_string bv) bv_len
    | Seq _ -> assert false
    | Unop (op, v) -> encode_unop op (encode_value v)
    | Binop (op, v1, v2) -> encode_binop op (encode_value v1) (encode_value v2)
    | Nop (op, v_list) -> encode_nop op (List.map encode_value v_list)
    | Ite (c, v1, v2) ->
        Mappings.ite (encode_value c) (encode_value v1) (encode_value v2)

  let init () = Solver.make ()
  let add_constraint solver value = Solver.add solver [ encode_value value ]

  let check_sat solver =
    match Solver.check solver ~assumptions:[] with
    | `Sat -> Symex.Solver_result.Sat
    | `Unsat -> Unsat
    | `Unknown -> Unknown

  let declare_var _solver _var _ty =
    (* No-op: don't need to do anything *)
    ()

  let push solver n =
    for _ = 0 to n - 1 do
      Solver.push solver
    done

  let pop solver n = Solver.pop solver n
  let reset solver = Solver.reset solver
end

module Make' (M : Smtml.Mappings_intf.M) : Solvers.Solver_interface.S = Make (M)
