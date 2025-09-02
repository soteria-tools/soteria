open Charon
open Charon_util
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Rust_val

module M (State : State_intf.S) = struct
  module Sptr = State.Sptr
  module Core = Core.M (State)
  module Encoder = Encoder.Make (Sptr)

  type nonrec rust_val = Sptr.t rust_val

  type ret =
    (rust_val * State.t, Error.t State.err * State.t, State.serialized) Result.t

  (* some utils *)
  let[@inline] as_ptr ?(null_ok = false) (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Base v when null_ok ->
        let v = Typed.cast_i Usize v in
        let ptr = Sptr.null_ptr_of v in
        (ptr, None)
    | _ -> failwith "expected pointer"

  let[@inline] as_base (ty : Types.literal_type) (v : rust_val) :
      [> T.sint ] Typed.t =
    match v with
    | Base v -> Typed.cast_lit ty v
    | _ -> failwith "expected base value"

  let[@inline] as_base_i ty = as_base (TUInt ty)

  let[@inline] as_float ty (v : rust_val) =
    match v with
    | Base v -> Typed.cast_f ty v
    | _ -> failwith "expected base value"

  (* the intrinsics  *)

  let abort st = State.error (`Panic (Some "aborted")) st

  let checked_op op ~t ~x ~y state : ret =
    let t = TypesUtils.ty_as_literal t in
    let x, y = (as_base t x, as_base t y) in
    let++ res = State.lift_err state @@ Core.eval_checked_lit_binop op t x y in
    (res, state)

  let add_with_overflow = checked_op (Add OUB)
  let sub_with_overflow = checked_op (Sub OUB)
  let mul_with_overflow = checked_op (Mul OUB)

  let align_of ~t state =
    let* align = Layout.align_of_s t in
    Result.ok (Base align, state)

  (* FIXME: how does the value influence the alignment? *)
  let align_of_val ~t ~ptr:_ state =
    let* align = Layout.align_of_s t in
    Result.ok (Base align, state)

  let ptr_add ~check ~t ~dst ~offset state : ret =
    let v = as_base_i Usize offset in
    let** ptr, meta =
      match dst with
      | Ptr ptr -> Result.ok ptr
      | Base base when not check ->
          let base = Typed.cast_i Usize base in
          let ptr = Sptr.null_ptr_of base in
          Result.ok (ptr, None)
      | Base _ -> State.error `UBPointerArithmetic state
      | _ -> not_impl "ptr_add: invalid arguments"
    in
    if%sat v ==@ Typed.BitVec.usizei 0 then Result.ok (Ptr (ptr, meta), state)
    else
      let++ ptr' = Sptr.offset ~check ~ty:t ptr v |> State.lift_err state in
      (Ptr (ptr', meta), state)

  let arith_offset = ptr_add ~check:false
  let offset ~ptr ~delta:_ = ptr_add ~check:true ~t:ptr

  let assert_inhabited ~t state =
    if Layout.is_inhabited t then Result.ok (Tuple [], state)
    else State.error (`Panic (Some "core::intrinsics::assert_inhabited")) state

  let assert_mem_uninitialized_valid ~t:_ state = Result.ok (Tuple [], state)

  let assert_zero_valid ~t state =
    match Layout.zeroed ~null_ptr:Sptr.null_ptr t with
    | None ->
        State.error (`Panic (Some "core::intrinsics::assert_zero_valid")) state
    | _ -> Result.ok (Tuple [], state)

  let assume ~b state =
    let cond = as_base TBool b in
    if%sat Typed.BitVec.to_bool cond then Result.ok (Tuple [], state)
    else State.error (`StdErr "core::intrinsics::assume with false") state

  let black_box ~t:_ ~dummy state = Result.ok (dummy, state)
  let breakpoint : State.t -> ret = State.error `Breakpoint

  let bswap ~t ~x state =
    let lit = TypesUtils.ty_as_literal t in
    let nbytes = Layout.size_of_literal_ty lit in
    let v = as_base lit x in
    let bytes =
      List.init nbytes (fun i ->
          Typed.BitVec.extract (i * 8) (((i + 1) * 8) - 1) v)
    in
    let rec aux = function
      | [] -> failwith "impossible: no bytes"
      | [ last ] -> last
      | hd :: tl -> Typed.BitVec.concat hd (aux tl)
    in
    let v' = aux bytes in
    Result.ok (Base (v' :> Typed.T.cval Typed.t), state)

  let catch_unwind exec_fun ~_try_fn:try_fn_ptr ~_data:data
      ~_catch_fn:catch_fn_ptr state =
    let[@inline] get_fn ptr state =
      let++ fn_ptr, state = State.lookup_fn ptr state in
      match fn_ptr.func with
      | FunId (FRegular fid) -> (Crate.get_fun fid, state)
      | TraitMethod (_, _, fid) -> (Crate.get_fun fid, state)
      | FunId (FBuiltin _) -> failwith "Can't have function pointer to builtin"
    in
    let try_fn_ptr, catch_fn_ptr = (as_ptr try_fn_ptr, as_ptr catch_fn_ptr) in
    let** try_fn, state = get_fn try_fn_ptr state in
    let** catch_fn, state = get_fn catch_fn_ptr state in
    let try_fn_ret = exec_fun try_fn ~args:[ data ] state in
    State.unwind_with try_fn_ret
      ~f:(fun (_, state) -> Result.ok (Base (Typed.BitVec.u32i 0), state))
      ~fe:(fun (_, state) ->
        let args = [ data; Ptr (Sptr.null_ptr (), None) ] in
        let catch_fn_ret = exec_fun catch_fn ~args state in
        State.unwind_with catch_fn_ret
          ~f:(fun (_, state) -> Result.ok (Base (Typed.BitVec.one 32), state))
          ~fe:(fun (_, state) ->
            State.error (`StdErr "catch_unwind unwinded in catch") state))

  let float_rounding rm fty ~x state : ret =
    let x = as_float fty x in
    let res = Typed.Float.round rm x in
    Result.ok (Base res, state)

  let ceilf16 = float_rounding Ceil F16
  let ceilf32 = float_rounding Ceil F32
  let ceilf64 = float_rounding Ceil F64
  let ceilf128 = float_rounding Ceil F128
  let floorf16 = float_rounding Floor F16
  let floorf32 = float_rounding Floor F32
  let floorf64 = float_rounding Floor F64
  let floorf128 = float_rounding Floor F128
  let round_ties_even_f16 = float_rounding NearestTiesToEven F16
  let round_ties_even_f32 = float_rounding NearestTiesToEven F32
  let round_ties_even_f64 = float_rounding NearestTiesToEven F64
  let round_ties_even_f128 = float_rounding NearestTiesToEven F128
  let roundf16 = float_rounding NearestTiesToAway F16
  let roundf32 = float_rounding NearestTiesToAway F32
  let roundf64 = float_rounding NearestTiesToAway F64
  let roundf128 = float_rounding NearestTiesToAway F128
  let truncf16 = float_rounding Truncate F16
  let truncf32 = float_rounding Truncate F32
  let truncf64 = float_rounding Truncate F64
  let truncf128 = float_rounding Truncate F128
  let cold_path state = Result.ok (Tuple [], state)

  let compare_bytes ~left ~right ~bytes state =
    let zero = Typed.BitVec.usizei 0 in
    let one = Typed.BitVec.usizei 1 in
    let l, _ = as_ptr left in
    let r, _ = as_ptr right in
    let len = as_base_i Usize bytes in
    let byte = Types.TLiteral (TUInt U8) in
    let rec aux ?(inc = one) l r len state =
      if%sat len ==@ zero then Result.ok (Base (Typed.BitVec.u32i 0), state)
      else
        let** l = Sptr.offset l inc |> State.lift_err state in
        let** r = Sptr.offset r inc |> State.lift_err state in
        let** bl, state = State.load (l, None) byte state in
        let bl = as_base_i U8 bl in
        let** br, state = State.load (r, None) byte state in
        let br = as_base_i U8 br in
        if%sat bl ==@ br then aux l r (len -@ one) state
        else
          if%sat bl <@ br then Result.ok (Base (Typed.BitVec.u32i (-1)), state)
          else Result.ok (Base (Typed.BitVec.one 32), state)
    in
    aux ~inc:zero l r len state

  let copy_ nonoverlapping ~t ~src ~dst ~count state : ret =
    let zero = Typed.BitVec.usizei 0 in
    let (src, _), (dst, _) = (as_ptr src, as_ptr dst) in
    let count = as_base_i Usize count in
    let** (), state = State.check_ptr_align src t state in
    let** (), state = State.check_ptr_align dst t state in
    let* ty_size = Layout.size_of_s t in
    if%sat ty_size ==@ zero then Result.ok (Tuple [], state)
    else
      let size = ty_size *@ count in
      let** () =
        if%sat Sptr.is_at_null_loc src ||@ Sptr.is_at_null_loc dst then
          State.error `NullDereference state
          (* Here we can cheat a little: for copy_nonoverlapping we need to check for overlap,
               but otherwise the copy is the exact same; since the State makes a copy of the src tree
               before storing into dst, the semantics are that of copy. *)
        else if nonoverlapping then
          (* check for overlap *)
          let** src_end = Sptr.offset src size |> State.lift_err state in
          let** dst_end = Sptr.offset dst size |> State.lift_err state in
          let* dist1 = Sptr.distance src dst_end in
          let* dist2 = Sptr.distance dst src_end in
          if%sat
            Sptr.is_same_loc src dst &&@ (dist1 <$@ zero &&@ (dist2 <$@ zero))
          then State.error (`StdErr "copy_nonoverlapping overlapped") state
          else Result.ok ()
        else Result.ok ()
      in
      let++ (), state =
        State.copy_nonoverlapping ~dst:(dst, None) ~src:(src, None) ~size state
      in
      (Tuple [], state)

  let copy = copy_ false
  let copy_nonoverlapping = copy_ true

  let copy_sign fty ~x ~y state : ret =
    let x, y = (as_float fty x, as_float fty y) in
    let zero = Typed.Float.like y 0.0 in
    if%sat [@lname "copy_sign < 0"] [@rname "copy_sign >=0"] y <.@ zero then
      let x' = Typed.Float.neg (Typed.Float.abs x) in
      Result.ok (Base x', state)
    else
      let x' = Typed.Float.abs x in
      Result.ok (Base x', state)

  let copysignf128 = copy_sign F128
  let copysignf64 = copy_sign F64
  let copysignf32 = copy_sign F32
  let copysignf16 = copy_sign F16

  (** Applies either [concrete] or [symbolic] to a bitvector. *)
  let binary_int_operation ~concrete ~symbolic ~t ~x state : ret =
    let t = TypesUtils.ty_as_literal t in
    let bits = 8 * Layout.size_of_literal_ty t in
    let x = as_base t x in
    let res =
      match Typed.kind x with
      | BitVec x -> concrete bits x
      | _ -> symbolic bits x
    in
    Result.ok (Base (res :> T.cval Typed.t), state)

  let ctpop =
    let concrete _bits x = Typed.BitVec.u32i @@ Z.popcount x in
    let symbolic bits x =
      Iter.fold
        (fun acc off ->
          let bit = Typed.BitVec.extract off off x in
          let bit32 = Typed.BitVec.extend ~signed:false 31 bit in
          acc +@ bit32)
        (Typed.BitVec.u32i 0)
        Iter.(0 -- bits)
    in
    binary_int_operation ~concrete ~symbolic

  let cttz =
    let concrete bits x =
      Typed.BitVec.u32i @@ if Z.equal x Z.zero then bits else Z.trailing_zeros x
    in
    (* we construct the following, from inside out:
      ite(x[0] == 1 ? 0 :
        ite(x[1] == 1 ? 1 :
          ...
          ite(x[bits-1] == 1 ? bits-1 : bits))) *)
    let symbolic bits x =
      Iter.fold
        (fun acc off ->
          let off = bits - 1 - off in
          let bit = Typed.BitVec.extract off off x in
          Typed.ite (bit ==@ Typed.BitVec.one 1) (Typed.BitVec.u32i off) acc)
        (Typed.BitVec.u32i bits)
        Iter.(0 -- (bits - 1))
    in
    binary_int_operation ~concrete ~symbolic

  let cttz_nonzero ~t ~x state =
    let tlit = TypesUtils.ty_as_literal t in
    let x_int = as_base tlit x in
    if%sat x_int ==@ Typed.BitVec.mki_lit tlit 0 then
      State.error (`StdErr "core::intrinsics::cttz_nonzero on zero") state
    else cttz ~t ~x state

  let discriminant_value ~t ~v state =
    let ptr = as_ptr v in
    let++ value, state = State.load ptr t state in
    match value with
    | Enum (discr, _) -> (Base discr, state)
    | _ -> (Base (Typed.BitVec.u32i 0), state)

  let exact_div ~t ~x ~y state =
    let lit = TypesUtils.ty_as_literal t in
    let x, y = (as_base lit x, as_base lit y) in
    let x, y, ty = Typed.cast_checked2 x y in
    let** res = State.lift_err state @@ Core.eval_lit_binop (Div OUB) lit x y in
    if Typed.is_float ty then Result.ok (Base res, state)
    else
      let zero = Typed.BitVec.mki_lit lit 0 in
      if%sat Typed.not (y ==@ zero) &&@ (x %@ Typed.cast y ==@ zero) then
        Result.ok (Base res, state)
      else
        State.error (`StdErr "core::intrinsics::exact_div on non divisible")
          state

  let abs fty ~x state : ret =
    let x = as_float fty x in
    Result.ok (Base (Typed.Float.abs x), state)

  let fabsf16 = abs F16
  let fabsf32 = abs F32
  let fabsf64 = abs F64
  let fabsf128 = abs F128

  let float_fast (bop : Expressions.binop) ~(t : Types.ty) ~a ~b state : ret =
    let t = ty_as_float t in
    let l, r = (as_float t a, as_float t b) in
    let bop, name =
      match bop with
      | Add _ -> (( +.@ ), "core::intrinsics::fadd_fast")
      | Sub _ -> (( -.@ ), "core::intrinsics::fsub_fast")
      | Mul _ -> (( *.@ ), "core::intrinsics::fmul_fast")
      | Div _ -> (( /.@ ), "core::intrinsics::fdiv_fast")
      | Rem _ -> (Typed.Float.rem, "core::intrinsics::frem_fast")
      | _ -> failwith "fast_float: invalid binop"
    in
    let is_finite f =
      Typed.((not (Float.is_nan f)) &&@ not (Float.is_infinite f))
    in
    let res = bop l r in
    if%sat is_finite l &&@ is_finite r &&@ is_finite (bop l r) then
      Result.ok (Base res, state)
    else
      State.error
        (`StdErr (name ^ ": operands and result must be finite"))
        state

  let fadd_fast = float_fast (Add OUB)
  let fdiv_fast = float_fast (Div OUB)
  let fmul_fast = float_fast (Mul OUB)
  let frem_fast = float_fast (Rem OUB)
  let fsub_fast = float_fast (Sub OUB)

  let float_to_int_unchecked ~float ~int ~value state =
    let fty = ty_as_float float in
    let ity = TypesUtils.ty_as_literal int in
    let f = as_float fty value in
    if%sat Typed.Float.is_nan f ||@ Typed.Float.is_infinite f then
      State.error (`StdErr "float_to_int_unchecked with NaN or infinite value")
        state
    else
      let signed = Layout.is_signed ity in
      let max = Z.succ @@ Layout.max_value_z ity in
      let min = Z.pred @@ Layout.min_value_z ity in
      let max = Typed.Float.mk fty @@ Float.to_string @@ Z.to_float max in
      let min = Typed.Float.mk fty @@ Float.to_string @@ Z.to_float min in
      (* we use min-1 and max+1, to be able to have a strict inequality, which avoids
             issues in cases of float precision loss (I think?) *)
      if%sat min <.@ f &&@ (f <.@ max) then
        Result.ok (Base (Typed.BitVec.of_float ~signed f), state)
      else State.error (`StdErr "float_to_int_unchecked out of int range") state

  let fmul_add fty ~a ~b ~c state : ret =
    let a = as_float fty a in
    let b = as_float fty b in
    let c = as_float fty c in
    Result.ok (Base ((a *.@ b) +.@ c), state)

  let fmaf16 = fmul_add F16
  let fmaf32 = fmul_add F32
  let fmaf64 = fmul_add F64
  let fmaf128 = fmul_add F128
  let fmuladdf16 = fmul_add F16
  let fmuladdf32 = fmul_add F32
  let fmuladdf64 = fmul_add F64
  let fmuladdf128 = fmul_add F128
  let forget ~t:_ ~arg:_ state = Result.ok (Tuple [], state)

  let is_val_statically_known ~t:_ ~_arg:_ state =
    (* see: https://doc.rust-lang.org/std/intrinsics/fn.is_val_statically_known.html *)
    let* b = Rustsymex.nondet Typed.t_bool in
    Result.ok (Base (Typed.BitVec.of_bool b), state)

  let likely ~b state = Result.ok (b, state)
  let unlikely ~b state = Result.ok (b, state)

  let float_minmax fty ~is_min ~x ~y state : ret =
    let x, y = (as_float fty x, as_float fty y) in
    let++ res =
      if%sat Typed.Float.is_nan x then Result.ok y
      else
        if%sat Typed.Float.is_nan y then Result.ok x
        else
          let op = if is_min then ( <.@ ) else ( >.@ ) in
          Result.ok (Typed.ite (op x y) x y)
    in
    (Base (res :> Typed.T.cval Typed.t), state)

  let minnumf16 = float_minmax F16 ~is_min:true
  let minnumf32 = float_minmax F32 ~is_min:true
  let minnumf64 = float_minmax F64 ~is_min:true
  let minnumf128 = float_minmax F128 ~is_min:true
  let maxnumf16 = float_minmax F16 ~is_min:false
  let maxnumf32 = float_minmax F32 ~is_min:false
  let maxnumf64 = float_minmax F64 ~is_min:false
  let maxnumf128 = float_minmax F128 ~is_min:false

  let ptr_guaranteed_cmp ~t:_ ~ptr ~other state =
    let++ res = State.lift_err state @@ Core.eval_ptr_binop Eq ptr other in
    (Base res, state)

  let ptr_offset_from_ ~unsigned ~t ~ptr ~base state : ret =
    let (ptr, _), (base, _) =
      (as_ptr ~null_ok:true ptr, as_ptr ~null_ok:true base)
    in
    let zero = Typed.BitVec.usizei 0 in
    let* size = Layout.size_of_s t in
    if%sat size ==@ zero then
      State.error (`Panic (Some "ptr_offset_from with ZST")) state
    else
      let size = Typed.cast size in
      let* off = Sptr.distance ptr base in
      (* If the pointers are not equal, they mustn't be dangling *)
      if%sat off ==@ zero ||@ (Sptr.constraints ptr &&@ Sptr.constraints base)
      then
        (* UB conditions:
           1. must be at the same address, OR derived from the same allocation
           2. the distance must be a multiple of sizeof(T) *)
        if%sat
          off ==@ zero ||@ Sptr.is_same_loc ptr base &&@ (off %$@ size ==@ zero)
        then
          if not unsigned then Result.ok (Base (off /$@ size), state)
          else
            if%sat off >=$@ zero then Result.ok (Base (off /$@ size), state)
            else
              State.error
                (`StdErr
                   "core::intrinsics::offset_from_unsigned negative offset")
                state
        else State.error `UBPointerComparison state
      else State.error `UBDanglingPointer state

  let ptr_offset_from = ptr_offset_from_ ~unsigned:false
  let ptr_offset_from_unsigned = ptr_offset_from_ ~unsigned:true

  let raw_eq ~t ~a ~b state =
    let l, r = (as_ptr a, as_ptr b) in
    let layout = Layout.layout_of t in
    let bytes = mk_array_ty (TLiteral (TUInt U8)) (Z.of_int layout.size) in
    (* this is hacky, but we do not keep the state post-load, as it
       will have its tree blocks split up per byte, which is suboptimal *)
    let** l, _ = State.load l bytes state in
    let** r, _ = State.load r bytes state in
    let byte_pairs =
      match (l, r) with
      | Array l, Array r -> List.combine l r
      | _ -> failwith "Unexpected read array"
    in
    let rec aux = function
      | [] -> Result.ok Typed.v_true
      | (Base l, Base r) :: rest ->
          if%sat l ==@ r then aux rest else Result.ok Typed.v_false
      | _ :: _ -> failwith "Unexpected read array"
    in
    let++ res = aux byte_pairs in
    (Base (Typed.BitVec.of_bool res), state)

  let saturating (op : Expressions.binop) ~t ~a ~b state : ret =
    let t = TypesUtils.ty_as_literal t in
    let signed = Layout.is_signed t in
    let a, b = (as_base t a, as_base t b) in
    let max = Typed.BitVec.mk_lit t @@ Layout.max_value_z t in
    let min = Typed.BitVec.mk_lit t @@ Layout.min_value_z t in
    let res =
      match op with
      | Add _ ->
          let ovf = Typed.BitVec.add_overflows ~signed a b in
          let if_ovf =
            if not signed then max
            else Typed.ite (Typed.BitVec.mki_lit t 0 <$@ a) max min
          in
          Typed.ite ovf if_ovf (a +@ b)
      | Sub _ ->
          let ovf = Typed.BitVec.sub_overflows ~signed a b in
          let if_ovf =
            if not signed then min
            else Typed.ite (Typed.BitVec.mki_lit t 0 <$@ a) max min
          in
          Typed.ite ovf if_ovf (a -@ b)
      | _ -> failwith "Unreachable: not add or sub?"
    in
    Result.ok (Base (res :> Typed.T.cval Typed.t), state)

  let saturating_add = saturating (Add OUB)
  let saturating_sub = saturating (Sub OUB)

  let size_of ~t state =
    let* size = Layout.size_of_s t in
    Result.ok (Base size, state)

  let size_of_val ~t ~ptr state =
    match (t, ptr) with
    | ( Types.TAdt { id = TBuiltin ((TSlice | TStr) as id); generics },
        Ptr (_, Some meta) ) ->
        let sub_ty =
          if id = TSlice then List.hd generics.types else TLiteral (TUInt U8)
        in
        let len = Typed.cast_i Usize meta in
        let* size = Layout.size_of_s sub_ty in
        let size = size *@ len in
        Result.ok (Base size, state)
    | _ ->
        let* size = Layout.size_of_s t in
        Result.ok (Base size, state)

  let transmute ~t_src ~dst ~src state =
    let++ v =
      State.lift_err state
      @@ Encoder.transmute ~verify_ptr:(State.is_valid_ptr state) ~from_ty:t_src
           ~to_ty:dst src
    in
    (v, state)

  let type_id ~t state =
    (* lazy but works *)
    let hash = Hashtbl.hash t in
    Result.ok (Base (Typed.BitVec.u128i hash), state)

  let type_name ~t state =
    let str = Fmt.to_to_string pp_ty t in
    let** ptr_res, state = State.load_str_global str state in
    match ptr_res with
    | Some ptr -> Result.ok (Ptr ptr, state)
    | None ->
        let len = String.length str in
        let chars =
          String.to_bytes str
          |> Bytes.fold_left
               (fun l c -> Base (Typed.BitVec.u8i (Char.code c)) :: l)
               []
          |> List.rev
        in
        let char_arr = Array chars in
        let str_ty : Types.ty =
          mk_array_ty (TLiteral (TUInt U8)) (Z.of_int len)
        in
        let** (ptr, _), state = State.alloc_ty str_ty state in
        let ptr = (ptr, Some (Typed.BitVec.usizei len)) in
        let** (), state = State.store ptr str_ty char_arr state in
        let++ (), state = State.store_str_global str ptr state in
        (Ptr ptr, state)

  let typed_swap_nonoverlapping ~t ~x ~y state =
    let ((from_ptr, _) as from), ((to_ptr, _) as to_) = (as_ptr x, as_ptr y) in
    let** (), state = State.check_ptr_align from_ptr t state in
    let** (), state = State.check_ptr_align to_ptr t state in
    let* size = Layout.size_of_s t in
    let** () =
      if%sat Sptr.is_at_null_loc from_ptr ||@ Sptr.is_at_null_loc to_ptr then
        State.error `NullDereference state
      else
        (* check for overlap *)
        let** from_ptr_end =
          Sptr.offset from_ptr size |> State.lift_err state
        in
        let** to_ptr_end = Sptr.offset to_ptr size |> State.lift_err state in
        let* dist1 = Sptr.distance from_ptr to_ptr_end in
        let* dist2 = Sptr.distance to_ptr from_ptr_end in
        let zero = Typed.BitVec.usizei 0 in
        if%sat
          Sptr.is_same_loc from_ptr to_ptr
          &&@ (dist1 <$@ zero &&@ (dist2 <$@ zero))
        then State.error (`StdErr "typed_swap_nonoverlapping overlapped") state
        else Result.ok ()
    in
    let** v_l, state = State.load from t state in
    let** v_r, state = State.load to_ t state in
    let** (), state = State.store from t v_r state in
    let++ (), state = State.store to_ t v_l state in
    (Tuple [], state)

  let unchecked_op op ~t ~x ~y state : ret =
    let t = TypesUtils.ty_as_literal t in
    let x, y = (as_base t x, as_base t y) in
    let++ res = State.lift_err state @@ Core.eval_lit_binop op t x y in
    (Base res, state)

  let unchecked_add = unchecked_op (Add OUB)
  let unchecked_div = unchecked_op (Div OUB)
  let unchecked_mul = unchecked_op (Mul OUB)
  let unchecked_rem = unchecked_op (Rem OUB)
  let unchecked_shl ~t ~u:_ = unchecked_op (Shl OUB) ~t
  let unchecked_shr ~t ~u:_ = unchecked_op (Shr OUB) ~t
  let unchecked_sub = unchecked_op (Sub OUB)

  let variant_count ~t state =
    match t with
    | Types.TAdt { id = TAdtId id; _ } when Crate.is_enum id ->
        let variants = Crate.as_enum id in
        let res = Typed.BitVec.usizei (List.length variants) in
        Result.ok (Base res, state)
    | _ ->
        State.error
          (`StdErr "core::intrinsics::variant_count used with non-enum") state

  let wrapping_op op ~t ~a ~b state : ret =
    let ity = TypesUtils.ty_as_literal t in
    let a, b = (as_base ity a, as_base ity b) in
    let res = Core.wrapping_binop op ity a b in
    Result.ok (Base res, state)

  let wrapping_add = wrapping_op (Add OUB)
  let wrapping_mul = wrapping_op (Mul OUB)
  let wrapping_sub = wrapping_op (Sub OUB)

  let write_bytes ~t ~dst ~val_ ~count state =
    let ((ptr, _) as dst) = as_ptr dst in
    let v, count = (as_base_i U8 val_, as_base_i Usize count) in
    let zero = Typed.BitVec.usizei 0 in
    let** (), state = State.check_ptr_align ptr t state in
    let* size = Layout.size_of_s t in
    let size = size *@ count in
    if%sat size ==@ zero then Result.ok (Tuple [], state)
    else
      (* if v == 0, then we can replace this mess by initialising a Zeros subtree *)
      if%sat v ==@ Typed.BitVec.u8i 0 then
        let++ (), state = State.zeros dst size state in
        (Tuple [], state)
      else
        match Typed.kind size with
        | BitVec bytes ->
            let++ (), state =
              Result.fold_iter ~init:((), state)
                ~f:(fun ((), state) i ->
                  let off = Typed.BitVec.usizei i in
                  let** ptr = Sptr.offset ptr off |> State.lift_err state in
                  State.store (ptr, None) (TLiteral (TUInt U8))
                    (Base (v :> T.cval Typed.t))
                    state)
                Iter.(0 -- (Z.to_int bytes - 1))
            in
            (Tuple [], state)
        | _ -> failwith "write_bytes: don't know how to handle symbolic sizes"
end
