open Charon
open Charon_util
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax

module M (State : State_intf.S) = struct
  module Sptr = State.Sptr
  module Core = Core.M (State)
  module Encoder = Encoder.Make (Sptr)

  type nonrec rust_val = Sptr.t rust_val

  type ret =
    ( rust_val * State.t,
      Error.t State.err * State.t,
      State.serialized list )
    Result.t

  (* some utils *)
  let[@inline] as_ptr (v : rust_val) =
    match v with Ptr ptr -> return ptr | _ -> not_impl "expected pointer"

  let[@inline] as_base ?(ty : 'ty Typed.ty option) (v : rust_val) :
      'ty Typed.t Rustsymex.t =
    match v with
    | Base v -> (
        match ty with
        | Some ty -> cast_checked ~ty v
        | None -> return (Typed.cast v))
    | _ -> not_impl "expected base value"

  let[@inline] as_float (v : rust_val) =
    match v with
    | Base v -> (
        match Typed.cast_float v with
        | Some v -> return v
        | None -> not_impl "expected float value")
    | _ -> not_impl "expected base value"

  let[@inline] ( &&* ) x y =
    let* x in
    let+ y in
    (x, y)

  (* the intrinsics  *)

  let abort st = State.error (`Panic (Some "aborted")) st

  let checked_op op ~t ~x ~y state : ret =
    let t = TypesUtils.ty_as_literal t in
    let* x, y = as_base x &&* as_base y in
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
    let* v = as_base ~ty:Typed.t_int offset in
    let** ptr, meta =
      match dst with
      | Ptr ptr -> Result.ok ptr
      | Base base when not check ->
          let* base = cast_checked base ~ty:Typed.t_int in
          let ptr = Sptr.null_ptr_of base in
          Result.ok (ptr, None)
      | Base _ -> State.error `UBPointerArithmetic state
      | _ -> not_impl "ptr_add: invalid arguments"
    in
    if%sat v ==@ 0s then Result.ok (Ptr (ptr, meta), state)
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
    let* cond = as_base ~ty:Typed.t_int b in
    if%sat Typed.bool_of_int cond then Result.ok (Tuple [], state)
    else State.error (`StdErr "core::intrinsics::assume with false") state

  let black_box ~t:_ ~dummy state = Result.ok (dummy, state)
  let breakpoint : State.t -> ret = State.error `Breakpoint

  let bswap ~t ~x state =
    let lit = TypesUtils.ty_as_literal t in
    let unsigned = Layout.lit_to_unsigned lit in
    let nbytes = Layout.size_of_literal_ty lit in
    let** v =
      State.lift_err state @@ Encoder.transmute ~from_ty:t ~to_ty:unsigned x
    in
    let* v = as_base ~ty:Typed.t_int v in
    let bytes =
      List.init nbytes (fun i ->
          v /@ Typed.nonzero_z Z.(pow (of_int 256) i) %@ Typed.nonzero 256)
    in
    let bytes = List.rev bytes in
    let _, v' =
      List.fold_left
        (fun (i, acc) byte ->
          (i + 1, acc +@ (byte *@ Typed.int_z Z.(pow (of_int 256) i))))
        (0, 0s) bytes
    in
    let v' = Base (v' :> Typed.T.cval Typed.t) in
    let++ v' =
      State.lift_err state @@ Encoder.transmute ~from_ty:unsigned ~to_ty:t v'
    in
    (v', state)

  let catch_unwind exec_fun ~_try_fn:try_fn_ptr ~_data:data
      ~_catch_fn:catch_fn_ptr state =
    let[@inline] get_fn ptr state =
      let++ fn_ptr, state = State.lookup_fn ptr state in
      match fn_ptr.func with
      | FunId (FRegular fid) -> (Crate.get_fun fid, state)
      | TraitMethod (_, _, fid) -> (Crate.get_fun fid, state)
      | FunId (FBuiltin _) -> failwith "Can't have function pointer to builtin"
    in
    let* try_fn_ptr, catch_fn_ptr = as_ptr try_fn_ptr &&* as_ptr catch_fn_ptr in
    let** try_fn, state = get_fn try_fn_ptr state in
    let** catch_fn, state = get_fn catch_fn_ptr state in
    let try_fn_ret = exec_fun try_fn ~args:[ data ] state in
    State.unwind_with try_fn_ret
      ~f:(fun (_, state) -> Result.ok (Base 0s, state))
      ~fe:(fun (_, state) ->
        let args = [ data; Ptr (Sptr.null_ptr, None) ] in
        let catch_fn_ret = exec_fun catch_fn ~args state in
        State.unwind_with catch_fn_ret
          ~f:(fun (_, state) -> Result.ok (Base 1s, state))
          ~fe:(fun (_, state) ->
            State.error (`StdErr "catch_unwind unwinded in catch") state))

  let float_rounding rm ~x state : ret =
    let* x = as_float x in
    let res = Typed.float_round rm x in
    Result.ok (Base res, state)

  let ceilf16 = float_rounding Ceil
  let ceilf32 = float_rounding Ceil
  let ceilf64 = float_rounding Ceil
  let ceilf128 = float_rounding Ceil
  let floorf16 = float_rounding Floor
  let floorf32 = float_rounding Floor
  let floorf64 = float_rounding Floor
  let floorf128 = float_rounding Floor
  let round_ties_even_f16 = float_rounding NearestTiesToEven
  let round_ties_even_f32 = float_rounding NearestTiesToEven
  let round_ties_even_f64 = float_rounding NearestTiesToEven
  let round_ties_even_f128 = float_rounding NearestTiesToEven
  let roundf16 = float_rounding NearestTiesToAway
  let roundf32 = float_rounding NearestTiesToAway
  let roundf64 = float_rounding NearestTiesToAway
  let roundf128 = float_rounding NearestTiesToAway
  let truncf16 = float_rounding Truncate
  let truncf32 = float_rounding Truncate
  let truncf64 = float_rounding Truncate
  let truncf128 = float_rounding Truncate
  let cold_path state = Result.ok (Tuple [], state)

  let compare_bytes ~left ~right ~bytes state =
    let* l, _ = as_ptr left in
    let* r, _ = as_ptr right in
    let* len = as_base ~ty:Typed.t_int bytes in
    let byte = Types.TLiteral (TUInt U8) in
    let rec aux ?(inc = 1s) l r len state =
      if%sat len ==@ 0s then Result.ok (Base 0s, state)
      else
        let** l = Sptr.offset l inc |> State.lift_err state in
        let** r = Sptr.offset r inc |> State.lift_err state in
        let** bl, state = State.load (l, None) byte state in
        let* bl = as_base ~ty:Typed.t_int bl in
        let** br, state = State.load (r, None) byte state in
        let* br = as_base ~ty:Typed.t_int br in
        if%sat bl ==@ br then aux l r (len -@ 1s) state
        else
          if%sat bl <@ br then Result.ok (Base (-1s), state)
          else Result.ok (Base 1s, state)
    in
    aux ~inc:0s l r len state

  let copy_ nonoverlapping ~t ~src ~dst ~count state : ret =
    let* (src, _), (dst, _) = as_ptr src &&* as_ptr dst in
    let* count = as_base ~ty:Typed.t_int count in
    let** (), state = State.check_ptr_align src t state in
    let** (), state = State.check_ptr_align dst t state in
    let* ty_size = Layout.size_of_s t in
    if%sat ty_size ==@ 0s then Result.ok (Tuple [], state)
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
          if%sat Sptr.is_same_loc src dst &&@ (dist1 <@ 0s &&@ (dist2 <@ 0s))
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

  let copy_sign ~x ~y state =
    let* x, y = as_float x &&* as_float y in
    let zero = Typed.float_like y 0.0 in
    if%sat [@lname "copy_sign < 0"] [@rname "copy_sign >=0"] y <.@ zero then
      let x' = Typed.neg (Typed.abs_f x) in
      Result.ok (Base (Typed.cast x'), state)
    else
      let x' = Typed.abs_f x in
      Result.ok (Base (Typed.cast x'), state)

  let copysignf128 = copy_sign
  let copysignf64 = copy_sign
  let copysignf32 = copy_sign
  let copysignf16 = copy_sign

  let ctpop ~t ~x state =
    let t = TypesUtils.ty_as_literal t in
    let bits = 8 * Layout.size_of_literal_ty t in
    let* x = as_base ~ty:Typed.t_int x in
    match Typed.kind x with
    | Int x ->
        (* convert to unsigned *)
        let v =
          if Layout.is_signed t then
            let maxv = Z.shift_left Z.one bits in
            if Z.(x < zero) then Z.(((x mod maxv) + maxv) mod maxv) else x
          else x
        in
        let v = Typed.int @@ Z.popcount v in
        Result.ok (Base v, state)
    | _ ->
        (* convert to unsigned *)
        let** x : Typed.T.sint Typed.t =
          if Layout.is_signed t then
            let to_ty = Layout.lit_to_unsigned t in
            let++ x =
              State.lift_err state
              @@ Encoder.transmute ~from_ty:(TLiteral t) ~to_ty (Base x)
            in
            as_base_of ~ty:Typed.t_int x
          else Result.ok (Typed.cast x)
        in
        let two = Typed.nonzero 2 in
        let res =
          List.init bits (fun i -> i)
          |> List.fold_left
               (fun acc off ->
                 let pow = Typed.nonzero_z (Z.shift_left Z.one off) in
                 acc +@ (x /@ pow %@ two))
               0s
        in
        Result.ok (Base (res :> Typed.T.cval Typed.t), state)

  let discriminant_value ~t ~v state =
    let* ptr = as_ptr v in
    let++ value, state = State.load ptr t state in
    match value with
    | Enum (discr, _) -> (Base discr, state)
    | _ -> (Base 0s, state)

  let exact_div ~t ~x ~y state =
    let lit = TypesUtils.ty_as_literal t in
    let* x, y = as_base x &&* as_base y in
    let* x, y, ty = cast_checked2 x y in
    let open Typed in
    let** res = State.lift_err state @@ Core.eval_lit_binop (Div OUB) lit x y in
    if is_float ty then Result.ok (Base res, state)
    else
      if%sat (not (y ==@ 0s)) &&@ (x %@ cast y ==@ 0s) then
        Result.ok (Base res, state)
      else
        State.error (`StdErr "core::intrinsics::exact_div on non divisible")
          state

  let abs ~x state =
    let* x = as_float x in
    Result.ok (Base (Typed.abs_f x), state)

  let fabsf16 = abs
  let fabsf32 = abs
  let fabsf64 = abs
  let fabsf128 = abs

  let float_fast (bop : Expressions.binop) ~t:(_ : Types.ty) ~a ~b state : ret =
    let* l, r = as_float a &&* as_float b in
    let bop, name =
      match bop with
      | Add _ -> (( +.@ ), "core::intrinsics::fadd_fast")
      | Sub _ -> (( -.@ ), "core::intrinsics::fsub_fast")
      | Mul _ -> (( *.@ ), "core::intrinsics::fmul_fast")
      | Div _ -> (( /.@ ), "core::intrinsics::fdiv_fast")
      | Rem _ -> (Typed.rem_f, "core::intrinsics::frem_fast")
      | _ -> failwith "fast_float: invalid binop"
    in
    let is_finite f = Typed.((not (is_nan f)) &&@ not (is_infinite f)) in
    let res = bop l r in
    if%sat is_finite l &&@ is_finite r &&@ is_finite (bop l r) then
      Result.ok (Base res, state)
    else State.error (`StdErr (name ^ ": result must be finite")) state

  let fadd_fast = float_fast (Add OUB)
  let fdiv_fast = float_fast (Div OUB)
  let fmul_fast = float_fast (Mul OUB)
  let frem_fast = float_fast (Rem OUB)
  let fsub_fast = float_fast (Sub OUB)

  let float_to_int_unchecked ~float:_ ~int ~value state =
    let ity = TypesUtils.ty_as_literal int in
    let* f = as_float value in
    if%sat Typed.is_nan f ||@ Typed.is_infinite f then
      State.error (`StdErr "float_to_int_unchecked with NaN or infinite value")
        state
    else
      let n = 8 * Layout.size_of_literal_ty ity in
      let max = Z.succ @@ Layout.max_value_z ity in
      let min = Z.pred @@ Layout.min_value_z ity in
      let max = Typed.float_like f @@ Z.to_float max in
      let min = Typed.float_like f @@ Z.to_float min in
      (* we use min-1 and max+1, to be able to have a strict inequality, which avoids
             issues in cases of float precision loss (I think?) *)
      if%sat min <.@ f &&@ (f <.@ max) then
        let v = Typed.int_of_float n f in
        Result.ok (Base v, state)
      else State.error (`StdErr "float_to_int_unchecked out of int range") state

  let fmul_add ~a ~b ~c state =
    let* a = as_float a in
    let* b = as_float b in
    let* c = as_float c in
    Result.ok (Base ((a *.@ b) +.@ c), state)

  let fmaf16 = fmul_add
  let fmaf32 = fmul_add
  let fmaf64 = fmul_add
  let fmaf128 = fmul_add
  let fmuladdf16 = fmul_add
  let fmuladdf32 = fmul_add
  let fmuladdf64 = fmul_add
  let fmuladdf128 = fmul_add
  let forget ~t:_ ~arg:_ state = Result.ok (Tuple [], state)

  let is_val_statically_known ~t:_ ~_arg:_ state =
    (* see: https://doc.rust-lang.org/std/intrinsics/fn.is_val_statically_known.html *)
    let* b = Rustsymex.nondet Typed.t_bool in
    Result.ok (Base (Typed.int_of_bool b), state)

  let likely ~b state = Result.ok (b, state)
  let unlikely ~b state = Result.ok (b, state)

  let float_minmax ~is_min ~x ~y state : ret =
    let* x, y = as_float x &&* as_float y in
    let++ res =
      if%sat Typed.is_nan x then Result.ok y
      else
        if%sat Typed.is_nan y then Result.ok x
        else
          let op = if is_min then ( <.@ ) else ( >.@ ) in
          Result.ok (Typed.ite (op x y) x y)
    in
    (Base (res :> Typed.T.cval Typed.t), state)

  let minnumf16 = float_minmax ~is_min:true
  let minnumf32 = float_minmax ~is_min:true
  let minnumf64 = float_minmax ~is_min:true
  let minnumf128 = float_minmax ~is_min:true
  let maxnumf16 = float_minmax ~is_min:false
  let maxnumf32 = float_minmax ~is_min:false
  let maxnumf64 = float_minmax ~is_min:false
  let maxnumf128 = float_minmax ~is_min:false

  let ptr_guaranteed_cmp ~t:_ ~ptr ~other state =
    let++ res = State.lift_err state @@ Core.eval_ptr_binop Eq ptr other in
    (Base res, state)

  let ptr_offset_from_ ~unsigned ~t ~ptr ~base state : ret =
    let* (ptr, _), (base, _) = as_ptr ptr &&* as_ptr base in
    let* size = Layout.size_of_s t in
    if%sat size ==@ 0s then
      State.error (`Panic (Some "ptr_offset_from with ZST")) state
    else
      let size = Typed.cast size in
      let* off = Sptr.distance ptr base in
      (* If the pointers are not equal, they mustn't be dangling *)
      if%sat off ==@ 0s ||@ (Sptr.constraints ptr &&@ Sptr.constraints base)
      then
        (* UB conditions:
           1. must be at the same address, OR derived from the same allocation
           2. the distance must be a multiple of sizeof(T) *)
        if%sat off ==@ 0s ||@ Sptr.is_same_loc ptr base &&@ (off %@ size ==@ 0s)
        then
          if not unsigned then Result.ok (Base (off /@ size), state)
          else
            if%sat off >=@ 0s then Result.ok (Base (off /@ size), state)
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
    let* l, r = as_ptr a &&* as_ptr b in
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
    (Base (Typed.int_of_bool res), state)

  let saturating op ~t ~a ~b state : ret =
    let litty = TypesUtils.ty_as_literal t in
    let* a, b = as_base a &&* as_base b in
    let max = Layout.max_value litty in
    let min = Layout.min_value litty in
    let** res = State.lift_err state @@ Core.safe_binop op litty a b in
    let* res = cast_checked ~ty:Typed.t_int res in
    let res = Typed.ite (res >@ max) max (Typed.ite (res <@ min) min res) in
    Result.ok (Base (res :> Typed.T.cval Typed.t), state)

  let saturating_add = saturating (Add OUB)
  let saturating_sub = saturating (Sub OUB)

  let size_of ~t state =
    let* size = Layout.size_of_s t in
    Result.ok (Base size, state)

  let size_of_val ~t ~ptr state =
    match (t, ptr) with
    | ( Types.TAdt { id = TBuiltin TSlice; generics = { types = [ sub_ty ]; _ } },
        Ptr (_, Some meta) ) ->
        let* len = cast_checked meta ~ty:Typed.t_int in
        let* size = Layout.size_of_s sub_ty in
        let size = size *@ len in
        Result.ok (Base size, state)
    | TAdt { id = TBuiltin TStr; _ }, Ptr (_, Some meta) ->
        let* len = cast_checked meta ~ty:Typed.t_int in
        let size = Layout.size_of_uint_ty U8 in
        let size = Typed.int size *@ len in
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
    Result.ok (Base (Typed.int hash), state)

  let type_name ~t state =
    let str = Fmt.to_to_string pp_ty t in
    let** ptr_res, state = State.load_str_global str state in
    match ptr_res with
    | Some ptr -> Result.ok (Ptr ptr, state)
    | None ->
        let len = String.length str in
        let chars =
          String.to_bytes str
          |> Bytes.fold_left (fun l c -> Base (Typed.int (Char.code c)) :: l) []
          |> List.rev
        in
        let char_arr = Array chars in
        let str_ty : Types.ty =
          mk_array_ty (TLiteral (TUInt U8)) (Z.of_int len)
        in
        let** (ptr, _), state = State.alloc_ty str_ty state in
        let ptr = (ptr, Some (Typed.int len)) in
        let** (), state = State.store ptr str_ty char_arr state in
        let++ (), state = State.store_str_global str ptr state in
        (Ptr ptr, state)

  let typed_swap_nonoverlapping ~t ~x ~y state =
    let* ((from_ptr, _) as from), ((to_ptr, _) as to_) =
      as_ptr x &&* as_ptr y
    in
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
        if%sat
          Sptr.is_same_loc from_ptr to_ptr &&@ (dist1 <@ 0s &&@ (dist2 <@ 0s))
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
    let* x, y = as_base x &&* as_base y in
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
        Result.ok (Base (Typed.int (List.length variants)), state)
    | _ ->
        State.error
          (`StdErr "core::intrinsics::variant_count used with non-enum") state

  let wrapping_op op ~t ~a ~b state : ret =
    let ity = TypesUtils.ty_as_literal t in
    let* a, b = as_base a &&* as_base b in
    let** res = State.lift_err state @@ Core.safe_binop op ity a b in
    let* res = Core.wrap_value ity res in
    Result.ok (Base res, state)

  let wrapping_add = wrapping_op (Add OUB)
  let wrapping_mul = wrapping_op (Mul OUB)
  let wrapping_sub = wrapping_op (Sub OUB)

  let write_bytes ~t ~dst ~val_ ~count state =
    let* ((ptr, _) as dst) = as_ptr dst in
    let* v, count = as_base val_ &&* as_base count in
    let* count = cast_checked count ~ty:Typed.t_int in
    let** (), state = State.check_ptr_align ptr t state in
    let* size = Layout.size_of_s t in
    let size = size *@ count in
    if%sat size ==@ 0s then Result.ok (Tuple [], state)
    else
      (* if v == 0, then we can replace this mess by initialising a Zeros subtree *)
      if%sat v ==@ 0s then
        let++ (), state = State.zeros dst size state in
        (Tuple [], state)
      else
        match Typed.kind size with
        | Int bytes ->
            let list = List.init (Z.to_int bytes) Fun.id in
            let++ (), state =
              Result.fold_list list ~init:((), state) ~f:(fun ((), state) i ->
                  let** ptr =
                    Sptr.offset ptr @@ Typed.int i |> State.lift_err state
                  in
                  State.store (ptr, None) (TLiteral (TUInt U8)) (Base v) state)
            in
            (Tuple [], state)
        | _ -> failwith "write_bytes: don't know how to handle symbolic sizes"
end
