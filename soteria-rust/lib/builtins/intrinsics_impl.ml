open Charon
open Charon_util
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Rust_val

module M (State : State_intf.S) = struct
  module Core = Core.M (State)
  module Encoder = Encoder.Make (State.Sptr)
  module Monad = State_monad.Make (State)
  open Monad
  open Syntax

  (* some utils *)
  type 'a ret = ('a, unit) Monad.t

  (* we retype these to avoid non-generalisable type variables in ['a Rust_val.t] *)
  let[@inline] as_ptr (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Base v ->
        let v = Typed.cast_i Usize v in
        let ptr = Sptr.null_ptr_of v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  (* the intrinsics  *)

  let abort : unit ret = error (`Panic (Some "aborted"))

  let checked_op op ~t ~x ~y : rust_val ret =
    let t = TypesUtils.ty_as_literal t in
    let x, y = (as_base t x, as_base t y) in
    State.with_decay_map_res @@ Core.eval_checked_lit_binop op t x y

  let add_with_overflow = checked_op (Add OUB)
  let sub_with_overflow = checked_op (Sub OUB)
  let mul_with_overflow = checked_op (Mul OUB)
  let align_of ~t = lift_symex @@ Layout.align_of_s t

  (* FIXME: how does the value influence the alignment? *)
  let align_of_val ~t ~ptr:_ = lift_symex @@ Layout.align_of_s t

  let arith_offset ~t ~dst:(dst, meta) ~offset =
    let^^+ dst' = Sptr.offset ~signed:true ~check:false ~ty:t dst offset in
    (dst', meta)

  let offset ~ptr ~delta ~dst ~offset =
    match dst with
    | Ptr (dst, meta) ->
        let offset = as_base_i Usize offset in
        let signed = Layout.is_signed @@ TypesUtils.ty_as_literal delta in
        let^^+ dst' = Sptr.offset ~signed ~ty:ptr dst offset in
        Ptr (dst', meta)
    | Base _ -> error `UBPointerArithmetic
    | _ -> not_impl "ptr_add: invalid arguments"

  let assert_inhabited ~t =
    if Layout.is_inhabited t then ok ()
    else error (`Panic (Some "core::intrinsics::assert_inhabited"))

  let assert_mem_uninitialized_valid ~t:_ = ok ()

  let assert_zero_valid ~t =
    match Layout.zeroed ~null_ptr:Sptr.null_ptr t with
    | None -> error (`Panic (Some "core::intrinsics::assert_zero_valid"))
    | _ -> ok ()

  let assume ~b =
    State.assert_ b (`StdErr "core::intrinsics::assume with false")

  let black_box ~t:_ ~dummy = ok dummy
  let breakpoint : unit ret = error `Breakpoint

  let bswap ~t ~x =
    let lit = TypesUtils.ty_as_literal t in
    let nbytes = Layout.size_of_literal_ty lit in
    let v = as_base lit x in
    let bytes =
      List.init nbytes (fun i -> BV.extract (i * 8) (((i + 1) * 8) - 1) v)
    in
    let rec aux = function
      | [] -> failwith "impossible: no bytes"
      | [ last ] -> last
      | hd :: tl -> BV.concat hd (aux tl)
    in
    let v' = aux bytes in
    ok (Base (v' :> Typed.T.cval Typed.t))

  let catch_unwind exec_fun ~_try_fn:try_fn_ptr ~_data:data
      ~_catch_fn:catch_fn_ptr =
    let[@inline] get_fn ptr =
      let+ fn_ptr = State.lookup_fn ptr in
      match fn_ptr.kind with
      | FunId (FRegular fid) -> Crate.get_fun fid
      | TraitMethod (_, _, fid) -> Crate.get_fun fid
      | FunId (FBuiltin _) -> failwith "Can't have function pointer to builtin"
    in
    let loc = !Rustsymex.current_loc in
    let[@inline] exec_fun msg fn args =
      with_extra_call_trace ~loc ~msg @@ exec_fun fn args
    in
    let try_fn_ptr, catch_fn_ptr = (as_ptr try_fn_ptr, as_ptr catch_fn_ptr) in
    let* try_fn = get_fn try_fn_ptr in
    let* catch_fn = get_fn catch_fn_ptr in
    exec_fun "catch_unwind try" try_fn [ Ptr data ]
    |> State.unwind_with
         ~f:(fun _ -> ok U32.(0s))
         ~fe:(fun _ ->
           exec_fun "catch_unwind catch" catch_fn
             [ Ptr data; Ptr (Sptr.null_ptr (), Thin) ]
           |> State.unwind_with
                ~f:(fun _ -> ok U32.(1s))
                ~fe:(fun _ -> error (`StdErr "catch_unwind unwinded in catch")))

  let[@inline] float_rounding rm x = ok (Typed.Float.round rm x)
  let ceilf16 ~x = float_rounding Ceil x
  let ceilf32 ~x = float_rounding Ceil x
  let ceilf64 ~x = float_rounding Ceil x
  let ceilf128 ~x = float_rounding Ceil x
  let floorf16 ~x = float_rounding Floor x
  let floorf32 ~x = float_rounding Floor x
  let floorf64 ~x = float_rounding Floor x
  let floorf128 ~x = float_rounding Floor x
  let round_ties_even_f16 ~x = float_rounding NearestTiesToEven x
  let round_ties_even_f32 ~x = float_rounding NearestTiesToEven x
  let round_ties_even_f64 ~x = float_rounding NearestTiesToEven x
  let round_ties_even_f128 ~x = float_rounding NearestTiesToEven x
  let roundf16 ~x = float_rounding NearestTiesToAway x
  let roundf32 ~x = float_rounding NearestTiesToAway x
  let roundf64 ~x = float_rounding NearestTiesToAway x
  let roundf128 ~x = float_rounding NearestTiesToAway x
  let truncf16 ~x = float_rounding Truncate x
  let truncf32 ~x = float_rounding Truncate x
  let truncf64 ~x = float_rounding Truncate x
  let truncf128 ~x = float_rounding Truncate x
  let cold_path : unit ret = ok ()

  let compare_bytes ~left:(l, _) ~right:(r, _) ~bytes =
    let zero = Usize.(0s) in
    let one = Usize.(1s) in
    let byte = Types.TLiteral (TUInt U8) in
    let rec aux ?(inc = one) l r len =
      if%sat len ==@ zero then ok U32.(0s)
      else
        let^^ l = Sptr.offset ~signed:false l inc in
        let^^ r = Sptr.offset ~signed:false r inc in
        let* bl = State.load (l, Thin) byte in
        let bl = as_base_i U8 bl in
        let* br = State.load (r, Thin) byte in
        let br = as_base_i U8 br in
        if%sat bl ==@ br then aux l r (len -!@ one)
        else if%sat bl <@ br then ok U32.(-1s) else ok U32.(1s)
    in
    aux ~inc:zero l r (bytes :> T.sint Typed.t)

  (** [check_overlap name l r size] ensures the pointers [l] and [r] do not
      overlap for a range of size [size]; otherwise errors, with
      [`StdErr (name ^ " overlapped")]. *)
  let check_overlap name l r size =
    let^^ l_end = Sptr.offset ~signed:false l size in
    let^^ r_end = Sptr.offset ~signed:false r size in
    let$ dist1 = Sptr.distance l r_end in
    let$ dist2 = Sptr.distance r l_end in
    let zero = Usize.(0s) in
    State.assert_not
      (Sptr.is_same_loc l r &&@ (dist1 <$@ zero &&@ (dist2 <$@ zero)))
      (`StdErr (name ^ " overlapped"))

  let copy_ nonoverlapping ~t ~src:((src, _) as fsrc : full_ptr)
      ~dst:((dst, _) as fdst : full_ptr) ~count : unit ret =
    let zero = Usize.(0s) in
    let* () = State.check_ptr_align fsrc t in
    let* () = State.check_ptr_align fdst t in
    let^ ty_size = Layout.size_of_s t in
    if%sat ty_size ==@ zero ||@ (count ==@ zero) then ok ()
    else
      let* () =
        State.assert_not
          (Sptr.is_at_null_loc src ||@ Sptr.is_at_null_loc dst)
          `NullDereference
      in
      let size, overflowed = ty_size *?@ count in
      let* () = State.assert_not overflowed `Overflow in
      (* Here we can cheat a little: for copy_nonoverlapping we need to check for overlap,
         but otherwise the copy is the exact same; since the State makes a copy of the src tree
         before storing into dst, the semantics are that of copy. *)
      let* () =
        if not nonoverlapping then ok ()
        else check_overlap "copy_nonoverlapping" src dst size
      in
      State.copy_nonoverlapping ~dst:(dst, Thin) ~src:(src, Thin) ~size

  let copy ~t ~src ~dst ~count = copy_ false ~t ~src ~dst ~count
  let copy_nonoverlapping ~t ~src ~dst ~count = copy_ true ~t ~src ~dst ~count

  let typed_swap_nonoverlapping ~t ~x:((from_ptr, _) as from)
      ~y:((to_ptr, _) as to_) =
    let* () = State.check_ptr_align from t in
    let* () = State.check_ptr_align to_ t in
    let^ size = Layout.size_of_s t in
    let* () =
      State.assert_not
        (Sptr.is_at_null_loc from_ptr ||@ Sptr.is_at_null_loc to_ptr)
        `NullDereference
    in
    let* () = check_overlap "typed_swap_nonoverlapping" from_ptr to_ptr size in
    let* v_l = State.load from t in
    let* v_r = State.load to_ t in
    let* () = State.store from t v_r in
    State.store to_ t v_l

  let copy_sign ~x ~y =
    let zero = Typed.Float.like y 0.0 in
    if%sat [@lname "copy_sign < 0"] [@rname "copy_sign >=0"] y <.@ zero then
      ok (Typed.Float.neg (Typed.Float.abs x))
    else ok (Typed.Float.abs x)

  let copysignf128 = copy_sign
  let copysignf64 = copy_sign
  let copysignf32 = copy_sign
  let copysignf16 = copy_sign

  (** Applies either [concrete] or [symbolic] to a bitvector. *)
  let binary_int_operation ~concrete ~symbolic ~t ~x : T.sint Typed.t ret =
    let t = TypesUtils.ty_as_literal t in
    let bits = 8 * Layout.size_of_literal_ty t in
    let x = as_base t x in
    let res =
      match BV.to_z x with Some z -> concrete bits z | None -> symbolic bits x
    in
    ok res

  let ctpop =
    let concrete _bits x = BV.u32i @@ Z.popcount x in
    let symbolic bits x =
      Iter.fold
        (fun acc off ->
          let bit = BV.extract off off x in
          let bit32 = BV.extend ~signed:false 31 bit in
          acc +!@ bit32)
        U32.(0s)
        Iter.(0 -- (bits - 1))
    in
    binary_int_operation ~concrete ~symbolic

  let cttz =
    let concrete bits x =
      BV.u32i @@ if Z.equal x Z.zero then bits else Z.trailing_zeros x
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
          let bit = BV.extract off off x in
          Typed.ite (bit ==@ BV.one 1) (BV.u32i off) acc)
        (BV.u32i bits)
        Iter.(0 -- (bits - 1))
    in
    binary_int_operation ~concrete ~symbolic

  let cttz_nonzero ~t ~x =
    let tlit = TypesUtils.ty_as_literal t in
    let x_int = as_base tlit x in
    let* () =
      State.assert_not
        (x_int ==@ BV.mki_lit tlit 0)
        (`StdErr "core::intrinsics::cttz_nonzero on zero")
    in
    cttz ~t ~x

  let ctlz =
    let concrete bits x =
      let rec aux n =
        if Z.testbit x (bits - 1 - n) then n
        else if n + 1 < bits then aux (n + 1)
        else bits
      in
      Typed.BitVec.u32i @@ aux 0
    in
    (* we construct the following, from inside out:
      ite(x[bits-1] == 1 ? 0 :
        ite(x[bits-2] == 1 ? 1 :
          ...
          ite(x[0] == 1 ? bits-1 : bits))) *)
    let symbolic bits x =
      Iter.fold
        (fun acc off ->
          let res = bits - 1 - off in
          let bit = Typed.BitVec.extract off off x in
          Typed.ite (bit ==@ Typed.BitVec.one 1) (Typed.BitVec.u32i res) acc)
        (Typed.BitVec.u32i bits)
        Iter.(0 -- (bits - 1))
    in
    binary_int_operation ~concrete ~symbolic

  let ctlz_nonzero ~t ~x =
    let tlit = TypesUtils.ty_as_literal t in
    let x_int = as_base tlit x in
    let* () =
      State.assert_not
        (x_int ==@ Typed.BitVec.mki_lit tlit 0)
        (`StdErr "core::intrinsics::ctlz_nonzero on zero")
    in
    ctlz ~t ~x

  let discriminant_value ~t ~v =
    let adt_id, _ = TypesUtils.ty_as_custom_adt t in
    let adt = Crate.get_adt adt_id in
    match adt.kind with
    | Enum variants ->
        let+ variant_id = State.load_discriminant v t in
        let variant = Types.VariantId.nth variants variant_id in
        Base (BV.of_literal variant.discriminant)
    | _ ->
        (* FIXME: this size is probably wrong *)
        ok (Base U8.(0s))

  let disjoint_bitor ~t ~a ~b =
    let ty = TypesUtils.ty_as_literal t in
    let a, b = (as_base ty a, as_base ty b) in
    let+ () =
      State.assert_
        (a &@ b ==@ BV.mki_lit ty 0)
        (`StdErr "core::intrinsics::disjoint_bitor with overlapping bits")
    in
    Base (a |@ b)

  let exact_div ~t ~x ~y =
    let lit = TypesUtils.ty_as_literal t in
    let x, y = (as_base lit x, as_base lit y) in
    let x, y, ty = Typed.cast_checked2 x y in
    let$$ res = Core.eval_lit_binop (Div OUB) lit x y in
    if Typed.is_float ty then ok (Base res)
    else
      let zero = BV.mki_lit lit 0 in
      let ( %@ ) = Typed.BitVec.rem ~signed:(Layout.is_signed lit) in
      let+ () =
        State.assert_
          (Typed.not (y ==@ zero) &&@ (x %@ Typed.cast y ==@ zero))
          (`StdErr "core::intrinsics::exact_div on non divisible")
      in
      Base res

  let abs ~x = ok (Typed.Float.abs x)
  let fabsf16 = abs
  let fabsf32 = abs
  let fabsf64 = abs
  let fabsf128 = abs

  let float_fast (bop : Expressions.binop) ~(t : Types.ty) ~a ~b : rust_val ret
      =
    let t = ty_as_float t in
    let l, r = (as_base_f t a, as_base_f t b) in
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
    let+ () =
      State.assert_
        (is_finite l &&@ is_finite r &&@ is_finite (bop l r))
        (`StdErr (name ^ ": operands and result must be finite"))
    in
    Base res

  let fadd_fast = float_fast (Add OUB)
  let fdiv_fast = float_fast (Div OUB)
  let fmul_fast = float_fast (Mul OUB)
  let frem_fast = float_fast (Rem OUB)
  let fsub_fast = float_fast (Sub OUB)

  let float_to_int_unchecked ~float ~int ~value =
    let fty = ty_as_float float in
    let ity = TypesUtils.ty_as_literal int in
    let f = as_base_f fty value in
    let* () =
      State.assert_not
        (Typed.Float.is_nan f ||@ Typed.Float.is_infinite f)
        (`StdErr "float_to_int_unchecked with NaN or infinite value")
    in
    let signed = Layout.is_signed ity in
    let size = 8 * Layout.size_of_literal_ty ity in
    let max = Z.succ @@ Layout.max_value_z ity in
    let min = Z.pred @@ Layout.min_value_z ity in
    let max = Typed.Float.mk fty @@ Float.to_string @@ Z.to_float max in
    let min = Typed.Float.mk fty @@ Float.to_string @@ Z.to_float min in
    (* we use min-1 and max+1, to be able to have a strict inequality, which avoids
       issues in cases of float precision loss (I think?) *)
    let+ () =
      State.assert_
        (min <.@ f &&@ (f <.@ max))
        (`StdErr "float_to_int_unchecked out of int range")
    in
    Base (BV.of_float ~rounding:Truncate ~signed ~size f)

  let fmul_add ~a ~b ~c = ok ((a *.@ b) +.@ c)
  let fmaf16 = fmul_add
  let fmaf32 = fmul_add
  let fmaf64 = fmul_add
  let fmaf128 = fmul_add
  let fmuladdf16 = fmul_add
  let fmuladdf32 = fmul_add
  let fmuladdf64 = fmul_add
  let fmuladdf128 = fmul_add
  let forget ~t:_ ~arg:_ = ok ()

  let is_val_statically_known ~t:_ ~_arg:_ =
    (* see: https://doc.rust-lang.org/std/intrinsics/fn.is_val_statically_known.html *)
    lift_symex @@ Rustsymex.nondet Typed.t_bool

  let likely ~b = ok (b :> T.sbool Typed.t)
  let unlikely ~b = ok (b :> T.sbool Typed.t)

  let float_minmax ~is_min ~x ~y : T.sfloat Typed.t ret =
    let x = (x :> T.sfloat Typed.t) in
    let y = (y :> T.sfloat Typed.t) in
    if%sat Typed.Float.is_nan x then ok y
    else
      if%sat Typed.Float.is_nan y then ok x
      else
        let op = if is_min then ( <.@ ) else ( >.@ ) in
        ok (Typed.ite (op x y) x y)

  let minnumf16 ~x ~y = float_minmax ~is_min:true ~x ~y
  let minnumf32 ~x ~y = float_minmax ~is_min:true ~x ~y
  let minnumf64 ~x ~y = float_minmax ~is_min:true ~x ~y
  let minnumf128 ~x ~y = float_minmax ~is_min:true ~x ~y
  let maxnumf16 ~x ~y = float_minmax ~is_min:false ~x ~y
  let maxnumf32 ~x ~y = float_minmax ~is_min:false ~x ~y
  let maxnumf64 ~x ~y = float_minmax ~is_min:false ~x ~y
  let maxnumf128 ~x ~y = float_minmax ~is_min:false ~x ~y

  let ptr_guaranteed_cmp ~t:_ ~ptr ~other =
    State.with_decay_map_res @@ Core.eval_ptr_binop Eq (Ptr ptr) (Ptr other)

  let ptr_offset_from_ ~unsigned ~t ~ptr:((ptr, _) : full_ptr)
      ~base:((base, _) : full_ptr) : T.sint Typed.t ret =
    let zero = Usize.(0s) in
    let^ size = Layout.size_of_s t in
    let* () =
      State.assert_not (size ==@ zero)
        (`Panic (Some "ptr_offset_from with ZST"))
    in
    let size = Typed.cast size in
    let$ off = Sptr.distance ptr base in
    (* If the pointers are not equal, they mustn't be dangling *)
    let* () =
      State.assert_
        (off ==@ zero ||@ (Sptr.constraints ptr &&@ Sptr.constraints base))
        `UBDanglingPointer
    in
    (* UB conditions:
       1. must be at the same address, OR derived from the same allocation
       2. the distance must be a multiple of sizeof(T) *)
    let* () =
      State.assert_
        (off ==@ zero ||@ Sptr.is_same_loc ptr base &&@ (off %$@ size ==@ zero))
        `UBPointerComparison
    in
    if not unsigned then ok (off /$@ size)
    else
      let+ () =
        State.assert_ (off >=$@ zero)
          (`StdErr "core::intrinsics::offset_from_unsigned negative offset")
      in
      off /$@ size

  let ptr_offset_from = ptr_offset_from_ ~unsigned:false
  let ptr_offset_from_unsigned = ptr_offset_from_ ~unsigned:true

  let raw_eq ~t ~a ~b =
    let layout = Layout.layout_of t in
    let bytes = mk_array_ty (TLiteral (TUInt U8)) (Z.of_int layout.size) in
    (* TODO: figure out if for these two reads we should ignore the modified state,
       as its leaves may be split in bytes which will require ugly transmutations
       to be read from again later. *)
    let* l = State.load a bytes in
    let* r = State.load b bytes in
    let byte_pairs =
      match (l, r) with
      | Array l, Array r -> List.combine l r
      | _ -> failwith "Unexpected read array"
    in
    let rec aux = function
      | [] -> ok Typed.v_true
      | (Base l, Base r) :: rest ->
          if%sat l ==@ r then aux rest else ok Typed.v_false
      | _ :: _ -> failwith "Unexpected read array"
    in
    aux byte_pairs

  let saturating (op : Expressions.binop) ~t ~a ~b : rust_val ret =
    let t = TypesUtils.ty_as_literal t in
    let signed = Layout.is_signed t in
    let a, b = (as_base t a, as_base t b) in
    let max = BV.mk_lit t @@ Layout.max_value_z t in
    let min = BV.mk_lit t @@ Layout.min_value_z t in
    let res =
      match op with
      | Add _ ->
          let ovf = BV.add_overflows ~signed a b in
          let if_ovf =
            if not signed then max else Typed.ite (a <$@ BV.mki_lit t 0) min max
          in
          Typed.ite ovf if_ovf (a +!@ b)
      | Sub _ ->
          let ovf = BV.sub_overflows ~signed a b in
          let if_ovf =
            if not signed then min else Typed.ite (a <$@ b) min max
          in
          Typed.ite ovf if_ovf (a -!@ b)
      | _ -> failwith "Unreachable: not add or sub?"
    in
    ok (Base (res :> Typed.T.cval Typed.t))

  let saturating_add = saturating (Add OUB)
  let saturating_sub = saturating (Sub OUB)
  let size_of ~t = lift_symex @@ Layout.size_of_s t

  let size_of_val ~t ~ptr:(_, meta) =
    match (t, meta) with
    | Types.TAdt { id = TBuiltin ((TSlice | TStr) as id); generics }, Len meta
      ->
        let sub_ty =
          if id = TSlice then List.hd generics.types else TLiteral (TUInt U8)
        in
        let len = Typed.cast_i Usize meta in
        let^ size = Layout.size_of_s sub_ty in
        let size, ovf = size *?@ len in
        let+ () = State.assert_not ovf `Overflow in
        size
    | _ -> lift_symex @@ Layout.size_of_s t

  let transmute ~t_src ~dst ~src =
    let* verify_ptr = State.is_valid_ptr_fn in
    State.with_decay_map_res
    @@ Encoder.transmute ~verify_ptr ~from_ty:t_src ~to_ty:dst src

  let type_id ~t =
    (* lazy but works *)
    let hash = Hashtbl.hash t in
    ok (BV.u128i hash)

  let type_name ~t =
    let str = Fmt.to_to_string pp_ty t in
    let* ptr_res = State.load_str_global str in
    match ptr_res with
    | Some ptr -> ok ptr
    | None ->
        let len = String.length str in
        let chars =
          String.to_bytes str
          |> Bytes.fold_left (fun l c -> Base (BV.u8i (Char.code c)) :: l) []
          |> List.rev
        in
        let char_arr = Array chars in
        let str_ty : Types.ty =
          mk_array_ty (TLiteral (TUInt U8)) (Z.of_int len)
        in
        let* ptr, _ = State.alloc_ty str_ty in
        let ptr = (ptr, Len (BV.usizei len)) in
        let* () = State.store ptr str_ty char_arr in
        let+ () = State.store_str_global str ptr in
        ptr

  let unchecked_op op ~t ~x ~y : rust_val ret =
    let t = TypesUtils.ty_as_literal t in
    let x, y = (as_base t x, as_base t y) in
    let$$+ res = Core.eval_lit_binop op t x y in
    Base res

  let unchecked_add = unchecked_op (Add OUB)
  let unchecked_div = unchecked_op (Div OUB)
  let unchecked_mul = unchecked_op (Mul OUB)
  let unchecked_rem = unchecked_op (Rem OUB)
  let unchecked_shl ~t ~u:_ = unchecked_op (Shl OUB) ~t
  let unchecked_shr ~t ~u:_ = unchecked_op (Shr OUB) ~t
  let unchecked_sub = unchecked_op (Sub OUB)

  let variant_count ~t =
    match t with
    | Types.TAdt { id = TAdtId id; _ } when Crate.is_enum id ->
        let variants = Crate.as_enum id in
        ok (BV.usizei (List.length variants))
    | _ -> error (`StdErr "core::intrinsics::variant_count used with non-enum")

  let wrapping_op op ~t ~a ~b : rust_val ret =
    let ity = TypesUtils.ty_as_literal t in
    let a, b = (as_base ity a, as_base ity b) in
    let$$+ res = Core.eval_lit_binop op ity a b in
    Base res

  let wrapping_add = wrapping_op (Add OWrap)
  let wrapping_mul = wrapping_op (Mul OWrap)
  let wrapping_sub = wrapping_op (Sub OWrap)

  let write_bytes ~t ~dst:((ptr, _) as dst) ~val_ ~count =
    let zero = Usize.(0s) in
    let* () = State.check_ptr_align dst t in
    let^ size = Layout.size_of_s t in
    let size, overflowed = size *?@ count in
    let* () = State.assert_not overflowed `Overflow in
    if%sat size ==@ zero then ok ()
    else
      (* if v == 0, then we can replace this mess by initialising a Zeros subtree *)
      let val_ : [> T.sint ] Typed.t = Typed.cast val_ in
      if%sure val_ ==@ U8.(0s) then State.zeros dst size
      else
        match BV.to_z size with
        | Some bytes ->
            fold_iter
              Iter.(0 -- (Z.to_int bytes - 1))
              ~init:()
              ~f:(fun () i ->
                let off = BV.usizei i in
                let^^ ptr = Sptr.offset ~signed:false ptr off in
                State.store (ptr, Thin) (TLiteral (TUInt U8)) (Base val_))
        | None ->
            not_impl "write_bytes: don't know how to handle symbolic sizes"
end
