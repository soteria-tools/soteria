open Syntaxes.FunctionWrap
open Charon
open Svalue
open Typed
open Typed.Syntax
open Typed.Infix
open Common.Charon_util

module M (StateM : State.StateM.S) : Intf.M(StateM).Impl = struct
  include Stubs.M (StateM)
  module Core = Core.M (StateM)
  open StateM
  open Syntax

  (* some utils *)
  type 'a ret = ('a, unit) StateM.t

  (* the intrinsics *)

  let abort () : unit ret = error (`Panic (Some "aborted"))

  let checked_op op ~t ~x ~y =
    let t = TypesUtils.ty_as_literal t in
    let x = Typed.cast_lit t x in
    let y = Typed.cast_lit t y in
    Core.eval_checked_lit_binop op t x y

  let add_with_overflow ~t ~x ~y = checked_op (Add OUB) ~t ~x ~y
  let sub_with_overflow ~t ~x ~y = checked_op (Sub OUB) ~t ~x ~y
  let mul_with_overflow ~t ~x ~y = checked_op (Mul OUB) ~t ~x ~y
  let align_of ~t = Layout.align_of t

  let arith_offset ~t ~dst:dst_full ~offset =
    let dst = Typed.Ptr.ptr_of dst_full in
    let+ dst' = Sptr.offset ~ty:t offset dst in
    Typed.Ptr.with_ptr dst_full dst'

  let offset ~ptr ~delta ~dst:dst_full ~offset =
    let dst_full = Typed.cast_ptr_f dst_full in
    let dst = Typed.Ptr.ptr_of dst_full in
    let offset = Typed.cast_i Usize offset in
    let check_signed = Layout.is_signed @@ TypesUtils.ty_as_literal delta in
    let+ dst' = Sptr.offset ~check_signed ~ty:ptr offset dst in
    Typed.Ptr.with_ptr dst_full dst'

  let assert_inhabited ~t =
    let* layout = Layout.layout_of t in
    if layout.uninhabited then
      error (`Panic (Some "core::intrinsics::assert_inhabited"))
    else ok ()

  let assert_mem_uninitialized_valid ~t:_ = ok ()

  let assert_zero_valid ~t =
    let* res = Core.zero_valid ~ty:t in
    if res then ok ()
    else error (`Panic (Some "core::intrinsics::assert_zero_valid"))

  (** Re-expose [StateM.assume] to not have the intrinsic take precedence *)
  let assume_sym = StateM.assume

  let assume ~b = assert_ b (`StdErr "core::intrinsics::assume with false")

  (* TODO: atomics are, for now, single-threaded *)
  let atomic_warn =
    let warning =
      String.Interned.intern
        "An atomic intrinsic was encountered; it will be executed as \
         sequential code"
    in
    fun () -> Soteria.Terminal.Warn.warn_once warning

  let atomic_fence ~ord:_ =
    atomic_warn ();
    ok ()

  let atomic_load ~t ~ord:_ ~volatile:_ ~src =
    atomic_warn ();
    State.load src t

  let atomic_singlethreadfence ~ord:_ =
    atomic_warn ();
    ok ()

  let atomic_store ~t ~ord:_ ~volatile:_ ~dst ~val_ =
    atomic_warn ();
    State.store dst t val_

  let atomic_xchg ~t ~ord:_ ~dst ~src =
    atomic_warn ();
    let* old = State.load dst t in
    let+ () = State.store dst t src in
    old

  let atomic_cxchgweak ~t ~ord_succ:_ ~ord_fail:_ ~dst ~old ~src =
    atomic_warn ();
    let* curr = State.load dst t in
    let* are_equal =
      match t with
      | TRawPtr _ | TRef _ ->
          let old = Typed.Ptr.ptr_of (Typed.cast_ptr_f old) in
          let curr = Typed.Ptr.ptr_of (Typed.cast_ptr_f curr) in
          let+ dist = Sptr.distance old curr in
          dist ==@ Usize.(0s)
      | TLiteral lit ->
          let old = Typed.cast_lit lit old in
          let curr = Typed.cast_lit lit curr in
          ok (old ==@ curr)
      | _ -> L.failwith "atomic_cxchgweak: invalid type, expects ptr or integer"
    in
    if%sat are_equal then
      let* () = State.store dst t src in
      ok (Typed.Adt.mk_tuple [ curr; BV.of_bool Typed.v_true ])
    else ok (Typed.Adt.mk_tuple [ curr; BV.of_bool Typed.v_false ])

  (* In our sequential model the strong compare-exchange behaves exactly like
     the weak one (the weak variant is only allowed to spuriously fail). *)
  let atomic_cxchg = atomic_cxchgweak

  (* Atomic read-modify-write for the intrinsics whose [T] may be an integer or
     a pointer (these take a [u] type parameter, [usize] for pointers). [int_op]
     computes the new integer value; [ptr_op] the new pointer from [src] (a
     [usize]) and the old pointer. (Executed sequentially -- see
     {!atomic_warn}.) *)
  let atomic_rmw ~(t : Types.ty) ~(u : Types.ty) ~dst ~src ~int_op ~ptr_op =
    atomic_warn ();
    let* old = State.load dst t in
    match (t, u) with
    | (TRawPtr (pointee, _) | TRef (_, pointee, _)), TLiteral (TUInt Usize) ->
        let old_fptr = Typed.cast_ptr_f old in
        let old_ptr = Typed.Ptr.ptr_of old_fptr in
        let* new_ptr = ptr_op (Typed.cast_i Usize src) old_ptr in
        let+ () = State.store dst t (Typed.Ptr.with_ptr old_fptr new_ptr) in
        old
    | TLiteral lit, _ ->
        let* res =
          int_op lit (Typed.cast_lit lit old) (Typed.cast_lit lit src)
        in
        let+ () = State.store dst t res in
        old
    | _ -> not_impl "atomic read-modify-write on unexpected type %a" pp_ty t

  (* The pointer case of a bitwise atomic: apply [op] to the pointer's address,
     dropping provenance -- like {!ptr_mask}. *)
  let atomic_addr_op op src old =
    let+ addr = Sptr.decay old in
    Typed.Ptr.of_address (op addr src)

  let atomic_and ~t ~u ~ord:_ ~dst ~src =
    atomic_rmw ~t ~u ~dst ~src
      ~int_op:(fun _ a b -> ok (a &@ b))
      ~ptr_op:(atomic_addr_op ( &@ ))

  let atomic_or ~t ~u ~ord:_ ~dst ~src =
    atomic_rmw ~t ~u ~dst ~src
      ~int_op:(fun _ a b -> ok (a |@ b))
      ~ptr_op:(atomic_addr_op ( |@ ))

  let atomic_xor ~t ~u ~ord:_ ~dst ~src =
    atomic_rmw ~t ~u ~dst ~src
      ~int_op:(fun _ a b -> ok (a ^@ b))
      ~ptr_op:(atomic_addr_op ( ^@ ))

  let atomic_nand ~t ~u ~ord:_ ~dst ~src =
    let nand a b = BV.not (a &@ b) in
    atomic_rmw ~t ~u ~dst ~src
      ~int_op:(fun _ a b -> ok (nand a b))
      ~ptr_op:(atomic_addr_op nand)

  let atomic_xadd ~t ~u ~ord:_ ~dst ~src =
    atomic_rmw ~t ~u ~dst ~src
      ~int_op:(Core.eval_lit_binop (Add OWrap))
      ~ptr_op:Sptr.offset

  let atomic_xsub ~t ~u ~ord:_ ~dst ~src =
    atomic_rmw ~t ~u ~dst ~src
      ~int_op:(Core.eval_lit_binop (Sub OWrap))
        (* subtract [src] bytes by offsetting by [-src] *)
      ~ptr_op:(fun src old -> Sptr.offset ~-!src old)

  (* Atomic read-modify-write on an integer (the min/max intrinsics, which only
     apply to integers). *)
  let atomic_int_rmw ~(t : Types.ty) ~dst ~src f =
    atomic_warn ();
    match t with
    | TLiteral lit ->
        let* old = State.load dst t in
        let+ () =
          State.store dst t
            (f (Typed.cast_lit lit old) (Typed.cast_lit lit src))
        in
        old
    | _ -> not_impl "atomic read-modify-write on non-integer type %a" pp_ty t

  (* [atomic_{min,max}] are guaranteed (signed) integers, [atomic_u{min,max}]
     unsigned integers. *)
  let atomic_max ~t ~ord:_ ~dst ~src =
    atomic_int_rmw ~t ~dst ~src (BV.max ~signed:true)

  let atomic_min ~t ~ord:_ ~dst ~src =
    atomic_int_rmw ~t ~dst ~src (BV.min ~signed:true)

  let atomic_umax ~t ~ord:_ ~dst ~src =
    atomic_int_rmw ~t ~dst ~src (BV.max ~signed:false)

  let atomic_umin ~t ~ord:_ ~dst ~src =
    atomic_int_rmw ~t ~dst ~src (BV.min ~signed:false)

  let black_box ~t:_ ~dummy = ok @@ Typed.as_any dummy
  let breakpoint () : unit ret = error `Breakpoint

  let bitreverse ~t ~x =
    let lit = TypesUtils.ty_as_literal t in
    let nbits = 8 * Layout.size_of_literal_ty lit in
    let v = Typed.cast_lit lit x in
    let bits = List.init nbits (fun i -> BV.extract i i v) in
    let rec aux = function
      | [] -> L.failwith "impossible: no bits"
      | [ last ] -> last
      | hd :: tl -> BV.concat hd (aux tl)
    in
    ok @@ Typed.as_any (aux bits)

  let bswap ~t ~x =
    let lit = TypesUtils.ty_as_literal t in
    let nbytes = Layout.size_of_literal_ty lit in
    let v = Typed.cast_lit lit x in
    let bytes =
      List.init nbytes (fun i -> BV.extract (i * 8) (((i + 1) * 8) - 1) v)
    in
    let rec aux = function
      | [] -> L.failwith "impossible: no bytes"
      | [ last ] -> last
      | hd :: tl -> BV.concat hd (aux tl)
    in
    ok @@ Typed.as_any (aux bytes)

  let caller_location () : Typed.([> T.sptr_f ] t) ret =
    (*
     * #[lang = "panic_location"]
     * pub struct Location<'a> {
     *     filename: NonNull<str>,
     *     line: u32,
     *     col: u32,
     *     _filename: PhantomData<&'a str>,
     * }
     *)
    let location_ref = Crate.get_adt_lang_item_ref RustcLangItemPanicLocation in
    let location_ty : Types.ty = TAdt location_ref in
    let* trace = get_trace () in
    let filename, col, line =
      match trace.loc with
      | None -> ("unknown", 0, 0)
      | Some { file = { name = Local file; _ }; beg_loc; _ } ->
          (Error.file_to_user_string file, beg_loc.col, beg_loc.line)
      | Some { file = { name = Virtual _ | NotReal _; _ }; beg_loc; _ } ->
          ("virtual", beg_loc.col, beg_loc.line)
    in
    let* ptr = Core.string_to_ptr filename in
    let location =
      Typed.Adt.mk_tuple
        [
          (* filename: *)
          Typed.Adt.mk_tuple [ ptr ];
          (* line: *)
          BV.u32i line;
          (* col: *)
          BV.u32i col;
          (* _filename: *)
          Typed.Adt.unit;
        ]
    in
    let@ () = with_alloc_kind ~kind:AnonConst in
    let* ptr = State.alloc_ty location_ty in
    let+ () = State.store ptr location_ty location in
    (ptr : Typed.T.sptr_f Typed.t :> Typed.([> T.sptr_f ] t))

  let carrying_mul_add ~t ~u ~multiplier ~multiplicand ~addend ~carry =
    let t = TypesUtils.ty_as_literal t in
    let u = TypesUtils.ty_as_literal u in
    let size_t = Layout.size_of_literal_ty t in
    let+ () =
      if size_t <> Layout.size_of_literal_ty u then
        not_impl "carrying_mul_add: size(U) != size(T)?"
      else ok ()
    in
    let double_bv = BV.extend ~signed:false (size_t * 8) in
    let multiplier = double_bv @@ Typed.cast_lit t multiplier in
    let multiplicand = double_bv @@ Typed.cast_lit t multiplicand in
    let addend = double_bv @@ Typed.cast_lit t addend in
    let carry = double_bv @@ Typed.cast_lit t carry in
    (* This cannot overflow as an *unsigned* 2n-bit operation:
     *   MAX * MAX + MAX + MAX
     *   => (2ⁿ-1) × (2ⁿ-1) + (2ⁿ-1) + (2ⁿ-1)
     *   => (2²ⁿ - 2ⁿ⁺¹ + 1) + (2ⁿ⁺¹ - 2)
     *   => 2²ⁿ - 1 *)
    let ( *!@ ) l r =
      BV.no_ovf_unsafe @@ BV.mul ~checked:(Typed.checked_of_signed false) l r
    in
    let ( +!@ ) l r =
      BV.no_ovf_unsafe @@ BV.add ~checked:(Typed.checked_of_signed false) l r
    in
    let res = (multiplier *!@ multiplicand) +!@ addend +!@ carry in
    let res_l, res_h =
      ( BV.extract 0 ((size_t * 8) - 1) res,
        BV.extract (size_t * 8) ((size_t * 16) - 1) res )
    in
    Typed.Adt.mk_tuple [ res_l; res_h ]

  let catch_unwind ~fun_exec ~t_data:_ ~try_fn:try_fn_ptr ~data
      ~catch_fn:catch_fn_ptr =
    let data = Typed.as_any data in
    let* trace = get_trace () in
    let[@inline] exec_fun msg fn args =
      with_extra_call_trace ~loc:(Trace.loc_or_default trace) ~msg
      @@ fun_exec fn args
    in
    let* try_fn = State.lookup_fn try_fn_ptr in
    let* catch_fn = State.lookup_fn catch_fn_ptr in
    exec_fun "catch_unwind try" try_fn [ data ]
    |> unwind_with
         ~f:(fun _ -> ok Typed.v_false)
         ~fe:(fun _ ->
           (* We can't use [null] here because this messes up with the niche of
              the return type, which checks if the pointer is 0! *)
           exec_fun "catch_unwind catch" catch_fn
             [ data; Typed.Ptr.of_address_f Usize.(1s) ]
           |> unwind_with
                ~f:(fun _ -> ok Typed.v_true)
                ~fe:(fun _ -> error (`StdErr "catch_unwind unwinded in catch")))

  (* HACK: complex float operations are heavily approximated, à la CBMC.

     See
     https://github.com/diffblue/cbmc/blob/develop/src/ansi-c/library/math.c *)
  let cos_ fp x =
    let* () = Core.floating_inaccuracy_warn () in
    match Typed.Float.approx Stdlib.Float.cos x with
    | Some res -> ok res
    | None ->
        let* res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
        let res = Typed.cast_float res in
        let+ () =
          if%sat Typed.Float.is_nan x ||@ Typed.Float.is_infinite x then
            assume_sym [ Typed.Float.is_nan res ]
          else
            assume_sym
              [
                res <=.@ Typed.Float.one fp;
                res >=.@ Typed.Float.neg (Typed.Float.one fp);
                Typed.not (x ==.@ Typed.Float.zero fp)
                ||@ (res ==.@ Typed.Float.one fp);
              ]
        in
        res

  let cosf16 ~x = cos_ F16 x
  let cosf32 ~x = cos_ F32 x
  let cosf64 ~x = cos_ F64 x
  let cosf128 ~x = cos_ F128 x

  let sin_ fp x =
    let* () = Core.floating_inaccuracy_warn () in
    match Typed.Float.approx Stdlib.Float.sin x with
    | Some res -> ok res
    | None ->
        let* res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
        let res = Typed.cast_float res in
        let+ () =
          if%sat Typed.Float.is_nan x ||@ Typed.Float.is_infinite x then
            assume_sym [ Typed.Float.is_nan res ]
          else
            assume_sym
              [
                res <=.@ Typed.Float.one fp;
                res >=.@ Typed.Float.neg (Typed.Float.one fp);
                Typed.not (x ==.@ Typed.Float.zero fp)
                ||@ (res ==.@ Typed.Float.zero fp);
              ]
        in
        res

  let sinf16 ~x = sin_ F16 x
  let sinf32 ~x = sin_ F32 x
  let sinf64 ~x = sin_ F64 x
  let sinf128 ~x = sin_ F128 x

  (* Largest exponent we expand; beyond this the term grows without buying much,
     so we fall back to a nondet. *)
  let max_pow_expansion = 64

  (* [x^n] by squaring, for a concrete [n]. This is what LLVM lowers a small
     [powi] to anyway, and unlike a nondet it preserves what the caller usually
     cares about -- that an even power is non-negative -- even when [x] is
     symbolic. Squaring keeps the term logarithmic in [n]; [acc] stays [None]
     until the first factor so that no identity multiply is emitted. *)
  let pow_by_mult fp (x : Typed.T.sfloat Typed.t) n =
    let rec go acc b i =
      if Z.equal i Z.zero then acc
      else
        let acc =
          if Z.is_odd i then
            Some (match acc with None -> b | Some a -> a *.@ b)
          else acc
        in
        go acc (b *.@ b) (Z.shift_right i 1)
    in
    let one = Typed.Float.one fp in
    let res = Option.value ~default:one (go None x (Z.abs n)) in
    if Z.lt n Z.zero then one /.@ res else res

  (** Ankerl's optimised [pow] approximation, as modelled by CBMC in
      [src/ansi-c/library/math.c] (function [pow]). See
      https://martin.ankerl.com/2007/10/04/optimized-pow-approximation-for-java-and-c-c/
  *)
  let pow_approx fp x y =
    let size = Typed.FloatPrecision.size fp in
    let word = Stdlib.min size 32 in
    let shift = size - word in
    let mant = Typed.FloatPrecision.significand_bits fp - 1 - shift in
    let exp_bits = Typed.FloatPrecision.exponent_bits fp in
    let bias =
      Z.mul (Z.shift_left Z.one mant)
        (Z.pred (Z.shift_left Z.one (exp_bits - 1)))
    in
    let tuning =
      Z.shift_right (Z.mul (Z.of_int 90253) (Z.shift_left Z.one mant)) 20
    in
    let* bits = Value_codec.float_to_bv_bits x in
    let lead = if shift = 0 then bits else BV.extract shift (size - 1) bits in
    let* c = lift_symex @@ Rustsymex.nondet (Typed.t_int word) in
    let* () =
      assume_sym
        [
          BV.leq ~signed:true (BV.mk_masked word (Z.neg tuning)) c;
          BV.leq ~signed:true c (BV.mki word 1);
        ]
    in
    (* wrapping is intended: this is bit arithmetic, not a Rust addition *)
    let base = BV.no_ovf_unsafe (BV.add (BV.mk_masked word bias) c) in
    let scaled =
      BV.to_float ~rounding:NearestTiesToEven ~signed:true ~fp
        (BV.no_ovf_unsafe (BV.sub lead base))
    in
    let mult = y *.@ scaled in
    (* the exponent field would overflow past this, so saturate as CBMC does *)
    let limit = Typed.Float.of_z fp (Z.shift_left Z.one 30) in
    if%sat Typed.Float.abs mult >.@ limit then
      if%sat y >.@ Typed.Float.zero fp then ok (Typed.Float.infinity fp)
      else ok (Typed.Float.zero fp)
    else
      let res_lead =
        BV.no_ovf_unsafe
          (BV.add
             (BV.of_float ~rounding:Truncate ~signed:true ~size:word mult)
             base)
      in
      let res_bits =
        if shift = 0 then res_lead else BV.concat res_lead (BV.zero shift)
      in
      ok (BV.to_float_raw res_bits)

  let pow_ fp x y =
    let* () = Core.floating_inaccuracy_warn () in
    match Typed.Float.to_float_opt y with
    | Some n
      when Stdlib.Float.is_integer n
           && Stdlib.Float.abs n <= Stdlib.float_of_int max_pow_expansion ->
        ok
          (Typed.cast_float
             (pow_by_mult fp (Typed.cast_float x) (Z.of_float n)))
    | _ -> (
        match Typed.Float.approx2 Stdlib.Float.pow x y with
        | Some res -> ok res
        | None ->
            (* the approximation reads [x]'s exponent field as a logarithm, so
               it only means anything for a positive, finite [x] *)
            if%sat
              x
              >.@ Typed.Float.zero fp
              &&@ Typed.not (Typed.Float.is_infinite x)
              &&@ Typed.not (Typed.Float.is_nan y)
            then pow_approx fp x y
            else
              let+ res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
              Typed.cast_float res)

  let powf16 ~a ~x = pow_ F16 a x
  let powf32 ~a ~x = pow_ F32 a x
  let powf64 ~a ~x = pow_ F64 a x
  let powf128 ~a ~x = pow_ F128 a x

  let powi_ fp x y =
    let* () = Core.floating_inaccuracy_warn () in
    let nondet () =
      let+ res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
      Typed.cast_float res
    in
    match
      Option.map (Typed.BitVec.bv_to_z (Signed I32)) (Typed.BitVec.to_z y)
    with
    | Some n when Z.leq (Z.abs n) (Z.of_int max_pow_expansion) ->
        ok (Typed.cast_float (pow_by_mult fp (Typed.cast_float x) n))
    | Some n -> (
        (* too many factors to expand, but [powi] is allowed to round
           differently from [powf], so the host's [pow] is a valid answer *)
        let n = Z.to_float n in
        match Typed.Float.approx (fun x -> Stdlib.Float.pow x n) x with
        | Some res -> ok res
        | None -> nondet ())
    | None -> nondet ()

  let powif16 ~a ~x = powi_ F16 a x
  let powif32 ~a ~x = powi_ F32 a x
  let powif64 ~a ~x = powi_ F64 a x
  let powif128 ~a ~x = powi_ F128 a x
  let sqrt_ ~x = ok (Typed.Float.sqrt x)
  let sqrtf16 = sqrt_
  let sqrtf32 = sqrt_
  let sqrtf64 = sqrt_
  let sqrtf128 = sqrt_
  let minimum_number_nsz ~x ~y = ok (Typed.Float.min x y)
  let minimum_number_nsz_f16 = minimum_number_nsz
  let minimum_number_nsz_f32 = minimum_number_nsz
  let minimum_number_nsz_f64 = minimum_number_nsz
  let minimum_number_nsz_f128 = minimum_number_nsz
  let maximum_number_nsz ~x ~y = ok (Typed.Float.max x y)
  let maximum_number_nsz_f16 = maximum_number_nsz
  let maximum_number_nsz_f32 = maximum_number_nsz
  let maximum_number_nsz_f64 = maximum_number_nsz
  let maximum_number_nsz_f128 = maximum_number_nsz
  let minimum_ ~x ~y = ok (Typed.Float.minimum x y)
  let minimumf16 = minimum_
  let minimumf32 = minimum_
  let minimumf64 = minimum_
  let minimumf128 = minimum_
  let maximum_ ~x ~y = ok (Typed.Float.maximum x y)
  let maximumf16 = maximum_
  let maximumf32 = maximum_
  let maximumf64 = maximum_
  let maximumf128 = maximum_

  (* A concrete argument is evaluated with the host's libm, which pins the
     result far more tightly than the constraints below; a symbolic one falls
     back to the range the function is known to lie in. *)
  let expf_ ~f fp x =
    let* () = Core.floating_inaccuracy_warn () in
    match Typed.Float.approx f x with
    | Some res -> ok res
    | None ->
        if%sat
          Typed.Float.is_nan x
          ||@ (Typed.Float.is_infinite x &&@ (x >.@ Typed.Float.zero fp))
        then ok (Typed.cast_float x)
        else if%sat Typed.Float.is_infinite x &&@ (x <.@ Typed.Float.zero fp)
        then ok (Typed.Float.zero fp)
        else
          let* res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
          let res = Typed.cast_float res in
          let+ () = assume_sym [ res >.@ Typed.Float.zero fp ] in
          res

  let expf16 ~x = expf_ ~f:Stdlib.Float.exp F16 x
  let expf32 ~x = expf_ ~f:Stdlib.Float.exp F32 x
  let expf64 ~x = expf_ ~f:Stdlib.Float.exp F64 x
  let expf128 ~x = expf_ ~f:Stdlib.Float.exp F128 x
  let exp2 = Stdlib.Float.pow 2.0
  let exp2f16 ~x = expf_ ~f:exp2 F16 x
  let exp2f32 ~x = expf_ ~f:exp2 F32 x
  let exp2f64 ~x = expf_ ~f:exp2 F64 x
  let exp2f128 ~x = expf_ ~f:exp2 F128 x

  let logf_ ~exp ~f fp x =
    let* () = Core.floating_inaccuracy_warn () in
    match Typed.Float.approx f x with
    | Some res -> ok res
    | None ->
        let exp = Typed.Float.mk fp exp in
        if%sat x <.@ Typed.Float.zero fp then ok (Typed.Float.nan fp)
        else if%sat x ==.@ Typed.Float.zero fp then
          ok (Typed.Float.neg_infinity fp)
        else if%sat Typed.Float.is_infinite x then ok (Typed.Float.infinity fp)
        else if%sat x ==.@ exp then ok (Typed.Float.one fp)
        else
          let* res = Value_codec.nondet_valid (TLiteral (TFloat fp)) in
          let res = Typed.cast_float res in
          let+ () =
            if%sat x <.@ exp then assume_sym [ res <.@ Typed.Float.one fp ]
            else assume_sym [ res >.@ Typed.Float.one fp ]
          in
          res

  let e_value = Stdlib.Float.(to_string @@ exp 1.0)
  let logf16 ~x = logf_ ~exp:e_value ~f:Stdlib.Float.log F16 x
  let logf32 ~x = logf_ ~exp:e_value ~f:Stdlib.Float.log F32 x
  let logf64 ~x = logf_ ~exp:e_value ~f:Stdlib.Float.log F64 x
  let logf128 ~x = logf_ ~exp:e_value ~f:Stdlib.Float.log F128 x
  let log10f16 ~x = logf_ ~exp:"10" ~f:Stdlib.Float.log10 F16 x
  let log10f32 ~x = logf_ ~exp:"10" ~f:Stdlib.Float.log10 F32 x
  let log10f64 ~x = logf_ ~exp:"10" ~f:Stdlib.Float.log10 F64 x
  let log10f128 ~x = logf_ ~exp:"10" ~f:Stdlib.Float.log10 F128 x
  let log2f16 ~x = logf_ ~exp:"2" ~f:Stdlib.Float.log2 F16 x
  let log2f32 ~x = logf_ ~exp:"2" ~f:Stdlib.Float.log2 F32 x
  let log2f64 ~x = logf_ ~exp:"2" ~f:Stdlib.Float.log2 F64 x
  let log2f128 ~x = logf_ ~exp:"2" ~f:Stdlib.Float.log2 F128 x
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
  let cold_path () : unit ret = ok ()

  let compare_bytes ~left ~right ~bytes =
    let l = Typed.Ptr.ptr_of left in
    let r = Typed.Ptr.ptr_of right in
    let zero = Usize.(0s) in
    let one = Usize.(1s) in
    let rec aux ?res ?(inc = one) l r len =
      if%sat len ==@ zero then ok @@ Option.value res ~default:U32.(0s)
      else
        let* l = Sptr.offset inc l in
        let* r = Sptr.offset inc r in
        let* bl = State.load (Typed.Ptr.of_ptr_t l) u8_ty in
        let* br = State.load (Typed.Ptr.of_ptr_t r) u8_ty in
        (* compare_bytes reads all bytes and mustn't short-circuit, so we must
           keep reading; here we only modify the result if we haven't reached a
           conclusion yet *)
        let* res =
          match res with
          | Some _ -> ok res
          | None ->
              let bl = Typed.cast_i U8 bl in
              let br = Typed.cast_i U8 br in
              if%sat bl ==@ br then ok None
              else if%sat bl <@ br then ok (Some U32.(-1s))
              else ok (Some U32.(1s))
        in
        aux ?res l r (len -!@ one)
    in
    aux ~inc:zero l r (bytes :> T.sint Typed.t)

  (** [check_overlap name l r size] ensures the pointers [l] and [r] do not
      overlap for a range of size [size]; otherwise errors, with
      [`StdErr (name ^ " overlapped")].

      We optimise the path where pointers don't have the same provenance to
      avoid spuriously decaying pointers. *)
  let check_overlap name l r size =
    let l = Typed.Ptr.ptr_of l in
    let r = Typed.Ptr.ptr_of r in
    let same_provenance = Typed.Ptr.have_same_provenance l r in
    if%sure not same_provenance then ok ()
    else
      let* l_end = Sptr.offset size l in
      let* r_end = Sptr.offset size r in
      let* dist1 = Sptr.distance l r_end in
      let* dist2 = Sptr.distance r l_end in
      let zero = Usize.(0s) in
      assert_not
        (same_provenance &&@ (dist1 <$@ zero &&@ (dist2 <$@ zero)))
        (`StdErr (name ^ " overlapped"))

  let copy_ nonoverlapping ~t ~src ~dst ~count : unit ret =
    [%l.debug
      "Performing copy%s: %a -> %a, count %a"
        (if nonoverlapping then "_non_overlapping" else "")
        Typed.ppa src Typed.ppa dst Typed.ppa count];
    let zero = Usize.(0s) in
    let* ty_size = Layout.size_of t in
    if%sat ty_size ==@ zero ||@ (count ==@ zero) then
      let* () = Sptr.check_aligned src t in
      let* () = Sptr.check_aligned dst t in
      ok ()
    else
      let size, overflowed = ty_size *?@ count in
      let* () = assert_not overflowed `Overflow in
      let* () = Sptr.check_non_dangling_untyped (Typed.Ptr.ptr_of src) size in
      let* () = Sptr.check_non_dangling_untyped (Typed.Ptr.ptr_of dst) size in
      (* Here we can cheat a little: for copy_nonoverlapping we need to check
         for overlap, but otherwise the copy is the exact same; since the State
         makes a copy of the src tree before storing into dst, the semantics are
         that of copy. *)
      let* () =
        if nonoverlapping then check_overlap "copy_nonoverlapping" src dst size
        else ok ()
      in
      State.copy_nonoverlapping ~dst ~src ~size

  let copy ~t ~src ~dst ~count = copy_ false ~t ~src ~dst ~count
  let copy_nonoverlapping ~t ~src ~dst ~count = copy_ true ~t ~src ~dst ~count

  let typed_swap_nonoverlapping ~t ~x:from ~y:to_ =
    let* size = Layout.size_of t in
    let* () = check_overlap "typed_swap_nonoverlapping" from to_ size in
    let* v_l = State.load from t in
    let* v_r = State.load to_ t in
    let* () = State.store from t v_r in
    State.store to_ t v_l

  let copy_sign ~x ~y =
    if%sat Typed.Float.is_negative y then
      ok (Typed.Float.neg (Typed.Float.abs x))
    else ok (Typed.Float.abs x)

  let copysignf128 = copy_sign
  let copysignf64 = copy_sign
  let copysignf32 = copy_sign
  let copysignf16 = copy_sign

  (** Applies either [concrete] or [symbolic] to a bitvector. *)
  let binary_int_operation ~concrete ~symbolic ~t ~x : [> T.sint ] Typed.t ret =
    let t = TypesUtils.ty_as_literal t in
    let bits = 8 * Layout.size_of_literal_ty t in
    let x = Typed.cast_lit t x in
    match BV.to_z x with
    | Some z -> ok (concrete bits z)
    | None -> ok (symbolic bits x)

  let ctpop ~t ~x =
    let concrete _bits x = BV.u32i @@ Z.popcount x in
    let symbolic bits x =
      let n =
        Iter.fold
          (fun acc off ->
            let bit = BV.extract off off x in
            let bit32 = BV.extend ~signed:false 31 bit in
            acc +!@ bit32)
          U32.(0s)
          Iter.(0 -- (bits - 1))
      in
      (n : Typed.T.sint Typed.t :> Typed.([> T.sint ] t))
    in
    binary_int_operation ~concrete ~symbolic ~t ~x

  let cttz ~t ~x =
    let concrete bits x =
      BV.u32i @@ if Z.equal x Z.zero then bits else Z.trailing_zeros x
    in
    (* we construct the following, from inside out:
     *   ite(x[0] == 1 ? 0 :
     *     ite(x[1] == 1 ? 1 :
     *       ...
     *       ite(x[bits-1] == 1 ? bits-1 : bits))) *)
    let symbolic bits x =
      Iter.fold
        (fun acc off ->
          let off = bits - 1 - off in
          let bit = BV.extract off off x in
          Typed.ite (bit ==@ BV.one 1) (BV.u32i off) acc)
        (BV.u32i bits)
        Iter.(0 -- (bits - 1))
    in
    binary_int_operation ~concrete ~symbolic ~t ~x

  let cttz_nonzero ~t ~x =
    let tlit = TypesUtils.ty_as_literal t in
    let x_int = Typed.cast_lit tlit x in
    let* () =
      assert_not
        (x_int ==@ BV.mki_lit tlit 0)
        (`StdErr "core::intrinsics::cttz_nonzero on zero")
    in
    cttz ~t ~x

  let ctlz ~t ~x =
    let concrete bits x =
      let rec aux n =
        if Z.testbit x (bits - 1 - n) then n
        else if n + 1 < bits then aux (n + 1)
        else bits
      in
      BV.u32i @@ aux 0
    in
    (* we construct the following, from inside out:
     *   ite(x[bits-1] == 1 ? 0 :
     *     ite(x[bits-2] == 1 ? 1 :
     *       ...
     *       ite(x[0] == 1 ? bits-1 : bits))) *)
    let symbolic bits x =
      Iter.fold
        (fun acc off ->
          let res = bits - 1 - off in
          let bit = BV.extract off off x in
          Typed.ite (bit ==@ BV.one 1) (BV.u32i res) acc)
        (BV.u32i bits)
        Iter.(0 -- (bits - 1))
    in
    binary_int_operation ~concrete ~symbolic ~t ~x

  let ctlz_nonzero ~t ~x =
    let tlit = TypesUtils.ty_as_literal t in
    let x_int = Typed.cast_lit tlit x in
    let* () =
      assert_not
        (x_int ==@ BV.mki_lit tlit 0)
        (`StdErr "core::intrinsics::ctlz_nonzero on zero")
    in
    ctlz ~t ~x

  let discriminant_value ~t ~v =
    let adt = ty_as_adt t in
    if Crate.is_enum adt then State.load_discriminant v t
    (* FIXME: this size is probably wrong *)
      else ok U8.(0s)

  let disjoint_bitor ~t ~a ~b =
    let ty = TypesUtils.ty_as_literal t in
    let a = Typed.cast_lit ty a in
    let b = Typed.cast_lit ty b in
    let+ () =
      assert_
        (a &@ b ==@ BV.mki_lit ty 0)
        (`StdErr "core::intrinsics::disjoint_bitor with overlapping bits")
    in
    a |@ b

  let exact_div ~t ~x ~y =
    let lit = TypesUtils.ty_as_literal t in
    let x = Typed.cast_lit lit x in
    let y = Typed.cast_lit lit y in
    let* res = Core.eval_lit_binop (Div OUB) lit x y in
    let zero = BV.mki_lit lit 0 in
    let ( %@ ) = BV.rem ~signed:(Layout.is_signed lit) in
    let+ () =
      assert_
        (Typed.not (y ==@ zero) &&@ (x %@ Typed.cast_nonzero y ==@ zero))
        (`StdErr "core::intrinsics::exact_div on non divisible")
    in
    res

  let fabs ~t ~x =
    let t = TypesUtils.ty_as_literal t in
    let f =
      match t with TFloat f -> f | _ -> L.failwith "fabs with non-float?"
    in
    let x = Typed.cast_f f x in
    ok (Typed.Float.abs x)

  let float_bop (bop : Expressions.binop) =
    match bop with
    | Add _ -> ((fun x y -> ok (x +.@ y)), "fadd")
    | Sub _ -> ((fun x y -> ok (x -.@ y)), "fsub")
    | Mul _ -> ((fun x y -> ok (x *.@ y)), "fmul")
    | Div _ -> ((fun x y -> ok (x /.@ y)), "fdiv")
    | Rem _ ->
        ( (fun x y ->
            let+ r = Value_codec.optimised_rem x y in
            Typed.Float.fmod_of_rem r x y),
          "frem" )
    | _ -> L.failwith "float_bop: invalid binop"

  let float_fast (bop : Expressions.binop) ~(t : Types.ty) ~a ~b =
    let t = ty_as_float t in
    let l = Typed.cast_f t a in
    let r = Typed.cast_f t b in
    let bop, name = float_bop bop in
    let is_finite f =
      Typed.((not (Float.is_nan f)) &&@ not (Float.is_infinite f))
    in
    let* res = bop l r in
    let+ () =
      assert_
        (is_finite l &&@ is_finite r &&@ is_finite res)
        (`StdErr
           (Fmt.str
              "core::intrinsics::%s_fast: operands and result must be finite"
              name))
    in
    Typed.as_any res

  let float_algebraic (bop : Expressions.binop) ~(t : Types.ty) ~a ~b =
    let t = ty_as_float t in
    let l = Typed.cast_f t a in
    let r = Typed.cast_f t b in
    let bop, _ = float_bop bop in
    let+ res = bop l r in
    Typed.as_any res

  let fadd_fast ~t ~a ~b = float_fast (Add OUB) ~t ~a ~b
  let fdiv_fast ~t ~a ~b = float_fast (Div OUB) ~t ~a ~b
  let fmul_fast ~t ~a ~b = float_fast (Mul OUB) ~t ~a ~b
  let frem_fast ~t ~a ~b = float_fast (Rem OUB) ~t ~a ~b
  let fsub_fast ~t ~a ~b = float_fast (Sub OUB) ~t ~a ~b
  let fadd_algebraic ~t ~a ~b = float_algebraic (Add OUB) ~t ~a ~b
  let fdiv_algebraic ~t ~a ~b = float_algebraic (Div OUB) ~t ~a ~b
  let fmul_algebraic ~t ~a ~b = float_algebraic (Mul OUB) ~t ~a ~b
  let frem_algebraic ~t ~a ~b = float_algebraic (Rem OUB) ~t ~a ~b
  let fsub_algebraic ~t ~a ~b = float_algebraic (Sub OUB) ~t ~a ~b

  let float_to_int_unchecked ~float ~int ~value =
    let fty = ty_as_float float in
    let ity = TypesUtils.ty_as_literal int in
    let f = Typed.cast_f fty value in
    let* () =
      assert_not
        (Typed.Float.is_nan f ||@ Typed.Float.is_infinite f)
        (`StdErr "float_to_int_unchecked with NaN or infinite value")
    in
    let signed = Layout.is_signed ity in
    let size = 8 * Layout.size_of_literal_ty ity in
    let max = Typed.Float.of_z fty (Z.succ @@ Layout.max_value_z ity) in
    let min = Typed.Float.of_z fty (Z.pred @@ Layout.min_value_z ity) in
    (* we use min-1 and max+1, to be able to have a strict inequality, which
       avoids issues in cases of float precision loss (I think?) *)
    let+ () =
      assert_
        (min <.@ f &&@ (f <.@ max))
        (`StdErr "float_to_int_unchecked out of int range")
    in
    BV.of_float ~rounding:Truncate ~signed ~size f

  let fmul_add ~a ~b ~c = ok (Typed.Float.fma a b c)
  let fmaf16 = fmul_add
  let fmaf32 = fmul_add
  let fmaf64 = fmul_add
  let fmaf128 = fmul_add
  let fmuladdf16 = fmul_add
  let fmuladdf32 = fmul_add
  let fmuladdf64 = fmul_add
  let fmuladdf128 = fmul_add
  let forget ~t:_ ~arg:_ = ok ()

  let is_val_statically_known_ux =
    String.Interned.intern
      "core::intrinsics::is_val_statically_known was stubbed to always return \
       false, to avoid path explosion. This is an under-approximation, some \
       paths may be missed."

  (* UX: the caller must be correct for either answer, so a symbolic one only
     doubles the paths -- and [format!] asks for one per call. We answer false,
     like the fallback body Rust ships. See:
     https://doc.rust-lang.org/std/intrinsics/fn.is_val_statically_known.html *)
  let is_val_statically_known ~t:_ ~arg:_ =
    if Soteria.Symex.Approx.As_ctx.is_ox () then
      Soteria.Terminal.Warn.warn_once is_val_statically_known_ux;
    ok Typed.v_false

  let float_minmax ~is_min ~x ~y : T.sfloat Typed.t ret =
    let x = (x :> T.sfloat Typed.t) in
    let y = (y :> T.sfloat Typed.t) in
    if%sat Typed.Float.is_nan x then ok y
    else if%sat Typed.Float.is_nan y then ok x
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
  let prefetch_read_data ~t:_ ~locality:_ ~data:_ = ok ()
  let prefetch_read_instruction ~t:_ ~locality:_ ~data:_ = ok ()
  let prefetch_write_data ~t:_ ~locality:_ ~data:_ = ok ()
  let prefetch_write_instruction ~t:_ ~locality:_ ~data:_ = ok ()

  let ptr_guaranteed_cmp ~t ~ptr ~other =
    Core.eval_ptr_binop ~pointee:t Eq ptr other

  let ptr_mask ~t ~ptr:ptr_full ~mask =
    let ptr = Typed.Ptr.ptr_of ptr_full in
    let+ addr = Sptr.decay ptr in
    let addr = addr &@ mask in
    Typed.Ptr.with_ptr ptr_full (Typed.Ptr.of_address addr)

  let ptr_offset_from_ ~unsigned ~t ~ptr ~base : [> T.sint ] Typed.t ret =
    let zero = Usize.(0s) in
    let* size = Layout.size_of t in
    let* () =
      assert_not (size ==@ zero) (`Panic (Some "ptr_offset_from with ZST"))
    in
    let size = Typed.cast_nonzero size in
    let ptr_in = Typed.Ptr.ptr_of ptr in
    let base_in = Typed.Ptr.ptr_of base in
    let* off = Sptr.distance ptr_in base_in in
    (* If the pointers are not equal, they mustn't be dangling *)
    let* () =
      if%sat off ==@ zero then ok ()
      else
        let* () = Sptr.check_non_dangling_untyped base_in off in
        Sptr.check_non_dangling_untyped ptr_in (cast ~-off)
    in
    (* UB conditions:
     * 1. must be at the same address, OR derived from the same allocation
     * 2. the distance must be a multiple of sizeof(T) *)
    let same_addr_or_alloc =
      off ==@ zero ||@ Typed.Ptr.have_same_provenance ptr_in base_in
    in
    let distance_multiple = off %$@ size ==@ zero in
    let* () =
      assert_ (same_addr_or_alloc &&@ distance_multiple) `UBPointerComparison
    in
    (* we cast to ignore the overflow for MIN/-1, since the size can't be -1 *)
    if unsigned then
      let+ () =
        assert_ (off >=$@ zero)
          (`StdErr "core::intrinsics::offset_from_unsigned negative offset")
      in
      BV.no_ovf_unsafe (off /$@ size)
    else ok (BV.no_ovf_unsafe (off /$@ size))

  let ptr_offset_from ~t ~ptr ~base =
    ptr_offset_from_ ~unsigned:false ~t ~ptr ~base

  let ptr_offset_from_unsigned ~t ~ptr ~base =
    ptr_offset_from_ ~unsigned:true ~t ~ptr ~base

  let raw_eq ~t ~a ~b =
    let* layout = Layout.layout_of t in
    let* size =
      of_opt_not_impl "raw_eq with nondet size" @@ BV.to_z layout.size
    in
    let bytes = mk_array_ty u8_ty size in
    (* TODO: figure out if for these two reads we should ignore the modified
       state, as its leaves may be split in bytes which will require ugly
       transmutations to be read from again later. *)
    let* l = State.load a bytes in
    let* r = State.load b bytes in
    let l = Typed.cast_array l in
    let r = Typed.cast_array r in
    Iter.of_iarray2 (Typed.Adt.as_array l) (Typed.Adt.as_array r)
    |> Iter.fold
         (fun acc (l, r) ->
           Typed.and_lazy acc (fun () ->
               Typed.cast_i U8 l ==@ Typed.cast_i U8 r))
         Typed.v_true
    |> fun b -> ok (b : Typed.T.sbool Typed.t :> Typed.([> T.sbool ] t))

  let rotate_ ~(side : [ `Left | `Right ]) ~t ~x ~shift =
    let t = TypesUtils.ty_as_literal t in
    let bits = 8 * Layout.size_of_literal_ty t in
    let x = Typed.cast_lit t x in
    match BV.to_z shift with
    | Some shift ->
        let shift = Z.(to_int (shift mod of_int bits)) in
        if shift = 0 then ok x
        else
          let shift = if side = `Left then shift else bits - shift in
          let high = BV.extract (bits - shift) (bits - 1) x in
          let low = BV.extract 0 (bits - shift - 1) x in
          let res = BV.concat low high in
          ok res
    | None ->
        let bits' = BV.mki_nz bits bits in
        (* we need shift to be of size [bits] (it originally is of size 32) *)
        let shift =
          if bits <= 32 then BV.extract 0 (bits - 1) shift
          else BV.extend ~signed:false (bits - 32) shift
        in
        let shift = BV.no_ovf_unsafe @@ BV.rem ~signed:false shift bits' in
        let res =
          if side = `Left then x <<@ shift |@ (x >>@ bits' -!@ shift)
          else x >>@ shift |@ (x <<@ bits' -!@ shift)
        in
        ok res

  let rotate_left ~t ~x ~shift = rotate_ ~side:`Left ~t ~x ~shift
  let rotate_right ~t ~x ~shift = rotate_ ~side:`Right ~t ~x ~shift

  let saturating (op : Expressions.binop) ~t ~a ~b =
    let t = TypesUtils.ty_as_literal t in
    let signed = Layout.is_signed t in
    let a = Typed.cast_lit t a in
    let b = Typed.cast_lit t b in
    let max = BV.mk_lit t @@ Layout.max_value_z t in
    let min = BV.mk_lit t @@ Layout.min_value_z t in
    let res =
      match op with
      | Add _ ->
          let ovf = BV.add_overflows ~signed a b in
          let if_ovf =
            if signed then Typed.ite (a <$@ BV.mki_lit t 0) min max else max
          in
          let res = BV.add ~checked:(Typed.checked_of_signed signed) a b in
          Typed.ite ovf if_ovf (BV.no_ovf_unsafe res)
      | Sub _ ->
          let ovf = BV.sub_overflows ~signed a b in
          let if_ovf = if signed then Typed.ite (a <$@ b) min max else min in
          let res = BV.sub ~checked:(Typed.checked_of_signed signed) a b in
          Typed.ite ovf if_ovf (BV.no_ovf_unsafe res)
      | _ -> L.failwith "Unreachable: not add or sub?"
    in
    ok res

  let saturating_add ~t ~a ~b = saturating (Add OUB) ~t ~a ~b
  let saturating_sub ~t ~a ~b = saturating (Sub OUB) ~t ~a ~b

  let select_unpredictable ~t:_ ~b ~true_val ~false_val =
    let b = (b :> T.sbool Typed.t) in
    if%sat b then ok @@ Typed.as_any true_val else ok @@ Typed.as_any false_val

  let size_of ~t = Layout.size_of t

  let size_of_val ~t ~ptr =
    let+ size, _ = State.size_and_align_of_val t ptr in
    size

  let align_of_val ~t ~ptr =
    let+ _, align = State.size_and_align_of_val t ptr in
    align

  let transmute ~t_src ~dst ~src = State.transmute ~from:t_src ~to_:dst src

  let type_name ~t =
    let str = Fmt.to_to_string pp_ty t in
    Core.string_to_ptr str

  let type_id_eq ~a ~b =
    let words id =
      Typed.Adt.as_tuple1 id |> Typed.cast_array |> Typed.Adt.as_array
    in
    let a = words a in
    let b = words b in
    Iter.of_iarray2 a b
    |> fold_iter ~init:Typed.v_true ~f:(fun acc (a, b) ->
        let* a = Sptr.decay Typed.(Ptr.ptr_of (cast_ptr_f a)) in
        let+ b = Sptr.decay Typed.(Ptr.ptr_of (cast_ptr_f b)) in
        acc &&@ (a ==@ b))
    |> map (fun b -> Typed.((b : T.sbool t :> [> T.sbool ] t)))

  let type_id_of_unknown name =
    not_impl ~issue:187 "compile-time TypeId reflection is not supported (%s)"
      name

  let size_of_type_id ~id:_ = type_id_of_unknown "size_of_type_id"

  let type_id_fields ~id:_ ~variant_index:_ =
    type_id_of_unknown "type_id_fields"

  let type_id_variants ~id:_ = type_id_of_unknown "type_id_variants"
  let type_id_vtable ~id:_ ~trait:_ = type_id_of_unknown "type_id_vtable"
  let type_of ~id:_ = type_id_of_unknown "type_of"

  let type_id_field_representing_type ~id:_ ~variant_index:_ ~field_index:_ =
    type_id_of_unknown "type_id_field_representing_type"

  let unchecked_op op ~t ~x ~y =
    let t = TypesUtils.ty_as_literal t in
    let x = Typed.cast_lit t x in
    let y = Typed.cast_lit t y in
    let+ res = Core.eval_lit_binop op t x y in
    res

  let funnel_ ~(side : [ `Left | `Right ]) ~t ~a ~b ~shift =
    let t = TypesUtils.ty_as_literal t in
    let bits = 8 * Layout.size_of_literal_ty t in
    let a = Typed.cast_lit t a in
    let b = Typed.cast_lit t b in
    let+ () = assert_ (shift <@ BV.mki 32 bits) `InvalidShift in
    let bits' = BV.mki_nz bits bits in
    let shift =
      if bits <= 32 then BV.extract 0 (bits - 1) shift
      else if bits > 32 then BV.extend ~signed:false (bits - 32) shift
      else (shift :> T.sint Typed.t)
    in
    let inv = bits' -!!@ shift in
    match side with
    | `Left -> a <<@ shift |@ (b >>@ inv)
    | `Right -> b >>@ shift |@ (a <<@ inv)

  let unchecked_funnel_shl ~t ~a ~b ~shift = funnel_ ~side:`Left ~t ~a ~b ~shift

  let unchecked_funnel_shr ~t ~a ~b ~shift =
    funnel_ ~side:`Right ~t ~a ~b ~shift

  let unchecked_add ~t ~x ~y = unchecked_op (Add OUB) ~t ~x ~y
  let unchecked_div ~t ~x ~y = unchecked_op (Div OUB) ~t ~x ~y
  let unchecked_mul ~t ~x ~y = unchecked_op (Mul OUB) ~t ~x ~y
  let unchecked_rem ~t ~x ~y = unchecked_op (Rem OUB) ~t ~x ~y
  let unchecked_shl ~t ~u:_ ~x ~y = unchecked_op (Shl OUB) ~t ~x ~y
  let unchecked_shr ~t ~u:_ ~x ~y = unchecked_op (Shr OUB) ~t ~x ~y
  let unchecked_sub ~t ~x ~y = unchecked_op (Sub OUB) ~t ~x ~y

  let variant_count ~t =
    match (t : Types.ty) with
    | TAdt adt when Crate.is_enum adt ->
        let variants = Crate.as_enum adt in
        ok (BV.usizei (List.length variants))
    | _ -> error (`StdErr "core::intrinsics::variant_count used with non-enum")

  let read_vtable ~slot ~ptr : [> T.sint ] Typed.t ret =
    let ptr = Typed.Ptr.ptr_of ptr in
    let* ptr =
      Sptr.offset ~check_signed:true ~ty:(TLiteral (TUInt Usize))
        (BV.usizei slot) ptr
    in
    let+ align = State.load (Typed.Ptr.of_ptr_t ptr) (TLiteral (TUInt Usize)) in
    Typed.cast_i Usize align

  let vtable_align ~ptr = read_vtable ~slot:2 ~ptr
  let vtable_size ~ptr = read_vtable ~slot:1 ~ptr

  let wrapping_op op ~t ~a ~b =
    let ity = TypesUtils.ty_as_literal t in
    let a = Typed.cast_lit ity a in
    let b = Typed.cast_lit ity b in
    let+ res = Core.eval_lit_binop op ity a b in
    res

  let wrapping_add ~t ~a ~b = wrapping_op (Add OWrap) ~t ~a ~b
  let wrapping_mul ~t ~a ~b = wrapping_op (Mul OWrap) ~t ~a ~b
  let wrapping_sub ~t ~a ~b = wrapping_op (Sub OWrap) ~t ~a ~b

  let write_bytes ~t ~dst ~val_ ~count =
    let zero = Usize.(0s) in
    let* () = Sptr.check_aligned dst t in
    let* size = Layout.size_of t in
    let size, overflowed = size *?@ count in
    let* () = assert_not overflowed `Overflow in
    (* Some optimisations we do to make this a bit more efficient:
     * - if the size is zero, we don't need to do anything
     * - if the value must be zero, we can just zero the memory over the range
     * - TODO: if [val_] is concrete, we can initialise one large bitvector and
     *   store it in one go.
     * - Finally, if the size is concrete we iterate byte-per-byte. We don't
     *   handle the symbolic size case yet.
     *)
    if%sat size ==@ zero then ok ()
    else if%sure val_ ==@ U8.(0s) then State.zeros dst (BV.cast_nonzero size)
    else
      let* bytes =
        of_opt_not_impl "write_bytes: don't know how to handle symbolic sizes"
        @@ BV.to_z size
      in
      let ptr = Typed.Ptr.ptr_of dst in
      iter_iter
        Iter.(0 -- (Z.to_int bytes - 1))
        ~f:(fun i ->
          let off = BV.usizei i in
          let* ptr = Sptr.offset ~check_signed:true off ptr in
          State.store (Typed.Ptr.of_ptr_t ptr) u8_ty val_)

  let volatile_load ~t ~src = State.load src t
  let volatile_set_memory = write_bytes
  let volatile_copy_memory ~t ~dst ~src ~count = copy ~t ~src ~dst ~count

  let volatile_copy_nonoverlapping_memory ~t ~dst ~src ~count =
    copy_nonoverlapping ~t ~src ~dst ~count

  let volatile_store ~t ~dst ~val_ = State.store dst t val_

  (* ---- SIMD ---- *)

  (* A SIMD vector is a `#[repr(simd)]` struct wrapping a `[T; N]` array, so its
     value is [Tuple [ Tuple lanes ]]. *)
  let simd_lanes_with cast (lanes : Typed.([< T.any ] t)) =
    match%ty lanes with
    | TExtension (TTuple [ TExtension (TArray _) ]) ->
        let wrapper = Typed.Adt.as_tuple1 @@ lanes in
        let elems = Typed.Adt.as_array @@ Typed.cast_array wrapper in
        ok (Iarray.map cast elems)
    | _ ->
        not_impl
          "unsupported SIMD type definition, expected a struct wrapping a [T; \
           N] array"

  let simd_lanes elem_ty = simd_lanes_with (Typed.cast_lit elem_ty)
  let simd_float_lanes fty = simd_lanes_with (Typed.cast_f fty)

  let simd_of_lanes ty lanes =
    Typed.Adt.mk_tuple [ Typed.Adt.mk_array (TLiteral ty) lanes ]

  (* The element type of a SIMD vector, i.e. the [T] in its `[T; N]` field. *)
  let simd_elem_lit (ty : Types.ty) : Types.literal_type =
    match Crate.as_struct_tys (ty_as_adt ty) with
    | [ TArray (TLiteral lit, _) ] -> lit
    | _ -> Fmt.failwith "expected a SIMD vector type, got %a" pp_ty ty

  (* The number of lanes [N] of a SIMD vector. *)
  let simd_len (ty : Types.ty) =
    match Crate.as_struct_tys (ty_as_adt ty) with
    | [ TArray (_, len) ] ->
        let* len =
          of_opt_not_impl "simd_len with generic length"
            (BV.of_constant_expr_opt len)
        in
        let+ len =
          of_opt_not_impl "simd_len with symbolic length" (BV.to_z len)
        in
        Z.to_int len
    | _ -> Fmt.failwith "expected a SIMD vector type, got %a" pp_ty ty

  (* Elementwise comparison producing a mask: each lane of the result is
     all-ones (if the comparison holds) or all-zeros, with the width of [u]'s
     element. *)
  let simd_cmp ~int_cmp ~float_cmp ~t ~u ~x ~y =
    let elt = simd_elem_lit t in
    let mask = simd_elem_lit u in
    let width = 8 * Layout.size_of_literal_ty mask in
    let ones = BV.mk width Z.(pred (one lsl width)) in
    let zeros = BV.mki width 0 in
    let lane cmp a b = Typed.ite (cmp a b) ones zeros in
    match elt with
    | TFloat fty ->
        let* x = simd_float_lanes fty x in
        let+ y = simd_float_lanes fty y in
        simd_of_lanes mask (Iarray.map2 (lane float_cmp) x y)
    | _ ->
        let* x = simd_lanes elt x in
        let+ y = simd_lanes elt y in
        simd_of_lanes mask (Iarray.map2 (lane int_cmp) x y)

  let simd_eq ~t ~u ~x ~y =
    simd_cmp ~int_cmp:Typed.sem_eq ~float_cmp:Typed.Float.eq ~t ~u ~x ~y

  let simd_ne ~t ~u ~x ~y =
    simd_cmp
      ~int_cmp:(fun a b -> Typed.not (a ==@ b))
      ~float_cmp:(fun a b -> Typed.not (Typed.Float.eq a b))
      ~t ~u ~x ~y

  let simd_ordered_cmp int_cmp float_cmp ~t ~u ~x ~y =
    let int_cmp a b =
      int_cmp ~signed:(Layout.is_signed (simd_elem_lit t)) a b
    in
    simd_cmp ~int_cmp ~float_cmp:(fun a b -> float_cmp a b) ~t ~u ~x ~y

  let simd_ge ~t ~u ~x ~y = simd_ordered_cmp BV.geq Typed.Float.geq ~t ~u ~x ~y
  let simd_gt ~t ~u ~x ~y = simd_ordered_cmp BV.gt Typed.Float.gt ~t ~u ~x ~y
  let simd_le ~t ~u ~x ~y = simd_ordered_cmp BV.leq Typed.Float.leq ~t ~u ~x ~y
  let simd_lt ~t ~u ~x ~y = simd_ordered_cmp BV.lt Typed.Float.lt ~t ~u ~x ~y

  (* Elementwise binary operation over two vectors of the same shape. *)
  let simd_binop t op x y =
    let lit = simd_elem_lit t in
    let lane a b = op a b in
    let* x = simd_lanes lit x in
    let+ y = simd_lanes lit y in
    simd_of_lanes lit (Iarray.map2 lane x y)

  let simd_and ~t ~x ~y = simd_binop t BV.and_ x y
  let simd_or ~t ~x ~y = simd_binop t BV.or_ x y
  let simd_xor ~t ~x ~y = simd_binop t BV.xor x y
  let simd_add ~t ~x ~y = simd_binop t ( +!@ ) x y
  let simd_mul ~t ~x ~y = simd_binop t ( *!@ ) x y
  let simd_sub ~t ~lhs ~rhs = simd_binop t ( -!@ ) lhs rhs
  let simd_shl ~t ~lhs ~rhs = simd_binop t BV.shl lhs rhs

  let simd_shr ~t ~lhs ~rhs =
    let op = if Layout.is_signed (simd_elem_lit t) then BV.ashr else BV.lshr in
    simd_binop t op lhs rhs

  (* Elementwise unary operation over a vector. *)
  let simd_unop t op x =
    let lit = simd_elem_lit t in
    let+ x = simd_lanes lit x in
    simd_of_lanes lit (Iarray.map op x)

  let simd_neg ~t ~x = simd_unop t ( ~-! ) x

  let simd_extract ~t:_ ~u ~x ~idx =
    let* idx =
      of_opt_not_impl "Using a symbolic index on simd_extract" (BV.to_z idx)
    in
    let lit = TypesUtils.ty_as_literal u in
    let+ x = simd_lanes lit x in
    Iarray.get x (Z.to_int idx)

  let simd_splat ~t ~u ~value =
    let ty = TypesUtils.ty_as_literal u in
    let+ len = simd_len t in
    simd_of_lanes ty (Iarray.init len (fun _ -> value))
end
