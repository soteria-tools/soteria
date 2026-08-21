open Svalue
open Common.Charon_util

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax
  open Typed.Infix
  open Typed.Syntax
  module Core = Core.M (StateM)
  module Alloc = Extern.Alloc.M (StateM)

  let do_panic ?msg () =
    match msg with
    | Some msg ->
        let* msg = Core.parse_string msg in
        error (`Panic msg)
    | _ -> error (`Panic None)

  (* ---- alloc ---- *)

  (** {@rust[
        const fn alloc_impl(&self, layout: Layout, zeroed: bool)
          -> Result<NonNull<[u8]>, AllocError> { ... }
      ]}

      with
      {@rust[
        pub struct Layout {
            size: usize,
            align: Alignment,
        }

        pub struct Alignment {
            _inner_repr_trick: AlignmentEnum,
        }

        enum AlignmentEnum { ... }
      ]} *)
  let alloc_impl ~(fun_sig : Charon.Types.fun_sig) ~self:_ ~layout ~zeroed =
    let zeroed = (zeroed :> Typed.T.sbool Typed.t) in
    let size, align =
      let size, align = Typed.Adt.as_tuple2 (Typed.cast_tuple layout) in
      let size = Typed.cast_i Usize size in
      let align_enum =
        Typed.cast_enum (Typed.Adt.as_tuple1 (Typed.cast_tuple align))
      in
      let align = Typed.Adt.discriminant_of align_enum in
      (size, Typed.cast_i Usize align)
    in
    let mk_res ptr len =
      let out_res = ty_as_adt fun_sig.output in
      let ptr = Typed.Ptr.mk_ptr_f ptr len in
      let nonnull = Typed.Adt.mk_tuple [ ptr ] in
      Typed.Adt.Checked.mk_enum out_res "Ok" [ nonnull ]
    in
    if%sat size ==@ Usize.(0s) then
      let dangling = Typed.Ptr.of_address align in
      ok (mk_res dangling Usize.(0s))
    else
      let* zeroed = if%sat zeroed then ok true else ok false in
      let+ ptr = Alloc.alloc ~zeroed [ size; align ] in
      let ptr = Typed.Ptr.ptr_of (Typed.cast_ptr_f ptr) in
      mk_res ptr size

  let align_offset_ux =
    String.Interned.intern
      "std::ptr::align_offset was stubbed to avoid path explosion. This is an \
       under-approximation, some paths may be missed."

  (** {@rust[
        pub(crate) const unsafe fn align_offset<T: Sized>(p: *const T, a: usize) -> usize
      ]}

      The standard library computes this from the pointer's address, which
      forces us to decay the pointer and makes every subsequent operation on the
      result symbolic, leading to path explosion.

      We can do better, and answer from what we know of the pointer instead: its
      address is congruent to its offset modulo the alignment of its allocation,
      so while [a] doesn't exceed that alignment the offset alone answers; and
      an address known outright answers directly. Failing both, we look for a
      small offset that {e provably} aligns the pointer, and failing that we
      under-approximate: we constrain the allocation to sit where we can answer,
      and answer. None of the last two branch. *)
  let align_offset ~t ~p ~a:tgt_align =
    let ( let/ ) x f = bind (function Some x -> ok x | None -> f ()) x in

    let usize_max = Typed.BV.usize (Layout.max_value_z (TUInt Usize)) in
    let* stride = Layout.size_of t in
    let ptr = Typed.Ptr.ptr_of p in
    let ofs = Typed.Ptr.ofs ptr in
    let alloc_align = Typed.Ptr.align_of ptr in
    let* () =
      assert_not
        (Usize.(0s) ==@ tgt_align)
        (`StdErr "zero given to align_offset")
    in
    let tgt_align = Typed.BV.cast_nonzero tgt_align in
    (* [gcd(stride, tgt_align)]: stepping by [stride] leaves the address
       unchanged modulo it, so it is what decides whether aligning is
       possible *)
    let gcd =
      match (Typed.BV.to_z tgt_align, Typed.BV.to_z stride) with
      | Some tgt_align_z, Some stride_z when Z.gt stride_z Z.zero ->
          Some (Z.gcd tgt_align_z stride_z)
      | _ -> None
    in

    (* Given an address modulo [tgt_align], the number of [stride]-sized steps
       to the next [tgt_align]-aligned address. Writing [g] for [gcd(stride,
       tgt_align)], the congruence [rem + k * stride = 0 (mod tgt_align)] has a
       solution exactly when [g] divides [rem]: [k = -(rem / g) * (stride /
       g)^-1 (mod tgt_align / g)]. *)
    let elements_to_align rem =
      match (Typed.BV.to_z tgt_align, Typed.BV.to_z stride, gcd) with
      | _, Some stride_z, _ when Z.equal stride_z Z.zero ->
          (* a ZST never moves, so it is aligned only if it already is *)
          let+ res =
            simplify (Typed.ite (rem ==@ Usize.(0s)) Usize.(0s) usize_max)
          in
          Some res
      | Some tgt_align_z, Some stride_z, Some gcd_z ->
          let modulus = Z.div tgt_align_z gcd_z in
          let inverse =
            if Z.equal modulus Z.one then Z.zero
            else Z.invert (Z.div stride_z gcd_z) modulus
          in
          let gcd = Typed.BV.cast_nonzero (Typed.BV.usize gcd_z) in
          let modulus_v = Typed.BV.cast_nonzero (Typed.BV.usize modulus) in
          let scaled = rem /@ gcd in
          let k =
            (modulus_v -!@ scaled)
            %@ modulus_v
            *!@ Typed.BV.usize inverse
            %@ modulus_v
          in
          let+ res =
            simplify (Typed.ite (rem %@ gcd ==@ Usize.(0s)) k usize_max)
          in
          Some res
      | _ -> ok None
    in

    (* the allocation is [tgt_align]-aligned, so the address is congruent to the
       offset; no need to look at the address at all *)
    let/ () =
      if%sat tgt_align <=@ alloc_align then elements_to_align (ofs %@ tgt_align)
      else ok None
    in

    let* addr = Sptr.decay ptr in
    (* the address is known, e.g. a pointer built from an integer *)
    let/ () =
      if Option.is_some (Typed.BV.to_z addr) then
        elements_to_align (addr %@ tgt_align)
      else ok None
    in

    (* Couldn't come up in constant time, so give up and try iterating a few
       times to see if we can find a working offset. We use [if%sure] to avoid
       branching. *)
    let/ () =
      let max_probes = 16 in
      let probes =
        match (Typed.BV.to_z tgt_align, gcd) with
        | Some tgt_align_z, Some gcd_z ->
            let period = Z.div tgt_align_z gcd_z in
            if Z.lt period (Z.of_int max_probes) then Z.to_int period
            else max_probes
        | _ -> max_probes
      in
      let rec probe k =
        if k >= probes then ok None
        else
          let k' = Typed.BV.usizei k in
          if%sure (addr +!@ (k' *!@ stride)) %@ tgt_align ==@ Usize.(0s) then
            ok (Some k')
          else probe (k + 1)
      in
      probe 0
    in

    (* UX: can't find a right answer, and we want to avoid branching, so we
       under-approximate by constraining the allocation to sit somewhere we can
       answer for. *)
    Soteria.Terminal.Warn.warn_once align_offset_ux;
    let assume_aligned () =
      let is_aligned = addr %@ tgt_align ==@ Usize.(0s) in
      (* ...unless the address is already pinned somewhere unaligned, to avoid
         vanishing *)
      if%sure Typed.not is_aligned then ok usize_max
      else
        let+ () = assume [ is_aligned ] in
        Usize.(0s)
    in
    match gcd with
    | None -> assume_aligned ()
    | Some gcd_z ->
        let gcd = Typed.BV.usizenz gcd_z in
        let divides_gcd = addr %@ gcd ==@ Usize.(0s) in
        if%sure divides_gcd then assume_aligned ()
        else
          let+ () = assume [ Typed.not divides_gcd ] in
          usize_max

  let handle_alloc_error ~layout:_ = do_panic ()
  let handle_error ~e:_ = do_panic ()

  (* ---- float helpers ---- *)

  let float_is (fp : Typed.FloatClass.t) =
    match fp with
    | Zero -> Typed.Float.is_zero
    | NaN -> Typed.Float.is_nan
    | Normal -> Typed.Float.is_normal
    | Infinite -> Typed.Float.is_infinite
    | Subnormal -> Typed.Float.is_subnormal

  let float_is_finite v =
    Typed.((not (Float.is_nan v)) &&@ not (Float.is_infinite v))

  let float_is_sign sign v =
    let res =
      match sign with
      | `Pos -> Typed.Float.(leq (like v 0.) v)
      | `Neg -> Typed.Float.(leq v (like v (-0.)))
    in
    Typed.(res ||@ Float.is_nan v)

  (* ---- f16 ----- *)

  let f16_is_finite ~arg = ok (float_is_finite arg)
  let f16_is_infinite ~arg = ok (float_is Infinite arg)
  let f16_is_nan ~arg = ok (float_is NaN arg)
  let f16_is_normal ~arg = ok (float_is Normal arg)
  let f16_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
  let f16_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
  let f16_is_subnormal ~arg = ok (float_is Subnormal arg)

  (* ---- f32 ---- *)

  let f32_is_finite ~arg = ok (float_is_finite arg)
  let f32_is_infinite ~arg = ok (float_is Infinite arg)
  let f32_is_nan ~arg = ok (float_is NaN arg)
  let f32_is_normal ~arg = ok (float_is Normal arg)
  let f32_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
  let f32_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
  let f32_is_subnormal ~arg = ok (float_is Subnormal arg)

  (* ---- f64 ---- *)

  let f64_is_finite ~arg = ok (float_is_finite arg)
  let f64_is_infinite ~arg = ok (float_is Infinite arg)
  let f64_is_nan ~arg = ok (float_is NaN arg)
  let f64_is_normal ~arg = ok (float_is Normal arg)
  let f64_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
  let f64_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
  let f64_is_subnormal ~arg = ok (float_is Subnormal arg)

  (* ---- f128 ---- *)

  let f128_is_finite ~arg = ok (float_is_finite arg)
  let f128_is_infinite ~arg = ok (float_is Infinite arg)
  let f128_is_nan ~arg = ok (float_is NaN arg)
  let f128_is_normal ~arg = ok (float_is Normal arg)
  let f128_is_sign_negative ~arg = ok (float_is_sign `Neg arg)
  let f128_is_sign_positive ~arg = ok (float_is_sign `Pos arg)
  let f128_is_subnormal ~arg = ok (float_is Subnormal arg)

  (* ---- panics ---- *)

  let option_unwrap_failed () = do_panic ()
  let result_unwrap_failed ~msg:_ ~error:_ = do_panic ()

  let assert_failed_inner ~kind:_ ~left:_ ~right:_ ~args:_ =
    error (`FailedAssert None)

  let panic ~expr = do_panic ~msg:expr ()
  let panic_fmt ~fmt:_ = do_panic ()
  let panic_nounwind_fmt ~fmt:_ ~force_no_backtrace:_ = do_panic ()

  let begin_panic ~m:_ ~msg =
    match%ty msg with
    | TExtension TFullPtr -> do_panic ~msg ()
    | _ -> do_panic ()

  (* ---- hashing ---- *)

  let hash_one_ux =
    String.Interned.intern
      "std::hash::BuildHasher::hash_one was stubbed to always hash to 0, to \
       avoid path explosion. This is an under-approximation, some paths may be \
       missed."

  (** UX: Replace the real [SipHasher] with the *constant* hash: every value
      hashes to 0. This is a valid hash function, and avoid branch explosion
      from symbolic hashes, as e.g. hashbrown uses the lower bits of the hash to
      pick a bucket. *)
  let hash_one ~types:_ ~t_self:_ ~t:_ ~self:_ ~x:_ =
    if Soteria.Symex.Approx.As_ctx.is_ox () then
      Soteria.Terminal.Warn.warn_once hash_one_ux;
    ok (Typed.BitVec.u64i 0)

  (* ---- I/O (no-ops) ---- *)

  let _eprint ~args:_ = ok ()
  let _print ~args:_ = ok ()
  let print_to ~t:_ ~args:_ ~global_s:_ ~label:_ = ok ()
  let print_to_buffer_if_capture_used ~args:_ = ok Typed.v_true
end
