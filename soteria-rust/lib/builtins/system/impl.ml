open Charon
open Rust_val
open Common.Charon_util

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  let hashmap_random_keys_ux =
    String.Interned.intern
      "std::sys::random::hashmap_random_keys was stubbed to constant random \
       keys, to avoid path explosion. This is an under-approximation, some \
       paths may be missed."

  (* Returning *symbolic* keys makes every hash symbolic, causing branch
     explosion; we instead stick to a concrete hash. *)
  let hashmap_random_keys ~fun_sig:_ =
    if Soteria.Symex.Approx.As_ctx.is_ox () then
      Soteria.Terminal.Warn.warn_once hashmap_random_keys_ux;
    let k0 = 0x0123456789abcdefL in
    let k1 = 0x94D049BB133111EBL in
    let to_u64 i = Rust_val.mk_int (Typed.BV.u64 (Z.of_int64 i)) in
    ok (Rust_val.mk_tuple [ to_u64 k0; to_u64 k1 ])

  (** {@rust[
        fn inner(key: &OsStr) -> Result<String, VarError>

        pub enum VarError {
            NotPresent,
            NotUnicode(OsString),
        }
      ]}
      HACK: we assume that hitting the environment never finds the variable
      we're looking for. Under-approximating behaviour. *)
  let inner ~(fun_sig : Types.fun_sig) ~key:_ =
    let out_res = ty_as_adt fun_sig.output in
    let var_error_ty =
      let err =
        Crate.as_enum out_res
        |> List.find (fun (v : Types.variant) -> v.variant_name = "Err")
      in
      ty_as_adt (List.hd err.fields).field_ty
    in
    (* We need to return the rust value `Err(VarError::NotPresent)`. `Err` is
       variant 1 in the Result enum, and `VarError` is defined as:

       VarError { NotPresent, NotUnicode(OsString) }

       So the variant of NotPresent is 0 *)

    let var_error = Rust_val.Checked.mk_enum var_error_ty "NotPresent" [] in
    ok @@ Rust_val.Checked.mk_enum out_res "Err" [ var_error ]

  (** HACK: We under-approximate and always return 1. *)
  let available_parallelism ~(fun_sig : Types.fun_sig) =
    (* We return 1, to under-approximate the behaviour. *)
    let one = mk_int (Typed.BV.usize Z.one) in
    (* `NonZero(1)` *)
    let nonzero_one = mk_tuple [ mk_tuple [ one ] ] in
    (* `Ok(Nonzero(1))` *)
    let out = ty_as_adt fun_sig.output in
    ok @@ Checked.mk_enum out "Ok" [ nonzero_one ]

  let now () =
    (* We need to return a Instant where
     * ```
     *  struct Instant(Timespec);
     *  struct Timespec {
     *   tv_sec: i64,
     *   tv_nsec: core::num::niche_types::Nanoseconds,
     * }
     * // Max value of Nanoseconds is 999,999,999.
     * struct Nanoseconds(u32);
     *)
    (* HACK: We use Unix.time for now, to be under-approximating. *)
    let now = Unix.time () in
    let sec = Int.of_float now in
    let nsec = Int.of_float ((now -. Float.of_int sec) *. 1_000_000_000.) in
    let sec = mk_int (Typed.BitVec.u64i sec) in
    let nsec = mk_int (Typed.BitVec.u32i nsec) in
    ok (mk_tuple [ mk_tuple [ sec; mk_tuple [ nsec ] ] ])
end
