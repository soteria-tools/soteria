open Charon
open Svalue

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  let hashmap_random_keys ~(fun_sig : Types.fun_sig) =
    Encoder.nondet_valid fun_sig.output

  (** Used on macOS to register thread local destructors; receives a function
      pointer and an argument. Should call the destructor with the argument at
      the end of the thread.

      [_tlv_atexit(dtor: unsafe extern "C" fn( *mut u8), arg: *mut u8)] *)
  let _tlv_atexit ~fun_exec ~args =
    let* dtor, arg =
      match args with
      | [ dtor_ptr; arg_ptr ] -> ok (Typed.cast_ptr_f dtor_ptr, arg_ptr)
      | _ ->
          Fmt.kstr not_impl "tlv_atexit: unexpected arguments: %a"
            Fmt.(list Typed.ppa)
            args
    in
    let+ () =
      State.register_thread_exit (fun () ->
          let* fn = State.lookup_fn dtor in
          let* _ = fun_exec fn [ arg ] in
          ok ())
    in
    Typed.Adt.mk_tuple []

  (** {@rust[
        fn getenv(k: &OsStr) -> Option<OsString>
      ]}
      HACK: we assume that hitting the environment never finds the variable
      we're looking for. Under-approximating behaviour. *)
  let getenv ~(fun_sig : Types.fun_sig) ~k:_ =
    let var_error_ty =
      match fun_sig.output with
      | TAdt { id = TAdtId id; _ } -> (
          let tydecl = Crate.get_adt_raw id in
          let name = tydecl.item_meta.name in

          match List.last_opt name with
          | Some
              (PeInstantiated
                 { binder_value = { types = [ _string; var_error_ty ]; _ }; _ })
            ->
              var_error_ty
          | _ ->
              Fmt.failwith
                "std::env::_var: unexpected type Result<_, VarError> (%a)"
                Types.pp_ty fun_sig.output)
      | _ ->
          Fmt.failwith "std::env::_var: unexpected return type %a"
            Charon.Types.pp_ty fun_sig.output
    in
    (* We need to return the rust value `Err(VarError::NotPresent)`. `Err` is
       variant 1 in the Result enum, and `VarError` is defined as:

       VarError { NotPresent, NotUnicode(OsString) }

       So the variant of NotPresent is 0 *)
    let var_error =
      Typed.Adt.Checked.mk_enum ~ty:var_error_ty "NotPresent" []
    in
    let res =
      Typed.Adt.Checked.mk_enum ~ty:fun_sig.output "Err" [ var_error ]
    in
    StateM.ok res

  (** HACK: We under-approximate and always return 1. *)
  let available_parallelism ~(fun_sig : Types.fun_sig) =
    (* We return 1, to under-approximate the behaviour. *)
    let one = Typed.BV.usize Z.one in
    (* `NonZero(1)` *)
    let nonzero_one = Typed.Adt.mk_tuple [ one ] in
    (* `Ok(Nonzero(1))` *)
    let res =
      Typed.Adt.Checked.mk_enum ~ty:fun_sig.output "Ok" [ nonzero_one ]
    in
    StateM.ok res

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
    let sec = Typed.BitVec.u64i sec in
    let nsec = Typed.BitVec.u32i nsec in
    ok Typed.Adt.(mk_tuple [ mk_tuple [ sec; mk_tuple [ nsec ] ] ])
end
