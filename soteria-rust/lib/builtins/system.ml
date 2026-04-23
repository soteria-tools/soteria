(** Builtins relating to particular operating systems. There is no central
    documentation for these; we must instead implement them as needed. *)

open Rust_val
open Charon

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  (** Used on macOS to register thread local destructors; receives a function
      pointer and an argument. Should call the destructor with the argument at
      the end of the thread.

      [_tlv_atexit(dtor: unsafe extern "C" fn( *mut u8), arg: *mut u8)] *)
  let tlv_atexit exec_fun args =
    let* dtor, arg =
      match args with
      | [ Ptr dtor_ptr; arg_ptr ] -> ok (dtor_ptr, arg_ptr)
      | _ ->
          Fmt.kstr not_impl "tlv_atexit: unexpected arguments: %a"
            Fmt.(list pp_rust_val)
            args
    in
    let+ () =
      State.register_thread_exit (fun () ->
          let* fn = State.lookup_fn dtor in
          let* _ = exec_fun fn [ arg ] in
          ok ())
    in
    Tuple []

  (** {@rust[
        fn _var(key: &OsStr) -> Result<String, VarError>
      ]}
      HACK: we assume that hitting the environment never finds the variable
      we're looking for. Under-approximating behaviour. *)
  let env_var ~(ret_ty : Types.ty) args =
    let () =
      match args with
      | [ _ ] -> ()
      | _ -> failwith "std::env::_var: expected exactly one argument"
    in
    let var_error_ty =
      match ret_ty with
      (* THE FOLLOWING DOESNT MATCH, THE TYPE HAS NO GENERICS *)
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
                Types.pp_ty ret_ty)
      | _ ->
          Fmt.failwith "std::env::_var: unexpected return type %a"
            Charon.Types.pp_ty ret_ty
    in
    (* We need to return the rust value `Err(VarError::NotPresent)`. `Err` is
       variant 1 in the Result enum, and `VarError` is defined as:

       VarError { NotPresent, NotUnicode(OsString) }

       So the variant of NotPresent is 0 *)
    let* var_error = Encoder.mk_enum ~ty:var_error_ty 0 [] in
    let+ res = Encoder.mk_enum ~ty:ret_ty 1 [ var_error ] in
    res

  (** {@rust[
        pub fn available_parallelism() -> Result<NonZero<usize>>
      ]}
      HACK: We under-approximate and always return 1. The return type is Result<
  *)
  let available_parallelism ~ret_ty args =
    let () =
      match args with
      | [] -> ()
      | _ ->
          failwith "std::thread::available_parallelism: expected no arguments"
    in
    (* We return 1, to under-approximate the behaviour. *)
    let one = Int (Typed.BV.usize Z.one) in
    (* `NonZero(1)` *)
    let nonzero_one = Tuple [ Tuple [ one ] ] in
    (* `Ok(Nonzero(1))` *)
    Encoder.mk_enum ~ty:ret_ty 0 [ nonzero_one ]

  let unix_time_now args =
    let () =
      match args with
      | [] -> ()
      | _ ->
          failwith "std::sys::time::unix::Instant::now: expected no arguments"
    in
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
    let sec = Int (Typed.BitVec.mki 64 sec) in
    let nsec = Int (Typed.BitVec.mki 32 nsec) in
    ok (Tuple [ Tuple [ sec; Tuple [ nsec ] ] ])

  let hashmap_random_keys _ =
    Encoder.nondet_valid
      (TAdt
         {
           id = TTuple;
           generics =
             {
               types = [ TLiteral (TUInt U64); TLiteral (TUInt U64) ];
               regions = [];
               const_generics = [];
               trait_refs = [];
             };
         })
end
