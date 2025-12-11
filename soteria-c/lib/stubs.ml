module Ctype = Cerb_frontend.Ctype
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
module BV = Typed.BitVec
module T = Typed.T
module Agv = Aggregate_val
open Agv

(* TODO: Generate skeleton for this file from signatures *)

let builtin_functions = [ "malloc"; "free"; "memcpy"; "calloc" ]

let failed_alloc_case state =
  if (Config.current ()).alloc_cannot_fail then []
  else [ (fun () -> Result.ok (Basic Typed.Ptr.null, state)) ]

module M (State : State_intf.S) = struct
  type 'err fun_exec =
    args:Agv.t list ->
    State.t ->
    (Agv.t * State.t, 'err, State.serialized) Result.t

  let malloc ~(args : Agv.t list) state =
    let* sz =
      match args with
      | [ Basic sz ] -> return sz
      | _ -> not_impl "malloc with non-one arguments"
    in
    let* sz =
      of_opt_not_impl ~msg:"malloc with non-integer argument"
      @@ BV.cast_to_size_t sz
    in
    Csymex.branches
      ([
         (fun () ->
           let++ ptr, state = State.alloc sz state in
           (Basic ptr, state));
       ]
      @ failed_alloc_case state)

  let calloc ~(args : Agv.t list) state =
    let* sz =
      match args with
      | [ Basic num; Basic sz ] ->
          let* num =
            of_opt_not_impl ~msg:"calloc with non-integer arguments"
            @@ BV.cast_to_size_t num
          in
          let* sz =
            of_opt_not_impl ~msg:"calloc with non-integer arguments"
            @@ BV.cast_to_size_t sz
          in
          let res, ovf = num *$?@ sz in
          let+ () = Csymex.assume [ Typed.not ovf ] in
          res
      | _ -> not_impl "calloc with non-one arguments"
    in
    Csymex.branches
      ([
         (fun () ->
           let++ ptr, state = State.alloc ~zeroed:true sz state in
           (Basic ptr, state));
       ]
      @ failed_alloc_case state)

  let free ~(args : Agv.t list) state =
    let* ptr =
      match args with
      | [ Basic ptr ] -> return ptr
      | _ -> not_impl "free with non-one arguments"
    in
    match Typed.get_ty ptr with
    | TPointer _ ->
        let++ (), state =
          if%sat Typed.Ptr.is_null (Typed.cast ptr) then Result.ok ((), state)
          else State.free (Typed.cast ptr) state
        in
        (Agv.void, state)
    | TBitVector _ ->
        Fmt.kstr not_impl "free with int argument: %a" Typed.ppa ptr
    | _ -> Fmt.kstr not_impl "free with non-pointer argument: %a" Typed.ppa ptr

  let memcpy ~(args : Agv.t list) state =
    let* dst, src, size =
      match args with
      | [ Basic dst; Basic src; Basic size ] -> return (dst, src, size)
      | _ -> not_impl "memcpy with non-three arguments"
    in
    let* dst =
      of_opt_not_impl ~msg:"memcpy with non-pointer dst"
      @@ Typed.cast_checked dst Typed.t_ptr
    in
    let* src =
      of_opt_not_impl ~msg:"memcpy with non-pointer src"
      @@ Typed.cast_checked src Typed.t_ptr
    in
    let* size =
      of_opt_not_impl ~msg:"memcpy with non-integer arguments"
      @@ BV.cast_to_size_t size
    in
    let dst = Typed.cast dst in
    let src = Typed.cast src in
    let size = Typed.cast size in
    let++ (), state = State.copy_nonoverlapping ~dst ~src ~size state in
    (Basic dst, state)

  let assert_ ~(args : Agv.t list) state =
    let open Typed.Infix in
    let* to_assert, size =
      match args with
      | [ Basic t ] | [ Basic t; _ ] ->
          Csymex.of_opt_not_impl ~msg:"assert: not an integer"
            (Typed.cast_int t)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ Typed.BitVec.zero size then
      State.error `FailedAssert state
    else Result.ok (Agv.void, state)

  let assert_fail ~args:_ state = State.error `FailedAssert state

  let assume_ ~(args : Agv.t list) state =
    let* to_assume, _ =
      match args with
      | [ Basic t ] ->
          Csymex.of_opt_not_impl ~msg:"assume: not an integer"
            (Typed.cast_int t)
      | _ -> not_impl "to_assume with non-one arguments"
    in
    let* () = Csymex.assume [ Typed.BitVec.to_bool to_assume ] in
    Result.ok (Agv.void, state)

  let nondet ty ~args:_ state =
    let* v = Layout.nondet_c_ty_aggregate_ ty in
    Result.ok (v, state)

  let strcmp ~args state =
    let* s1, s2 =
      match args with
      | [ Basic s1; Basic s2 ] ->
          let* s1_ptr =
            of_opt_not_impl ~msg:"strcmp with non-pointer s1"
            @@ Typed.cast_checked s1 Typed.t_ptr
          in
          let+ s2_ptr =
            of_opt_not_impl ~msg:"strcmp with non-pointer s2"
            @@ Typed.cast_checked s2 Typed.t_ptr
          in
          (s1_ptr, s2_ptr)
      | _ -> not_impl "strcmp with non-two arguments"
    in
    (* TODO: Optimise strcmp directly on the heap model! *)
    let[@inline] next ptr = Typed.Ptr.add_ofs ptr (BV.usizei 1) in
    let rec loop s1_ptr s2_ptr state =
      let** c1, state = State.load s1_ptr Ctype.char state in
      let** c2, state = State.load s2_ptr Ctype.char state in
      let* c1 =
        Agv.basic_or_unsupported ~msg:"strcmp: loaded but not char" c1
      in
      let* c2 =
        Agv.basic_or_unsupported ~msg:"strcmp: loaded but not char" c2
      in
      if%sat c1 ==@ U8.(0s) &&@ (c2 ==@ U8.(0s)) then
        Result.ok (Agv.c_int 0, state)
      else
        if%sat c1 ==@ c2 then loop (next s1_ptr) (next s2_ptr) state
        else
          let* c1, _ = Typed.cast_int c1 |> of_opt_not_impl ~msg:"strcmp c1" in
          let* c2, _ = Typed.cast_int c2 |> of_opt_not_impl ~msg:"strcmp c2" in
          let c1 = BV.fit_to ~signed:true (Layout.c_int_size * 8) c1 in
          let c2 = BV.fit_to ~signed:true (Layout.c_int_size * 8) c2 in
          (* This cannot overflow because they were chars and operation happens in integer world *)
          let res = c1 -!@ c2 in
          Result.ok (Basic res, state)
    in
    loop s1 s2 state

  let memset ~args state =
    let* dest, char_int, count =
      match args with
      | [ Basic s1; Basic s2; Basic s3 ] ->
          let* s1_ptr =
            of_opt_not_impl ~msg:"memset with non-pointer s1"
            @@ Typed.cast_checked s1 Typed.t_ptr
          in
          let sizeofint = Layout.c_int_size * 8 in
          let* char_int =
            of_opt_not_impl ~msg:"memset with non-pointer s2"
            @@ Typed.cast_checked s2 (Typed.t_int sizeofint)
          in
          let+ count =
            of_opt_not_impl ~msg:"memset with non-integer count"
            @@ BV.cast_to_size_t s3
          in
          (s1_ptr, char_int, count)
      | _ -> not_impl "memset with non-thre arguments"
    in
    let char = BV.fit_to ~signed:false 8 char_int in
    if%sure char ==@ U8.(0s) then
      let++ (), state = State.zero_range dest count state in
      (Agv.void, state)
    else
      let rec loop dest count state =
        if%sat count ==@ Usize.(0s) then Result.ok (Agv.void, state)
        else
          let** (), state =
            State.store dest Ctype.char
              (Agv.Basic (char :> T.cval Typed.t))
              state
          in
          let s1_ptr = Typed.Ptr.add_ofs dest (BV.usizei 1) in
          let count = count -!@ Usize.(1s) in
          loop s1_ptr count state
      in
      loop dest count state

  let havoc ~return_ty ~args state =
    let rec havoc_aggregate state (v : Agv.t) =
      match v with
      | Basic v -> (
          match Typed.cast_checked v Typed.t_ptr with
          | Some _ ->
              Csymex.not_impl "Havocking input pointer for undefined function"
          | None -> Result.ok state)
      | Struct fields -> Result.fold_list fields ~init:state ~f:havoc_aggregate
      | Array elements ->
          Result.fold_list elements ~init:state ~f:havoc_aggregate
    in
    let** state = Result.fold_list args ~init:state ~f:havoc_aggregate in
    let* ret =
      match return_ty with
      | Some return_ty -> Layout.nondet_c_ty_aggregate return_ty
      | None ->
          (* No return type, I guess it returns void? *)
          Csymex.return Agv.void
    in
    Result.ok (ret, state)

  let vanish_fn ~args:_ _state = vanish ()

  module Arg_filter = struct
    (** HACK: Some internal functions such as __builtin___memcpy_chk are not
        needed in our tool, since we perform all checks. For this function, we
        return the real implementation (here, memcpy), with a filter saying that
        the last argument should be elided. See:
        https://gcc.gnu.org/onlinedocs/gcc-4.3.0/gcc/Object-Size-Checking.html
    *)

    type t = (int -> bool) option

    let no_filter : t = None

    let apply filter args =
      match filter with
      | None -> args
      | Some f -> List.filteri (fun i _ -> f i) args
  end

  (* FIXME: make this more global when concurrency is added.
     Also, it cannot be made local to the function without heavy type annotation
    because of non-generalisable type vars. *)
  let signaled_cbmc = ref false

  let with_cbmc_support x =
    if (Config.current ()).cbmc_compat then Some x
    else
      let () =
        if not !signaled_cbmc then (
          signaled_cbmc := true;
          L.warn (fun m ->
              m
                "CBMC support is not enabled, but detected use of the \
                 __CPROVER API. Soteria will consider the function as missing \
                 a body."))
      in
      None

  let signaled_textcomp = ref false

  let with_testcomp_support x =
    if (Config.current ()).testcomp_compat then Some x
    else
      let () =
        if not !signaled_cbmc then (
          signaled_cbmc := true;
          L.warn (fun m ->
              m
                "Test-Comp support is not enabled, but detected use of the \
                 __VERIFIER API. Soteria will consider the function as missing \
                 a body."))
      in
      None

  let find_stub (fname : Cerb_frontend.Symbol.sym) :
      ('err fun_exec * Arg_filter.t) option =
    let (Symbol (_, _, descr)) = fname in
    match descr with
    | Cerb_frontend.Symbol.SD_Id name -> (
        match name with
        | "malloc" -> Some (malloc, None)
        | "calloc" -> Some (calloc, None)
        | "free" -> Some (free, None)
        | "memcpy" -> Some (memcpy, None)
        | "memmove" ->
            (* We model memmove as memcpy, we should do non-overlapping checks but heh. *)
            Some (memcpy, None)
        | "strcmp" -> Some (strcmp, None)
        | "memset" -> Some (memset, None)
        | "__builtin___memset_chk" -> Some (memset, Some (( <> ) 3))
        | "__builtin___memcpy_chk" ->
            (* See definition of this builtin, the last argument is not useful to us. *)
            Some (memcpy, Some (( <> ) 3))
        | "__soteria___nondet_int" ->
            Some (nondet (Basic (Integer (Signed Int_))), None)
        | "__VERIFIER_nondet_char" ->
            with_testcomp_support (nondet (Basic (Integer Char)), None)
        | "__VERIFIER_nondet_uchar" ->
            with_testcomp_support
              (nondet (Basic (Integer (Unsigned Ichar))), None)
        | "__VERIFIER_nondet_short" ->
            with_testcomp_support (nondet (Basic (Integer (Signed Short))), None)
        | "__VERIFIER_nondet_ushort" ->
            with_testcomp_support
              (nondet (Basic (Integer (Unsigned Short))), None)
        | "__VERIFIER_nondet_int" ->
            with_testcomp_support (nondet (Basic (Integer (Signed Int_))), None)
        | "__VERIFIER_nondet_uint" ->
            with_testcomp_support
              (nondet (Basic (Integer (Unsigned Int_))), None)
        | "__VERIFIER_nondet_int128" ->
            with_testcomp_support
              (nondet (Basic (Integer (Signed (IntN_t 128)))), None)
        | "__VERIFIER_nondet_uint128" ->
            with_testcomp_support
              (nondet (Basic (Integer (Unsigned (IntN_t 128)))), None)
        (* | "__VERIFIER_nondet_charp" ->
            with_testcomp_support 
              (nondet (Basic (Typed.TPointer Typed.t_char)), None) *)
        | "__VERIFIER_nondet_long" ->
            with_testcomp_support (nondet (Basic (Integer (Signed Long))), None)
        | "__VERIFIER_nondet_ulong" ->
            with_testcomp_support
              (nondet (Basic (Integer (Unsigned Long))), None)
        | "__VERIFIER_nondet_longlong" ->
            with_testcomp_support
              (nondet (Basic (Integer (Signed LongLong))), None)
        | "__VERIFIER_nondet_ulonglong" ->
            with_testcomp_support
              (nondet (Basic (Integer (Unsigned LongLong))), None)
        | "__VERIFIER_nondet_float" ->
            with_testcomp_support
              (nondet (Basic (Floating (RealFloating Float))), None)
        | "__VERIFIER_nondet_double" ->
            with_testcomp_support
              (nondet (Basic (Floating (RealFloating Double))), None)
        | "__soteria___assert" -> Some (assert_, None)
        | "__assert_fail" -> with_testcomp_support (assert_fail, None)
        | "abort" | "exit" -> with_testcomp_support (vanish_fn, None)
        | "__CPROVER_assert" ->
            (* CPROVER_assert receives two arguments, we don't care about the second one for now. *)
            with_cbmc_support (assert_, Some (( == ) 0))
        | "__CPROVER_assume" -> with_cbmc_support (assume_, None)
        | _ -> None)
    | _ -> None
end
