module Ctype = Cerb_frontend.Ctype
open Csymex
open Csymex.Syntax
open Typed.Syntax
module T = Typed.T
open Ail_tys
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
    match Typed.cast_checked sz Typed.t_int with
    | Some sz ->
        Csymex.branches
          ([
             (fun () ->
               let++ ptr, state = State.alloc sz state in
               (Basic ptr, state));
           ]
          @ failed_alloc_case state)
    | None -> not_impl "malloc with non-integer argument"

  let calloc ~(args : Agv.t list) state =
    let* sz =
      match args with
      | [ Basic num; Basic sz ] -> (
          let num = Typed.cast_checked num Typed.t_int in
          let sz = Typed.cast_checked sz Typed.t_int in
          match (num, sz) with
          | Some num, Some sz -> Csymex.return (Typed.times num sz)
          | None, _ | _, None -> not_impl "calloc with non-integer arguments")
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
    | TPointer ->
        let++ (), state =
          if%sat Typed.Ptr.is_null (Typed.cast ptr) then Result.ok ((), state)
          else State.free (Typed.cast ptr) state
        in
        (Agv.int 0, state)
    | TInt -> Fmt.kstr not_impl "free with int argument: %a" Typed.ppa ptr
    | _ -> Fmt.kstr not_impl "free with non-pointer argument: %a" Typed.ppa ptr

  let memcpy ~(args : Agv.t list) state =
    let* dst, src, size =
      match args with
      | [ Basic dst; Basic src; Basic size ] -> return (dst, src, size)
      | _ -> not_impl "memcpy with non-three arguments"
    in
    let dst = Typed.cast dst in
    let src = Typed.cast src in
    let size = Typed.cast size in
    let++ (), state = State.copy_nonoverlapping ~dst ~src ~size state in
    (Basic dst, state)

  let assert_ ~(args : Agv.t list) state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Basic t ] | [ Basic t; _ ] ->
          Csymex.of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then State.error `FailedAssert state
    else Result.ok (Agv.int 0, state)

  let assume_ ~(args : Agv.t list) state =
    let* to_assume =
      match args with
      | [ Basic t ] ->
          Csymex.of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assume with non-one arguments"
    in
    let* () = Csymex.assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Agv.int 0, state)

  let nondet_int_fun ~args:_ state =
    let* v = Csymex.nondet Typed.t_int in
    let constrs = Layout.int_constraints (Ctype.Signed Int_) |> Option.get in
    let* () = Csymex.assume (constrs v) in
    let v = (v :> T.cval Typed.t) in
    Result.ok (Basic v, state)

  let havoc ~return_ty ~args state =
    let rec havoc_aggregate state (v : Agv.t) =
      match v with
      | Basic v ->
          if Svalue.equal_ty (Typed.get_ty v) Svalue.t_ptr then
            Csymex.not_impl "Havocking input pointer for undefined function"
          else Result.ok state
      | Struct fields -> Result.fold_list fields ~init:state ~f:havoc_aggregate
    in
    let** state = Result.fold_list args ~init:state ~f:havoc_aggregate in
    let* ret =
      match return_ty with
      | Some return_ty -> Layout.nondet_c_ty_aggregate return_ty
      | None ->
          (* No return type, I guess it returns void? *)
          Csymex.return (Agv.int 0)
    in
    Result.ok (ret, state)

  module Arg_filter = struct
    (** HACK: Some internal functions such as __builtin___memcpy_chk are not
        needed in our tool, since we perform all checks. For this function, we
        return the real implementation (here, memcpy), with a filter saying that
        the last argument should be elided. See:
        https://gcc.gnu.org/onlinedocs/gcc-4.3.0/gcc/Object-Size-Checking.html
    *)

    type t = (int -> expr -> bool) option

    let no_filter : t = None

    let apply filter args =
      match filter with None -> args | Some f -> List.filteri f args
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
        | "__builtin___memcpy_chk" ->
            (* See definition of this builtin, the last argument is not useful to us. *)
            Some (memcpy, Some (fun i _ -> i <> 3))
        | "__soteria___nondet_int" -> Some (nondet_int_fun, None)
        | "__soteria___assert" -> Some (assert_, None)
        | "__CPROVER_assert" ->
            (* CPROVER_assert receives two arguments, we don't care about the second one for now. *)
            with_cbmc_support (assert_, Some (fun i _ -> i == 0))
        | "__CPROVER_assume" -> with_cbmc_support (assume_, None)
        | _ -> None)
    | _ -> None
end
