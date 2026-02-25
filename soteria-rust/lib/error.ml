open Charon
open Common.Charon_util

type t =
  [ `DoubleFree  (** Tried freeing the same allocation twice *)
  | `InvalidFree  (** Tried freeing memory at a non-0 offset *)
  | `MemoryLeak  (** Dynamically allocated memory was not freed *)
  | `MisalignedPointer of
    Typed.T.nonzero Typed.t * Typed.T.nonzero Typed.t * Typed.T.sint Typed.t
    (** Tried accessing memory with a misaligned pointer
        [(expected, received, offset)] *)
  | `NullDereference  (** Dereferenced a null pointer *)
  | `OutOfBounds  (** Tried accessing memory outside the allocation bounds *)
  | `UninitializedMemoryAccess  (** Accessed uninitialised memory *)
  | `UseAfterFree  (** Use a location in memory after it was freed *)
  | `MisalignedFnPointer
    (** Tried calling a function pointer with a non-0 offset *)
  | `NotAFnPointer
    (** Tried calling a function pointer, but it doesn't represent a function *)
  | `AccessedFnPointer  (** Tried accessing a function pointer's pointee *)
  | `InvalidFnArgCount of int * int
    (** Invalid argument count for function (function pointers only)
        [(expected, received)] *)
  | `InvalidFnArgTys of Types.ty * Types.ty
    (** Invalid arguments for function (function pointers only)
        [(expected, received)] *)
  | `DivisionByZero  (** Integer division by zero *)
  | `InvalidShift
    (** Binary shift that is less than 0 or that exceeds the bit size of the
        type *)
  | `Overflow  (** Arithmetic over or underflow *)
  | `RefToUninhabited
    (** Attempt to have a reference to an uninhabited value *)
  | `InvalidRef of t
    (** Attempt to have a reference that is not valid, e.g. because it points to
        uninitialised memory. Keeps track of the inner error *)
  | `StdErr of string
    (** Error caused by the std library (mainly intrinsics); kind of equivalent
        to `Panic *)
  | `UBAbort  (** Abort caused by an UB trap being triggered *)
  | `UBDanglingPointer  (** Pointer was offset outside of its allocation *)
  | `UBPointerArithmetic  (** Arithmetics on two pointers *)
  | `UBPointerComparison
    (** Comparison of pointers with different provenance *)
  | `UBIntToPointerNoProvenance of Typed.T.sint Typed.t
    (** Integer to pointer cast with no provenance *)
  | `UBIntToPointerStrict  (** Integer to pointer cast with strict provenance *)
  | `UBTransmute of string
    (** Invalid transmute, e.g. null reference, wrong enum discriminant *)
  | `AliasingError  (** Tree borrow violation that lead to UB *)
  | `RefInvalidatedEarly
    (** A protected reference was invalidated before the end of the function *)
  | `InvalidFreeStrongProtector
    (** Tried freeing an allocation when a strongly protected reference to it
        still exists *)
  | `FailedAssert of string option  (** Failed assert!(cond) *)
  | `Panic of string option  (** Regular panic, with a message *)
  | `UnwindTerminate  (** Unwinding terminated *)
  | `DeadVariable  (** Local was not initialised (impossible without `mir!`) *)
  | `Breakpoint  (** Breakpoint intrinsic *)
  | `MetaExpectedError
    (** Function was marked as expecting an error; none happened *)
  | `InvalidLayout of Types.ty  (** Type is too large for memory *)
  | `InvalidAlloc
    (** Error in alloc/realloc; a wrong alignment or size was provided *) ]

type warn_reason = InvalidReference of Typed.T.sloc Typed.t [@@deriving hash]

let is_unwindable : [> t ] -> bool = function
  | `NullDereference | `OutOfBounds | `DivisionByZero | `FailedAssert _
  | `Panic _ | `Overflow ->
      true
  | _ -> false

let rec pp ft : [> t ] -> unit = function
  | `AccessedFnPointer -> Fmt.string ft "Accessed function pointer's pointee"
  | `AliasingError -> Fmt.string ft "Aliasing error"
  | `Breakpoint -> Fmt.string ft "Breakpoint hit"
  | `DeadVariable -> Fmt.string ft "Dead variable accessed"
  | `DivisionByZero -> Fmt.string ft "Division by zero"
  | `DoubleFree -> Fmt.string ft "Double free"
  | `FailedAssert (Some msg) -> Fmt.pf ft "Failed assertion: %s" msg
  | `FailedAssert None -> Fmt.string ft "Failed assertion"
  | `InvalidAlloc ->
      Fmt.string ft "Invalid allocation, wrong size or align provided"
  | `InvalidFnArgCount (exp, got) ->
      Fmt.pf ft
        "Wrong number of arguments in function call; expected %d, received %d"
        exp got
  | `InvalidFnArgTys (exp, got) ->
      Fmt.pf ft
        "Mismatch in types expected of function; expected %a, received %a" pp_ty
        exp pp_ty got
  | `InvalidFree -> Fmt.string ft "Invalid free"
  | `InvalidFreeStrongProtector ->
      Fmt.string ft
        "Tried freeing an allocation which was passed to a function by \
         reference"
  | `InvalidLayout ty -> Fmt.pf ft "Invalid layout: %a" pp_ty ty
  | `InvalidRef e -> Fmt.pf ft "Invalid reference: %a" pp e
  | `InvalidShift -> Fmt.string ft "Invalid binary shift"
  | `MemoryLeak -> Fmt.string ft "Memory leak"
  | `MetaExpectedError -> Fmt.string ft "Meta: expected an error"
  | `MisalignedFnPointer -> Fmt.string ft "Misaligned function pointer"
  | `MisalignedPointer (exp, got, ofs) ->
      Fmt.pf ft "Misaligned pointer; expected %a, received %a with offset %a"
        Typed.ppa exp Typed.ppa got Typed.ppa ofs
  | `NotAFnPointer -> Fmt.string ft "Not a function pointer"
  | `NullDereference -> Fmt.string ft "Null dereference"
  | `OutOfBounds -> Fmt.string ft "Out of bounds"
  | `Overflow -> Fmt.string ft "Overflow"
  | `Panic (Some msg) -> Fmt.pf ft "Panic: %s" msg
  | `Panic None -> Fmt.pf ft "Panic"
  | `RefInvalidatedEarly ->
      Fmt.string ft "Protected ref invalidated before function ended"
  | `RefToUninhabited -> Fmt.string ft "Ref to uninhabited type"
  | `StdErr msg -> Fmt.pf ft "UB in std: %s" msg
  | `UninitializedMemoryAccess -> Fmt.string ft "Uninitialized memory access"
  | `UBAbort -> Fmt.string ft "UB: undefined behaviour trap reached"
  | `UBDanglingPointer -> Fmt.string ft "Dangling pointer"
  | `UBPointerArithmetic -> Fmt.string ft "UB: pointer arithmetic"
  | `UBPointerComparison -> Fmt.string ft "UB: pointer comparison"
  | `UBIntToPointerNoProvenance addr ->
      Fmt.pf ft "UB: int to pointer without exposed address: %a" Typed.ppa addr
  | `UBIntToPointerStrict ->
      Fmt.string ft
        "Attempted to cast an integer to an pointer with strict provenance"
  | `UBTransmute msg -> Fmt.pf ft "UB: Transmute: %s" msg
  | `UnwindTerminate -> Fmt.string ft "Terminated unwind"
  | `UseAfterFree -> Fmt.string ft "Use after free"

let pp_err_and_call_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" pp err
    (Soteria.Terminal.Call_trace.pp pp_span_data)
    call_trace

let severity : t -> Soteria.Terminal.Diagnostic.severity = function
  | `MemoryLeak -> Warning
  | e when is_unwindable e -> Error
  | _ -> Bug

type with_trace = t * Meta.span_data Soteria.Terminal.Call_trace.t

let add_to_call_trace ((err, trace_elem) : with_trace) trace_elem' =
  (err, trace_elem' :: trace_elem)

let log_at (where : Trace.t) error =
  let pp_loc ft = function
    | Some loc -> Fmt.pf ft " at %a" pp_span_data loc
    | None -> Fmt.string ft ""
  in
  let pp_op ft = function
    | Some op -> Fmt.pf ft " due to %s" op
    | None -> Fmt.string ft ""
  in
  L.error (fun m -> m "Error %a%a%a" pp error pp_loc where.loc pp_op where.op)

let decorate (where : Trace.t) (e : t) : with_trace =
  let msg = Option.value ~default:"Triggering operation" where.op in
  match where.loc with
  (* FIXME: Right now, C and Rust generate traces in reverse order. Once they
     generate the traces the same way, modify Soteria.Termial and remove
     List.revs *)
  | Some loc ->
      let elem = Soteria.Terminal.Call_trace.mk_element ~loc ~msg () in
      (e, List.rev @@ (elem :: where.stack))
  | None -> (e, List.rev where.stack)

module Diagnostic = struct
  let to_loc (pos : Charon.Meta.loc) = (pos.line - 1, pos.col - 1)

  let replace_subpath_opt sub_str replacement path =
    match String.index_of ~sub_str path with
    | Some idx ->
        let idx = idx + String.length sub_str in
        let rel_path = String.sub path idx (String.length path - idx) in
        Some (replacement ^ rel_path)
    | None -> None

  let as_ranges (span : Charon.Meta.span_data) =
    match span.file.name with
    | Local file when String.starts_with ~prefix:"/rustc/" file -> []
    | Local file ->
        let ( / ) = Filename.concat in
        let filename =
          match replace_subpath_opt ("lib" / "rustlib") "$RUSTLIB" file with
          | Some f -> Some f
          | None ->
              replace_subpath_opt
                ("soteria-rust" / "plugins")
                "$SOTERIA-RUST" file
        in
        [
          Soteria.Terminal.Diagnostic.mk_range_file ?filename
            ?content:span.file.contents file (to_loc span.beg_loc)
            (to_loc span.end_loc);
        ]
    | Virtual _ | NotReal _ -> []

  let print_diagnostic_simple =
    Soteria.Terminal.Diagnostic.print_diagnostic_simple

  let print_diagnostic ~fname ~error:((error, call_trace) : with_trace) =
    let msg = Fmt.str "%a in %s" pp error fname in
    Soteria.Terminal.Diagnostic.print_diagnostic ~call_trace ~as_ranges ~msg
      ~severity:(severity error)

  let warn_trace_once ~reason ((error, call_trace) : with_trace) =
    let msg = Fmt.to_to_string pp error in
    let id = hash_warn_reason reason in
    Soteria.Terminal.Warn.warn_trace_once ~id ~call_trace ~as_ranges msg
end
