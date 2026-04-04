open Charon
open Common.Charon_util

type t =
  [ `DoubleFree
    [@diag.format "Double free"]
    [@diag.severity Bug]
    (** Tried freeing the same allocation twice *)
  | `InvalidFree
    [@diag.format "Invalid free"]
    [@diag.severity Bug]
    (** Tried freeing memory at a non-0 offset *)
  | `MemoryLeak
    [@diag.format "Memory leak"]
    [@diag.severity Warning]
    (** Dynamically allocated memory was not freed *)
  | `MisalignedPointer of
    Typed.T.nonzero Typed.t * Typed.T.nonzero Typed.t * Typed.T.sint Typed.t
    [@diag.format "Misaligned pointer; expected %a, received %a with offset %a"]
    [@diag.severity Bug]
    (** Tried accessing memory with a misaligned pointer
        [(expected, received, offset)] *)
  | `NullDereference
    [@diag.format "Null dereference"]
    (** Dereferenced a null pointer *)
  | `OutOfBounds
    [@diag.format "Out of bounds"]
    (** Tried accessing memory outside the allocation bounds *)
  | `UninitializedMemoryAccess
    [@diag.format "Uninitialized memory access"]
    [@diag.severity Bug]
    (** Accessed uninitialised memory *)
  | `UseAfterFree
    [@diag.format "Use after free"]
    [@diag.severity Bug]
    (** Use a location in memory after it was freed *)
  | `MisalignedFnPointer
    [@diag.format "Misaligned function pointer"]
    [@diag.severity Bug]
    (** Tried calling a function pointer with a non-0 offset *)
  | `NotAFnPointer
    [@diag.format "Not a function pointer"]
    [@diag.severity Bug]
    (** Tried calling a function pointer, but it doesn't represent a function *)
  | `AccessedFnPointer
    [@diag.format "Accessed function pointer's pointee"]
    [@diag.severity Bug]
    (** Tried accessing a function pointer's pointee *)
  | `InvalidFnArgCount of int * int
    [@diag.format
      "Wrong number of arguments in function call; expected %a, received %a"]
    [@diag.severity Bug]
    (** Invalid argument count for function (function pointers only)
        [(expected, received)] *)
  | `InvalidFnArgTys of (Types.ty[@printer pp_ty]) * (Types.ty[@printer pp_ty])
    [@diag.format
      "Mismatch in types expected of function; expected %a, received %a"]
    [@diag.severity Bug]
    (** Invalid arguments for function (function pointers only)
        [(expected, received)] *)
  | `DivisionByZero
    [@diag.format "Division by zero"]
    (** Integer division by zero *)
  | `InvalidShift
    [@diag.format "Invalid binary shift"]
    [@diag.severity Bug]
    (** Binary shift that is less than 0 or that exceeds the bit size of the
        type *)
  | `Overflow [@diag.format "Overflow"]  (** Arithmetic over or underflow *)
  | `RefToUninhabited of (Types.ty[@printer pp_ty])
    [@diag.format "Ref to uninhabited type: %a"]
    [@diag.severity Bug]
    (** Attempt to have a reference to an uninhabited value *)
  | `InvalidRef of t
    [@diag.format "Invalid reference: %a"]
    [@diag.severity Bug]
    (** Attempt to have a reference that is not valid, e.g. because it points to
        uninitialised memory. Keeps track of the inner error *)
  | `StdErr of string
    [@diag.format "UB in std: %a"]
    [@diag.severity Bug]
    (** Error caused by the std library (mainly intrinsics); kind of equivalent
        to `Panic *)
  | `UBAbort
    [@diag.format "UB: undefined behaviour trap reached"]
    [@diag.severity Bug]
    (** Abort caused by an UB trap being triggered *)
  | `UBDanglingPointer
    [@diag.format "Dangling pointer"]
    [@diag.severity Bug]
    (** Pointer was offset outside of its allocation *)
  | `UBPointerArithmetic
    [@diag.format "UB: pointer arithmetic"]
    [@diag.severity Bug]
    (** Arithmetics on two pointers *)
  | `UBPointerComparison
    [@diag.format "UB: pointer comparison"]
    [@diag.severity Bug]
    (** Comparison of pointers with different provenance *)
  | `UBIntToPointerNoProvenance of Typed.T.sint Typed.t
    [@diag.format "UB: int to pointer without exposed address: %a"]
    [@diag.severity Bug]
    (** Integer to pointer cast with no provenance *)
  | `UBIntToPointerStrict
    [@diag.format
      "Attempted to cast an integer to an pointer with strict provenance"]
    [@diag.severity Bug]
    (** Integer to pointer cast with strict provenance *)
  | `UBTransmute of string
    [@diag.format "UB: Transmute: %a"]
    [@diag.severity Bug]
    (** Invalid transmute, e.g. null reference, wrong enum discriminant *)
  | `AliasingError
    [@diag.format "Aliasing error"]
    [@diag.severity Bug]
    (** Tree borrow violation that lead to UB *)
  | `RefInvalidatedEarly
    [@diag.format "Protected ref invalidated before function ended"]
    [@diag.severity Bug]
    (** A protected reference was invalidated before the end of the function *)
  | `InvalidFreeStrongProtector
    [@diag.format
      "Tried freeing an allocation which was passed to a function by reference"]
    [@diag.severity Bug]
    (** Tried freeing an allocation when a strongly protected reference to it
        still exists *)
  | `FailedAssert of string option
    [@diag.format "Failed assertion: %a"]
    (** Failed assert!(cond) *)
  | `Panic of string option
    [@diag.format "Panic: %a"]
    (** Regular panic, with a message *)
  | `UnwindTerminate
    [@diag.format "Terminated unwind"]
    [@diag.severity Bug]
    (** Unwinding terminated *)
  | `DeadVariable
    [@diag.format "Dead variable accessed"]
    [@diag.severity Bug]
    (** Local was not initialised (impossible without `mir!`) *)
  | `Breakpoint
    [@diag.format "Breakpoint hit"]
    [@diag.severity Bug]
    (** Breakpoint intrinsic *)
  | `MetaExpectedError
    [@diag.format "Meta: expected an error"]
    [@diag.severity Bug]
    (** Function was marked as expecting an error; none happened *)
  | `InvalidLayout of (Types.ty[@printer pp_ty])
    [@diag.format "Invalid layout: %a"]
    [@diag.severity Bug]
    (** Type is too large for memory *)
  | `InvalidAlloc
    [@diag.format "Invalid allocation, wrong size or align provided"]
    [@diag.severity Bug]
    (** Error in alloc/realloc; a wrong alignment or size was provided *) ]
[@@deriving diagnostic]

type warn_reason = InvalidReference of Typed.T.sloc Typed.t [@@deriving hash]

let is_unwindable : [> t ] -> bool = function
  | `NullDereference | `OutOfBounds | `DivisionByZero | `FailedAssert _
  | `Panic _ | `Overflow ->
      true
  | _ -> false

type with_trace = t * Meta.span_data Soteria.Terminal.Call_trace.t

let pp_with_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" pp err
    (Soteria.Terminal.Call_trace.pp pp_span_data)
    call_trace

let compare_with_trace (e1, t1) (e2, t2) =
  let str1 = Fmt.to_to_string pp e1 in
  let str2 = Fmt.to_to_string pp e2 in
  match String.compare str1 str2 with 0 -> compare t1 t2 | x -> x

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
