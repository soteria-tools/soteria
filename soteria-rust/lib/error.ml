open Charon
open Charon_util

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
  | `StdErr of string
    (** Error caused by the std library (mainly intrinsics); kind of equivalent
        to `Panic *)
  | `UBAbort  (** Abort caused by an UB trap being triggered *)
  | `UBDanglingPointer  (** Pointer was offset outside of its allocation *)
  | `UBPointerArithmetic  (** Arithmetics on two pointers *)
  | `UBPointerComparison
    (** Comparison of pointers with different provenance *)
  | `UBTransmute of string
    (** Invalid transmute, e.g. null reference, wrong enum discriminant *)
  | `AliasingError  (** Tree borrow violation that lead to UB *)
  | `RefInvalidatedEarly
    (** A protected reference was invalidated before the end of the function *)
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

let is_unwindable : [> t ] -> bool = function
  | `NullDereference | `OutOfBounds | `DivisionByZero | `FailedAssert _
  | `Panic _ | `Overflow ->
      true
  | _ -> false

let pp ft : [> t ] -> unit = function
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
  | `InvalidLayout ty -> Fmt.pf ft "Invalid layout: %a" pp_ty ty
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
  | `UBDanglingPointer -> Fmt.string ft "UB: dangling pointer"
  | `UBPointerArithmetic -> Fmt.string ft "UB: pointer arithmetic"
  | `UBPointerComparison -> Fmt.string ft "UB: pointer comparison"
  | `UBTransmute msg -> Fmt.pf ft "UB: Transmute: %s" msg
  | `UnwindTerminate -> Fmt.string ft "Terminated unwind"
  | `UseAfterFree -> Fmt.string ft "Use after free"

let pp_err_and_call_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" pp err
    (Soteria.Terminal.Call_trace.pp Charon.Meta.pp_span)
    call_trace

let severity : t -> Soteria.Terminal.Diagnostic.severity = function
  | `MemoryLeak -> Warning
  | e when is_unwindable e -> Error
  | _ -> Bug
