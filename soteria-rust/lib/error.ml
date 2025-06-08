type t =
  [ (* Memory safety *)
    `DoubleFree
  | `InvalidFree
  | `MemoryLeak
  | `MisalignedPointer
  | `NullDereference
  | `OutOfBounds
  | `UninitializedMemoryAccess
  | `UseAfterFree
  | `MisalignedFnPointer
  | (* Arithmetic *)
    `DivisionByZero
  | `InvalidShift
  | `Overflow
  | (* Undefined behaviour *)
    `RefToUninhabited
  | `StdErr of string
  | `UBAbort
  | `UBDanglingPointer
  | `UBPointerArithmetic
  | `UBPointerComparison
  | `UBTransmute
  | (* Tree borrows *)
    `AliasingError
  | (* Explicit errors *)
    `FailedAssert of string option
  | `Panic of string option
  | `UnwindTerminate
  | (* Local was not initialised (impossible without `mir!`) *)
    `DeadVariable
  | (* Breakpoint intrinsic *)
    `Breakpoint
  | (* Meta errors *)
    `MetaExpectedError
    (* Type is too large for memory *)
  | `InvalidLayout ]

let is_unwindable : [> t ] -> bool = function
  | `NullDereference | `OutOfBounds | `DivisionByZero | `FailedAssert _
  | `Panic _ | `Overflow ->
      true
  | _ -> false

let pp ft = function
  | `AliasingError -> Fmt.string ft "Aliasing error"
  | `Breakpoint -> Fmt.string ft "Breakpoint hit"
  | `DeadVariable -> Fmt.string ft "Dead variable accessed"
  | `DivisionByZero -> Fmt.string ft "Division by zero"
  | `DoubleFree -> Fmt.string ft "Double free"
  | `FailedAssert (Some msg) -> Fmt.pf ft "Failed assertion: %s" msg
  | `FailedAssert None -> Fmt.string ft "Failed assertion"
  | `InvalidFree -> Fmt.string ft "Invalid free"
  | `InvalidLayout -> Fmt.string ft "Invalid layout"
  | `InvalidShift -> Fmt.string ft "Invalid binary shift"
  | `MemoryLeak -> Fmt.string ft "Memory leak"
  | `MetaExpectedError -> Fmt.string ft "Meta: expected an error"
  | `MisalignedFnPointer -> Fmt.string ft "Misaligned function pointer"
  | `MisalignedPointer -> Fmt.string ft "Misaligned pointer"
  | `NullDereference -> Fmt.string ft "Null dereference"
  | `OutOfBounds -> Fmt.string ft "Out of bounds"
  | `Overflow -> Fmt.string ft "Overflow"
  | `Panic (Some msg) -> Fmt.pf ft "Panic: %s" msg
  | `Panic None -> Fmt.pf ft "Panic"
  | `RefToUninhabited -> Fmt.string ft "Ref to uninhabited type"
  | `StdErr msg -> Fmt.pf ft "UB in std: %s" msg
  | `UninitializedMemoryAccess -> Fmt.string ft "Uninitialized memory access"
  | `UBAbort -> Fmt.string ft "UB: undefined behaviour trap reached"
  | `UBDanglingPointer -> Fmt.string ft "UB: dangling pointer"
  | `UBPointerArithmetic -> Fmt.string ft "UB: pointer arithmetic"
  | `UBPointerComparison -> Fmt.string ft "UB: pointer comparison"
  | `UBTransmute -> Fmt.string ft "UB: Transmute"
  | `UnwindTerminate -> Fmt.string ft "Terminated unwind"
  | `UseAfterFree -> Fmt.string ft "Use after free"

let pp_err_and_call_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" pp err
    (Soteria_terminal.Call_trace.pp Charon.Meta.pp_span)
    call_trace

let severity : t -> Soteria_terminal.Diagnostic.severity = function
  | `MemoryLeak -> Warning
  | e when is_unwindable e -> Error
  | _ -> Bug

module Diagnostic = struct
  let to_loc (pos : Charon.Meta.loc) = (pos.line - 1, pos.col)

  let as_ranges (loc : Charon.Meta.span) =
    let span = Option.value ~default:loc.span loc.generated_from_span in
    match span.file.name with
    | Local file when String.starts_with ~prefix:"/rustc/" file -> []
    | Local file ->
        let filename =
          if String.starts_with ~prefix:Plugin.lib_root file then
            let root_l = String.length Plugin.lib_root in
            let rel_path =
              String.sub file root_l (String.length file - root_l)
            in
            Some ("$RUSTERIA" ^ rel_path)
          else None
        in
        [
          Soteria_terminal.Diagnostic.mk_range_file ?filename file
            (to_loc span.beg_loc) (to_loc span.end_loc);
        ]
    | Virtual _ -> []

  let print_diagnostic ~fname ~call_trace ~error =
    Soteria_terminal.Diagnostic.print_diagnostic ~call_trace ~as_ranges
      ~error:(Fmt.to_to_string pp error)
      ~severity:(severity error) ~fname
end
