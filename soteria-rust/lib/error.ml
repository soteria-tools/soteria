let pp_err ft (err, call_trace) =
  Format.open_vbox 0;
  let () =
    match err with
    | `NullDereference -> Fmt.string ft "NullDereference"
    | `OutOfBounds -> Fmt.string ft "OutOfBounds"
    | `UninitializedMemoryAccess -> Fmt.string ft "UninitializedMemoryAccess"
    | `UseAfterFree -> Fmt.string ft "UseAfterFree"
    | `DivisionByZero -> Fmt.string ft "DivisionByZero"
    | `ParsingError s -> Fmt.pf ft "ParsingError: %s" s
    | `UBPointerComparison -> Fmt.string ft "UBPointerComparison"
    | `UBPointerArithmetic -> Fmt.string ft "UBPointerArithmetic"
    | `UBAbort -> Fmt.string ft "UBAbort"
    | `UBArithShift -> Fmt.string ft "UBArithShift"
    | `UBTransmute -> Fmt.string ft "UBTransmute"
    | `UBTreeBorrow -> Fmt.string ft "UBTreeBorrow"
    | `DeadVariable -> Fmt.string ft "DeadVariable"
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `MisalignedPointer -> Fmt.string ft "MisalignedPointer"
    | `RefToUninhabited -> Fmt.string ft "RefToUninhabited"
    | `InvalidLayout -> Fmt.string ft "InvalidLayout"
    | `MemoryLeak -> Fmt.string ft "Memory leak"
    | `FailedAssert (Some msg) -> Fmt.pf ft "Failed assertion: %s" msg
    | `FailedAssert None -> Fmt.string ft "Failed assertion"
    | `Overflow -> Fmt.string ft "Overflow"
    | `StdErr msg -> Fmt.pf ft "Std error: %s" msg
    | `Panic (Some msg) -> Fmt.pf ft "Panic: %s" msg
    | `Panic None -> Fmt.pf ft "Panic"
    | `UnwindTerminate -> Fmt.string ft "UnwindTerminate"
    | `MetaExpectedError -> Fmt.string ft "MetaExpectedError"
  in
  Fmt.pf ft "@,Trace:@,%a" Call_trace.pp call_trace;
  Format.close_box ()
