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
  | `UBPointerArithmetic -> Fmt.string ft "UB: pointer arithmetic"
  | `UBPointerComparison -> Fmt.string ft "UB: pointer comparison"
  | `UBTransmute -> Fmt.string ft "UB: Transmute"
  | `UnwindTerminate -> Fmt.string ft "Terminated unwind"
  | `UseAfterFree -> Fmt.string ft "Use after free"

let pp_err_and_call_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" pp err Call_trace.pp call_trace

type severity = Error | Bug | Warning

let severity = function
  | `MemoryLeak -> Warning
  | e when is_unwindable e -> Error
  | _ -> Bug

module Grace = struct
  module Meta = Charon.Meta

  let utf8_to_byte_offset str idx =
    let i = ref 0 in
    let ofs = ref 0 in
    while !i < idx do
      let len = Uchar.utf_decode_length (String.get_utf_8_uchar str !ofs) in
      ofs := !ofs + len;
      incr i
    done;
    !ofs

  let real_index (file : string) (pos : Meta.loc) =
    let open Syntaxes.FunctionWrap in
    let line = pos.line - 1 in
    let column = pos.col in
    if line == 0 then column
    else
      let@ ic = Channels.with_in_file file in
      let current_index = ref 0 in
      let current_line = ref 0 in
      while !current_line < line + 1 do
        match input_line ic with
        | str when !current_line = line ->
            current_index := !current_index + utf8_to_byte_offset str column;
            incr current_line
        | str ->
            current_index := !current_index + String.length str + 1;
            incr current_line
        | exception End_of_file ->
            (* Terminate here, return 0 *)
            current_index := 0;
            current_line := line
      done;
      !current_index

  let to_grace_ranges (loc : Meta.span) =
    let open Grace.Range in
    let bi = Grace.Byte_index.of_int in
    let span = Option.value ~default:loc.span loc.generated_from_span in
    match span.file.name with
    | Local file ->
        let source : Grace.Source.t =
          if String.starts_with ~prefix:Plugin.lib_root file then
            let ic = open_in file in
            let rec loop acc =
              match input_line ic with
              | s -> loop (s :: acc)
              | exception End_of_file ->
                  close_in ic;
                  List.rev acc |> String.concat "\n"
            in
            let content = loop [] in
            let root_l = String.length Plugin.lib_root in
            let name =
              "$RUSTERIA" ^ String.sub file root_l (String.length file - root_l)
            in
            `String { name = Some name; content }
          else `File file
        in
        let idx1 = real_index file span.beg_loc in
        let idx2 = real_index file span.end_loc in
        [ create ~source (bi idx1) (bi idx2) ]
    | Virtual _ -> []

  let severity_to_grace : severity -> Grace.Diagnostic.Severity.t = function
    | Warning -> Warning
    | Error -> Error
    | Bug -> Bug

  let call_trace_to_labels (call_trace : Call_trace.t) =
    let open Grace.Diagnostic in
    let rec aux acc (call_trace : Call_trace.t) =
      match call_trace with
      | [] -> acc
      | [ { loc; msg } ] ->
          let this =
            to_grace_ranges loc
            |> List.map @@ fun range ->
               Label.primary ~range (Message.of_string msg)
          in
          this @ acc
      | { loc; msg } :: rest ->
          let sec =
            to_grace_ranges loc
            |> List.map @@ fun range ->
               Label.secondary ~range (Message.of_string msg)
          in
          aux (sec @ acc) rest
    in
    aux [] call_trace

  let to_diagnostic ~fn ~call_trace error =
    let open Grace.Diagnostic in
    let severity = severity error |> severity_to_grace in
    let labels = call_trace_to_labels call_trace in
    createf ~labels severity "%a in %s" pp error fn
end
