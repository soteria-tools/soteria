type t =
  [ `NullDereference
  | `OutOfBounds
  | `UninitializedMemoryAccess
  | `UseAfterFree
  | `DivisionByZero
  | `ParsingError of string
  | `LinkError of string
  | `UBPointerComparison
  | `UBPointerArithmetic
  | `InvalidFunctionPtr
  | `DoubleFree
  | `InvalidFree
  | `Memory_leak
  | `FailedAssert ]

type severity = Error | Warning

let pp ft = function
  | `NullDereference -> Fmt.string ft "Null pointer dereference"
  | `OutOfBounds -> Fmt.string ft "Buffer overflow or underflow"
  | `UninitializedMemoryAccess -> Fmt.string ft "Accessing uninitialized memory"
  | `UseAfterFree -> Fmt.string ft "Use after free"
  | `DivisionByZero -> Fmt.string ft "Division by zero"
  | `ParsingError s -> Fmt.pf ft "Parsing Error: %s" s
  | `LinkError s -> Fmt.pf ft "Linker Error: %s" s
  | `UBPointerComparison -> Fmt.string ft "Invalid pointer comparison (UB)"
  | `UBPointerArithmetic -> Fmt.string ft "Invalid pointer arithmetic (UB)"
  | `InvalidFunctionPtr -> Fmt.string ft "Using invalid function pointer"
  | `DoubleFree -> Fmt.string ft "Freeing a pointer twice (double free)"
  | `InvalidFree ->
      Fmt.string ft "Freeing a pointer that was not obtained from malloc"
  | `Memory_leak -> Fmt.string ft "Memory leak"
  | `FailedAssert -> Fmt.string ft "Failed assertion"

let severity = function `Memory_leak -> Warning | _ -> Error

module Grace = struct
  let real_index (file : string) (pos : Cerb_position.t) =
    let open Syntaxes.FunctionWrap in
    let line = Cerb_position.line pos - 1 in
    let column = Cerb_position.column pos - 1 in
    if line == 0 then column
    else
      let@ ic = Channels.with_in_file file in
      let current_index = ref 0 in
      let current_line = ref 0 in
      let current_column = ref 0 in
      while !current_line < line || !current_column < column do
        match input_char ic with
        | '\n' ->
            incr current_line;
            current_column := 0;
            incr current_index
        | _ ->
            incr current_column;
            incr current_index
        | exception End_of_file ->
            (* Terminate here, return 0 *)
            current_index := 0;
            current_column := column;
            current_line := line
      done;
      !current_index

  let cerb_to_grace_ranges (cerb_loc : Cerb_location.t) =
    let open Grace.Range in
    let bi = Grace.Byte_index.of_int in
    match cerb_loc with
    | Loc_unknown | Loc_other _ -> []
    | Loc_point position ->
        let file = Cerb_position.file position in
        let idx = real_index file position in
        let idx = bi idx in
        [ create ~source:(`File file) idx idx ]
    | Loc_region (pos1, pos2, _) ->
        let file = Cerb_position.file pos1 in
        (* Could be optimised if indexes are in the same file,
           but I don't think printing errors is what's going to take time.
           Also, it shouldn't be required anyway, see https://github.com/johnyob/grace/issues/46
        *)
        let idx1 = real_index file pos1 in
        let idx2 = real_index file pos2 in
        [ create ~source:(`File file) (bi idx1) (bi idx2) ]
    | Loc_regions (l, _) ->
        List.map
          (fun (pos1, pos2) ->
            let file = Cerb_position.file pos1 in
            let idx1 = real_index file pos1 in
            let idx2 = real_index file pos2 in
            create ~source:(`File file) (bi idx1) (bi idx2))
          l

  let severity_to_grace (sev : severity) : Grace.Diagnostic.Severity.t =
    match sev with Warning -> Warning | Error -> Error

  let call_trace_to_labels (call_trace : Call_trace.t) =
    let open Grace.Diagnostic in
    let rec aux acc (call_trace : Call_trace.t) =
      match call_trace with
      | [] -> acc
      | [ { loc; msg } ] ->
          let this =
            cerb_to_grace_ranges loc
            |> List.map (fun range ->
                   Label.primary ~range (Message.of_string msg))
          in
          this @ acc
      | { loc; msg } :: rest ->
          let sec =
            cerb_to_grace_ranges loc
            |> List.map (fun range ->
                   Label.secondary ~range (Message.of_string msg))
          in
          aux (sec @ acc) rest
    in
    aux [] call_trace

  let to_diagnostic ~fid ~call_trace error =
    let open Grace.Diagnostic in
    let severity = severity error |> severity_to_grace in
    let labels = call_trace_to_labels call_trace in
    let diagnostic =
      createf ~labels severity "%a in %a" pp error Ail_helpers.pp_sym_hum fid
    in
    diagnostic
end
