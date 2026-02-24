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
  | `FailedAssert
  | `Overflow
  | `Gave_up of string ]

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
  | `Overflow -> Fmt.string ft "Integer overflow"
  | `Gave_up s -> Fmt.pf ft "Analysis gave up: %s" s

(** Same as `show` but does not include details about the error, only the kind.
*)
let kind_string = function
  | `NullDereference -> "NULL_DEREFERENCE"
  | `OutOfBounds -> "BUFFER_OVERFLOW"
  | `UninitializedMemoryAccess -> "UNINITIALIZED_VALUE"
  | `UseAfterFree -> "USE_AFTER_FREE"
  | `DivisionByZero -> "DIVISION_BY_ZERO"
  | `ParsingError _ -> "PARSING_ERROR"
  | `LinkError _ -> "LINK_ERROR"
  | `UBPointerComparison -> "POINTER_COMPARISON_UB"
  | `UBPointerArithmetic -> "POINTER_ARITHMETIC_UB"
  | `InvalidFunctionPtr -> "INVALID_FUNCTION_POINTER"
  | `DoubleFree -> "DOUBLE_FREE"
  | `InvalidFree -> "INVALID_FREE"
  | `Memory_leak -> "MEMORY_LEAK"
  | `FailedAssert -> "ASSERTION_FAILURE"
  | `Overflow -> "INTEGER_OVERFLOW"
  | `Gave_up _ -> "GAVE_UP"

let is_ub = function
  | `NullDereference | `OutOfBounds | `UninitializedMemoryAccess | `UseAfterFree
  | `DivisionByZero | `UBPointerComparison | `UBPointerArithmetic
  | `InvalidFunctionPtr | `DoubleFree | `InvalidFree | `Overflow
  | `Memory_leak (* Not really UB but we want to ignore it in Test-Comp *) ->
      true
  | `LinkError _ | `FailedAssert | `ParsingError _ | `Gave_up _ -> false

let severity : t -> Soteria.Terminal.Diagnostic.severity = function
  | `Memory_leak -> Warning
  | _ -> Error

type with_trace = t * Cerb_location.t Soteria.Terminal.Call_trace.t

let with_trace ?(msg = "Triggering operation") e loc =
  (e, Soteria.Terminal.Call_trace.singleton ~loc ~msg ())

let add_to_call_trace ((err, trace_elem) : with_trace) trace_elem' =
  (err, trace_elem' :: trace_elem)

module Diagnostic = struct
  let to_loc pos =
    let col = Cerb_position.column pos in
    (Cerb_position.line pos - 1, col - 1)

  let as_ranges (cerb_loc : Cerb_location.t) =
    let mk_range_file = Soteria.Terminal.Diagnostic.mk_range_file in
    match cerb_loc with
    | Loc_unknown | Loc_other _ -> []
    | Loc_point position ->
        let file = Cerb_position.file position in
        let loc = to_loc position in
        [ mk_range_file file loc loc ]
    | Loc_region (pos1, pos2, _) ->
        let file = Cerb_position.file pos1 in
        let loc1 = to_loc pos1 in
        let loc2 = to_loc pos2 in
        [ mk_range_file file loc1 loc2 ]
    | Loc_regions (l, _) ->
        List.map
          (fun (pos1, pos2) ->
            let file = Cerb_position.file pos1 in
            let loc1 = to_loc pos1 in
            let loc2 = to_loc pos2 in
            mk_range_file file loc1 loc2)
          l

  let print_diagnostic ~fid ~call_trace ~error =
    Soteria.Terminal.Diagnostic.print_diagnostic ~call_trace ~as_ranges
      ~error:(Fmt.to_to_string pp error)
      ~severity:(severity error) ~fname:fid

  let extract_location (cerb_loc : Cerb_location.t) =
    match cerb_loc with
    | Loc_unknown | Loc_other _ -> None
    | Loc_point position ->
        let file = Cerb_position.file position in
        let line, col = to_loc position in
        Some (file, line + 1, col + 1)
        (* Convert back to 1-indexed *)
    | Loc_region (pos1, _, _) ->
        let file = Cerb_position.file pos1 in
        let line, col = to_loc pos1 in
        Some (file, line + 1, col + 1)
    | Loc_regions ((pos1, _) :: _, _) ->
        let file = Cerb_position.file pos1 in
        let line, col = to_loc pos1 in
        Some (file, line + 1, col + 1)
    | Loc_regions ([], _) -> None

  let to_json ~fid ~call_trace ~error =
    let kind = kind_string error in
    let sev = Soteria.Terminal.Diagnostic.show_severity (severity error) in
    let details = Fmt.to_to_string pp error in

    (* Extract location from call trace if available *)
    let file, line, column =
      match call_trace with
      | [] -> (fid, 0, 0)
      | elem :: _ -> (
          match extract_location elem.Soteria.Terminal.Call_trace.loc with
          | Some (f, l, c) -> (f, l, c)
          | None -> (fid, 0, 0))
    in

    `Assoc
      [
        ("kind", `String kind);
        ("details", `String details);
        ("severity", `String sev);
        ("file", `String file);
        ("line", `Int line);
        ("column", `Int column);
        ("function", `String fid);
      ]
end

module Exit_code = struct
  type t =
    | Success  (** Everything terminated correctly and no bug was found. *)
    | Found_bug  (** Analysis was successful and found a bug in the program. *)
    | Tool_error  (** Soteria-C gave up on analysis *)
    | Arg_parsing_error  (** User gave invalid arguments *)

  let to_int = function
    | Success -> 0
    | Found_bug -> 13
    | Tool_error -> 2
    | Arg_parsing_error -> Cmdliner.Cmd.Exit.cli_error

  let explain = function
    | Success -> "on successful analysis (no bugs found)."
    | Found_bug -> "when a bug or error was found in the analysed program."
    | Tool_error ->
        "when Soteria-C did not complete the analysis because of a missing \
         feature or internal error."
    | Arg_parsing_error -> "on invalid cli arguments"
end
