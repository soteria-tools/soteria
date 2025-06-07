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

let severity : t -> Soteria_terminal.Diagnostic.severity = function
  | `Memory_leak -> Warning
  | _ -> Error

module Diagnostic = struct
  let to_loc pos = (Cerb_position.line pos - 1, Cerb_position.column pos - 1)

  let as_ranges (cerb_loc : Cerb_location.t) =
    let mk_range_file = Soteria_terminal.Diagnostic.mk_range_file in
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

  let mk_diagnostic ~fid ~call_trace ~error =
    Soteria_terminal.Diagnostic.mk_diagnostic ~call_trace ~as_ranges
      ~error:(Fmt.to_to_string pp error)
      ~severity:(severity error)
      ~fname:(Fmt.to_to_string Ail_helpers.pp_sym_hum fid)
end
