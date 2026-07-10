module Call_trace = Soteria.Terminal.Call_trace

type t =
  | Failed_assertion
  | Division_by_zero
  | Invalid_argument_count of {
      function_name : string;
      expected : int;
      actual : int;
    }
  | Invalid_argument_type of {
      function_name : string;
      position : int;
      expected : string;
      actual : Value.kind;
    }

let pp formatter = function
  | Failed_assertion -> Format.pp_print_string formatter "Failed assertion"
  | Division_by_zero -> Format.pp_print_string formatter "Division by zero"
  | Invalid_argument_count { function_name; expected; actual } ->
      Format.fprintf formatter "%s expects %d argument%s, %d given"
        function_name expected
        (if expected = 1 then "" else "s")
        actual
  | Invalid_argument_type { function_name; position; expected; actual } ->
      Format.fprintf formatter "%s argument #%d must be of type %s, %s given"
        function_name position expected (Value.kind_name actual)

module Trace = struct
  type t = {
    location : Php_ir.location option;
    call_trace : Php_ir.location Call_trace.t;
    fuel : Soteria.Symex.Fuel_gauge.t;
  }

  let empty =
    {
      location = None;
      call_trace = Call_trace.empty;
      fuel = Soteria.Symex.Fuel_gauge.infinite;
    }

  let with_fuel fuel = { empty with fuel }
end

type with_trace = t * Php_ir.location Call_trace.t

let decorate ?(message = "Triggering operation") trace error =
  let call_trace =
    match trace.Trace.location with
    | None -> List.rev trace.call_trace
    | Some location ->
        Call_trace.mk_element ~loc:location ~msg:message () :: trace.call_trace
        |> List.rev
  in
  (error, call_trace)

let pp_location formatter location =
  Format.fprintf formatter "%s:%d:%d" location.Php_ir.file location.start.line
    location.start.column

let pp_with_trace formatter (error, call_trace) =
  Format.fprintf formatter "@[<v>%a@,%a@]" pp error
    (Call_trace.pp pp_location)
    call_trace

module Diagnostic = struct
  let as_ranges location =
    let position position = (position.Php_ir.line - 1, position.column - 1) in
    [
      Soteria.Terminal.Diagnostic.mk_range_file location.Php_ir.file
        (position location.start) (position location.end_);
    ]

  let print ((error, call_trace) : with_trace) =
    Soteria.Terminal.Diagnostic.print_diagnostic ~call_trace ~as_ranges
      ~msg:(Format.asprintf "%a" pp error)
      ~severity:Error
end
