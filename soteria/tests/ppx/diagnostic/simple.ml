type t =
  | Overflow [@diag.kind "INTEGER_OVERFLOW"] [@diag.severity Warning]
      [@diag.format "Overflow"]
  | Parse_error of string [@diag.kind "PARSING_ERROR"] [@diag.severity Error]
      [@diag.format "Parsing error: %a"]
[@@deriving diagnostic]
