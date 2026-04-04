type t =
  [ `Overflow [@diag.kind "INTEGER_OVERFLOW"] [@diag.severity Warning]
  | `Parse of string [@diag.format "parse=%a"] ]
[@@deriving diagnostic]
