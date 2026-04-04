type t = Parse_error of string [@diag.format "bad format"]
[@@deriving diagnostic]
