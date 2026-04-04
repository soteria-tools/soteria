Reject invalid diagnostic format arity
  $ ../test.sh invalid_format.ml
  File "invalid_format.ml", lines 1-2, characters 0-23:
  1 | type t = Parse_error of string [@diag.format "bad format"]
  2 | [@@deriving diagnostic]
  Error: [@deriving diagnostic] constructor Parse_error has 1 args but [@diag.format] has 0 %a placeholders
  [1]
