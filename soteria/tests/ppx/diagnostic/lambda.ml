type t =
  | Pair of string * int
      [@diag.format fun ft (s, i) -> Fmt.pf ft "pair(%s,%d)" s i]
[@@deriving diagnostic]
