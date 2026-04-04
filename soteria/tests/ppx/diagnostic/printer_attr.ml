module Types = struct
  type ty = int
end

let pp_ty ft i = Fmt.pf ft "T%d" i

type t =
  | Variant of (Types.ty [@printer pp_ty]) [@diag.format "v=%a"]
[@@deriving diagnostic]
