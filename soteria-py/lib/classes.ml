(* https://docs.python.org/3.12/reference/datamodel.html#the-standard-type-hierarchy *)
type builtin =
  | NoneType
  | NotImplementedType
  | EllipsisType
  | Int
  | Bool
  | Float
  | Complex
[@@deriving eq]

type t = Builtin of builtin | User of { qualname : Istring.t } [@@deriving eq]

let hash (t : t) = Hashtbl.hash t
