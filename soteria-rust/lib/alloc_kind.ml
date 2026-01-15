open Charon

type t =
  | Heap
  | Function of Fun_kind.t
  | VTable of Types.ty [@printer Charon_util.pp_ty]
  | Static of Types.global_decl_ref [@printer Crate.pp_global_decl_ref]
  | StaticString
[@@deriving show { with_path = false }]
