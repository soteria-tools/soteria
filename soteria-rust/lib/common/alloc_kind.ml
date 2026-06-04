open Charon

type t =
  | Heap
  | Function of Fun_kind.t
  | VTable of Types.ty [@printer Charon_util.pp_ty]
  | Static of Types.global_decl_ref [@printer Crate.pp_global_decl_ref]
  | Const of Types.global_decl_ref [@printer Crate.pp_global_decl_ref]
  | StaticString
  | AnonConst
[@@deriving show { with_path = false }]
