open Charon

type t =
  | Heap
  | Function of Fun_kind.t
  | VTable of Types.ty [@printer Fmt.(any "VTable: " ++ Charon_util.pp_ty)]
  | Const of Types.global_decl_ref
      [@printer Fmt.(any "Const: " ++ Crate.pp_global_decl_ref)]
  | Static of Types.global_decl_ref
      [@printer Fmt.(any "Static: " ++ Crate.pp_global_decl_ref)]
  | StaticString
[@@deriving show { with_path = false }]
