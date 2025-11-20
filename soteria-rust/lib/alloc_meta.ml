open Charon

type kind =
  | Heap
  | Function of Types.fun_decl_ref [@printer Crate.pp_fun_decl_ref]
  | VTable of Types.ty [@printer Charon_util.pp_ty]
  | Static of Types.global_decl_ref [@printer Crate.pp_global_decl_ref]
  | StaticString
[@@deriving show { with_path = false }]

type t = { alignment : Typed.T.nonzero Typed.t; kind : kind; span : Meta.span }
[@@deriving show { with_path = false }]
