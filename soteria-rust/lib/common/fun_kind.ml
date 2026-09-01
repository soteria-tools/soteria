type synth_fn = GenericDropInPlace
[@@deriving ord, eq, show { with_path = false }]

type t =
  | Real of Charon.Types.fun_decl_ref [@printer Crate.pp_fun_decl_ref]
  | Synthetic of synth_fn
[@@deriving ord, eq, show { with_path = false }]

let span = function
  | Real fn -> Some (Crate.get_fun fn.id).item_meta.span.data
  | Synthetic _ -> None
