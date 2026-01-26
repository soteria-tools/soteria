type synth_fn = GenericDropInPlace
[@@deriving ord, show { with_path = false }]

type t =
  | Real of Charon.Types.fun_decl_ref [@printer Crate.pp_fun_decl_ref]
  | Synthetic of synth_fn
[@@deriving ord, show { with_path = false }]
