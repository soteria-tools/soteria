open Charon

(* Functions we stub to avoid problems in the interpreter *)
type fixme_fn = PanicCleanup | CatchUnwindCleanup
[@@deriving ord, show { with_path = false }]

(* Functions we could not stub, but we do for performance *)
type optim_fn =
  | FloatIs of Svalue.FloatClass.t
  | FloatIsFinite
  | FloatIsSign of { positive : bool }
  | AllocImpl
  | Panic of Types.name
[@@deriving ord, show { with_path = false }]

(* Rusteria builtin functions *)
type rusteria_fn = Assert | Assume | Nondet of Types.ty | Panic
[@@deriving ord, show { with_path = false }]

(* Miri builtin functions *)
type miri_fn = AllocId | PromiseAlignement | Nop
[@@deriving ord, show { with_path = false }]

(* Functions related to the allocator, see https://doc.rust-lang.org/src/alloc/alloc.rs.html#11-36 *)
type alloc_fn =
  | Alloc of { zeroed : bool }
  | Dealloc
  | Realloc
  | NoAllocShimIsUnstable
[@@deriving ord, show { with_path = false }]

type stubbed_fn =
  | Alloc of alloc_fn
  | Fixme of fixme_fn
  | Miri of miri_fn
  | Optim of optim_fn
  | Rusteria of rusteria_fn
  | DropInPlace
[@@deriving ord, show { with_path = false }]

type t =
  | Real of Types.fun_decl_ref [@printer Crate.pp_fun_decl_ref]
  | Stubbed of stubbed_fn * Types.generic_params
  | Intrinsic of string * Types.generic_args
  | Builtin of Types.builtin_fun_id * Types.generic_args
[@@deriving ord, show { with_path = false }]
