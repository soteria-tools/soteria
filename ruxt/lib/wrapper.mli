open Soteria_rust_lib
open Charon.Types

type t

val make :
  Frontend.fun_decl TypeDeclId.Map.t -> Frontend.fun_decl -> t * ty list

val process :
  t ->
  Summary.t list ->
  ( Heap.Sptr.t Rust_val.t * Heap.t * ty,
    Error.t Heap.err,
    Heap.serialized )
  Rustsymex.Result.t
