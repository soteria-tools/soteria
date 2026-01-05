open Soteria_rust_lib
open Charon.Types

type t

val make :
  Frontend.fun_decl TypeDeclId.Map.t -> Frontend.fun_decl -> t * ty list

val exec :
  fuel:Soteria.Symex.Fuel_gauge.t ->
  t ->
  Summary.t list ->
  ((ty * Summary.t) list, [> `MemoryLeak | `TypeUnsound ]) result
