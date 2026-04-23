open Charon

type t

val make :
  UllbcAst.fun_decl Types.TypeDeclId.Map.t ->
  UllbcAst.fun_decl ->
  t * Types.ty list

val exec :
  fuel:Soteria.Symex.Fuel_gauge.t ->
  t ->
  Summary.t list ->
  ((Types.ty * Summary.t) list, [> `MemoryLeak | `TypeUnsound ]) result
