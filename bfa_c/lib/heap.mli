open Typed
open T

type t [@@deriving show]

val empty : t

val load :
  [< sptr ] Typed.t ->
  Tree_block.Ctype.ctype ->
  t ->
  ( cval Typed.t * t,
    [> `MissingKey
    | `MissingResource
    | `NullDereference
    | `OutOfBounds
    | `UninitializedMemoryAccess
    | `UseAfterFree ]
    * Cerb_location.t )
  Csymex.Result.t

val store :
  [< sptr ] Typed.t ->
  Tree_block.Ctype.ctype ->
  cval Typed.t ->
  t ->
  ( unit * t,
    [> `MissingKey
    | `MissingResource
    | `NullDereference
    | `OutOfBounds
    | `UseAfterFree ]
    * Cerb_location.t )
  Csymex.Result.t

val alloc :
  sint Typed.t ->
  t ->
  ([> sptr ] Typed.t * t, 'a * Cerb_location.t) Csymex.Result.t

val alloc_ty :
  Tree_block.Ctype.ctype ->
  t ->
  ([> sptr ] Typed.t * t, 'a * Cerb_location.t) result Csymex.t

val free :
  [< sptr ] Typed.t ->
  t ->
  ( unit * t,
    [> `DoubleFree | `InvalidFree | `MissingKey | `MissingOwnership ]
    * Cerb_location.t )
  Result.t
