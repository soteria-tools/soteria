open Svalue.Infix
open Csymex.Syntax

type t = Svalue.t * Svalue.t [@@deriving eq]

let pp ft (l, u) = Fmt.pf ft "[%a, %a[" Svalue.pp l Svalue.pp u
let sem_eq (a, c) (b, d) = a #== b #&& (c #== d)
let is_inside (l1, u1) (l2, u2) = l2 #<= l1 #&& (u1 #<= u2)
let strictly_inside x (l, u) = l #< x #&& (x #< u)
let size (l, u) = u #- l
let split_at (l, h) x = ((l, x), (x, h))

let of_low_and_chunk low chunk =
  let chunk_size = Chunk.size chunk in
  (low, low #+ (int chunk_size))

let of_low_and_type low ty =
  let+ size = Layout.size_of_s ty in
  (low, low #+ size)
