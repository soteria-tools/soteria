open Csymex.Syntax
open Svalue.Infix
open Csymex

module SPmap = Pmap (struct
  type t = Svalue.t
  type value = t
  type 'a symex = 'a Csymex.t

  let sem_eq = Svalue.sem_eq
  let compare = Svalue.compare
  let distinct = Svalue.distinct
  let fresh () = Csymex.nondet TInt
end)

type t = Tree_block.t Freeable.t SPmap.t

let with_ptr (ptr : Svalue.t) (st : t)
    (f : ofs:Svalue.t -> Tree_block.t -> ('a * Tree_block.t, 'err) Result.t) :
    ('a * t, 'err) Result.t =
  let loc = Svalue.Ptr.loc ptr in
  let ofs = Svalue.Ptr.ofs ptr in
  (SPmap.wrap (Freeable.wrap (f ~ofs))) loc st

let load ptr chunk st =
  with_ptr ptr st (fun ~ofs block -> Tree_block.load ofs chunk block)

let store ptr chunk sval st =
  with_ptr ptr st (fun ~ofs block -> Tree_block.store ofs chunk sval block)

let alloc size st =
  let block = Tree_block.alloc size in
  let++ loc, st = SPmap.alloc ~new_codom:block st in
  let ptr = Svalue.Ptr.mk loc Svalue.zero in
  (ptr, st)

let free (ptr : Svalue.t) (st : t) : (unit * t, 'err) Result.t =
  if%sat Svalue.Ptr.ofs ptr #== Svalue.zero then
    (SPmap.wrap
       (Freeable.free ~is_exclusively_owned:Tree_block.is_exclusively_owned))
      (Svalue.Ptr.loc ptr) st
  else Result.error `InvalidFree
