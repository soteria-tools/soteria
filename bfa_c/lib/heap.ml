open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Csymex
module Result = Typed.Result

module SPmap = Pmap (struct
  type t = T.sloc Typed.t
  type value = Svalue.t
  type 'a symex = 'a Csymex.t

  let pp = Typed.ppa
  let sem_eq x y = Typed.sem_eq x y |> Typed.untyped
  let compare = Typed.compare
  let distinct l = Typed.distinct l |> Typed.untyped
  let fresh () = Typed.nondet Typed.t_loc
end)

type t = Tree_block.t Freeable.t SPmap.t [@@deriving show { with_path = false }]

let pp_pretty ~ignore_freed =
  let ignore =
    if ignore_freed then function _, Freeable.Freed -> true | _ -> false
    else fun _ -> false
  in
  SPmap.pp ~ignore (Freeable.pp Tree_block.pp_pretty)

let empty = SPmap.empty

let log action ptr st =
  L.debug (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action
        Typed.ppa ptr Fmt_ail.pp_loc (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_ptr (ptr : [< T.sptr ] Typed.t) (st : t)
    (f :
      ofs:[< T.sint ] Typed.t ->
      Tree_block.t ->
      ('a * Tree_block.t, 'err) Result.t) : ('a * t, 'err) Result.t =
  let loc = Typed.Ptr.loc ptr in
  let ofs = Typed.Ptr.ofs ptr in
  (SPmap.wrap (Freeable.wrap (f ~ofs))) loc st

let with_ptr_read_only (ptr : [< T.sptr ] Typed.t) (st : t)
    (f : ofs:[< T.sint ] Typed.t -> Tree_block.t -> ('a, 'err) Result.t) =
  let loc = Typed.Ptr.loc ptr in
  let ofs = Typed.Ptr.ofs ptr in
  (SPmap.wrap_read_only (Freeable.wrap_read_only (f ~ofs))) loc st

let load ptr ty st =
  let@ () = with_loc_err () in
  log "load" ptr st;
  if%sat Typed.Ptr.is_at_null_loc ptr then Result.error `NullDereference
  else with_ptr ptr st (fun ~ofs block -> Tree_block.load ofs ty block)

let store ptr ty sval st =
  let@ () = with_loc_err () in
  log "store" ptr st;
  if%sat Typed.Ptr.is_at_null_loc ptr then Result.error `NullDereference
  else with_ptr ptr st (fun ~ofs block -> Tree_block.store ofs ty sval block)

let copy_nonoverlapping ~dst ~src ~size st =
  let open Typed.Infix in
  let@ () = with_loc_err () in
  if%sat (Typed.Ptr.is_at_null_loc dst) #|| (Typed.Ptr.is_at_null_loc src) then
    Result.error `NullDereference
  else
    let** tree_to_write =
      with_ptr_read_only src st (fun ~ofs block ->
          let++ tree, _ = Tree_block.get_raw_tree_owned ofs size block in
          tree)
    in
    with_ptr dst st (fun ~ofs block ->
        Tree_block.put_raw_tree ofs tree_to_write block)

let alloc size st =
  let@ () = with_loc_err () in
  let block = Freeable.Alive (Tree_block.alloc size) in
  let** loc, st = SPmap.alloc ~new_codom:block st in
  let ptr = Typed.Ptr.mk loc 0s in
  (* The pointer is necessarily not null *)
  Result.ok ~learned:Typed.[ not loc #== Ptr.null_loc ] (ptr, st)

let alloc_ty ty st =
  let* size = Layout.size_of_s ty in
  alloc size st

let free (ptr : [< T.sptr ] Typed.t) (st : t) : (unit * t, 'err) Result.t =
  let is_exclusively_owned tb =
    (* TODO: this will be unnecessary when the core library is properly typed *)
    let+ r = Tree_block.is_exclusively_owned tb in
    Typed.untyped r
  in
  if%sat (Typed.Ptr.ofs ptr) #== 0s then
    let@ () = with_loc_err () in
    (SPmap.wrap (Freeable.free ~is_exclusively_owned)) (Typed.Ptr.loc ptr) st
  else error `InvalidFree
