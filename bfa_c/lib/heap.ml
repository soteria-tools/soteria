open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Csymex

type 'a err = 'a * Call_trace.t

let add_to_call_trace (err, trace_elem) trace_elem' =
  (err, trace_elem' :: trace_elem)

let with_error_loc_as_call_trace () f =
  let open Csymex.Syntax in
  let+- err, loc = f () in
  (err, Call_trace.singleton ~loc ~msg:"Triggering memory operation" ())

module SPmap = Pmap_direct_access (struct
  type t = T.sloc Typed.t

  module Symex = Csymex

  let pp = Typed.ppa
  let sem_eq x y = Typed.sem_eq x y |> Typed.untyped
  let compare = Typed.compare
  let distinct l = Typed.distinct l |> Typed.untyped
  let subst subst_var t = Typed.subst subst_var t

  let fresh ?(constrs : (t -> Svalue.t list) option) () : t Csymex.t =
    let (constrs : (Svalue.t -> Svalue.t list) option) =
      match constrs with
      | None -> None
      | Some f -> Some (fun loc -> f (Typed.type_ loc))
    in
    let+ v = Csymex.nondet ?constrs TLoc in
    Typed.type_ v

  let iter_vars = Typed.iter_vars
end)

type t = Tree_block.t Freeable.t SPmap.t option
[@@deriving show { with_path = false }]

type serialized = Tree_block.serialized Freeable.serialized SPmap.serialized
[@@deriving show { with_path = false }]

let serialize st =
  match st with
  | None -> []
  | Some st -> SPmap.serialize (Freeable.serialize Tree_block.serialize) st

let subst_serialized (subst_var : Svalue.Var.t -> Svalue.Var.t)
    (serialized : serialized) : serialized =
  SPmap.subst_serialized
    (Freeable.subst_serialized Tree_block.subst_serialized)
    subst_var serialized

let iter_vars_serialized =
  SPmap.iter_vars_serialized
    (Freeable.iter_vars_serialized Tree_block.iter_vars_serialized)

let pp_pretty ~ignore_freed ft st =
  let ignore =
    if ignore_freed then function _, Freeable.Freed -> true | _ -> false
    else fun _ -> false
  in
  match st with
  | None -> Fmt.pf ft "Empty Heap"
  | Some st -> SPmap.pp ~ignore (Freeable.pp Tree_block.pp_pretty) ft st

let empty = None

let log action ptr st =
  L.debug (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action
        Typed.ppa ptr Fmt_ail.pp_loc (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_ptr (ptr : [< T.sptr ] Typed.t) (st : t)
    (f :
      ofs:[< T.sint ] Typed.t ->
      Tree_block.t option ->
      ('a * Tree_block.t option, 'err, 'fix) Result.t) :
    ('a * t, 'err, serialized) Result.t =
  let loc = Typed.Ptr.loc ptr in
  let ofs = Typed.Ptr.ofs ptr in
  (SPmap.wrap (Freeable.wrap (f ~ofs))) loc st

let with_ptr_read_only (ptr : [< T.sptr ] Typed.t) (st : t)
    (f :
      ofs:[< T.sint ] Typed.t ->
      Tree_block.t option ->
      ('a, 'err, Tree_block.serialized) Result.t) =
  let loc = Typed.Ptr.loc ptr in
  let ofs = Typed.Ptr.ofs ptr in
  (SPmap.wrap_read_only (Freeable.wrap_read_only (f ~ofs))) loc st

let load ptr ty st =
  let@ () = with_error_loc_as_call_trace () in
  let@ () = with_loc_err () in
  log "load" ptr st;
  if%sat Typed.Ptr.is_at_null_loc ptr then Result.error `NullDereference
  else with_ptr ptr st (fun ~ofs block -> Tree_block.load ofs ty block)

let store ptr ty sval st =
  let@ () = with_error_loc_as_call_trace () in
  let@ () = with_loc_err () in
  log "store" ptr st;
  if%sat Typed.Ptr.is_at_null_loc ptr then Result.error `NullDereference
  else with_ptr ptr st (fun ~ofs block -> Tree_block.store ofs ty sval block)

let copy_nonoverlapping ~dst ~src ~size st =
  let open Typed.Infix in
  let@ () = with_error_loc_as_call_trace () in
  let@ () = with_loc_err () in
  if%sat Typed.Ptr.is_at_null_loc dst ||@ Typed.Ptr.is_at_null_loc src then
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
  (* Commenting this out as alloc cannot fail *)
  (* let@ () = with_loc_err () in*)
  let@ () = with_error_loc_as_call_trace () in
  let block = Freeable.Alive (Tree_block.alloc size) in
  let** loc, st = SPmap.alloc ~new_codom:block st in
  let ptr = Typed.Ptr.mk loc 0s in
  (* The pointer is necessarily not null *)
  let+ () = Typed.(assume [ not (loc ==@ Ptr.null_loc) ]) in
  Bfa_symex.Compo_res.ok (ptr, st)

let alloc_ty ty st =
  let* size = Layout.size_of_s ty in
  alloc size st

let free (ptr : [< T.sptr ] Typed.t) (st : t) :
    (unit * t, 'err, serialized) Result.t =
  let@ () = with_error_loc_as_call_trace () in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let@ () = with_loc_err () in
    (SPmap.wrap
       (Freeable.free
          ~assert_exclusively_owned:Tree_block.assert_exclusively_owned))
      (Typed.Ptr.loc ptr) st
  else error `InvalidFree

let error err _st =
  let@ () = with_error_loc_as_call_trace () in
  error err

let produce (serialized : serialized) (heap : t) : t Csymex.t =
  let non_null_locs =
    List.map (fun (loc, _) -> Typed.not (Typed.Ptr.null_loc ==@ loc)) serialized
  in
  let* () = Typed.assume non_null_locs in
  SPmap.produce (Freeable.produce Tree_block.produce) serialized heap

let consume (serialized : serialized) (heap : t) :
    (t, 'err, serialized) Csymex.Result.t =
  SPmap.consume (Freeable.consume Tree_block.consume) serialized heap
