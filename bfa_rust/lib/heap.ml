open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex
module Sptr = Sptr.ArithPtr

type 'a err = 'a * Call_trace.t

let add_to_call_trace (err, trace_elem) trace_elem' =
  (err, trace_elem' :: trace_elem)

let with_error_loc_as_call_trace () f =
  let open Rustsymex.Syntax in
  let+- err, loc = f () in
  (err, Call_trace.singleton ~loc ~msg:"Triggering memory operation" ())

module SPmap = Pmap_direct_access (struct
  include Typed
  module Symex = Rustsymex

  type t = T.sloc Typed.t

  let pp = ppa
  let fresh ?constrs () = Rustsymex.nondet ?constrs Typed.t_loc
end)

type global = String of string | Global of Charon.Types.global_decl_id
[@@deriving show { with_path = false }, ord]

module GlobMap = struct
  include Map.Make (struct
    type t = global

    let compare = compare_global
  end)

  let pp pp_v fmt m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") pp_global pp_v in
    Fmt.pf fmt "%a" (Fmt.iter_bindings ~sep:Fmt.comma iter pp_pair) m
end

type sub_t = Tree_block.t * Tree_borrow.t

type t = {
  heap : (Tree_block.t * Tree_borrow.t) Freeable.t SPmap.t option;
  globals : Sptr.t GlobMap.t;
}
[@@deriving show { with_path = false }]

type serialized = Tree_block.serialized Freeable.serialized SPmap.serialized
[@@deriving show { with_path = false }]

let serialize { heap; _ } =
  match heap with
  | None -> []
  | Some heap ->
      SPmap.serialize
        (Freeable.serialize (fun (tb, _) -> Tree_block.serialize tb))
        heap

let subst_serialized (subst_var : Svalue.Var.t -> Svalue.Var.t)
    (serialized : serialized) : serialized =
  SPmap.subst_serialized
    (Freeable.subst_serialized Tree_block.subst_serialized)
    subst_var serialized

let iter_vars_serialized (s : serialized) :
    (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit =
  SPmap.iter_vars_serialized
    (Freeable.iter_vars_serialized Tree_block.iter_vars_serialized)
    s

let pp_pretty ~ignore_freed ft { heap; _ } =
  let ignore =
    if ignore_freed then function _, Freeable.Freed -> true | _ -> false
    else fun _ -> false
  in
  match heap with
  | None -> Fmt.pf ft "Empty Heap"
  | Some st ->
      SPmap.pp ~ignore
        (Freeable.pp (fun fmt (tb, _) -> Tree_block.pp_pretty fmt tb))
        ft st

let empty = { heap = None; globals = GlobMap.empty }

let log action ptr st =
  L.debug (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action
        Sptr.pp ptr Call_trace.pp_span (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_ptr ((ptr, _, _) : Sptr.t) ({ heap; _ } as st : t)
    (f :
      ofs:[< T.sint ] Typed.t ->
      sub_t option ->
      ('a * sub_t option, 'err, 'fix list) Result.t) :
    ('a * t, 'err, serialized list) Result.t =
  let loc, ofs = Typed.Ptr.decompose ptr in
  let++ v, heap = (SPmap.wrap (Freeable.wrap (f ~ofs))) loc heap in
  (v, { st with heap })

let load ?is_move ((_, meta, tag) as ptr) ty st =
  let@ () = with_error_loc_as_call_trace () in
  let@ () = with_loc_err () in
  log "load" ptr st;
  if%sat Sptr.is_null ptr then Result.error `NullDereference
  else
    with_ptr ptr st (fun ~ofs block ->
        let block, tb =
          match block with
          | None -> (None, Tree_borrow.init ())
          | Some (block, tb) -> (Some block, tb)
        in
        L.debug (fun f ->
            f "Recursively reading from block tree at %a:@.%a" Sptr.pp ptr
              Fmt.(option ~none:(any "None") Tree_block.pp)
              block);
        let rec aux block = function
          | `Done v -> Result.ok (v, block)
          | `More (blocks, callback) ->
              L.debug (fun f ->
                  let pp_block ft (ty, ofs) =
                    Fmt.pf ft "%s [%a] "
                      (Charon_util.lit_to_string ty)
                      Typed.ppa ofs
                  in
                  f "Loading blocks [%a]" Fmt.(list ~sep:comma pp_block) blocks);
              let** values, block =
                Result.fold_list blocks ~init:([], block)
                  ~f:(fun (vals, block) (ty, ofs) ->
                    let++ value, block =
                      Tree_block.load ?is_move ofs ty tag tb block
                    in
                    (value :: vals, block))
              in
              let values = List.rev values in
              let* res = callback values in
              aux block res
        in
        let parser = Encoder.rust_of_cvals ~offset:ofs ?meta ty in
        let++ value, block = aux block parser in
        L.debug (fun f ->
            f "Finished reading rust value %a"
              (Charon_util.pp_rust_val Sptr.pp)
              value);
        let block = Option.map (fun b -> (b, tb)) block in
        (value, block))

let store ((_, _, tag) as ptr) ty sval st =
  let@ () = with_error_loc_as_call_trace () in
  let@ () = with_loc_err () in
  log "store" ptr st;
  if%sat Sptr.is_null ptr then Result.error `NullDereference
  else
    with_ptr ptr st (fun ~ofs block ->
        let block, tb =
          match block with
          | None -> (None, Tree_borrow.init ())
          | Some (block, tb) -> (Some block, tb)
        in
        let parts = Encoder.rust_to_cvals ~offset:ofs sval ty in
        L.debug (fun f ->
            let pp_part f ({ value; ty; offset } : Encoder.cval_info) =
              Fmt.pf f "%a: %s [%a]"
                (Charon_util.pp_rust_val Sptr.pp)
                value
                (Charon_util.lit_to_string ty)
                Typed.ppa offset
            in
            f "Parsed to parts [%a]" (Fmt.list ~sep:Fmt.comma pp_part) parts);
        let++ (), block =
          Result.fold_list parts ~init:((), block)
            ~f:(fun ((), block) { value; ty; offset } ->
              Tree_block.store offset ty value tag tb block)
        in
        let block = Option.map (fun b -> (b, tb)) block in
        ((), block))

let alloc size ({ heap; _ } as st) =
  (* Commenting this out as alloc cannot fail *)
  (* let@ () = with_loc_err () in*)
  let@ () = with_error_loc_as_call_trace () in
  let tb = Tree_borrow.init () in
  let block = Freeable.Alive (Tree_block.alloc size, tb) in
  let** loc, heap = SPmap.alloc ~new_codom:block heap in
  let ptr = Typed.Ptr.mk loc 0s in
  let ptr = (ptr, None, tb.tag) in
  (* The pointer is necessarily not null *)
  let+ () = assume [ Typed.(not (loc ==@ Ptr.null_loc)) ] in
  Bfa_symex.Compo_res.ok (ptr, { st with heap })

let alloc_ty ty st =
  let* size = Layout.size_of_s ty in
  alloc size st

let free ((ptr, _, _) : Sptr.t) ({ heap; _ } as st : t) :
    (unit * t, 'err, serialized list) Result.t =
  let@ () = with_error_loc_as_call_trace () in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let@ () = with_loc_err () in
    (* TODO: does the tag not play a role in freeing? *)
    let++ (), heap =
      SPmap.wrap
        (Freeable.free ~assert_exclusively_owned:(fun t ->
             Tree_block.assert_exclusively_owned @@ Option.map fst t))
        (Typed.Ptr.loc ptr) heap
    in
    ((), { st with heap })
  else error `InvalidFree

let uninit ptr ty st =
  let@ () = with_error_loc_as_call_trace () in
  let@ () = with_loc_err () in
  log "uninit" ptr st;
  let* size = Layout.size_of_s ty in
  if%sat Sptr.is_null ptr then Result.error `NullDereference
  else
    with_ptr ptr st (fun ~ofs block ->
        let block, tb =
          match block with
          | None -> (None, Tree_borrow.init ())
          | Some (block, tb) -> (Some block, tb)
        in
        let++ (), block = Tree_block.uninit_range ofs size block in
        let block = Option.map (fun b -> (b, tb)) block in
        ((), block))

let error err _st =
  let@ () = with_error_loc_as_call_trace () in
  error err

let store_str_global str ptr ({ globals; _ } as st) =
  let globals = GlobMap.add (String str) ptr globals in
  Result.ok ((), { st with globals })

let store_global g ptr ({ globals; _ } as st) =
  let globals = GlobMap.add (Global g) ptr globals in
  Result.ok ((), { st with globals })

let load_str_global str ({ globals; _ } as st) =
  let ptr = GlobMap.find_opt (String str) globals in
  Result.ok (ptr, st)

let load_global g ({ globals; _ } as st) =
  let ptr = GlobMap.find_opt (Global g) globals in
  Result.ok (ptr, st)
