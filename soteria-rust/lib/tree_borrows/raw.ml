(** A simple Tree Borrows implementation. It doesn't support any sort of
    symbolic reasoning or anything fancy, it's just the raw logic. *)

open Common
module Ptr_tag = Svalue.Ptr_tag

(** Whether this node has a protector (this is distinct from having the
    protector toggled!), its parents (including this node's ID!), and its
    initial state if it doesn't exist in the state. *)
type node = {
  protector : protector option;
  parents : Ptr_tag.WeakSet.t;
  initial_state : state;
}
[@@deriving show { with_path = false }]

let pp_state fmt = function
  | Reserved true -> Fmt.string fmt "Re T"
  | Reserved false -> Fmt.string fmt "Re F"
  | Unique -> Fmt.string fmt "Uniq"
  | Frozen -> Fmt.string fmt "Froz"
  | ReservedIM -> Fmt.string fmt "ReIM"
  | Cell -> Fmt.string fmt "Cell"
  | Disabled -> Fmt.string fmt "Dis "

let[@inline] meet st1 st2 =
  match (st1, st2) with
  | Disabled, _ | _, Disabled -> Disabled
  | Frozen, _ | _, Frozen -> Frozen
  | Unique, _ | _, Unique -> Unique
  | Reserved b1, Reserved b2 -> Reserved (b1 || b2)
  | ReservedIM, ReservedIM -> ReservedIM
  | Cell, Cell -> Cell
  | Cell, _ | _, Cell -> L.failwith "Can't compare Cell with non-Cell"
  | Reserved _, ReservedIM | ReservedIM, Reserved _ ->
      L.failwith "Can't compare Reserved and ReservedIM"

let[@inline] meet' (p1, st1) (p2, st2) = (p1 || p2, meet st1 st2)

type exn += AliasingError

let transition =
  let[@inline] transition st local act =
    match (st, local, act) with
    | Cell, _, _ -> Cell
    | Reserved b, _, Read -> Reserved b
    | Reserved _, Local, Write -> Unique
    | Reserved _, Foreign, Write -> Disabled
    | Unique, Local, _ -> Unique
    | Unique, Foreign, Read -> Frozen
    | Unique, Foreign, Write -> Disabled
    | Frozen, _, Read -> Frozen
    | Frozen, Local, Write -> raise_notrace AliasingError
    | Frozen, Foreign, Write -> Disabled
    | ReservedIM, _, Read | ReservedIM, Foreign, _ -> ReservedIM
    | ReservedIM, Local, Write -> Unique
    | Disabled, Foreign, _ -> Disabled
    | Disabled, Local, _ -> raise_notrace AliasingError
  in

  let[@inline] transition_protected st local act =
    match (st, local, act) with
    | Reserved false, Foreign, Read -> Reserved true
    | Reserved true, _, Write -> raise_notrace AliasingError
    | Unique, Foreign, Read -> raise_notrace AliasingError
    | _ ->
        let st' = transition st local act in
        if[@cold] Common.equal_state st' Disabled then
          raise_notrace AliasingError;
        st'
  in
  fun ~protected -> if protected then transition_protected else transition

(* Compact the root trie when it reaches this size. Dead ephemeron leaves become
   Empty after GC but their parent Branch nodes persist, making the walk over
   [root] in [access] O(total borrows) instead of O(live). Triggering on map
   size rather than tag count means small maps (e.g. a freshly allocated
   variable with few borrows) never pay the GC cost. *)
let compact_threshold = 128
let compact_map = Ptr_tag.WeakMap.filter_map_no_share (Fun.const Option.some)

type t = {
  tags : node Ptr_tag.WeakMap.t;
  known_size : int;
  next_compact_at : int;
}

let pp ft { tags; _ } =
  Fmt.iter_bindings Ptr_tag.WeakMap.iter
    (fun ft (tag, node) -> Fmt.pf ft "%a -> %a" Ptr_tag.pp tag pp_node node)
    ft tags

let show = Fmt.to_to_string pp

let init ?(initial_state = Unique) () =
  let tag = Ptr_tag.fresh_tag () in
  let parents = Ptr_tag.WeakSet.create 1 in
  Ptr_tag.WeakSet.add parents tag;
  let node = { protector = None; parents; initial_state } in
  let tags = Ptr_tag.WeakMap.singleton tag node in
  (tag, { tags; known_size = 1; next_compact_at = compact_threshold })

let ub_state = fst @@ init ~initial_state:Disabled ()

let[@inline] compact ({ tags; known_size; next_compact_at } as st : t) =
  if known_size < next_compact_at then st
  else (
    Gc.minor ();
    let tags = compact_map tags in
    let known_size = Ptr_tag.WeakMap.cardinal tags in
    if known_size < next_compact_at then
      { tags; known_size; next_compact_at = compact_threshold }
    else (
      (* Tags survived minor GC — they are promoted; need a full cycle. *)
      Gc.full_major ();
      let tags = compact_map tags in
      let known_size = Ptr_tag.WeakMap.cardinal tags in
      (* If still large after a full GC all entries are genuinely live; back off
         exponentially so we don't retry on every subsequent borrow. *)
      let next_compact_at =
        if known_size >= next_compact_at then known_size * 2
        else compact_threshold
      in
      { tags; known_size; next_compact_at }))

let borrow ?protector parent ~state (st : t) =
  let tag = Ptr_tag.fresh_tag () in
  let ({ tags; known_size; _ } as st) = compact st in
  let node_parent = Ptr_tag.WeakMap.find parent tags in
  let parents =
    Ptr_tag.WeakSet.create (Ptr_tag.WeakSet.count node_parent.parents + 1)
  in
  Ptr_tag.WeakSet.add parents tag;
  Ptr_tag.WeakSet.iter (Ptr_tag.WeakSet.add parents) node_parent.parents;
  let node = { protector; parents; initial_state = state } in
  let tags = Ptr_tag.WeakMap.add tag node tags in
  (tag, { st with tags; known_size = known_size + 1 })

let unprotect tag ({ tags; _ } as st) =
  let tags =
    Ptr_tag.WeakMap.update tag
      (function
        | None -> raise Not_found | Some n -> Some { n with protector = None })
      tags
  in
  { st with tags }

let strong_protector_exists { tags; _ } =
  (* Annoyingly, there is no [exists], only [for_all] *)
  not
    (Ptr_tag.WeakMap.for_all
       (fun _ { protector; _ } -> protector <> Some Strong)
       tags)

(** [tag -> (protected * state)], [protected] indicating the tag's protector
    (managed outside [tb_state]) was toggled. *)
type tb_state = (bool * state) Ptr_tag.WeakMap.t

(* Lets [access] walk the [root] trie (the structure, mapping tags to nodes) and
   the [tb_state] trie together in a single pass. *)
module WeakMap_with_root = Ptr_tag.WeakMap.WithForeign (Ptr_tag.WeakMap.BaseMap)

let pp_tb_state =
  Fmt.iter_bindings ~sep:(Fmt.any ", ") Ptr_tag.WeakMap.iter
    (fun ft (tag, (protected, st)) ->
      Fmt.pf ft "%a -> %a%s" Ptr_tag.pp tag pp_state st
        (if protected then " (p)" else ""))

let empty_state = Ptr_tag.WeakMap.empty
let is_empty_state = Ptr_tag.WeakMap.is_empty

let equal_state =
  Ptr_tag.WeakMap.reflexive_equal (Pair.equal Bool.equal equal_state)

let set_protector ~protected tag ({ tags; _ } : t) =
  Ptr_tag.WeakMap.update tag (function
    | None ->
        let node = Ptr_tag.WeakMap.find tag tags in
        Some (protected, node.initial_state)
    | Some (_, st) -> Some (protected, st))

(** [access accessed im e structure state]: Update all nodes in the mapping
    [state] for the tree structure [structure] with an event [e], that happened
    at [accessed]. *)
let access accessed e ({ tags; _ } as root : t) (st : tb_state) =
  let accessed_node = Ptr_tag.WeakMap.find accessed tags in
  let parents = accessed_node.parents in
  try
    WeakMap_with_root.update_multiple_from_foreign tags
      {
        f =
          (fun tag old (Snd { protector; initial_state; _ }) ->
            let protected0, st0 =
              match old with None -> (false, initial_state) | Some ps -> ps
            in
            let rel =
              if Ptr_tag.WeakSet.mem parents tag then Local else Foreign
            in
            (* if the tag has a protector and is accessed, this toggles the
               protector! *)
            let protected =
              (protected0 || Ptr_tag.equal tag accessed)
              && Option.is_some protector
            in
            let st' = transition ~protected st0 rel e in
            if (not protected) && Common.equal_state st' initial_state then None
            else if protected = protected0 && Common.equal_state st' st0 then
              old
            else Some (protected, st'));
      }
      st
    |> Result.ok
  with AliasingError ->
    [%l.debug
      "TB: Undefined behavior encountered for %a at %a in structure@.%a"
        pp_access e Ptr_tag.pp accessed pp root];
    Result.error `AliasingError

let merge = Ptr_tag.WeakMap.idempotent_union @@ fun _ -> meet'
