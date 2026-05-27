(** A simple Tree Borrows implementation. It doesn't support any sort of
    symbolic reasoning or anything fancy, it's just the raw logic. *)

open Common

module Tag : sig
  type t [@@deriving show, eq]

  val fresh_tag : unit -> t
  val zero : t

  module WeakMap : PatriciaTree.MAP with type key = t

  module WeakSet : sig
    include Weak.S with type data = t
    include Sigs.Printable with type t := t
  end
end = struct
  type t = Tag of int [@@ocaml.boxed]

  let[@inline] equal (Tag t1) (Tag t2) = Int.equal t1 t2
  let pp fmt (Tag tag) = Fmt.pf fmt "‖%d‖" tag
  let show = Fmt.to_to_string pp
  let zero = Tag 0
  let tag_counter = ref 0

  let fresh_tag () =
    incr tag_counter;
    Tag !tag_counter

  module Key = struct
    type nonrec t = t

    let[@inline] to_int (Tag tag) = tag
  end

  module MapNode =
    PatriciaTree.WeakNode
      (struct
        type 'k t = Key.t
      end)
      (PatriciaTree.WrappedHomogeneousValue)

  module SetNode = PatriciaTree.WeakSetNode (struct
    type 'k t = Key.t
  end)

  module WeakMap =
    PatriciaTree.MakeCustomMap (Key) (PatriciaTree.Value) (MapNode)

  module WeakSet = struct
    include Weak.Make (struct
      type nonrec t = t

      let[@inline] hash (Tag tag) = tag
      let equal = equal
    end)

    let pp = Fmt.iter ~sep:(Fmt.any ", ") iter pp
  end
end

(** Whether this node has a protector (this is distinct from having the
    protector toggled!), its parents (including this node's ID!), and its
    initial state if it doesn't exist in the state. *)
type node = {
  protector : protector option;
  parents : Tag.WeakSet.t;
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
  | Cell, _ | _, Cell -> failwith "Can't compare Cell with non-Cell"
  | Reserved _, ReservedIM | ReservedIM, Reserved _ ->
      failwith "Can't compare Reserved and ReservedIM"

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

type t = { tags : node Tag.WeakMap.t; known_size : int }

let pp ft { tags; _ } =
  Fmt.iter_bindings Tag.WeakMap.iter
    (fun ft (tag, node) -> Fmt.pf ft "%a -> %a" Tag.pp tag pp_node node)
    ft tags

let show = Fmt.to_to_string pp

let init ?(initial_state = Unique) () =
  let tag = Tag.fresh_tag () in
  let parents = Tag.WeakSet.create 1 in
  Tag.WeakSet.add parents tag;
  let node = { protector = None; parents; initial_state } in
  let tags = Tag.WeakMap.singleton tag node in
  (tag, { tags; known_size = 1 })

let ub_state = fst @@ init ~initial_state:Disabled ()

(* Compact the root trie when it reaches this size. Dead ephemeron leaves become
   Empty after GC but their parent Branch nodes persist, making
   [filter_map_no_share] in [access] O(total borrows) instead of O(live).
   Triggering on map size rather than tag count means small maps (e.g. a freshly
   allocated variable with few borrows) never pay the GC cost. *)
let compact_threshold = 64
let compact_map = Tag.WeakMap.filter_map_no_share (Fun.const Option.some)

let borrow ?protector parent ~state ({ tags; known_size } : t) =
  let tag = Tag.fresh_tag () in
  let tags, known_size =
    if known_size >= compact_threshold then (
      Gc.minor ();
      let tags' = compact_map tags in
      let known_size = Tag.WeakMap.cardinal tags' in
      (* If most entries survived the minor GC they are promoted to the major
         heap; a full cycle is needed to null their ephemeron keys. *)
      if known_size >= compact_threshold then begin
        Gc.full_major ();
        let tags' = compact_map tags in
        let known_size = Tag.WeakMap.cardinal tags' in
        (tags', known_size)
      end
      else (tags', known_size))
    else (tags, known_size)
  in
  let node_parent = Tag.WeakMap.find parent tags in
  let parents =
    Tag.WeakSet.create (Tag.WeakSet.count node_parent.parents + 1)
  in
  Tag.WeakSet.add parents tag;
  Tag.WeakSet.iter (Tag.WeakSet.add parents) node_parent.parents;
  let node = { protector; parents; initial_state = state } in
  let tags = Tag.WeakMap.add tag node tags in
  (tag, { tags; known_size = known_size + 1 })

let unprotect tag ({ tags; _ } as st) =
  let tags =
    Tag.WeakMap.update tag
      (function
        | None -> raise Not_found | Some n -> Some { n with protector = None })
      tags
  in
  { st with tags }

let strong_protector_exists { tags; _ } =
  (* Annoyingly, there is no [exists], only [for_all] *)
  not
    (Tag.WeakMap.for_all
       (fun _ { protector; _ } -> protector <> Some Strong)
       tags)

(** [tag -> (protected * state)], [protected] indicating the tag's protector
    (managed outside [tb_state]) was toggled. *)
type tb_state = (bool * state) Tag.WeakMap.t

let pp_tb_state =
  Fmt.iter_bindings ~sep:(Fmt.any ", ") Tag.WeakMap.iter
    (fun ft (tag, (protected, st)) ->
      Fmt.pf ft "%a -> %a%s" Tag.pp tag pp_state st
        (if protected then " (p)" else ""))

let empty_state = Tag.WeakMap.empty
let is_empty_state = Tag.WeakMap.is_empty

let equal_state =
  Tag.WeakMap.reflexive_equal (Pair.equal Bool.equal equal_state)

let set_protector ~protected tag ({ tags; _ } : t) =
  Tag.WeakMap.update tag (function
    | None ->
        let node = Tag.WeakMap.find tag tags in
        Some (protected, node.initial_state)
    | Some (_, st) -> Some (protected, st))

(** [access accessed im e structure state]: Update all nodes in the mapping
    [state] for the tree structure [structure] with an event [e], that happened
    at [accessed]. *)
let access accessed e ({ tags; _ } as root : t) (st : tb_state) =
  let accessed_node = Tag.WeakMap.find accessed tags in
  let parents = accessed_node.parents in
  try
    Tag.WeakMap.filter_map_no_share
      (fun tag { protector; initial_state; _ } ->
        let protected, st =
          match Tag.WeakMap.find_opt tag st with
          | None -> (false, initial_state)
          | Some ps -> ps
        in
        let rel = if Tag.WeakSet.mem parents tag then Local else Foreign in
        (* if the tag has a protector and is accessed, this toggles the
           protector! *)
        let protected =
          (Tag.equal tag accessed || protected) && Option.is_some protector
        in
        let st' = transition ~protected st rel e in
        if (not protected) && Common.equal_state st' initial_state then None
        else Some (protected, st'))
      tags
    |> Result.ok
  with AliasingError ->
    [%l.debug
      "TB: Undefined behavior encountered for %a at %a in structure@.%a"
        pp_access e Tag.pp accessed pp root];
    Result.error `AliasingError

let merge = Tag.WeakMap.idempotent_union @@ fun _ -> meet'
