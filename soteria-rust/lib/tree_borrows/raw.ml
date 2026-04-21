(** A simple Tree Borrows implementation. It doesn't support any sort of
    symbolic reasoning or anything fancy, it's just the raw logic. *)

open Common

module Tag : sig
  type t [@@deriving show, eq]

  val fresh_tag : unit -> t
  val zero : t

  module Map : PatriciaTree.MAP with type key = t
end = struct
  type t = int [@@deriving eq]

  let pp fmt tag = Fmt.pf fmt "‖%d‖" tag
  let show = Fmt.to_to_string pp
  let zero = 0
  let tag_counter = ref 0

  let fresh_tag () =
    incr tag_counter;
    !tag_counter

  module Map = PatriciaTree.MakeMap (Int)
end

(** Whether this node has a protector (this is distinct from having the
    protector toggled!), its parents (including this node's ID!), and its
    initial state if it doesn't exist in the state. *)
type node = {
  protector : protector option;
  parents : Tag.t list;
  initial_state : state;
}
[@@deriving show { with_path = false }, eq]

let pp_state fmt = function
  | Reserved true -> Fmt.string fmt "Re T"
  | Reserved false -> Fmt.string fmt "Re F"
  | Unique -> Fmt.string fmt "Uniq"
  | Frozen -> Fmt.string fmt "Froz"
  | ReservedIM -> Fmt.string fmt "ReIM"
  | Cell -> Fmt.string fmt "Cell"
  | Disabled -> Fmt.string fmt "Dis "
  | UB -> Fmt.string fmt "UB  "

let[@inline] meet st1 st2 =
  match (st1, st2) with
  | UB, _ | _, UB -> UB
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

let transition =
  let transition st e =
    match (st, e) with
    | Cell, _ -> Cell
    | Reserved b, (_, Read) -> Reserved b
    | Reserved _, (Local, Write) -> Unique
    | Reserved _, (Foreign, Write) -> Disabled
    | Unique, (Local, _) -> Unique
    | Unique, (Foreign, Read) -> Frozen
    | Unique, (Foreign, Write) -> Disabled
    | Frozen, (_, Read) -> Frozen
    | Frozen, (Local, Write) -> UB
    | Frozen, (Foreign, Write) -> Disabled
    | ReservedIM, (_, Read | Foreign, _) -> ReservedIM
    | ReservedIM, (Local, Write) -> Unique
    | Disabled, (Foreign, _) -> Disabled
    | Disabled, (Local, _) -> UB
    | UB, _ -> UB
  in

  let transition_protected st e =
    match (st, e) with
    | Reserved false, (Foreign, Read) -> Reserved true
    | Reserved true, (_, Write) -> UB
    | Unique, (Foreign, Read) -> UB
    | _ ->
        let st' = transition st e in
        if st' = Disabled then UB else st'
  in
  fun ~protected -> if protected then transition_protected else transition

type t = node Tag.Map.t

let pp ft t =
  let open Fmt in
  iter_bindings Tag.Map.iter
    (fun ft (tag, node) -> pf ft "%a -> %a" Tag.pp tag pp_node node)
    ft t

let show = Fmt.to_to_string pp

let init ?(initial_state = Unique) () =
  let tag = Tag.fresh_tag () in
  let node = { protector = None; parents = [ tag ]; initial_state } in
  (tag, Tag.Map.singleton tag node)

let ub_state = fst @@ init ~initial_state:UB ()

let borrow ?protector parent ~state st =
  let tag = Tag.fresh_tag () in
  let node_parent = Tag.Map.find parent st in
  let node =
    { protector; parents = tag :: node_parent.parents; initial_state = state }
  in
  (Tag.Map.add tag node st, tag)

let unprotect tag =
  Tag.Map.update tag (function
    | None -> raise Not_found
    | Some n -> Some { n with protector = None })

let strong_protector_exists st =
  (* Annoyingly, there is no [exists], only [for_all] *)
  not (Tag.Map.for_all (fun _ { protector; _ } -> protector <> Some Strong) st)

(** [tag -> (protected * state)], [protected] indicating the tag's protector
    (managed outside [tb_state]) was toggled. *)
type tb_state = (bool * state) Tag.Map.t

let pp_tb_state =
  Fmt.iter_bindings ~sep:(Fmt.any ", ") Tag.Map.iter
    (fun ft (tag, (protected, st)) ->
      Fmt.pf ft "%a -> %a%s" Tag.pp tag pp_state st
        (if protected then " (p)" else ""))

let empty_state = Tag.Map.empty
let is_empty_state = Tag.Map.is_empty
let equal_state = Tag.Map.reflexive_equal (Pair.equal Bool.equal equal_state)

let set_protector ~protected tag root =
  Tag.Map.update tag (function
    | None ->
        let node = Tag.Map.find tag root in
        Some (protected, node.initial_state)
    | Some (_, st) -> Some (protected, st))

(** [access accessed im e structure state]: Update all nodes in the mapping
    [state] for the tree structure [structure] with an event [e], that happened
    at [accessed]. *)
let access accessed e (root : t) (st : tb_state) =
  let ub_happened = ref false in
  let accessed_node = Tag.Map.find accessed root in
  let st' =
    Tag.Map.filter_map_no_share
      (fun tag { protector; initial_state; _ } ->
        let protected, st =
          match Tag.Map.find_opt tag st with
          | None -> (false, initial_state)
          | Some ps -> ps
        in
        let rel =
          if List.mem tag accessed_node.parents then Local else Foreign
        in
        (* if the tag has a protector and is accessed, this toggles the
           protector! *)
        let protected =
          (tag = accessed || protected) && Option.is_some protector
        in
        let st' = transition ~protected st (rel, e) in
        if st' = UB then (
          ub_happened := true;
          [%l.debug
            "TB: Undefined behavior encountered for %a, %a %a (protected? %b): \
             %a -> %a in structure@.%a"
            Tag.pp tag pp_locality rel pp_access e protected pp_state st
              pp_state st' pp root]);
        if (not protected) && st' = initial_state then None
        else Some (protected, st'))
      root
  in
  if !ub_happened then Result.error `AliasingError else Result.ok st'

let merge = Tag.Map.idempotent_union @@ fun _ -> meet'
