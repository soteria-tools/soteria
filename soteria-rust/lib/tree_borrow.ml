type tag = int
and access = Read | Write
and locality = Local | Foreign
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB

(** The tag of the node, whether it has a protector (this is distinct from
    having the protector toggled!), its children, and its initial state if it
    doesn't exist in the state. *)
and t = {
  tag : tag;
  protector : bool;
  children : t list;
  initial_state : state;
}
[@@deriving show { with_path = false }]

let tag_counter = ref 0

let fresh_tag () =
  incr tag_counter;
  !tag_counter

let zero = 0
let pp_tag fmt tag = Fmt.pf fmt "‖%d‖" tag

let pp_state fmt = function
  | Reserved true -> Fmt.string fmt "Re T"
  | Reserved false -> Fmt.string fmt "Re F"
  | Unique -> Fmt.string fmt "Uniq"
  | Frozen -> Fmt.string fmt "Froz"
  | ReservedIM -> Fmt.string fmt "ReIM"
  | Disabled -> Fmt.string fmt "Dis "
  | UB -> Fmt.string fmt "UB  "

let rec pp fmt t =
  if List.is_empty t.children then pp_tag fmt t.tag
  else Fmt.pf fmt "%a(%a)" pp_tag t.tag Fmt.(list ~sep:comma pp) t.children

let init ?(protector = false) ~state () =
  { tag = fresh_tag (); protector; children = []; initial_state = state }

let equal n1 n2 = n1.tag == n2.tag

let meet st1 st2 =
  match (st1, st2) with
  | UB, _ | _, UB -> UB
  | Disabled, _ | _, Disabled -> Disabled
  | Frozen, _ | _, Frozen -> Frozen
  | Unique, _ | _, Unique -> Unique
  | Reserved b1, Reserved b2 -> Reserved (b1 || b2)
  | ReservedIM, ReservedIM -> ReservedIM
  | Reserved _, ReservedIM | ReservedIM, Reserved _ ->
      failwith "Can't compare Reserved and ReservedIM"

let meet' (p1, st1) (p2, st2) = (p1 || p2, meet st1 st2)

let transition =
  let transition st e =
    match (st, e) with
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

(* let[@tailrec] rec root n = match n.parent with None -> n | Some p -> root p *)

let rec iter n f =
  f n;
  List.iter (fun n -> iter n f) n.children

(** [find n t] Looks for the node with tag [t] in the tree rooted at [n] *)
let find n t = Iter.find_pred_exn (fun n -> n.tag = t) (iter n)

(** [is_derived n t]: Returns [Local] if [t] is derived from [n], i.e. [t] is a
    descendant of [n], [Foreign] otherwise *)
let is_derived n t =
  try
    let _ = find n t in
    Local
  with Not_found -> Foreign

let add_child ~parent ~root child =
  let found = ref false in
  let rec aux node =
    if node.tag = parent then (
      found := true;
      { node with children = child :: node.children })
    else { node with children = List.map aux node.children }
  in
  let root' = aux root in
  if !found then root' else raise Not_found

let update root f tag =
  let found = ref false in
  let rec aux node =
    if node.tag = tag then (
      found := true;
      f node)
    else { node with children = List.map aux node.children }
  in
  let root' = aux root in
  if !found then root' else raise Not_found

module TagMap = Map.Make (struct
  type t = tag

  let compare = compare
end)

(** { tag -> (protected * state) }, we store whether the tag is protected outside the tree borrow because *)
type tb_state = (bool * state) TagMap.t

let empty_state = TagMap.empty

let set_protector ~protected root tag st =
  TagMap.update tag
    (function
      | None ->
          let node = find root tag in
          Some (protected, node.initial_state)
      | Some (_, st) -> Some (protected, st))
    st

(** [access root accessed im e state]: Update all nodes in the mapping [state]
    for the tree rooted at [root] with an event [e], that happened at
    [accessed]. [im] indicates whether this location is considered interiorly
    mutable, which affects the default state of tags (Reserved vs ReservedIM).
    Returns the new state and a boolean indicating whether an undefined behavior
    was encountered *)
let access (root : t) accessed _im e st : tb_state * bool =
  let ub_happened = ref false in
  let st =
    Iter.fold
      (fun st node ->
        TagMap.update node.tag
          (function
            | None -> Some (false, node.initial_state) | Some _ as st -> st)
          st)
      st
    @@ iter root
  in
  L.debug (fun m ->
      let pp_binding fmt (tag, (protected, st)) =
        Fmt.pf fmt "%a -> %a%s" pp_tag tag pp_state st
          (if protected then " (p)" else "")
      in
      m "TB: %a at %a, for tree %a, state@[<hov 2> %a@]" pp_access e pp_tag
        accessed pp root
        Fmt.(iter_bindings ~sep:(Fmt.any ", ") TagMap.iter pp_binding)
        st);
  let st' =
    TagMap.mapi
      (fun tag (protected, st) ->
        let node = find root tag in
        let rel = is_derived node accessed in
        (* if the tag has a protector and is accessed, this toggles the protector! *)
        let protected = node.protector && (tag = accessed || protected) in
        let st' = transition ~protected st (rel, e) in
        if st' = UB then (
          ub_happened := true;
          L.debug (fun m ->
              m
                "TB: Undefined behavior encountered for %a, %a %a (protected? \
                 %b): %a->%a"
                pp_tag tag pp_locality rel pp_access e protected pp_state st
                pp_state st'));
        (protected, st'))
      st
  in
  (st', !ub_happened)

let merge = TagMap.merge @@ fun _ -> Option.merge meet'
