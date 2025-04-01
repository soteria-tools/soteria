type tag = (int[@opaque])
and access = Read | Write
and locality = Local | Foreign
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB

and t = {
  tag : tag;
  tag_protected : bool;
  (* parent : node option; *)
  children : t list;
}

let tag_counter = ref 0

let fresh_tag () =
  incr tag_counter;
  !tag_counter

let zero = 0

let rec pp fmt t =
  Fmt.pf fmt
    (if t.tag_protected then "[%d](%a)" else "<%d>(%a)")
    t.tag
    Fmt.(list ~sep:comma pp)
    t.children

let pp_tag : tag Fmt.t = Fmt.int

let init ?(protected = false) () =
  {
    tag = fresh_tag ();
    tag_protected = protected;
    (* parent = None; *)
    children = [];
  }

let equal n1 n2 = n1.tag == n2.tag

let meet st1 st2 =
  match (st1, st2) with
  | UB, _ | _, UB -> UB
  | Disabled, _ | _, Disabled -> Disabled
  | Frozen, _ | _, Frozen -> Frozen
  | Unique, _ | _, Unique -> Unique
  | Reserved _, _ | _, Reserved _ -> ReservedIM
  | ReservedIM, _ -> ReservedIM

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
    | Reserved b, (Local, Read) -> Reserved b
    | Reserved _, (Foreign, Read) -> Reserved true
    | Reserved true, (_, Write) -> UB
    | Unique, (Foreign, Read) -> UB
    | _ ->
        let st' = transition st e in
        if st' = Disabled then UB else st'
  in
  fun ~protected -> if protected then transition_protected else transition

(* let[@tailrec] rec root n = match n.parent with None -> n | Some p -> root p *)

(** [find n t] Looks for the node with tag [t] in the tree rooter at [n] *)
let find n t =
  let[@tailrec] rec aux = function
    | [] -> raise Not_found
    | n :: _ when n.tag = t -> n
    | n :: ns -> aux (n.children @ ns)
  in
  aux [ n ]

(** [relation n t]: Relation of the tag [t], with regards to node [n]; if
    [n.tag = t] or if [t] is a descendant of [n], then it is Local, otherwise it
    is Foreign. *)
let relation n t =
  try
    let _ = find n t in
    Local
  with Not_found -> Foreign

module TagMap = Map.Make (struct
  type t = tag

  let compare = compare
end)

type tb_state = state TagMap.t

let empty_state = TagMap.empty

(** [access root tag e state]: Update all nodes in the mapping [state] for the
    tree rooted at [root] with an event [e], that happened at [tag]. Returns the
    new state and a boolean indicating whether an undefined behavior was
    encountered *)
let access (root : t) tag e st : tb_state * bool =
  let accessed = find root tag in
  let ub_happened = ref false in
  let st' =
    TagMap.mapi
      (fun tag' st ->
        let node = find root tag' in
        let rel = relation accessed tag' in
        let st' = transition ~protected:node.tag_protected st (rel, e) in
        if st' = UB then ub_happened := true;
        st')
      st
  in
  (st', !ub_happened)

let merge =
  TagMap.merge @@ fun _ l r ->
  match (l, r) with
  | None, None -> None
  | Some v, None | None, Some v -> Some v
  | Some l, Some r -> Some (meet l r)
