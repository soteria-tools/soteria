open Rustsymex

let[@inline] is_disabled () = !Config.current.ignore_aliasing

type tag = int

let tag_counter = ref 0

let fresh_tag () =
  incr tag_counter;
  !tag_counter

let zero = 0
let pp_tag fmt tag = Fmt.pf fmt "‖%d‖" tag

module TagMap = Map.Make (struct
  type t = tag

  let compare = compare
end)

type access = Read | Write
and locality = Local | Foreign
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB

(** The tag of the node, whether it has a protector (this is distinct from
    having the protector toggled!), its children and parent, and its initial
    state if it doesn't exist in the state. *)
and node = {
  tag : tag;
  protector : bool;
  parent : tag option;
  children : tag list;
  initial_state : state;
}
[@@deriving show { with_path = false }]

let pp_state fmt = function
  | Reserved true -> Fmt.string fmt "Re T"
  | Reserved false -> Fmt.string fmt "Re F"
  | Unique -> Fmt.string fmt "Uniq"
  | Frozen -> Fmt.string fmt "Froz"
  | ReservedIM -> Fmt.string fmt "ReIM"
  | Disabled -> Fmt.string fmt "Dis "
  | UB -> Fmt.string fmt "UB  "

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

type t = node TagMap.t

let pp ft t =
  let open Fmt in
  iter_bindings ~sep:(any ", ") TagMap.iter
    (fun ft (tag, node) ->
      pf ft "%a -> [%a]" pp_tag tag (list ~sep:(any ", ") pp_tag) node.children)
    ft t

let init ~state () =
  let node =
    {
      tag = fresh_tag ();
      protector = false;
      parent = None;
      children = [];
      initial_state = state;
    }
  in
  (TagMap.singleton node.tag node, node.tag)

let ub_state = fst @@ init ~state:UB ()

(** [is_derived n t]: Returns [Local] if tag [t] is derived from tag [n], i.e.
    [t] is a descendant of [n]. [Foreign] otherwise *)
let is_derived n t st =
  let rec aux current_tag =
    if current_tag = n then true
    else
      match TagMap.find_opt current_tag st with
      | None | Some { parent = None; _ } -> false
      | Some { parent = Some p; _ } -> aux p
  in
  if aux t then Local else Foreign

let add_child ~parent ?(protector = false) ~state st =
  let node =
    {
      tag = fresh_tag ();
      protector;
      parent = Some parent;
      children = [];
      initial_state = state;
    }
  in
  let st =
    TagMap.update parent
      (function
        | None -> raise Not_found
        | Some p -> Some { p with children = node.tag :: p.children })
      st
  in
  (TagMap.add node.tag node st, node.tag)

let unprotect tag =
  TagMap.update tag (function
    | None -> raise Not_found
    | Some n -> Some { n with protector = false })

(** [tag -> (protected * state)], [protected] indicating the tag's protector
    (managed outside [tb_state]) was toggled. *)
type tb_state = (bool * state) TagMap.t

let pp_tb_state =
  Fmt.iter_bindings ~sep:(Fmt.any ", ") TagMap.iter
    (fun ft (tag, (protected, st)) ->
      Fmt.pf ft "%a -> %a%s" pp_tag tag pp_state st
        (if protected then " (p)" else ""))

let empty_state = TagMap.empty

let set_protector ~protected tag root =
  TagMap.update tag (function
    | None ->
        let node = TagMap.find tag root in
        Some (protected, node.initial_state)
    | Some (_, st) -> Some (protected, st))

(** [access accessed im e structure state]: Update all nodes in the mapping
    [state] for the tree structure [structure] with an event [e], that happened
    at [accessed]. *)
let access accessed e (root : t) st =
  let ub_happened = ref false in
  let st =
    TagMap.fold
      (fun tag node ->
        TagMap.update tag (function
          | None -> Some (false, node.initial_state)
          | Some _ as st -> st))
      root st
  in
  L.trace (fun m ->
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
        let node = TagMap.find tag root in
        let rel = is_derived node.tag accessed root in
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
  if !ub_happened then Result.error `AliasingError else Result.ok st'

let merge = TagMap.merge @@ fun _ -> Option.merge meet'
