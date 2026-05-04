open Soteria_std

module Info : sig
  type t

  val make : height:int -> is_balanced:bool -> unit -> t

  (** Height of the tree (maximum depth) *)
  val height : t -> int

  (** [is_balanced] says whether we know {e for sure} that the tree is balanced.
  *)
  val is_balanced : t -> bool

  (** Information associated with a leaf node (height = 0; balanced = true) *)
  val leaf : t
end = struct
  (** To avoid increasing the size of allocation, and because the number of
      nodes in the tree is exponential in the height, we can safely assume that
      the height of the tree will ever at most take e.g. ~30bits (already
      insanely big). So we can just store additional flags in the remaining bits
      of the height field. *)
  type t = int
  (* NOTE: On 32bit machines, OCaml integers are 31 bits, so we can trivially
     store 1 flag. *)

  let make ~height ~is_balanced () =
    let balanced_bit = if is_balanced then 0x40000000 else 0 in
    height lor balanced_bit

  let height t = t land 0x3FFFFFFF
  let is_balanced t = t land 0x40000000 <> 0
  let leaf = make ~height:0 ~is_balanced:true ()
end

type ('a, 'sint) t = {
  node : 'a;
  range : 'sint * 'sint;
  children : (('a, 'sint) t * ('a, 'sint) t) option;
  info : Info.t;
}

let pp pp_node pp_int ft t =
  let pp_range fmt (a, b) = Fmt.pf fmt "[%a, %a[" pp_int a pp_int b in
  let rec pp ft { node; range; children; info = _ } =
    let open Fmt in
    pf ft "@[<v 2>{%a %a%a}@]" pp_node node pp_range range
      (option ~none:nop (fun fmt (l, r) -> pf fmt " -->@,@[%a@,%a@]" pp l pp r))
      children
  in
  pp ft t

let height t = Info.height t.info
let is_balanced t = Info.is_balanced t.info

let make_raw ~node ~range ?children () =
  let info =
    match children with
    | None -> Info.make ~height:0 ~is_balanced:true ()
    | Some (left, right) ->
        let h = 1 + max (height left) (height right) in
        let balanced = abs (height left - height right) <= 1 in
        Info.make ~height:h ~is_balanced:balanced ()
  in
  { node; range; children; info }

(** Builds a balanced range tree node from a given [node] value, [range], and
    optional [children], performing AVL-style rotations as needed.

    [merge l r] is used to recompute the data value of an internal node whose
    left child carries value [l] and right child carries value [r]. This is
    necessary during rotations, when the tree structure changes and some
    internal nodes get new children.

    {2 Invariant maintained}

    At every node, the heights of its left and right subtrees differ by at most
    1. When a new node would violate this (|hl - hr| > 1), [build] performs the
    appropriate single or double AVL rotation to restore balance.

    {2 Rotations}

    There are four cases, mirroring those of a standard AVL tree. In each
    description the letters [a < b < c] (or [a < m < b < c]) denote range
    endpoints, illustrating which subtree covers which contiguous slice.

    {3 Left-LEFT (single right rotation)}

    Triggered when [hl > hr + 1] and the left child's left grandchild is at
    least as tall as its right grandchild ([ll.height >= lr.height]).

    Before rotation:
    {v
               root [a..c]
             /             \
        left [a..b]       right [b..c]
        /         \
    ll [a..m]   lr [m..b]
    v}

    After single right rotation:
    {v
               root [a..c]
             /             \
        ll [a..m]       new_right [m..c]
                        /              \
                    lr [m..b]       right [b..c]
    v}

    The new intermediate node [new_right] covers [[m..c]] and its value is
    [merge lr.node right.node]. The root keeps the same [node] value and range.

    {3 Left-RIGHT (double rotation)}

    Triggered when [hl > hr + 1] and [lr.height > ll.height]. [lr] must have
    children [(lrl, lrr)].

    Before rotation:
    {v
              root [a..c]
             /             \
        left [a..b]       right [b..c]
       /         \
     ll [a..m]  lr [m..b]
                /       \
           lrl [m..n]  lrr [n..b]
    v}

    After double rotation, [lr] is promoted: the root keeps the same [node]
    value and range, but its new children are:
    {v
                  root [a..c]
             /                  \
        new_left [a..n]       new_right [n..c]
        /         \            /             \
    ll [a..m]  lrl [m..n]  lrr [n..b]    right [b..c]
    v}

    where [new_left] has value [merge ll.node lrl.node] and [new_right] has
    value [merge lrr.node right.node].

    {3 Right-RIGHT (single left rotation)}

    Mirror of Left-LEFT. Triggered when [hr > hl + 1] and
    [rr.height >= rl.height].

    Before rotation:
    {v
        root [a..c]
       /              \
     left [a..b]    right [b..c]
                   /            \
               rl [b..m]      rr [m..c]
    v}

    After single left rotation:
    {v
                root [a..c]
             /                \
        new_left [a..m]      rr [m..c]
        /              \
     left [a..b]     rl [b..m]
    v}

    The new intermediate node [new_left] covers [[a..m]] and its value is
    [merge left.node rl.node]. The root keeps the same [node] value and range.

    {3 Right-LEFT (double rotation)}

    Mirror of Left-RIGHT. Triggered when [hr > hl + 1] and
    [rl.height > rr.height]. [rl] must have children [(rll, rlr)].

    Before rotation:
    {v
               root [a..c]
             /             \
        left [a..b]       right [b..c]
                          /         \
                      rl [b..n]    rr [n..c]
                      /       \
                 rll [b..m]  rlr [m..n]
    v}

    After double rotation, the root keeps the same [node] value and range, but
    its new children are:
    {v
                root [a..c]
             /                  \
        new_left [a..m]       new_right [m..c]
        /         \            /             \
    left [a..b]  rll [b..m]  rlr [m..n]    rr [n..c]
    v}

    where [new_left] has value [merge left.node rll.node] and [new_right] has
    value [merge rlr.node rr.node]. *)
let rec build ~node ~range ~merge ?children () =
  match children with
  | None -> { node; range; children = None; info = Info.leaf }
  | Some (left, right) ->
      let hl = height left and hr = height right in
      if hl > hr + 1 then
        let ll, lr = Option.get left.children in
        if height ll >= height lr then
          (* Left-LEFT heavy: single right rotation. ll covers [a..m], lr covers
             [m..b], right covers [b..c]. *)
          let new_right =
            build ~node:(merge lr.node right.node)
              ~range:(fst lr.range, snd right.range)
              ~merge ~children:(lr, right) ()
          in
          build ~node ~range ~merge ~children:(ll, new_right) ()
        else
          (* Left-RIGHT heavy: double rotation. lr.children = (lrl, lrr). New
             root = lr's range, left = (ll, lrl), right = (lrr, right). *)
          let lrl, lrr = Option.get lr.children in
          let new_left =
            build ~node:(merge ll.node lrl.node)
              ~range:(fst ll.range, snd lrl.range)
              ~merge ~children:(ll, lrl) ()
          in
          let new_right =
            build
              ~node:(merge lrr.node right.node)
              ~range:(fst lrr.range, snd right.range)
              ~merge ~children:(lrr, right) ()
          in
          build ~node ~range ~merge ~children:(new_left, new_right) ()
      else if hr > hl + 1 then
        let rl, rr = Option.get right.children in
        if height rr >= height rl then
          (* Right-RIGHT heavy: single left rotation. left covers [a..b], rl
             covers [b..m], rr covers [m..c]. *)
          let new_left =
            build ~node:(merge left.node rl.node)
              ~range:(fst left.range, snd rl.range)
              ~merge ~children:(left, rl) ()
          in
          build ~node ~range ~merge ~children:(new_left, rr) ()
        else
          (* Right-LEFT heavy: double rotation. rl.children = (rll, rlr). New
             root = rl's range, left = (left, rll), right = (rlr, rr). *)
          let rll, rlr = Option.get rl.children in
          let new_left =
            build ~node:(merge left.node rll.node)
              ~range:(fst left.range, snd rll.range)
              ~merge ~children:(left, rll) ()
          in
          let new_right =
            build ~node:(merge rlr.node rr.node)
              ~range:(fst rlr.range, snd rr.range)
              ~merge ~children:(rlr, rr) ()
          in
          build ~node ~range ~merge ~children:(new_left, new_right) ()
      else
        (* Balanced within ±1, just build the node *)
        make_raw ~node ~range ~children:(left, right) ()

let rec rebuild ~merge t =
  if is_balanced t then t
  else
    (* Safety: a leaf node cannot be unbalanced *)
    let left, right = Option.get t.children in
    let left = rebuild ~merge left in
    let right = rebuild ~merge right in
    build ~node:t.node ~range:t.range ~merge ~children:(left, right) ()

let offset ~add ~by tree =
  let apply_ofs = add by in
  let rec aux tree =
    let low, high = tree.range in
    let range = (apply_ofs low, apply_ofs high) in
    let children = Option.map (fun (l, r) -> (aux l, aux r)) tree.children in
    { tree with range; children }
  in
  aux tree

let rec iter_leaves_rev t f =
  match t.children with
  | None -> f t
  | Some (left, right) ->
      iter_leaves_rev right f;
      iter_leaves_rev left f

(* FIXME: to be replaced soon when modular explicits make an appearance. *)
module With_monad3 (M : sig
  type ('ok, 'err, 'fix) t

  val bind :
    ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

  val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t
end) =
struct
  let ( let+ ) = M.map
  let ( let* ) = M.bind

  let rec map_leaves (f : 'a -> ('a, 'b, 'c) M.t) (t : ('a, 'sint) t) :
      (('a, 'sint) t, 'b, 'c) M.t =
    match t.children with
    | None ->
        let+ node = f t.node in
        { t with node }
    | Some (l, r) ->
        let* l = map_leaves f l in
        let+ r = map_leaves f r in
        { t with children = Some (l, r) }
end
