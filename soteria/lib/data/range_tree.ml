open Soteria_std
open Logs.Import

type ('a, 'sint) t = {
  node : 'a;
  range : 'sint * 'sint;
  children : (('a, 'sint) t * ('a, 'sint) t) option;
  height : int;
}

let height t = t.height

let make_node ~node ~range ?children () =
  match children with
  | None -> { node; range; children; height = 0 }
  | Some (left, right) ->
      let h = 1 + max (height left) (height right) in
      { node; range; children = Some (left, right); height = h }

(** Builds a tree given a node, a range, and children, and automatically
    balances it. *)
let rec build ~node ~range ~merge ?children () =
  [%l.debug "CALLED BUILD!"];
  (* Classic AVL tree algorithm to re-balance trees on construction. We assume
     the invariant that the input children are already balanced, so we only need
     to check the height difference of the input children, and not the whole
     subtree. *)
  match children with
  | None -> { node; range; children = None; height = 0 }
  | Some (left, right) ->
      let hl = height left and hr = height right in
      if hl > hr + 1 then
        let ll, lr = Option.get left.children in
        if ll.height >= lr.height then
          (* Left-LEFT heavy: single right rotation.
             ll covers [a..m], lr covers [m..b], right covers [b..c]. *)
          let new_right =
            build ~node:(merge lr.node right.node)
              ~range:(fst lr.range, snd right.range)
              ~merge ~children:(lr, right) ()
          in
          build ~node ~range ~merge ~children:(ll, new_right) ()
        else
          (* Left-RIGHT heavy: double rotation.
             lr.children = (lrl, lrr). New root = lr's range, left = (ll, lrl),
             right = (lrr, right). *)
          let lrl, lrr = Option.get lr.children in
          let new_left =
            build ~node:(merge ll.node lrl.node)
              ~range:(fst ll.range, snd lrl.range)
              ~merge ~children:(ll, lrl) ()
          in
          let new_right =
            build ~node:(merge lrr.node right.node)
              ~range:(fst lrr.range, snd right.range)
              ~merge ~children:(lrr, right) ()
          in
          build ~node ~range ~merge ~children:(new_left, new_right) ()
      else if hr > hl + 1 then
        let rl, rr = Option.get right.children in
        if rr.height >= rl.height then
          (* Right-RIGHT heavy: single left rotation.
             left covers [a..b], rl covers [b..m], rr covers [m..c]. *)
          let new_left =
            build ~node:(merge left.node rl.node)
              ~range:(fst left.range, snd rl.range)
              ~merge ~children:(left, rl) ()
          in
          build ~node ~range ~merge ~children:(new_left, rr) ()
        else
          (* Right-LEFT heavy: double rotation.
             rl.children = (rll, rlr). New root = rl's range, left = (left, rll),
             right = (rlr, rr). *)
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
        make_node ~node ~range ~children:(left, right) ()

let rec rebalance ~merge t =
  match t.children with
  | None -> t
  | Some (left, right) ->
      let left = rebalance ~merge left in
      let right = rebalance ~merge right in
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
