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
let rec build ~node ~range ?children () =
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
        (* Left-heavy: pull left's children up. left covers [a..b], right covers
           [b..c], together [a..c]. We need to split left into its own children
           and rebalance. *)
        let ll, lr = Option.get left.children in
        (* ll covers [a..m], lr covers [m..b], right covers [b..c]. Strategy:
           keep ll on the left, rebuild the right subtree from (lr, right) with
           the current node's data, then rebuild root. *)
        let new_right =
          build ~node
            ~range:(fst lr.range, snd right.range)
            ~children:(lr, right) ()
        in
        build ~node:left.node ~range ~children:(ll, new_right) ()
      else if hr > hl + 1 then
        (* Right-heavy: mirror of the above. left covers [a..b], right covers
           [b..c], together [a..c]. right's children: rl covers [b..m], rr
           covers [m..c]. *)
        let rl, rr = Option.get right.children in
        (* left covers [a..b], rl covers [b..m], rr covers [m..c]. Keep rr on
           the right, rebuild left subtree from (left, rl). *)
        let new_left =
          build ~node
            ~range:(fst left.range, snd rl.range)
            ~children:(left, rl) ()
        in
        build ~node:right.node ~range ~children:(new_left, rr) ()
      else
        (* Balanced within ±1, just build the node *)
        make_node ~node ~range ~children:(left, right) ()

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
