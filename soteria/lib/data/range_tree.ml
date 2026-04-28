open Soteria_std

type ('a, 'sint) t = {
  node : 'a;
  range : 'sint * 'sint;
  children : (('a, 'sint) t * ('a, 'sint) t) option;
  height : int;
}

let build ~node:_ ~range:_ ?children:_ () =
  (* Classic AVL tree algorithm to re-balance trees on construction. We assume
     the invariant that the input children are already balanced, so we only need
     to check the height difference of the input children, and not the whole
     subtree. *)
  failwith "TODO"

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
