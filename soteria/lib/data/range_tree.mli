(** This module describe "Range trees", a datastructure for symbolic reasoning
    about a range of data.

    Range trees are representation of an array of symbolic size. Each node of
    the tree represents a contiguous range of the array (a {!S_range.Make.t}
    using a given type of symbolic integers ['sint]). Each node also some data,
    which must be mergeable.

    Each node has either no children, or exactly two children, which represent a
    split of the node's range into two contiguous subranges, i.e. if the parent
    node covers [a..c] (note that it doesn't include "c"), then the children
    must cover [a..b] and [b..c] with [a < b < c].

    Additionally, we maintain the invariant that the tree is balanced, i.e. for
    any node, the height of its left and right children can differ by at most 1.
    This is for obvious performance reasons. *)

(** A "Range Tree", representing a symbolic range of data sequences ['a], using
    ranges over symbolic integer representation ['sint]. *)
type ('a, 'sint) t = private {
  node : 'a;
  range : 'sint * 'sint;
  children : (('a, 'sint) t * ('a, 'sint) t) option;
  height : int;
}

(** Creates a tree node without rebalancing. The caller is responsible for
    ensuring the resulting tree is balanced. *)
val make_node :
  node:'a ->
  range:'sint * 'sint ->
  ?children:('a, 'sint) t * ('a, 'sint) t ->
  unit ->
  ('a, 'sint) t

(** Builds a tree given a node, a range, and children, and automatically
    balances it. *)
val build :
  node:'a ->
  range:'sint * 'sint ->
  merge:('a -> 'a -> 'a) ->
  ?children:('a, 'sint) t * ('a, 'sint) t ->
  unit ->
  ('a, 'sint) t

(** Rebalances an existing tree bottom-up, using [merge] to recompute
    intermediate node values created during rotations. *)
val rebuild : merge:('a -> 'a -> 'a) -> ('a, 'sint) t -> ('a, 'sint) t

(** [offset ~apply_ofs t] uses the function [apply_ofs] on all offset in the
    ranges of the tree [t].

    Effectively, this function is just a [map_offsets], but applying any other
    transformation to the offsets of the tree than offsetting by a constant
    integer is invalid. *)
val offset :
  add:('sint -> 'sint -> 'sint) -> by:'sint -> ('a, 'sint) t -> ('a, 'sint) t

(** [iter_leaves_rev t] is an iterator over the leaves of the tree [t] from
    right to left. *)
val iter_leaves_rev : ('a, 'sint) t -> (('a, 'sint) t -> unit) -> unit

module With_monad3 (M : sig
  type ('ok, 'err, 'fix) t

  val bind :
    ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

  val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t
end) : sig
  (** [map_leaves f t] receives a mapping function [f] operating within the
      monad [M], as well as a tree, and applys [f] to all {b leaves} of the tree
      (not the inner nodes). *)
  val map_leaves :
    ('a -> ('a, 'b, 'c) M.t) -> ('a, 'sint) t -> (('a, 'sint) t, 'b, 'c) M.t
end
