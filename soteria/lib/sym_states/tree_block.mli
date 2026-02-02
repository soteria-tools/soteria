(** {1 Tree-Structured Memory Blocks}

    This module implements a tree-based memory representation that supports
    efficient splitting and merging of memory regions. It is the foundation
    for modeling memory in symbolic execution, particularly for handling
    partial ownership and bi-abduction.

    {2 Overview}

    A tree block represents a contiguous memory region as a binary tree where:
    - Leaf nodes hold actual memory values
    - Internal nodes track partial ownership and enable efficient access
    - Ranges are tracked symbolically (using symbolic integers)

    The key operations are:
    - {b Splitting}: Dividing a node at a specific offset
    - {b Merging}: Combining adjacent nodes back together
    - {b Framing}: Isolating a specific range for access

    {2 Memory Value Interface}

    The tree block is parameterized by a {!MemVal} module that defines
    how memory values behave when split or merged. This enables different
    memory models (e.g., byte-level, typed) to reuse the tree infrastructure.

    {2 Bi-Abduction Support}

    Tree blocks support serialization to and from predicates, enabling
    bi-abductive reasoning. When consuming a predicate fails due to missing
    ownership, the system can propose "fixes" (additional predicates to add).

    {2 Example}

    {[
      module MyMemVal = struct
        (* ... implement MemVal signature ... *)
      end

      module MyTreeBlock = Tree_block.Make(Symex)(MyMemVal)

      let block = MyTreeBlock.alloc initial_value size in
      (* Access, split, merge operations... *)
    ]}
*)

open Symex

(** {2 Split Tree}

    A simplified tree representation without absolute offsets, used during
    splitting operations. *)
module Split_tree : sig
  (** A split tree with values of type ['a] and size type ['sint].
      - [Leaf x]: A leaf containing value [x]
      - [Node(left, at, right)]: A node split at relative offset [at] *)
  type ('a, 'sint) t =
    | Leaf of 'a
    | Node of ('a, 'sint) t * 'sint * ('a, 'sint) t

  val pp :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'sint -> unit) ->
    Format.formatter ->
    ('a, 'sint) t ->
    unit

  val show :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'sint -> unit) ->
    ('a, 'sint) t ->
    string

  (** [map f t] applies [f] to all leaf values in the tree. *)
  val map : ('a -> 'b) -> ('a, 'sint) t -> ('b, 'sint) t
end

(** {2 Tree Types} *)

(** A tree node representing memory ownership.

    - [NotOwned Totally]: No ownership of this region
    - [NotOwned Partially]: Some descendants may be owned
    - [Owned v]: Full ownership with value [v] *)
type 'a node =
  | NotOwned of node_qty
  | Owned of 'a

(** Quantity of ownership: total (no children owned) or partial. *)
and node_qty =
  | Partially  (** Some children may be owned *)
  | Totally  (** No ownership at all in this subtree *)

(** A tree with nodes of type ['a] and symbolic integer type ['sint].

    Each tree has:
    - [node]: The ownership/value at this level
    - [range]: The half-open interval [\[low, high)] covered
    - [children]: Optional left and right subtrees *)
type ('a, 'sint) tree = {
  node : 'a node;
  range : 'sint * 'sint;
  children : (('a, 'sint) tree * ('a, 'sint) tree) option;
}

val pp_tree :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'sint -> unit) ->
  Format.formatter ->
  ('a, 'sint) tree ->
  unit

val show_tree :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'sint -> unit) ->
  ('a, 'sint) tree ->
  string

val pp_node : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a node -> unit
val show_node : (Format.formatter -> 'a -> unit) -> 'a node -> string
val pp_node_qty : Format.formatter -> node_qty -> unit
val show_node_qty : node_qty -> string

(** Create a tree with the given components. *)
val make_tree :
  node:'a node ->
  range:'sint * 'sint ->
  ?children:('a, 'sint) tree * ('a, 'sint) tree ->
  unit ->
  ('a, 'sint) tree

(** {2 Memory Value Signature}

    The input signature for tree blocks. A memory value represents owned
    content that can be split and merged. *)
module MemVal (Symex : Symex.Base) : sig
  module type S = sig
    (** Bounded integer operations for symbolic sizes/offsets. *)
    module SBoundedInt : sig
      type +'a t = 'a Symex.Value.t
      type sbool
      type sint

      val v_false : sbool t
      val zero : unit -> sint t
      val ( +@ ) : sint t -> sint t -> sint t
      val ( -@ ) : sint t -> sint t -> sint t
      val ( <@ ) : sint t -> sint t -> sbool t
      val ( <=@ ) : sint t -> sint t -> sbool t
      val ( ==@ ) : sint t -> sint t -> sbool t
      val ( &&@ ) : sbool t -> sbool t -> sbool t
      val in_bound : sint t -> sbool t
    end

    (** The memory value type. *)
    type t

    val pp : Format.formatter -> t -> unit

    (** [merge ~left ~right] combines two adjacent memory values.

        The merge is overapproximating: information may be lost. For example,
        merging [Value] and [Uninit] might yield [PartlyUninit]. The children
        are always preserved in the tree for precise access. *)
    val merge : left:t -> right:t -> t

    (** [split ~at node] splits [node] at relative offset [at].

        Precondition: [at] is in range [\[1, size(node))], strictly within.

        Returns [(left_tree, right_tree)] where each may be a complex
        split tree if further subdivision is needed. *)
    val split :
      at:SBoundedInt.sint SBoundedInt.t ->
      t ->
      ( (t, SBoundedInt.sint SBoundedInt.t) Split_tree.t
        * (t, SBoundedInt.sint SBoundedInt.t) Split_tree.t )
      Symex.t

    (** {3 Serialization for Bi-Abduction} *)

    (** The serialized predicate form of a memory value. *)
    type serialized

    val pp_serialized : Format.formatter -> serialized -> unit
    val show_serialized : serialized -> string
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized
    val iter_vars_serialized : serialized -> (Var.t * 'b Symex.Value.ty -> unit) -> unit

    (** [serialize t] converts the memory value to predicates.

        Returns [Some seq] with the predicates, or [None] to signal that
        children should be serialized instead (for partial ownership). *)
    val serialize : t -> serialized Seq.t option

    (** [consume pred tree] extracts predicate [pred] from the tree.

        The tree covers exactly the predicate's range. Returns:
        - [Ok tree']: Successfully consumed, with updated tree
        - [Error e]: Consumption failed with error [e]
        - [Missing fixes]: Missing ownership; [fixes] lists predicates needed *)
    val consume :
      serialized ->
      (t, SBoundedInt.sint SBoundedInt.t) tree ->
      ( (t, SBoundedInt.sint SBoundedInt.t) tree,
        'err,
        serialized )
      Symex.Result.t

    (** [produce pred tree] adds predicate [pred] to the tree.

        May vanish if the predicate overlaps with existing ownership. *)
    val produce :
      serialized ->
      (t, SBoundedInt.sint SBoundedInt.t) tree ->
      (t, SBoundedInt.sint SBoundedInt.t) tree Symex.t

    (** [assert_exclusively_owned t] checks that [t] represents full ownership.

        Returns [Ok ()] if exclusively owned, otherwise [Missing] with the
        predicates needed to achieve exclusive ownership. *)
    val assert_exclusively_owned : t -> (unit, 'err, serialized) Symex.Result.t
  end
end

(** {2 Tree Block Functor} *)

(** [Make(Symex)(MemVal)] creates a tree block module for the given
    symbolic execution monad and memory value implementation. *)
module Make
    (Symex : Symex.Base)
    (MemVal : MemVal(Symex).S
                with type 'a SBoundedInt.t = 'a Symex.Value.t
                 and type SBoundedInt.sbool = Symex.Value.sbool) : sig
  (** {3 Types} *)

  (** Symbolic integer type. *)
  type sint = MemVal.SBoundedInt.sint Symex.Value.t

  (** Re-export tree type specialized to this memory value. *)
  type nonrec ('a, 'sint) tree = ('a, 'sint) tree = {
    node : 'a node;
    range : 'sint * 'sint;
    children : (('a, 'sint) tree * ('a, 'sint) tree) option;
  }

  (** {3 Range Operations} *)

  module Range : sig
    type t = sint * sint

    val pp : Format.formatter -> t -> unit

    (** Semantic equality of two ranges. *)
    val sem_eq : t -> t -> MemVal.SBoundedInt.sbool Symex.Value.t

    (** [is_inside inner outer] checks if [inner] is within [outer]. *)
    val is_inside : t -> t -> MemVal.SBoundedInt.sbool Symex.Value.t

    (** [strictly_inside x range] checks if [x] is strictly within [range]. *)
    val strictly_inside : sint -> t -> MemVal.SBoundedInt.sbool Symex.Value.t

    (** Size of the range (high - low). *)
    val size : t -> sint

    (** [split_at range x] returns [(low, x), (x, high)]. *)
    val split_at : t -> sint -> t * t

    (** Offset a range by a given amount. *)
    val offset : t -> sint -> t

    (** [of_low_and_size low size] creates range [\[low, low + size)]. *)
    val of_low_and_size : sint -> sint -> t
  end

  module Split_tree = Split_tree

  (** {3 Node Operations} *)

  module Node : sig
    type qty = node_qty = Partially | Totally

    val pp_qty : Format.formatter -> qty -> unit
    val merge_qty : qty -> qty -> qty

    type t = MemVal.t node

    val pp : Format.formatter -> t -> unit
    val is_empty : t -> bool
    val is_fully_owned : t -> bool

    (** Check exclusive ownership; see {!MemVal.S.assert_exclusively_owned}. *)
    val assert_exclusively_owned :
      t -> (unit, 'err, MemVal.serialized) Symex.Result.t

    (** [merge ~left ~right] merges two nodes.

        Returns [(merged_node, keep_children)] where [keep_children] indicates
        whether the original children should be preserved. *)
    val merge : left:t -> right:t -> t * bool

    (** [split ~at node] splits the node at relative offset [at].

        Returns split trees for left and right portions. *)
    val split :
      at:sint ->
      t ->
      ((t, sint) Split_tree.t * (t, sint) Split_tree.t) Symex.t
  end

  (** {3 Tree Operations} *)

  module Tree : sig
    type t = (MemVal.t, sint) tree

    val pp : Format.formatter -> t -> unit
    val make : node:Node.t -> range:Range.t -> ?children:t * t -> unit -> t
    val is_empty : t -> bool

    (** Create a tree with no ownership over the given range. *)
    val not_owned : Range.t -> t

    (** [map_leaves t f] applies [f] to all leaf nodes.

        Returns a compositional result handling errors and missing cases. *)
    val map_leaves :
      t ->
      (t -> (t, 'err, 'fix) Symex.Result.t) ->
      (t, 'err, 'fix) Symex.Result.t

    (** Iterate over leaves in reverse (right-to-left) order. *)
    val iter_leaves_rev : t -> (t -> unit) -> unit

    (** Construct a parent node from its children (recomputes node). *)
    val of_children_s : left:t -> right:t -> t Symex.t

    (** Like [of_children_s] but ignores the first argument. *)
    val of_children : 'a -> left:t -> right:t -> t Symex.t

    (** Update a tree's children without recomputing the node. *)
    val with_children : t -> left:t -> right:t -> t Symex.t

    (** Offset all ranges in the tree by the given amount. *)
    val offset : by:sint -> t -> t

    (** Convert a split tree to a proper tree with ranges. *)
    val of_split_tree : Range.t -> (Node.t, sint) Split_tree.t -> t Symex.t

    (** Convert a tree to a split tree (erases absolute offsets). *)
    val to_split_tree : t -> (Node.t, sint) Split_tree.t

    (** [split ~range t] isolates [range] from [t].

        Precondition: [range] is a strict subrange of [t.range].

        Returns [(node, left, right)] where [node] covers exactly [range]
        and the tree can be reconstructed with [left] and [right] as children. *)
    val split : range:Range.t -> t -> (Node.t * t * t) Symex.t

    (** [extract t range] extracts the subtree covering [range].

        Returns [(extracted, remainder)] where [remainder] is [Some t']
        if there's remaining tree structure. *)
    val extract : t -> Range.t -> (t * t option) Symex.t

    (** Extend the tree with [NotOwned] nodes to cover the given range. *)
    val extend_if_needed : t -> Range.t -> t Symex.t

    (** Add a tree to the right of this tree. *)
    val add_to_the_right : t -> t -> t Symex.t

    (** Add a tree to the left of this tree. *)
    val add_to_the_left : t -> t -> t Symex.t

    (** [frame_range t ~replace_node ~rebuild_parent range] frames [range].

        This is the core operation: it navigates to the subtree covering
        [range], applies [replace_node] to modify it, then rebuilds the
        path to the root using [rebuild_parent].

        Returns [(original_subtree, new_tree)]. *)
    val frame_range :
      t ->
      replace_node:(t -> (t, 'err, 'fix) Symex.Result.t) ->
      rebuild_parent:(t -> left:t -> right:t -> t Symex.t) ->
      Range.t ->
      (t * t, 'err, 'fix) Symex.Result.t

    (** [get_raw ofs size t] extracts the subtree at [\[ofs, ofs+size)]. *)
    val get_raw :
      sint -> sint -> t -> (t * t, 'err, 'fix) Symex.Result.t

    (** [put_raw tree t] replaces the region covered by [tree]. *)
    val put_raw :
      t -> t -> (unit * t, 'err, MemVal.serialized) Symex.Result.t

    (** {4 Consume/Produce} *)

    (** [consume pred range t] consumes [pred] from range in [t]. *)
    val consume :
      MemVal.serialized ->
      Range.t ->
      t ->
      (t, 'err, MemVal.serialized) Symex.Result.t

    (** [produce pred range t] produces [pred] at range in [t]. *)
    val produce : MemVal.serialized -> Range.t -> t -> t Symex.t
  end

  (** {3 Tree Block (with Bound)} *)

  (** A tree block with an optional size bound.

      - [root]: The tree structure
      - [bound]: If [Some size], accesses beyond [size] are out-of-bounds *)
  type t = {
    root : Tree.t;
    bound : sint option;
  }

  val pp : Format.formatter -> t -> unit
  val pp_pretty : Format.formatter -> t -> unit
  val is_empty : t -> bool

  (** State monad for tree block operations. *)
  module SM :
    State_monad.S with type Symex.t = Symex.t and type state = t option

  (** {3 Serialization} *)

  (** Serialized form of tree block content. *)
  type serialized =
    | MemVal of {
        offset : sint;
        len : sint;
        v : MemVal.serialized;
      }
    | Bound of sint

  val pp_serialized : Format.formatter -> serialized -> unit
  val show_serialized : serialized -> string
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized
  val iter_vars_serialized : serialized -> (Var.t * 'a Symex.Value.ty -> unit) -> unit

  (** Lift a missing result, adding offset/length to fixes. *)
  val lift_miss :
    offset:sint ->
    len:sint ->
    ('a, 'err, MemVal.serialized) Symex.Result.t ->
    ('a, 'err, serialized list) Symex.Result.t

  (** Iterate over values in a serialized list. *)
  val iter_values_serialized :
    serialized list -> (MemVal.serialized -> unit) -> unit

  (** Convert [None] to empty, [Some t] to [t]. *)
  val of_opt :
    ?mk_fixes:(unit -> serialized list list Symex.t) ->
    t option ->
    (t, 'err, serialized list) Symex.Result.t

  (** Convert empty to [None]. *)
  val to_opt : t -> t option

  (** {3 Operations} *)

  (** Check that the entire block is exclusively owned. *)
  val assert_exclusively_owned :
    t option -> (unit, 'err, serialized list) Symex.Result.t

  (** Get the owned tree at [\[ofs, ofs+size)] (must be fully owned). *)
  val get_raw_tree_owned :
    sint -> sint -> (Tree.t, 'err, serialized list) SM.Result.t

  (** Put a tree at offset (tree should be at offset 0). *)
  val put_raw_tree :
    sint -> Tree.t -> (unit, 'err, serialized list) SM.Result.t

  (** Allocate a new block with value [v] and size [size]. *)
  val alloc : MemVal.t -> sint -> t

  (** Serialize the entire tree block to predicates. *)
  val serialize : t -> serialized list

  (** Consume a bound predicate. *)
  val consume_bound :
    sint -> t option -> (t option, 'err, serialized list) Symex.Result.t

  (** Produce a bound predicate. *)
  val produce_bound : sint -> unit SM.t

  (** Produce a memory value predicate. *)
  val produce_mem_val : sint -> sint -> MemVal.serialized -> unit SM.t

  (** Consume a memory value predicate. *)
  val consume_mem_val :
    sint ->
    sint ->
    MemVal.serialized ->
    t option ->
    (t option, 'err, serialized list) Symex.Result.t

  (** Produce from a serialized predicate. *)
  val produce : serialized -> unit SM.t
end
