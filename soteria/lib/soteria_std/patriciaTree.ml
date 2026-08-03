(* Big-endian patricia tries, following Okasaki and Gill, "Fast Mergeable
   Integer Maps" (1998), with the unsigned branching-bit comparison of
   Midtgaard, "QuickChecking Patricia Trees" (2017) — without it, keys whose
   sign bit differs may end up bound twice.

   The leaf's key is stored in a [slot], which is either the key itself or a
   weak pointer to it; this is the only difference between the strong and the
   weak variants, so the whole algorithmic core is shared. Every leaf also
   stores [Key.to_int key] directly, so that navigating and comparing keys never
   dereferences a slot. *)

module type Key = sig
  type t

  val to_int : t -> int
  val pp : Format.formatter -> t -> unit
end

module type Map = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int
  val singleton : key -> 'a -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val add : key -> 'a -> 'a t -> 'a t
  val add_assert_new : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val update_from :
    'b t -> (key -> 'a option -> 'b -> 'a option) -> 'a t -> 'a t

  val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val to_seq : 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val compact : 'a t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Set = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val equal : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val to_list : t -> elt list
  val compact : t -> t
  val pp : Format.formatter -> t -> unit
end

(** How a leaf holds on to its key. *)
module type Slot = sig
  type key
  type t

  val make : key -> t

  (** Whether the key is still reachable; always [true] for a strong slot. *)
  val alive : t -> bool

  val get : t -> key option

  (** [apply s x y f default] is [f key x y] if the key is still reachable, and
      [default] otherwise. Callers pass their own arguments through rather than
      capturing them, so that traversals allocate no closure per leaf. *)
  val apply : t -> 'x -> 'y -> (key -> 'x -> 'y -> 'r) -> 'r -> 'r
end

module Strong_slot (Key : Key) : Slot with type key = Key.t and type t = Key.t =
struct
  type key = Key.t
  type t = key

  let make k = k
  let alive _ = true
  let get k = Some k
  let apply s x y f _ = f s x y
end

module Weak_slot (Key : Key) :
  Slot with type key = Key.t and type t = Key.t Weak.t = struct
  type key = Key.t
  type t = key Weak.t

  let make k =
    let w = Weak.create 1 in
    Weak.set w 0 (Some k);
    w

  let alive w = Weak.check w 0
  let get w = Weak.get w 0

  let apply w x y f default =
    match Weak.get w 0 with Some k -> f k x y | None -> default
end

(* [Leaf (id, slot, data)] binds the key of [slot], whose [Key.to_int] is [id].
   [Branch (prefix, bit, left, right)] holds keys that all agree with [prefix]
   strictly above [bit], [left] holding those whose [bit] is 0. [bit] is a power
   of two, possibly [min_int]. Both children of a branch are non-empty, hence
   [Empty] only ever occurs at the root. Given a set of keys, this
   representation is unique. *)
type ('k, 'a) tree =
  | Empty
  | Leaf of int * 'k * 'a
  | Branch of int * int * ('k, 'a) tree * ('k, 'a) tree

exception Short_circuit

let[@inline] zero_bit k m = k land m = 0

(* Keeps the bits of [k] strictly above [m], setting the lower ones to a fixed
   pattern so that all the keys below a branch share the same prefix. *)
let[@inline] mask k m = k lor (m - 1) land lnot m
let[@inline] match_prefix k p m = mask k m = p

(* Highest set bit of a non-zero [x]. *)
let highest_bit x =
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = if Sys.int_size > 32 then x lor (x lsr 32) else x in
  x - (x lsr 1)

let[@inline] branching_bit p0 p1 = highest_bit (p0 lxor p1)

(* [m] is a more significant bit than [n]. Branching bits are powers of two, and
   the sign bit is [min_int], so they must be compared as unsigned integers:
   this is the discrepancy Midtgaard's model uncovered. *)
let[@inline] higher_bit m n = n >= 0 && (m < 0 || n < m)

(* Assumes [p0] and [p1] disagree above both of their branching bits. *)
let join p0 t0 p1 t1 =
  let m = branching_bit p0 p1 in
  let p = mask p0 m in
  if zero_bit p0 m then Branch (p, m, t0, t1) else Branch (p, m, t1, t0)

(* Branch node that collapses when a child has become empty. *)
let[@inline] branch p m l r =
  match (l, r) with Empty, t | t, Empty -> t | _ -> Branch (p, m, l, r)

let prefix_of = function
  | Empty -> 0
  | Leaf (j, _, _) -> j
  | Branch (p, _, _, _) -> p

(* Descends to the only leaf that may carry [id]. Prefixes are deliberately not
   checked on the way down: a missing key is caught at the leaf, and successful
   lookups are the common case. *)
let rec find_node id t =
  match t with
  | Empty -> Empty
  | Leaf (j, _, _) -> if j = id then t else Empty
  | Branch (_, m, l, r) -> find_node id (if zero_bit id m then l else r)

let rec remove_id id t =
  match t with
  | Empty -> Empty
  | Leaf (j, _, _) -> if j = id then Empty else t
  | Branch (p, m, l, r) ->
      if not (match_prefix id p m) then t
      else if zero_bit id m then
        let l' = remove_id id l in
        if l' == l then t else branch p m l' r
      else
        let r' = remove_id id r in
        if r' == r then t else branch p m l r'

(* Sound because the representation is unique. *)
let rec equal_tree eq s t =
  s == t
  ||
  match (s, t) with
  | Leaf (j, _, v), Leaf (j', _, v') -> j = j' && eq v v'
  | Branch (p, m, l, r), Branch (p', m', l', r') ->
      p = p' && m = m' && equal_tree eq l l' && equal_tree eq r r'
  | _ -> false

module Make_tree (Key : Key) (S : Slot with type key = Key.t) :
  Map with type key = Key.t = struct
  type key = Key.t
  type 'a t = (S.t, 'a) tree

  let empty = Empty
  let is_empty t = t == Empty
  let singleton key data = Leaf (Key.to_int key, S.make key, data)

  let rec cardinal = function
    | Empty -> 0
    | Leaf (_, s, _) -> if S.alive s then 1 else 0
    | Branch (_, _, l, r) -> cardinal l + cardinal r

  let mem key t =
    match find_node (Key.to_int key) t with
    | Leaf (_, s, _) -> S.alive s
    | _ -> false

  let find key t =
    match find_node (Key.to_int key) t with
    | Leaf (_, s, v) when S.alive s -> v
    | _ -> raise Not_found

  let find_opt key t =
    match find_node (Key.to_int key) t with
    | Leaf (_, s, v) when S.alive s -> Some v
    | _ -> None

  (* [id], [key] and [data] are passed down rather than captured, so that a call
     that changes nothing allocates nothing at all. *)
  let rec add_rec id key data assert_new t =
    match t with
    | Empty -> Leaf (id, S.make key, data)
    | Leaf (j, s, v) ->
        if j <> id then join id (Leaf (id, S.make key, data)) j t
        else if not (S.alive s) then Leaf (id, S.make key, data)
        else if assert_new then
          Format.kasprintf invalid_arg
            "PatriciaTree.add_assert_new: %a is already bound" Key.pp key
        else if v == data then t
        else Leaf (id, s, data)
    | Branch (p, m, l, r) ->
        if not (match_prefix id p m) then
          join id (Leaf (id, S.make key, data)) p t
        else if zero_bit id m then
          let l' = add_rec id key data assert_new l in
          if l' == l then t else Branch (p, m, l', r)
        else
          let r' = add_rec id key data assert_new r in
          if r' == r then t else Branch (p, m, l, r')

  let add key data t = add_rec (Key.to_int key) key data false t
  let add_assert_new key data t = add_rec (Key.to_int key) key data true t
  let remove key t = remove_id (Key.to_int key) t

  let update_fresh id key f =
    match f None with None -> Empty | Some v -> Leaf (id, S.make key, v)

  let rec update_rec id key f t =
    match t with
    | Empty -> update_fresh id key f
    | Leaf (j, s, v) -> (
        if j <> id then
          match update_fresh id key f with Empty -> t | l -> join id l j t
        else if not (S.alive s) then update_fresh id key f
        else
          match f (Some v) with
          | None -> Empty
          | Some v' -> if v' == v then t else Leaf (id, s, v'))
    | Branch (p, m, l, r) ->
        if not (match_prefix id p m) then
          match update_fresh id key f with Empty -> t | n -> join id n p t
        else if zero_bit id m then
          let l' = update_rec id key f l in
          if l' == l then t else branch p m l' r
        else
          let r' = update_rec id key f r in
          if r' == r then t else branch p m l r'

  let update key f t = update_rec (Key.to_int key) key f t

  let update_from guide f m =
    let fresh k y () = f k None y in
    (* Keeps the bindings of [g] that [f] maps to [Some], reusing [g]'s shape
       and its slots — both maps then agree on when the key dies. *)
    let rec build g =
      match g with
      | Empty -> Empty
      | Leaf (j, s, y) -> (
          match S.apply s y () fresh None with
          | None -> Empty
          | Some v -> Leaf (j, s, v))
      | Branch (p, m, l, r) -> branch p m (build l) (build r)
    in
    let new_leaf j s y =
      match S.apply s y () fresh None with
      | None -> Empty
      | Some v -> Leaf (j, s, v)
    in
    (* Applies the single guide binding [(j, s, y)] to [t]. *)
    let rec upd1 j s y t =
      match t with
      | Empty -> new_leaf j s y
      | Leaf (j', s', v') -> (
          if j' <> j then
            match new_leaf j s y with Empty -> t | leaf -> join j leaf j' t
          else
            let live = S.alive s' in
            let cur = if live then Some v' else None in
            let slot = if live then s' else s in
            match S.apply s cur y f None with
            | None -> Empty
            | Some v -> (
                match cur with Some c when c == v -> t | _ -> Leaf (j, slot, v))
          )
      | Branch (p, m, l, r) ->
          if not (match_prefix j p m) then
            match new_leaf j s y with Empty -> t | leaf -> join j leaf p t
          else if zero_bit j m then
            let l' = upd1 j s y l in
            if l' == l then t else branch p m l' r
          else
            let r' = upd1 j s y r in
            if r' == r then t else branch p m l r'
    in
    (* No guide key is bound in [t]. *)
    let disjoint g t pt =
      let g' = build g in
      if g' == Empty then t else join (prefix_of g') g' pt t
    in
    let rec go g t =
      match (g, t) with
      | Empty, _ -> t
      | _, Empty -> build g
      | Leaf (j, s, y), _ -> upd1 j s y t
      | Branch (pg, mg, gl, gr), Leaf (j, _, _) ->
          if not (match_prefix j pg mg) then disjoint g t j
          else if zero_bit j mg then branch pg mg (go gl t) (build gr)
          else branch pg mg (build gl) (go gr t)
      | Branch (pg, mg, gl, gr), Branch (p, m, l, r) ->
          if mg = m && pg = p then
            let l' = go gl l in
            let r' = go gr r in
            if l' == l && r' == r then t else branch p m l' r'
          else if higher_bit m mg && match_prefix pg p m then
            (* the whole guide sits under one child of [t] *)
            if zero_bit pg m then
              let l' = go g l in
              if l' == l then t else branch p m l' r
            else
              let r' = go g r in
              if r' == r then t else branch p m l r'
          else if higher_bit mg m && match_prefix p pg mg then
            (* [t] sits under one child of the guide; the other child has no
               counterpart in [t] *)
            if zero_bit p mg then branch pg mg (go gl t) (build gr)
            else branch pg mg (build gl) (go gr t)
          else disjoint g t p
    in
    go guide m

  let union f s t =
    (* Adds a leaf to [t]; [comb] receives the values in the order [f] expects,
       so the same code serves both directions. *)
    let ins comb =
      let rec go j sl v t =
        match t with
        | Empty -> Leaf (j, sl, v)
        | Leaf (j', sl', v') ->
            if j' <> j then join j (Leaf (j, sl, v)) j' t
            else if not (S.alive sl') then Leaf (j, sl, v)
            else
              let v'' = S.apply sl v v' comb v' in
              if v'' == v' then t else Leaf (j, sl', v'')
        | Branch (p, m, l, r) ->
            if not (match_prefix j p m) then join j (Leaf (j, sl, v)) p t
            else if zero_bit j m then
              let l' = go j sl v l in
              if l' == l then t else Branch (p, m, l', r)
            else
              let r' = go j sl v r in
              if r' == r then t else Branch (p, m, l, r')
      in
      go
    in
    let ins_left = ins f in
    let ins_right = ins (fun k a b -> f k b a) in
    let rec go s t =
      if s == t then s
      else
        match (s, t) with
        | Empty, _ -> t
        | _, Empty -> s
        | Leaf (j, sl, v), _ -> ins_left j sl v t
        | _, Leaf (j, sl, v) -> ins_right j sl v s
        | Branch (p, m, s0, s1), Branch (q, n, t0, t1) ->
            if m = n && p = q then
              let l = go s0 t0 in
              let r = go s1 t1 in
              if l == s0 && r == s1 then s
              else if l == t0 && r == t1 then t
              else Branch (p, m, l, r)
            else if higher_bit m n && match_prefix q p m then
              if zero_bit q m then
                let l = go s0 t in
                if l == s0 then s else Branch (p, m, l, s1)
              else
                let r = go s1 t in
                if r == s1 then s else Branch (p, m, s0, r)
            else if higher_bit n m && match_prefix p q n then
              if zero_bit p n then
                let l = go s t0 in
                if l == t0 then t else Branch (q, n, l, t1)
              else
                let r = go s t1 in
                if r == t1 then t else Branch (q, n, t0, r)
            else join p s q t
    in
    go s t

  let inter f s t =
    let rec go s t =
      if s == t then s
      else
        match (s, t) with
        | Empty, _ | _, Empty -> Empty
        | Leaf (j, sl, v), _ -> (
            match find_node j t with
            | Leaf (_, sl', v') when S.alive sl && S.alive sl' ->
                let v'' = S.apply sl v v' f v in
                if v'' == v then s else Leaf (j, sl, v'')
            | _ -> Empty)
        | _, Leaf (j, sl', v') -> (
            match find_node j s with
            | Leaf (_, sl, v) when S.alive sl && S.alive sl' ->
                let v'' = S.apply sl v v' f v' in
                if v'' == v' then t else Leaf (j, sl', v'')
            | _ -> Empty)
        | Branch (p, m, s0, s1), Branch (q, n, t0, t1) ->
            if m = n && p = q then
              let l = go s0 t0 in
              let r = go s1 t1 in
              if l == s0 && r == s1 then s
              else if l == t0 && r == t1 then t
              else branch p m l r
            else if higher_bit m n && match_prefix q p m then
              go (if zero_bit q m then s0 else s1) t
            else if higher_bit n m && match_prefix p q n then
              go s (if zero_bit p n then t0 else t1)
            else Empty
    in
    go s t

  let equal eq s t = equal_tree eq s t

  let fold f t acc =
    let rec go t acc =
      match t with
      | Empty -> acc
      | Leaf (_, s, v) -> S.apply s v acc f acc
      | Branch (_, _, l, r) -> go r (go l acc)
    in
    go t acc

  let iter f t = fold (fun k v () -> f k v) t ()

  let for_all f t =
    try
      iter (fun k v -> if not (f k v) then raise_notrace Short_circuit) t;
      true
    with Short_circuit -> false

  let exists f t =
    try
      iter (fun k v -> if f k v then raise_notrace Short_circuit) t;
      false
    with Short_circuit -> true

  let to_seq t =
    let rec go stack () =
      match stack with
      | [] -> Seq.Nil
      | Empty :: tl -> go tl ()
      | Branch (_, _, l, r) :: tl -> go (l :: r :: tl) ()
      | Leaf (_, s, v) :: tl -> (
          match S.get s with
          | None -> go tl ()
          | Some k -> Seq.Cons ((k, v), go tl))
    in
    go [ t ]

  let add_seq seq t = Seq.fold_left (fun t (k, v) -> add k v t) t seq

  (* Dropping the dead leaves and collapsing the branches they leave behind
     yields exactly the trie of the surviving keys, since that trie is
     unique. *)
  let rec compact t =
    match t with
    | Empty -> Empty
    | Leaf (_, s, _) -> if S.alive s then t else Empty
    | Branch (p, m, l, r) ->
        let l' = compact l in
        let r' = compact r in
        if l' == l && r' == r then t else branch p m l' r'

  let pp pp_data ppf t =
    let first = ref true in
    Format.fprintf ppf "@[<hov 2>{";
    iter
      (fun k v ->
        if !first then first := false else Format.fprintf ppf ";@ ";
        Format.fprintf ppf "@[%a ->@ %a@]" Key.pp k pp_data v)
      t;
    Format.fprintf ppf "}@]"
end

module Set_of_map (Key : Key) (M : Map with type key = Key.t) :
  Set with type elt = Key.t = struct
  type elt = Key.t
  type t = unit M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let singleton e = M.singleton e ()
  let mem = M.mem
  let add e t = M.add e () t
  let union a b = M.union (fun _ _ v -> v) a b
  let inter a b = M.inter (fun _ _ v -> v) a b
  let equal a b = M.equal (fun () () -> true) a b
  let iter f t = M.iter (fun k () -> f k) t
  let to_list t = M.fold (fun k () acc -> k :: acc) t []
  let compact = M.compact

  let pp ppf t =
    let first = ref true in
    Format.fprintf ppf "@[<hov 2>{";
    iter
      (fun e ->
        if !first then first := false else Format.fprintf ppf ";@ ";
        Key.pp ppf e)
      t;
    Format.fprintf ppf "}@]"
end

module MakeMap (Key : Key) : Map with type key = Key.t = struct
  include Make_tree (Key) (Strong_slot (Key))

  (* No leaf can ever die. *)
  let compact t = t
end

module MakeWeak (Key : Key) : Map with type key = Key.t = struct
  include Make_tree (Key) (Weak_slot (Key))

  (* Two tries with the same live bindings can have different shapes as long as
     dead leaves remain, so they must be compacted before being compared. *)
  let equal eq a b = equal eq (compact a) (compact b)
end

module MakeSet (Key : Key) : Set with type elt = Key.t =
  Set_of_map (Key) (MakeMap (Key))

module MakeWeakSet (Key : Key) : Set with type elt = Key.t =
  Set_of_map (Key) (MakeWeak (Key))
