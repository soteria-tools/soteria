include Iter

(** The bind operator for iterators, also known as [flat_map]. *)
let bind = flat_map

let rec of_list_combine l1 l2 k =
  match (l1, l2) with
  | [], [] -> ()
  | [ a1 ], [ a2 ] -> k (a1, a2)
  | a1 :: b1 :: l1, a2 :: b2 :: l2 ->
      k (a1, a2);
      k (b1, b2);
      of_list_combine l1 l2 k
  | _, _ -> invalid_arg "Iter.of_list_combine"

let rec of_list_combine3 l1 l2 l3 k =
  match (l1, l2, l3) with
  | [], [], [] -> ()
  | [ a1 ], [ a2 ], [ a3 ] -> k (a1, a2, a3)
  | a1 :: b1 :: l1, a2 :: b2 :: l2, a3 :: b3 :: l3 ->
      k (a1, a2, a3);
      k (b1, b2, b3);
      of_list_combine3 l1 l2 l3 k
  | _, _, _ -> invalid_arg "Iter.of_list_combine3"

(** [combine_list i l] will combine the iterator [i] with the list [l],
    returning a new iterator over the values of both [i] and [l]. If [l] is
    smaller than [i], will raise a [Invalid_argument] at the end of the
    iterator. *)
let[@inline] combine_list (i : 'a t) (l : 'b list) k =
  let l = ref l in
  i (fun x ->
      match !l with
      | [] -> invalid_arg "Iter.combine_list"
      | a :: b ->
          l := b;
          k (x, a))

(** Iterates over the elements of an immutable array. *)
let[@inline] of_iarray a k = Iarray.iter k a

(** Iterates over the elements of two immutable arrays in parallel.

    @raise Invalid_argument if the arrays are not of the same length. *)
let[@inline] of_iarray2 a1 a2 k =
  let n = Iarray.length a1 in
  if n <> Iarray.length a2 then invalid_arg "Iter.of_iarray2";
  for i = 0 to n - 1 do
    k (Iarray.unsafe_get a1 i, Iarray.unsafe_get a2 i)
  done

(** Like {!combine_list}, but combines the iterator [i] with an immutable array
    [a] element-wise.

    @raise Invalid_argument if [a] is smaller than [i]. *)
let[@inline] combine_iarray (i : 'a t) (a : 'b Iarray.t) k =
  let n = Iarray.length a in
  let idx = ref 0 in
  i (fun x ->
      if !idx >= n then invalid_arg "Iter.combine_iarray";
      let v = Iarray.unsafe_get a !idx in
      incr idx;
      k (x, v))

let[@inline] repeati n x k =
  let i = ref 0 in
  while !i < n do
    incr i;
    k x
  done

let[@inline] repeatz n x k =
  let i = ref Z.zero in
  while Z.lt !i n do
    i := Z.succ !i;
    k x
  done

(** Groups an iterator over pairs, using the provided comparison function for
    sorting the keys. Eagerly traverses the provided iterator. *)
let group_pairs_by (type k) ?(compare = compare) seq =
  let module Map = Map.Make (struct
    type t = k

    let compare = compare
  end) in
  (* compute group table *)
  let map =
    lazy
      (let map = ref Map.empty in
       seq (fun (k, v) ->
           map :=
             Map.update k
               (function None -> Some [ v ] | Some l -> Some (v :: l))
               !map);
       !map)
  in
  fun yield -> Map.iter (fun k vs -> yield (k, vs)) (Lazy.force map)

(** Creates an iterator from a function that iterates over bindings. *)
let of_iter_bindings (f : ('k -> 'v -> unit) -> 'a -> unit) (x : 'a) seq =
  f (fun k v -> seq (k, v)) x
