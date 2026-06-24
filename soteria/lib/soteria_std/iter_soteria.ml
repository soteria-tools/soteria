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
