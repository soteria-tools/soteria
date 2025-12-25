include Iter

let rec of_list_combine l1 l2 k =
  match (l1, l2) with
  | [], [] -> ()
  | [ a1 ], [ a2 ] -> k (a1, a2)
  | a1 :: b1 :: l1, a2 :: b2 :: l2 ->
      k (a1, a2);
      k (b1, b2);
      of_list_combine l1 l2 k
  | _, _ -> invalid_arg "Iter.of_list_combine"

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
