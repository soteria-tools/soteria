(** Fuzz test: {!PatriciaTree} against a reference model.

    Each property states an agreement between the implementation and an
    obviously-correct association-list model, over containers built by an
    arbitrary tree of API calls. *)

open Pt_model

let test_count =
  let default = 1000 in
  lazy
    (match Sys.getenv "QCHECK_TEST_COUNT" with
    | s -> ( match int_of_string_opt s with Some n -> n | None -> default)
    | exception Not_found -> default)

let render pp x =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  Format.pp_set_margin ppf 1_000_000;
  pp ppf x;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let print_pair to_string (a, b) =
  Printf.sprintf "%s\nand\n%s" (to_string a) (to_string b)

(* ------------------------------------------------------------------ *)
(* Maps                                                               *)
(* ------------------------------------------------------------------ *)

module Map_props (I : Pt_model.Map_impl) = struct
  module P = Pt_model.Make_map (I)
  open P

  let mk ~name:n ~print gen prop =
    QCheck2.Test.make
      ~count:(I.cases (Lazy.force test_count))
      ~name:(I.name ^ ": " ^ n)
      ~print gen prop

  let print_expr_and_key (e, k) = Printf.sprintf "%s @ key %d" (to_string e) k

  let print_expr_and_keys (e, ks) =
    Printf.sprintf "%s @ keys [%s]" (to_string e)
      (String.concat "; " (List.map string_of_int ks))

  (** The master property: every constructor of the expression language agrees
      with the model. *)
  let agreement =
    mk ~name:"interpretation agrees with the model" ~print:to_string Pt_gen.expr
    @@ fun e ->
    with_map e @@ fun m _ -> abstract m = model e

  (** Patricia tries have a unique representation for a given set of bindings,
      and [equal] relies on it by comparing structurally. Two expressions with
      the same bindings must therefore be [equal] however they were built — this
      is what a merge that duplicates a binding breaks. *)
  let canonicity =
    mk ~name:"equal agrees with the model" ~print:(print_pair to_string)
      Pt_gen.expr_pair
    @@ fun (a, b) ->
    with_map a @@ fun ma _ ->
    with_map b @@ fun mb _ -> M.equal Int.equal ma mb = (model a = model b)

  let queries =
    mk ~name:"queries agree with the model" ~print:print_expr_and_key
      Pt_gen.expr_and_key
    @@ fun (e, k) ->
    with_map e @@ fun m key ->
    let ml = model e in
    let key = key k in
    M.mem key m = Model.mem k ml
    && M.find_opt key m = Model.find_opt k ml
    && M.cardinal m = Model.cardinal ml
    && M.is_empty m = (ml = [])
    &&
    match M.find key m with
    | v -> Model.find_opt k ml = Some v
    | exception Not_found -> Model.find_opt k ml = None

  let traversals =
    mk ~name:"traversals agree with the model" ~print:to_string Pt_gen.expr
    @@ fun e ->
    with_map e @@ fun m _ ->
    let ml = model e in
    let iterated = ref [] in
    M.iter (fun k v -> iterated := (I.id k, v) :: !iterated) m;
    let folded = M.fold (fun k v acc -> (I.id k, v) :: acc) m [] in
    (* [iter] and [fold] must visit the same bindings in the same order. *)
    !iterated = folded
    && Model.sort folded = ml
    && Model.sort
         (List.map (fun (k, v) -> (I.id k, v)) (List.of_seq (M.to_seq m)))
       = ml
    && M.for_all (fun k v -> List.assoc_opt (I.id k) ml = Some v) m
    && M.for_all (fun _ v -> v < 0) m = List.for_all (fun (_, v) -> v < 0) ml
    && M.exists (fun _ v -> v = 3) m = List.exists (fun (_, v) -> v = 3) ml
    && (not (M.exists (fun _ _ -> false) m))
    (* [pp] renders exactly the bindings, in traversal order. *)
    &&
    let items =
      List.rev_map (fun (k, v) -> Printf.sprintf "%d -> %d" k v) folded
    in
    render (M.pp Fmt.int) m = "{" ^ String.concat "; " items ^ "}"

  let round_trip =
    mk ~name:"add_seq undoes to_seq" ~print:to_string Pt_gen.expr @@ fun e ->
    with_map e @@ fun m _ ->
    M.equal Int.equal (M.add_seq (M.to_seq m) M.empty) m

  (** The interface promises that an operation which changes nothing returns a
      physically equal container. Getting this wrong is invisible to every
      property above, but is the whole point of the data structure. *)
  let sharing =
    mk ~name:"operations that change nothing return the same map"
      ~print:print_expr_and_keys Pt_gen.expr_and_keys
    @@ fun (e, ks) ->
    with_map e @@ fun m key ->
    with_map e @@ fun guide _ ->
    let ks = List.map key ks in
    let comb = comb_fn Left in
    (* Sub- and super-maps that agree with [m] on their common keys, so that a
       merge with either has to give back one of its two arguments untouched. *)
    let sub = List.fold_left (fun m k -> M.remove k m) m ks in
    let super =
      List.fold_left (fun m k -> if M.mem k m then m else M.add k 42 m) m ks
    in
    M.update_from M.empty (ufn_fn Drop) m == m
    && M.update_from guide (ufn_fn Keep_cur) m == m
    && M.add_seq Seq.empty m == m
    && M.compact m == m
    && M.union comb m m == m
    && M.inter comb m m == m
    && M.union comb m sub == m
    && M.union comb sub m == m
    && M.union comb m super == super
    && M.union comb super m == super
    && M.inter comb m super == m
    && M.inter comb super m == m
    && M.inter comb m sub == sub
    && M.inter comb sub m == sub
    && List.for_all
         (fun k ->
           M.update k (upd_fn Keep) m == m
           &&
           match M.find_opt k m with
           | Some v -> M.add k v m == m
           | None -> M.remove k m == m)
         ks

  let assert_new =
    mk ~name:"add_assert_new agrees with add on fresh keys"
      ~print:print_expr_and_key Pt_gen.expr_and_key
    @@ fun (e, k) ->
    with_map e @@ fun m key ->
    let key = key k in
    match M.add_assert_new key 7 m with
    | m' -> (not (M.mem key m)) && abstract m' = Model.add k 7 (model e)
    | exception Invalid_argument _ -> M.mem key m

  let all =
    [
      agreement;
      canonicity;
      queries;
      traversals;
      round_trip;
      sharing;
      assert_new;
    ]
end

(* ------------------------------------------------------------------ *)
(* Sets                                                               *)
(* ------------------------------------------------------------------ *)

module Set_props (I : Pt_model.Set_impl) = struct
  module P = Pt_model.Make_set (I)
  open P

  let mk ~name:n ~print gen prop =
    QCheck2.Test.make
      ~count:(I.cases (Lazy.force test_count))
      ~name:(I.name ^ ": " ^ n)
      ~print gen prop

  let agreement =
    mk ~name:"sets agree with the model" ~print:Set_expr.to_string
      Pt_gen.set_expr
    @@ fun e ->
    with_set e @@ fun s elt ->
    let ml = set_model e in
    let iterated = ref [] in
    S.iter (fun x -> iterated := I.id x :: !iterated) s;
    abstract s = ml
    && S.is_empty s = (ml = [])
    && List.for_all (fun x -> S.mem (elt x) s) ml
    && S.compact s == s
    && S.union s s == s
    && S.inter s s == s
    && List.sort compare !iterated = ml
    && render S.pp s
       = "{" ^ String.concat "; " (List.rev_map string_of_int !iterated) ^ "}"

  let canonicity =
    mk ~name:"set equal agrees with the model"
      ~print:(print_pair Set_expr.to_string)
      Pt_gen.set_expr_pair
    @@ fun (a, b) ->
    with_set a @@ fun sa _ ->
    with_set b @@ fun sb _ -> S.equal sa sb = (set_model a = set_model b)

  let all = [ agreement; canonicity ]
end

(* ------------------------------------------------------------------ *)

module Strong_map_props = Map_props (Pt_model.Strong_map_impl)
module Weak_map_props = Map_props (Pt_model.Weak_map_impl)
module Strong_set_props = Set_props (Pt_model.Strong_set_impl)
module Weak_set_props = Set_props (Pt_model.Weak_set_impl)

let () =
  let suite =
    List.concat
      [
        Strong_map_props.all;
        Strong_set_props.all;
        Weak_map_props.all;
        Weak_set_props.all;
      ]
  in
  Alcotest.run "fuzz_patricia"
    [ ("patricia_tree", List.map QCheck_alcotest.to_alcotest suite) ]
