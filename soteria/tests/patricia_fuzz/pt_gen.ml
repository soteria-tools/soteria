(** Generators for {!Pt_model} expressions. *)

open QCheck2

(** The key generator is the part that matters most.

    [Gen.int] is uniform over 63-bit integers, so it emits [min_int] or
    [max_int] with probability [2^-62] and essentially never repeats a key;
    [Gen.int_small] repeats keys often but never reaches the corner cases at
    all. Since the historical Patricia tree bug only shows up when the topmost
    branching bit is the sign bit, neither distribution on its own would find
    it. Mixing the three keeps collisions frequent {i and} the sign boundary
    reachable. *)
let key =
  Gen.oneof_weighted
    [
      (5, Gen.int_small);
      (3, Gen.int);
      ( 2,
        Gen.oneof_list [ min_int; max_int; min_int + 1; max_int - 1; -1; 0; 1 ]
      );
    ]

(* Small values, so that rebinding a key to a physically equal value — the case
   in which the map must be returned unchanged — happens often. *)
let value = Gen.int_range 0 5
let comb = Gen.oneof_list Pt_model.[ Left; Right; Min; Max ]

let upd =
  Gen.oneof_weighted
    Pt_model.
      [
        (2, Gen.return Keep);
        (2, Gen.return Del);
        (3, Gen.map (fun v -> Set v) value);
        (2, Gen.return Incr);
      ]

let ufn = Gen.oneof_list Pt_model.[ Take_guide; Drop; Keep_cur; Sum ]

let expr =
  let open Gen in
  sized
  @@ fix (fun self n ->
      let leaf =
        oneof_weighted
          [
            (1, return Pt_model.Empty);
            ( 3,
              let+ k = key and+ v = value in
              Pt_model.Singleton (k, v) );
          ]
      in
      if n <= 0 then leaf
      else
        oneof_weighted
          [
            (1, leaf);
            ( 4,
              let+ k = key and+ v = value and+ t = self (n - 1) in
              Pt_model.Add (k, v, t) );
            ( 2,
              let+ k = key and+ t = self (n - 1) in
              Pt_model.Remove (k, t) );
            ( 3,
              let+ k = key and+ u = upd and+ t = self (n - 1) in
              Pt_model.Update (k, u, t) );
            ( 4,
              let+ c = comb and+ a = self (n / 2) and+ b = self (n / 2) in
              Pt_model.Union (c, a, b) );
            ( 3,
              let+ c = comb and+ a = self (n / 2) and+ b = self (n / 2) in
              Pt_model.Inter (c, a, b) );
            ( 3,
              let+ f = ufn and+ g = self (n / 2) and+ t = self (n / 2) in
              Pt_model.Update_from (f, g, t) );
            ( 2,
              let+ l = list_small (pair key value) and+ t = self (n - 1) in
              Pt_model.Add_seq (l, t) );
            ( 1,
              let+ t = self (n - 1) in
              Pt_model.Compact t );
          ])

let set_expr =
  let open Gen in
  sized
  @@ fix (fun self n ->
      let leaf =
        oneof_weighted
          [
            (1, return Pt_model.Set_expr.Empty);
            (3, map (fun e -> Pt_model.Set_expr.Singleton e) key);
          ]
      in
      if n <= 0 then leaf
      else
        oneof_weighted
          [
            (1, leaf);
            ( 4,
              let+ e = key and+ t = self (n - 1) in
              Pt_model.Set_expr.Add (e, t) );
            ( 3,
              let+ a = self (n / 2) and+ b = self (n / 2) in
              Pt_model.Set_expr.Union (a, b) );
            ( 3,
              let+ a = self (n / 2) and+ b = self (n / 2) in
              Pt_model.Set_expr.Inter (a, b) );
            ( 1,
              let+ t = self (n - 1) in
              Pt_model.Set_expr.Compact t );
          ])

(* ------------------------------------------------------------------ *)
(* Shrinking                                                          *)
(* ------------------------------------------------------------------ *)

(* Rreplace a node by [empty] or by one of its sub-expressions, then shrink the
   arguments, as Midtgaard's [tshrink] does. *)

let ( @+ ) = Seq.append
let shrink_int = QCheck2.Shrink.int_towards 0

let shrink_pair sa sb (a, b) =
  Seq.map (fun a -> (a, b)) (sa a) @+ Seq.map (fun b -> (a, b)) (sb b)

(* Drop an element, or shrink one in place. *)
let rec shrink_list shrink_elt = function
  | [] -> Seq.empty
  | x :: tl ->
      Seq.cons tl
        (Seq.map (fun x -> x :: tl) (shrink_elt x)
        @+ Seq.map (fun tl -> x :: tl) (shrink_list shrink_elt tl))

let shrink_comb =
  let open Pt_model in
  function Left -> Seq.empty | Right | Min | Max -> Seq.return Left

let shrink_upd =
  let open Pt_model in
  function
  | Keep -> Seq.empty
  | Del | Incr -> Seq.return Keep
  | Set v ->
      List.to_seq [ Keep; Del ] @+ Seq.map (fun v -> Set v) (shrink_int v)

let shrink_ufn =
  let open Pt_model in
  function
  | Take_guide -> Seq.empty
  | Drop | Keep_cur | Sum -> Seq.return Take_guide

let rec shrink_expr =
  let open Pt_model in
  function
  | Empty -> Seq.empty
  | Singleton (k, v) ->
      Seq.return Empty
      @+ Seq.map (fun k -> Singleton (k, v)) (shrink_int k)
      @+ Seq.map (fun v -> Singleton (k, v)) (shrink_int v)
  | Add (k, v, t) ->
      List.to_seq [ Empty; t; Singleton (k, v) ]
      @+ Seq.map (fun t -> Add (k, v, t)) (shrink_expr t)
      @+ Seq.map (fun k -> Add (k, v, t)) (shrink_int k)
      @+ Seq.map (fun v -> Add (k, v, t)) (shrink_int v)
  | Remove (k, t) ->
      List.to_seq [ Empty; t ]
      @+ Seq.map (fun t -> Remove (k, t)) (shrink_expr t)
      @+ Seq.map (fun k -> Remove (k, t)) (shrink_int k)
  | Update (k, u, t) ->
      List.to_seq [ Empty; t ]
      @+ Seq.map (fun t -> Update (k, u, t)) (shrink_expr t)
      @+ Seq.map (fun u -> Update (k, u, t)) (shrink_upd u)
      @+ Seq.map (fun k -> Update (k, u, t)) (shrink_int k)
  | Union (c, a, b) ->
      List.to_seq [ Empty; a; b ]
      @+ Seq.map (fun a -> Union (c, a, b)) (shrink_expr a)
      @+ Seq.map (fun b -> Union (c, a, b)) (shrink_expr b)
      @+ Seq.map (fun c -> Union (c, a, b)) (shrink_comb c)
  | Inter (c, a, b) ->
      List.to_seq [ Empty; a; b ]
      @+ Seq.map (fun a -> Inter (c, a, b)) (shrink_expr a)
      @+ Seq.map (fun b -> Inter (c, a, b)) (shrink_expr b)
      @+ Seq.map (fun c -> Inter (c, a, b)) (shrink_comb c)
  | Update_from (f, g, t) ->
      List.to_seq [ Empty; t; g ]
      @+ Seq.map (fun g -> Update_from (f, g, t)) (shrink_expr g)
      @+ Seq.map (fun t -> Update_from (f, g, t)) (shrink_expr t)
      @+ Seq.map (fun f -> Update_from (f, g, t)) (shrink_ufn f)
  | Add_seq (l, t) ->
      List.to_seq [ Empty; t ]
      @+ Seq.map (fun t -> Add_seq (l, t)) (shrink_expr t)
      @+ Seq.map
           (fun l -> Add_seq (l, t))
           (shrink_list (shrink_pair shrink_int shrink_int) l)
  | Compact t ->
      List.to_seq [ Empty; t ] @+ Seq.map (fun t -> Compact t) (shrink_expr t)

let rec shrink_set_expr =
  let open Pt_model.Set_expr in
  function
  | Empty -> Seq.empty
  | Singleton e ->
      Seq.return Empty @+ Seq.map (fun e -> Singleton e) (shrink_int e)
  | Add (e, t) ->
      List.to_seq [ Empty; t; Singleton e ]
      @+ Seq.map (fun t -> Add (e, t)) (shrink_set_expr t)
      @+ Seq.map (fun e -> Add (e, t)) (shrink_int e)
  | Union (a, b) ->
      List.to_seq [ Empty; a; b ]
      @+ Seq.map (fun a -> Union (a, b)) (shrink_set_expr a)
      @+ Seq.map (fun b -> Union (a, b)) (shrink_set_expr b)
  | Inter (a, b) ->
      List.to_seq [ Empty; a; b ]
      @+ Seq.map (fun a -> Inter (a, b)) (shrink_set_expr a)
      @+ Seq.map (fun b -> Inter (a, b)) (shrink_set_expr b)
  | Compact t ->
      List.to_seq [ Empty; t ]
      @+ Seq.map (fun t -> Compact t) (shrink_set_expr t)

let expr = Gen.set_shrink shrink_expr expr
let set_expr = Gen.set_shrink shrink_set_expr set_expr

let expr_pair =
  Gen.set_shrink (shrink_pair shrink_expr shrink_expr) (Gen.pair expr expr)

let set_expr_pair =
  Gen.set_shrink
    (shrink_pair shrink_set_expr shrink_set_expr)
    (Gen.pair set_expr set_expr)

let expr_and_keys =
  Gen.set_shrink
    (shrink_pair shrink_expr (shrink_list shrink_int))
    (Gen.pair expr (Gen.list_small key))

(** An expression together with a key that is likely — but not certain — to be
    bound in it, so that both the hit and the miss paths of the queries are
    exercised. *)
let expr_and_key =
  let open Gen in
  let gen =
    let* e = expr in
    let keys = List.map fst (Pt_model.model e) in
    let+ k =
      match keys with
      | [] -> key
      | _ -> oneof_weighted [ (3, oneof_list keys); (1, key) ]
    in
    (e, k)
  in
  set_shrink (shrink_pair shrink_expr shrink_int) gen
