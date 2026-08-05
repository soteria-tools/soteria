(** Symbolic representation of {!PatriciaTree} call trees, plus a reference
    model, in the style of Midtgaard, {i QuickChecking Patricia Trees} (TFP
    2017).

    Rather than building a map by a plain sequence of insertions, we generate a
    {i tree} of API calls and check that interpreting it into a real map agrees
    with interpreting it into an obviously-correct but inefficient model. Unions
    and intersections of sub-expressions reach trie shapes that repeated
    insertion never produces, which is what makes the merge paths reachable at
    all. *)

module PT = Soteria.Soteria_std.PatriciaTree

(* ------------------------------------------------------------------ *)
(* Symbolic functions                                                 *)
(* ------------------------------------------------------------------ *)

(** The combining function of [union] and [inter]. It must be idempotent, so
    that shared subtrees may be returned without being traversed; all four
    choices below are. *)
type comb = Left | Right | Min | Max

let comb_fn = function
  | Left -> fun _ a _ -> a
  | Right -> fun _ _ b -> b
  | Min -> fun _ -> min
  | Max -> fun _ -> max

let comb_name = function
  | Left -> "left"
  | Right -> "right"
  | Min -> "min"
  | Max -> "max"

(** The ['a option -> 'a option] argument of [update]. [Keep] is the identity,
    which the interface requires to leave the map physically unchanged. *)
type upd = Keep | Del | Set of int | Incr

let upd_fn = function
  | Keep -> Fun.id
  | Del -> fun _ -> None
  | Set v -> fun _ -> Some v
  | Incr -> Option.map succ

let upd_name = function
  | Keep -> "keep"
  | Del -> "del"
  | Set v -> Printf.sprintf "set %d" v
  | Incr -> "incr"

(** The [key -> 'a option -> 'b -> 'a option] argument of [update_from]. *)
type ufn = Take_guide | Drop | Keep_cur | Sum

let ufn_fn = function
  | Take_guide -> fun _ _ y -> Some y
  | Drop -> fun _ _ _ -> None
  | Keep_cur -> fun _ cur _ -> cur
  | Sum -> fun _ cur y -> Some (Option.value ~default:0 cur + y)

let ufn_name = function
  | Take_guide -> "take_guide"
  | Drop -> "drop"
  | Keep_cur -> "keep_cur"
  | Sum -> "sum"

(* ------------------------------------------------------------------ *)
(* Symbolic expressions                                               *)
(* ------------------------------------------------------------------ *)

type t =
  | Empty
  | Singleton of int * int
  | Add of int * int * t
  | Remove of int * t
  | Update of int * upd * t
  | Union of comb * t * t
  | Inter of comb * t * t
  | Update_from of ufn * t * t  (** guide, then map *)
  | Add_seq of (int * int) list * t
  | Compact of t

let rec to_string = function
  | Empty -> "empty"
  | Singleton (k, v) -> Printf.sprintf "(singleton %d %d)" k v
  | Add (k, v, t) -> Printf.sprintf "(add %d %d %s)" k v (to_string t)
  | Remove (k, t) -> Printf.sprintf "(remove %d %s)" k (to_string t)
  | Update (k, u, t) ->
      Printf.sprintf "(update %d [%s] %s)" k (upd_name u) (to_string t)
  | Union (c, a, b) ->
      Printf.sprintf "(union [%s] %s %s)" (comb_name c) (to_string a)
        (to_string b)
  | Inter (c, a, b) ->
      Printf.sprintf "(inter [%s] %s %s)" (comb_name c) (to_string a)
        (to_string b)
  | Update_from (f, g, t) ->
      Printf.sprintf "(update_from [%s] ~guide:%s %s)" (ufn_name f)
        (to_string g) (to_string t)
  | Add_seq (l, t) ->
      Printf.sprintf "(add_seq [%s] %s)"
        (String.concat "; "
           (List.map (fun (k, v) -> Printf.sprintf "%d,%d" k v) l))
        (to_string t)
  | Compact t -> Printf.sprintf "(compact %s)" (to_string t)

(** Sets support a strict subset of the map operations, so a set expression is
    modelled by the map expression that performs the same calls. *)
module Set_expr = struct
  type t =
    | Empty
    | Singleton of int
    | Add of int * t
    | Union of t * t
    | Inter of t * t
    | Compact of t

  let rec to_string = function
    | Empty -> "empty"
    | Singleton e -> Printf.sprintf "(singleton %d)" e
    | Add (e, t) -> Printf.sprintf "(add %d %s)" e (to_string t)
    | Union (a, b) -> Printf.sprintf "(union %s %s)" (to_string a) (to_string b)
    | Inter (a, b) -> Printf.sprintf "(inter %s %s)" (to_string a) (to_string b)
    | Compact t -> Printf.sprintf "(compact %s)" (to_string t)
end

let rec set_to_map_expr = function
  | Set_expr.Empty -> Empty
  | Set_expr.Singleton e -> Singleton (e, 0)
  | Set_expr.Add (e, t) -> Add (e, 0, set_to_map_expr t)
  | Set_expr.Union (a, b) -> Union (Left, set_to_map_expr a, set_to_map_expr b)
  | Set_expr.Inter (a, b) -> Inter (Left, set_to_map_expr a, set_to_map_expr b)
  | Set_expr.Compact t -> Compact (set_to_map_expr t)

(* ------------------------------------------------------------------ *)
(* The model: an association list, kept sorted with unique keys        *)
(* ------------------------------------------------------------------ *)

module Model = struct
  type nonrec t = (int * int) list

  let sort l = List.sort (fun (a, _) (b, _) -> compare a b) l
  let empty = []
  let find_opt = List.assoc_opt
  let remove = List.remove_assoc
  let add k v l = sort ((k, v) :: remove k l)
  let mem = List.mem_assoc
  let cardinal = List.length

  let update k f l =
    match f (find_opt k l) with None -> remove k l | Some v -> add k v l

  let keys l r = List.sort_uniq compare (List.map fst l @ List.map fst r)

  let union f l r =
    keys l r
    |> List.filter_map (fun k ->
        match (find_opt k l, find_opt k r) with
        | Some a, Some b -> Some (k, f k a b)
        | Some a, None -> Some (k, a)
        | None, Some b -> Some (k, b)
        | None, None -> None)

  let inter f l r =
    keys l r
    |> List.filter_map (fun k ->
        match (find_opt k l, find_opt k r) with
        | Some a, Some b -> Some (k, f k a b)
        | _ -> None)

  (* Guide keys are unique, so touching them one at a time is the same as
     applying [f] to the original map everywhere at once. *)
  let update_from f guide m =
    List.fold_left
      (fun acc (k, y) -> update k (fun cur -> f k cur y) acc)
      m guide

  let add_seq l m = List.fold_left (fun acc (k, v) -> add k v acc) m l
end

(** Interprets an expression into the model. *)
let rec model = function
  | Empty -> Model.empty
  | Singleton (k, v) -> [ (k, v) ]
  | Add (k, v, t) -> Model.add k v (model t)
  | Remove (k, t) -> Model.remove k (model t)
  | Update (k, u, t) -> Model.update k (upd_fn u) (model t)
  | Union (c, a, b) -> Model.union (comb_fn c) (model a) (model b)
  | Inter (c, a, b) -> Model.inter (comb_fn c) (model a) (model b)
  | Update_from (f, g, t) -> Model.update_from (ufn_fn f) (model g) (model t)
  | Add_seq (l, t) -> Model.add_seq l (model t)
  | Compact t -> model t

(** The model of a set expression is the key set of the equivalent map
    expression. *)
let set_model e = List.map fst (model (set_to_map_expr e))

(* ------------------------------------------------------------------ *)
(* How keys are supplied to a container                                *)
(* ------------------------------------------------------------------ *)

(** A key type for the container under test, together with a per-interpretation
    context that keeps every key it hands out reachable. *)
module type Keys = sig
  include PT.Key

  type ctx

  val ctx : unit -> ctx

  (** Injective, and returns the same key for the same integer within a context.
  *)
  val key : ctx -> int -> t

  val id : t -> int

  (** How the container is named in test names. *)
  val name : string

  (** Run between building a container and querying it. Reachable keys must
      survive it. *)
  val settle : unit -> unit

  (** Caps the number of test cases; {!settle} may be expensive. *)
  val cases : int -> int
end

module Strong_keys = struct
  type t = int
  type ctx = unit

  let to_int = Fun.id
  let pp = Fmt.int
  let ctx () = ()
  let key () i = i
  let id = Fun.id
  let name = "strong"
  let settle () = ()
  let cases n = n
end

(** Keys are boxed so that they {i can} be weakly referenced, and the context
    holds every one of them, so that none of them {i is} collected. *)
module Weak_keys = struct
  type t = int ref
  type ctx = (int, int ref) Hashtbl.t

  let to_int r = !r
  let pp ppf r = Fmt.int ppf !r
  let ctx () = Hashtbl.create 64

  let key tbl i =
    match Hashtbl.find_opt tbl i with
    | Some r -> r
    | None ->
        let r = ref i in
        Hashtbl.add tbl i r;
        r

  let id = ( ! )
  let name = "weak"
  let settle () = Gc.full_major ()
  let cases n = min 500 n
end

(* ------------------------------------------------------------------ *)
(* Interpretation                                                     *)
(* ------------------------------------------------------------------ *)

module type Map_impl = sig
  include Keys
  module M : PT.Map with type key = t
end

module type Set_impl = sig
  include Keys
  module S : PT.Set with type elt = t
end

module Make_map (I : Map_impl) = struct
  module M = I.M

  (** [with_map e f] interprets [e] and passes the resulting map, along with the
      function that turns an integer into one of its keys, to [f] — keeping
      every key reachable until [f] returns. *)
  let with_map e f =
    let ctx = I.ctx () in
    let key = I.key ctx in
    let rec go = function
      | Empty -> M.empty
      | Singleton (k, v) -> M.singleton (key k) v
      | Add (k, v, t) -> M.add (key k) v (go t)
      | Remove (k, t) -> M.remove (key k) (go t)
      | Update (k, u, t) -> M.update (key k) (upd_fn u) (go t)
      | Union (c, a, b) -> M.union (comb_fn c) (go a) (go b)
      | Inter (c, a, b) -> M.inter (comb_fn c) (go a) (go b)
      | Update_from (f, g, t) -> M.update_from (go g) (ufn_fn f) (go t)
      | Add_seq (l, t) ->
          M.add_seq (List.to_seq (List.map (fun (k, v) -> (key k, v)) l)) (go t)
      | Compact t -> M.compact (go t)
    in
    let m = go e in
    I.settle ();
    let r = f m key in
    ignore (Sys.opaque_identity ctx);
    r

  let abstract m = Model.sort (M.fold (fun k v acc -> (I.id k, v) :: acc) m [])
end

module Make_set (I : Set_impl) = struct
  module S = I.S

  (** See {!Make_map.with_map}. *)
  let with_set e f =
    let ctx = I.ctx () in
    let elt = I.key ctx in
    let rec go =
      let open Set_expr in
      function
      | Empty -> S.empty
      | Singleton e -> S.singleton (elt e)
      | Add (e, t) -> S.add (elt e) (go t)
      | Union (a, b) -> S.union (go a) (go b)
      | Inter (a, b) -> S.inter (go a) (go b)
      | Compact t -> S.compact (go t)
    in
    let s = go e in
    I.settle ();
    let r = f s elt in
    ignore (Sys.opaque_identity ctx);
    r

  let abstract s = List.sort compare (List.map I.id (S.to_list s))
end

module Strong_map_impl = struct
  include Strong_keys
  module M = PT.MakeMap (Strong_keys)
end

module Weak_map_impl = struct
  include Weak_keys
  module M = PT.MakeWeak (Weak_keys)
end

module Strong_set_impl = struct
  include Strong_keys
  module S = PT.MakeSet (Strong_keys)
end

module Weak_set_impl = struct
  include Weak_keys
  module S = PT.MakeWeakSet (Weak_keys)
end
