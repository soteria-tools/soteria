let rec iter f (v : Svalue.t) =
  f v;
  match v.node.kind with
  | Var _ | Bool _ | Int _ | Float _ | BitVec _ -> ()
  | Unop (_, x) -> iter f x
  | Ptr (l, r) | Binop (_, l, r) ->
      iter f l;
      iter f r
  | Ite (b, i, e) ->
      iter f b;
      iter f i;
      iter f e
  | Nop (_, xs) | Seq xs -> List.iter (iter f) xs

(** Fold, in preorder traversal *)
let rec fold f acc (v : Svalue.t) =
  let acc = f acc v in
  match v.node.kind with
  | Var _ | Bool _ | Int _ | Float _ | BitVec _ -> acc
  | Unop (_, x) -> fold f acc x
  | Ptr (l, r) | Binop (_, l, r) ->
      let acc = fold f acc l in
      fold f acc r
  | Ite (x, y, z) ->
      let acc = fold f acc x in
      let acc = fold f acc y in
      fold f acc z
  | Nop (_, xs) | Seq xs -> List.fold_left (fold f) acc xs

let should_lower (v : Svalue.t) : bool =
  let exception FoundBv in
  try
    iter
      (fun v ->
        match v.node.kind with Unop (BvOfInt, _) -> raise FoundBv | _ -> ())
      v;
    false
  with FoundBv -> true

(** Tries lowering, as needed, a value into bitvectors. Returns the new value,
    along with a list of variables that have been replaced by a bit vector of
    the given size. *)
let lower (v : Svalue.t) : Svalue.t * int Soteria_symex.Var.Map.t =
  let module Var = Soteria_symex.Var in
  let module BvData = struct
    type t = bool * int

    let eq (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
    let pp ft (s, z) = Fmt.pf ft "%s%d" (if s then "s" else "u") z
  end in
  (* 1. get, for each Int variable, its desired size and sizedness *)
  let _, int_vars =
    fold
      (fun (bv, acc) v ->
        match (bv, v.node) with
        | Some ((sign, size) as bv_info), { kind = Var v; ty = TInt } -> (
            match Var.Map.find_opt v acc with
            | None -> (bv, Var.Map.add v (Ok bv_info) acc)
            (* if same or smaller, ignore *)
            | Some (Ok ((prev_sign, prev_size) as prev_info)) ->
                if prev_sign = sign then
                  if prev_size >= size then (bv, acc)
                  else
                    (* if larger, re-use *)
                    (bv, Var.Map.add v (Ok bv_info) acc)
                else (
                  Fmt.pr
                    "Warning: variable %a has conflicting bitvector info: \
                     previously %a, now %a@."
                    Var.pp v BvData.pp prev_info BvData.pp bv_info;
                  (bv, Var.Map.add v (Error ()) acc))
            | Some (Error ()) -> (bv, acc))
        | _, { kind = Unop (IntOfBv signed, bv); _ } ->
            let size = Svalue.size_of_bv bv.node.ty in
            (Some (signed, size), acc)
        | _ -> (bv, acc))
      (None, Var.Map.empty) v
  in

  (* helper method to get the bv information expected of an Int expression *)
  let guess_bv : Svalue.t list -> BvData.t option =
    let exception MismatchBv in
    let merge_bvs l r =
      match (l, r) with
      | Some l, Some r -> if BvData.eq l r then Some l else raise MismatchBv
      | Some l, None | None, Some l -> Some l
      | None, None -> None
    in
    let rec aux (v : Svalue.t) =
      match v.node.kind with
      | Var var -> (
          Option.bind (Var.Map.find_opt var int_vars) @@ function
          | Ok bv -> Some bv
          | Error () -> None)
      | Unop (_, x) -> aux x
      | Binop (_, l, r) | Ite (_, l, r) -> merge_bvs (aux l) (aux r)
      | _ -> None
    in
    fun vs ->
      try List.fold_left (fun acc v -> merge_bvs acc @@ aux v) None vs
      with MismatchBv -> None
  in

  (* 2. replace all occurencers of V|X| with IntOfBv(V|X|), basically assuming that var is a bitvector *)
  let v =
    v
    |> Eval.eval ~eval_var:(fun v ty ->
           match Var.Map.find_opt v int_vars with
           | Some (Ok (signed, size)) ->
               let bv_ty = Svalue.t_bv signed size in
               (* assume it's a bit vector *)
               let v = Svalue.mk_var v bv_ty in
               (* and apply a conversion *)
               Some (Svalue.BitVec.to_int signed v)
           | _ -> Some (Svalue.mk_var v ty))
    |> Option.get
  in

  let re_eval =
    Fun.compose (Option.get ?msg:None)
      (Eval.eval ~force:true ~eval_var:(fun v ty -> Some (Svalue.mk_var v ty)))
  in
  (* 3. convert integers to bitvectors everywhere! *)
  let rec aux (v : Svalue.t) : Svalue.t =
    match v.node.kind with
    | Binop (((Eq | Lt | Leq) as op), l, r) -> (
        match guess_bv [ l; r ] with
        | None -> v
        | Some (signed, size) ->
            let l = re_eval @@ Svalue.BitVec.of_int signed size l in
            let r = re_eval @@ Svalue.BitVec.of_int signed size r in
            let op =
              match op with
              | Eq -> Svalue.sem_eq
              | Lt -> Svalue.BitVec.Raw.lt signed
              | Leq -> Svalue.BitVec.Raw.leq signed
              | _ -> failwith "unreachable: not comparison"
            in
            op l r)
    | Binop (And, l, r) -> Svalue.and_ (aux l) (aux r)
    | Binop (Or, l, r) -> Svalue.or_ (aux l) (aux r)
    | Unop (Not, x) -> Svalue.not (aux x)
    | _ -> re_eval v
  in
  let v = aux v in

  (* 4. Finally, return the new value along with all new var types *)
  let var_tys =
    Var.Map.filter_map
      (fun _ bv -> match bv with Ok (_, size) -> Some size | Error () -> None)
      int_vars
  in

  (v, var_tys)
