open Soteria_rust_lib
open Rustsymex.Syntax
module M = Map.Make (Svalue.Var)

type 'a t = 'a M.t

let empty = M.empty
let find_opt = M.find_opt

let add_vars subst iter_vars : Svalue.Var.t t Rustsymex.t =
  Rustsymex.fold_iter iter_vars ~init:subst ~f:(fun subst (var, ty) ->
      if M.mem var subst then Rustsymex.return subst
      else
        let+ var' = Rustsymex.fresh_var ty in
        M.add var var' subst)

let to_fn subst x = M.find x subst

module Value = struct
  include Soteria.Bv_values.Svalue
  include Soteria.Bv_values.Eval

  let sem_eq v1 v2 = Typed.sem_eq (Typed.type_ v1) (Typed.type_ v2)
end

type values = Value.t t

type learned =
  | Outcome of (Value.t -> values * Typed.sbool Typed.t list)
  | Known of Value.t
  | MissingVar

(** Maps an outcome for a value of type A to an outcome for a value of type B
    given a function from B to A *)
let map_outcome f_subst (map : Value.t -> Value.t) =
  Outcome (fun r -> f_subst @@ map r)

let learn_from_ptr s l o =
  let f_subst r =
    let pc_loc = Value.sem_eq l (Value.Ptr.loc r) in
    let pc_ofs = Value.sem_eq o (Value.Ptr.ofs r) in
    (s, [ pc_loc; pc_ofs ])
  in
  Outcome f_subst

let learn_seq f_subst map_curr map_next v subst =
  let f_subst r =
    let curr, next = (map_curr r, map_next r) in
    let s, pcs = f_subst @@ curr in
    match subst s v with
    | Known v -> (s, Value.sem_eq v next :: pcs)
    | Outcome f_subst -> f_subst @@ next
    | MissingVar -> (s, Value.sem_eq v next :: pcs)
  in
  Outcome f_subst

let rec learn (s : values) (v : Value.t) : learned =
  match Value.kind v with
  (* Literals have no variables to learn from *)
  | Bool _ | Float _ | BitVec _ -> Known v
  (* If we reach a variable, we can learn its value *)
  | Var v -> (
      match M.find_opt v s with
      | Some v -> Known v
      | None -> Outcome (fun r -> (M.add v r s, [])))
  (* For pointers, we can learn their location and offset *)
  | Ptr (l, o) -> (
      match learn s l with
      | Known l -> (
          match learn s o with
          | Known o -> Known (Value.Ptr.mk l o)
          | Outcome f_subst ->
              let learn = fun _ _ -> Known l in
              learn_seq f_subst Value.Ptr.ofs Value.Ptr.loc l learn
          | MissingVar -> learn_from_ptr s l o)
      | Outcome f_subst -> learn_seq f_subst Value.Ptr.loc Value.Ptr.ofs o learn
      | MissingVar -> (
          match learn s o with
          | Known o -> learn_from_ptr s l o
          | Outcome f_subst ->
              learn_seq f_subst Value.Ptr.ofs Value.Ptr.loc l learn
          | MissingVar -> learn_from_ptr s l o))
  (* Invertible unary operations *)
  | Unop (op, v) -> (
      match learn s v with
      | Known v -> Known (Value.eval_unop op v)
      | Outcome f_subst -> (
          let learn = map_outcome f_subst in
          match op with
          | Not -> learn (fun r -> Value.eval_unop Not r)
          | BvNot -> learn (fun r -> Value.eval_unop BvNot r)
          | Neg -> learn (fun r -> Value.eval_unop Neg r)
          | _ -> MissingVar)
      | MissingVar -> MissingVar)
  (* Invertible binary operations *)
  | Binop (op, v1, v2) -> (
      (* What should I do about the checked flag? *)
      match (learn s v1, learn s v2) with
      | Known l, Known r -> Known (Value.eval_binop op l r)
      | Outcome f_subst, Known v -> (
          let learn = map_outcome f_subst in
          match op with
          | Add { checked } ->
              learn (fun r -> Value.eval_binop (Sub { checked }) r v)
          | Sub { checked } ->
              learn (fun r -> Value.eval_binop (Add { checked }) r v)
          (* What's the division sign for inverting multiplication? *)
          | Div _ ->
              learn (fun r -> Value.eval_binop (Mul { checked = true }) r v)
          | FAdd -> learn (fun r -> Value.eval_binop FSub r v)
          | FSub -> learn (fun r -> Value.eval_binop FAdd r v)
          | _ -> MissingVar)
      | Known v, Outcome f_subst -> (
          let learn = map_outcome f_subst in
          match op with
          | Add { checked } ->
              learn (fun r -> Value.eval_binop (Sub { checked }) r v)
          | Sub { checked } ->
              learn (fun r -> Value.eval_binop (Sub { checked }) v r)
          (* What's the division sign for inverting multiplication? *)
          | Div signed -> learn (fun r -> Value.eval_binop (Div signed) v r)
          | FAdd -> learn (fun r -> Value.eval_binop FSub r v)
          | FSub -> learn (fun r -> Value.eval_binop FSub v r)
          | _ -> MissingVar)
      | _ -> MissingVar)
  (* Other unsupported stuff *)
  | _ -> MissingVar
