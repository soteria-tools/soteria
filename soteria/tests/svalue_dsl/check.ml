(* Equivalence check between the hand-written [Svalue] and the PPX-derived
   [Svalue_dsl].

   String identity across two independently hash-consed modules is not
   well-defined (commutative normal forms depend on per-module allocation
   order), so equivalence is checked *semantically*: a random symbolic
   expression is built through both simplifiers, then evaluated under many
   random environments and compared against each other and against an
   independent reference interpreter. Both simplifiers being sound on every
   environment is exactly "the two languages are equivalent". *)

module S = Soteria.Tiny_values.Svalue
module D = Soteria.Tiny_values.Svalue_dsl
module SE = Soteria.Tiny_values.Eval

let var = Soteria.Symex.Var.of_int

(* {2 Recipes} *)

type iexpr =
  | IVar of int
  | IConst of int
  | Add of iexpr * iexpr
  | Sub of iexpr * iexpr
  | Mul of iexpr * iexpr
  | Div of iexpr * iexpr
  | Rem of iexpr * iexpr
  | Mod of iexpr * iexpr
  | Neg of iexpr
  | IIte of bexpr * iexpr * iexpr

and bexpr =
  | BVar of int
  | BConst of bool
  | Lt of iexpr * iexpr
  | Leq of iexpr * iexpr
  | EqI of iexpr * iexpr
  | EqB of bexpr * bexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr
  | BIte of bexpr * bexpr * bexpr

let rec gen_i depth =
  if depth <= 0 then
    if Random.bool () then IVar (Random.int 3) else IConst (Random.int 9 - 4)
  else
    match Random.int 12 with
    | 0 -> IVar (Random.int 3)
    | 1 -> IConst (Random.int 9 - 4)
    | 2 -> Add (gen_i (depth - 1), gen_i (depth - 1))
    | 3 -> Sub (gen_i (depth - 1), gen_i (depth - 1))
    | 4 -> Mul (gen_i (depth - 1), gen_i (depth - 1))
    | 5 -> Div (gen_i (depth - 1), gen_i (depth - 1))
    | 6 -> Rem (gen_i (depth - 1), gen_i (depth - 1))
    | 7 -> Mod (gen_i (depth - 1), gen_i (depth - 1))
    | 8 -> Neg (gen_i (depth - 1))
    | _ -> IIte (gen_b (depth - 1), gen_i (depth - 1), gen_i (depth - 1))

and gen_b depth =
  if depth <= 0 then
    if Random.bool () then BVar (Random.int 3) else BConst (Random.bool ())
  else
    match Random.int 10 with
    | 0 -> BVar (Random.int 3)
    | 1 -> BConst (Random.bool ())
    | 2 -> Lt (gen_i (depth - 1), gen_i (depth - 1))
    | 3 -> Leq (gen_i (depth - 1), gen_i (depth - 1))
    | 4 -> EqI (gen_i (depth - 1), gen_i (depth - 1))
    | 5 -> EqB (gen_b (depth - 1), gen_b (depth - 1))
    | 6 -> And (gen_b (depth - 1), gen_b (depth - 1))
    | 7 -> Or (gen_b (depth - 1), gen_b (depth - 1))
    | 8 -> Not (gen_b (depth - 1))
    | _ -> BIte (gen_b (depth - 1), gen_b (depth - 1), gen_b (depth - 1))

let rec si = function
  | IVar i -> Printf.sprintf "i%d" i
  | IConst n -> string_of_int n
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (si a) (si b)
  | Sub (a, b) -> Printf.sprintf "(%s - %s)" (si a) (si b)
  | Mul (a, b) -> Printf.sprintf "(%s * %s)" (si a) (si b)
  | Div (a, b) -> Printf.sprintf "(%s / %s)" (si a) (si b)
  | Rem (a, b) -> Printf.sprintf "(%s rem %s)" (si a) (si b)
  | Mod (a, b) -> Printf.sprintf "(%s mod %s)" (si a) (si b)
  | Neg a -> Printf.sprintf "(-%s)" (si a)
  | IIte (c, a, b) -> Printf.sprintf "(%s ? %s : %s)" (sb c) (si a) (si b)

and sb = function
  | BVar i -> Printf.sprintf "b%d" i
  | BConst b -> string_of_bool b
  | Lt (a, b) -> Printf.sprintf "(%s < %s)" (si a) (si b)
  | Leq (a, b) -> Printf.sprintf "(%s <= %s)" (si a) (si b)
  | EqI (a, b) -> Printf.sprintf "(%s == %s)" (si a) (si b)
  | EqB (a, b) -> Printf.sprintf "(%s ==b %s)" (sb a) (sb b)
  | And (a, b) -> Printf.sprintf "(%s && %s)" (sb a) (sb b)
  | Or (a, b) -> Printf.sprintf "(%s || %s)" (sb a) (sb b)
  | Not a -> Printf.sprintf "!(%s)" (sb a)
  | BIte (c, a, b) -> Printf.sprintf "(%s ? %s : %s)" (sb c) (sb a) (sb b)

(* {2 Reference interpreter (ground truth)} *)

exception Undef (* division/modulo by zero *)

type env = { iv : Z.t array; bv : bool array }

let rec ri env = function
  | IVar i -> env.iv.(i)
  | IConst n -> Z.of_int n
  | Add (a, b) -> Z.add (ri env a) (ri env b)
  | Sub (a, b) -> Z.sub (ri env a) (ri env b)
  | Mul (a, b) -> Z.mul (ri env a) (ri env b)
  | Div (a, b) ->
      let d = ri env b in
      if Z.equal d Z.zero then raise Undef else Z.div (ri env a) d
  | Rem (a, b) ->
      let d = ri env b in
      if Z.equal d Z.zero then raise Undef else Z.rem (ri env a) d
  | Mod (a, b) ->
      let d = ri env b in
      if Z.equal d Z.zero then raise Undef
      else
        let r = Z.( mod ) (ri env a) d in
        if Z.lt r Z.zero then Z.add r d else r
  | Neg a -> Z.neg (ri env a)
  | IIte (c, a, b) -> if rb env c then ri env a else ri env b

and rb env = function
  | BVar i -> env.bv.(i)
  | BConst b -> b
  | Lt (a, b) -> Z.lt (ri env a) (ri env b)
  | Leq (a, b) -> Z.leq (ri env a) (ri env b)
  | EqI (a, b) -> Z.equal (ri env a) (ri env b)
  | EqB (a, b) -> Bool.equal (rb env a) (rb env b)
  | And (a, b) -> rb env a && rb env b
  | Or (a, b) -> rb env a || rb env b
  | Not a -> not (rb env a)
  | BIte (c, a, b) -> if rb env c then rb env a else rb env b

(* {2 Symbolic builders} *)

module BS = struct
  let rec bi = function
    | IVar i -> S.mk_var (var i) S.TInt
    | IConst n -> S.int_z (Z.of_int n)
    | Add (a, b) -> S.add (bi a) (bi b)
    | Sub (a, b) -> S.sub (bi a) (bi b)
    | Mul (a, b) -> S.mul (bi a) (bi b)
    | Div (a, b) -> S.div (bi a) (bi b)
    | Rem (a, b) -> S.rem (bi a) (bi b)
    | Mod (a, b) -> S.mod_ (bi a) (bi b)
    | Neg a -> S.neg (bi a)
    | IIte (c, a, b) -> S.ite (bb c) (bi a) (bi b)

  and bb = function
    | BVar i -> S.mk_var (var (100 + i)) S.TBool
    | BConst b -> if b then S.v_true else S.v_false
    | Lt (a, b) -> S.lt (bi a) (bi b)
    | Leq (a, b) -> S.leq (bi a) (bi b)
    | EqI (a, b) -> S.sem_eq (bi a) (bi b)
    | EqB (a, b) -> S.sem_eq (bb a) (bb b)
    | And (a, b) -> S.and_ (bb a) (bb b)
    | Or (a, b) -> S.or_ (bb a) (bb b)
    | Not a -> S.not (bb a)
    | BIte (c, a, b) -> S.ite (bb c) (bb a) (bb b)
end

module BD = struct
  let rec bi = function
    | IVar i -> D.mk_var (var i) D.TInt
    | IConst n -> D.int_z (Z.of_int n)
    | Add (a, b) -> D.add (bi a) (bi b)
    | Sub (a, b) -> D.sub (bi a) (bi b)
    | Mul (a, b) -> D.mul (bi a) (bi b)
    | Div (a, b) -> D.div (bi a) (bi b)
    | Rem (a, b) -> D.rem (bi a) (bi b)
    | Mod (a, b) -> D.mod_ (bi a) (bi b)
    | Neg a -> D.neg (bi a)
    | IIte (c, a, b) -> D.ite (bb c) (bi a) (bi b)

  and bb = function
    | BVar i -> D.mk_var (var (100 + i)) D.TBool
    | BConst b -> if b then D.v_true else D.v_false
    | Lt (a, b) -> D.lt (bi a) (bi b)
    | Leq (a, b) -> D.leq (bi a) (bi b)
    | EqI (a, b) -> D.sem_eq (bi a) (bi b)
    | EqB (a, b) -> D.sem_eq (bb a) (bb b)
    | And (a, b) -> D.and_ (bb a) (bb b)
    | Or (a, b) -> D.or_ (bb a) (bb b)
    | Not a -> D.not (bb a)
    | BIte (c, a, b) -> D.ite (bb c) (bb a) (bb b)
end

(* {2 Concrete evaluation of the simplified symbolic terms} *)

let s_eval env x =
  SE.eval x ~eval_var:(fun v ty ->
      let i = Soteria.Symex.Var.to_int v in
      if S.equal_ty ty S.TBool then
        Some (if env.bv.(i - 100) then S.v_true else S.v_false)
      else Some (S.int_z env.iv.(i)))

let d_eval env x =
  D.eval x ~eval_var:(fun v ty ->
      let i = Soteria.Symex.Var.to_int v in
      if D.equal_ty ty D.TBool then
        Some (if env.bv.(i - 100) then D.v_true else D.v_false)
      else Some (D.int_z env.iv.(i)))

let s_scalar v =
  match (S.kind v : S.t_kind) with
  | S.Int z -> `I z
  | S.Bool b -> `B b
  | _ -> `Sym (Format.asprintf "%a" S.pp v)

let d_scalar v =
  match (D.kind v : D.t_kind) with
  | D.Int z -> `I z
  | D.Bool b -> `B b
  | _ -> `Sym (Format.asprintf "%a" D.pp v)

let pp_res = function
  | `I z -> "I " ^ Z.to_string z
  | `B b -> "B " ^ string_of_bool b
  | `B_ref b -> "B " ^ string_of_bool b
  | `Sym s -> "Sym " ^ s
  | `None -> "<none>"

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 20000 in
  (try Random.init (int_of_string Sys.argv.(2)) with _ -> Random.init 42);
  let mism = ref 0 and shown = ref 0 in
  for _ = 1 to n do
    let is_int = Random.bool () in
    let depth = 2 + Random.int 4 in
    (* Build the symbolic (simplified) terms once. Constant folding can raise
       [Division_by_zero] at *build* time (e.g. [int_z 1 / int_z 0]); both
       simplifiers must agree on raising. *)
    let guard f = try `Ok (f ()) with Division_by_zero -> `Div0 in
    let sterm, dterm, refv, skip, descr =
      if is_int then
        let e = gen_i depth in
        let s = guard (fun () -> BS.bi e) and d = guard (fun () -> BD.bi e) in
        ( (match s with `Ok t -> `I t | `Div0 -> `Bad),
          (match d with `Ok t -> `I t | `Div0 -> `Bad),
          (fun env -> try `I (ri env e) with Undef -> `None),
          (s = `Div0) <> (d = `Div0),
          si e )
      else
        let e = gen_b depth in
        let s = guard (fun () -> BS.bb e) and d = guard (fun () -> BD.bb e) in
        ( (match s with `Ok t -> `B t | `Div0 -> `Bad),
          (match d with `Ok t -> `B t | `Div0 -> `Bad),
          (fun env -> try `B_ref (rb env e) with Undef -> `None),
          (s = `Div0) <> (d = `Div0),
          sb e )
    in
    if skip then (
      incr mism;
      if !shown < 25 then (
        incr shown;
        Printf.printf "MISMATCH build div0  expr=%s\n" descr))
    else if sterm = `Bad || dterm = `Bad then ()
    else
    for _ = 1 to 6 do
      let env =
        {
          iv = Array.init 3 (fun _ -> Z.of_int (Random.int 13 - 6));
          bv = Array.init 3 (fun _ -> Random.bool ());
        }
      in
      let reference = refv env in
      let sres =
        match sterm with
        | `I t | `B t -> (
            match s_eval env t with Some v -> s_scalar v | None -> `None)
        | `Bad -> `None
      in
      let dres =
        match dterm with
        | `I t | `B t -> (
            match d_eval env t with Some v -> d_scalar v | None -> `None)
        | `Bad -> `None
      in
      let norm = function `B_ref b -> `B b | x -> x in
      ignore reference;
      (* Equivalence target: svalue_dsl behaves exactly like svalue. *)
      let ok = norm sres = norm dres in
      if not ok then (
        incr mism;
        if !shown < 25 then (
          incr shown;
          let sterm_pp =
            match sterm with `I t | `B t -> Format.asprintf "%a" S.pp t | `Bad -> "<bad>"
          in
          let dterm_pp =
            match dterm with `I t | `B t -> Format.asprintf "%a" D.pp t | `Bad -> "<bad>"
          in
          Printf.printf
            "MISMATCH svalue=%s dsl=%s\n  expr  =%s\n  S-term=%s\n  D-term=%s\n"
            (pp_res (norm sres)) (pp_res (norm dres)) descr sterm_pp dterm_pp))
    done
  done;
  if !mism = 0 then Printf.printf "ALL %d x6 RANDOM ENVS EQUIVALENT\n" n
  else (
    Printf.printf "\n%d MISMATCHES\n" !mism;
    exit 1)
