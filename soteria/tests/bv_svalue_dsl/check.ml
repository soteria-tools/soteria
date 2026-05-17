(* Equivalence check between the hand-written bitvector [Svalue] and the
   PPX-derived [Svalue_dsl] (handoff §8).

   As for [tiny], string identity across two independently hash-consed
   modules is ill-defined, so equivalence is checked *semantically*: a random
   well-typed symbolic expression (bitvectors of a fixed width, booleans,
   pointers) is built through both simplifiers, then evaluated under many
   random environments and the concrete results compared. Both simplifiers
   being sound on every environment is exactly "the two languages are
   equivalent" — and a module with fewer (but only sound) simplifications
   still evaluates every closed term to the same scalar, so the current
   fold-complete [Svalue_dsl] subset is provably equivalent. *)

module S = Soteria.Bv_values.Svalue
module D = Soteria.Bv_values.Svalue_dsl
module SE = Soteria.Bv_values.Eval
module Var = Soteria.Symex.Var

let var = Var.of_int
let w = 8 (* bitvector width used throughout *)
let modulus = Z.(one lsl w)
let mask z = Z.logand z (Z.pred modulus)

(* {2 Recipes} *)

type iexpr =
  | IVar of int
  | IConst of int
  | Add of iexpr * iexpr
  | Sub of iexpr * iexpr
  | Mul of iexpr * iexpr
  | Neg of iexpr
  | BAnd of iexpr * iexpr
  | BOr of iexpr * iexpr
  | BXor of iexpr * iexpr
  | Shl of iexpr * iexpr
  | LShr of iexpr * iexpr
  | AShr of iexpr * iexpr
  | IIte of bexpr * iexpr * iexpr

and bexpr =
  | BVar of int
  | BConst of bool
  | Lt of bool * iexpr * iexpr (* signed *)
  | Leq of bool * iexpr * iexpr (* signed *)
  | EqI of iexpr * iexpr
  | EqB of bexpr * bexpr
  | PtrEq of (iexpr * iexpr) * (iexpr * iexpr)
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr
  | BIte of bexpr * bexpr * bexpr

(* {b Total fragment only.} [div]/[rem]/[mod] are deliberately excluded:
   their partiality is *observable*. [Svalue] has simplifications that drop
   a whole subterm ([0*_→0], [shl≥size→0], [0 rem _→0], ite short-circuit,
   …) and therefore never evaluates — never faults on — that subterm. A
   fold-only [Svalue_dsl] keeps the subterm and *does* fault (div-by-zero)
   when it later becomes concrete-zero, so the two disagree precisely when a
   dropped subterm would div0. For *total* operations dropping a subterm
   never changes the final scalar, so fold-only [Svalue_dsl] is provably
   equivalent. Re-introducing div/rem/mod requires their faithful (not
   fold-only) transcription so the subterm-dropping arms match — that is
   the next transcription milestone (handoff step 5). *)
let rec gen_i depth =
  if depth <= 0 then
    if Random.bool () then IVar (Random.int 3) else IConst (Random.int 256)
  else
    match Random.int 13 with
    | 0 -> IVar (Random.int 3)
    | 1 -> IConst (Random.int 256)
    | 2 -> Add (gen_i (depth - 1), gen_i (depth - 1))
    | 3 -> Sub (gen_i (depth - 1), gen_i (depth - 1))
    | 4 -> Mul (gen_i (depth - 1), gen_i (depth - 1))
    | 5 -> Neg (gen_i (depth - 1))
    | 6 -> BAnd (gen_i (depth - 1), gen_i (depth - 1))
    | 7 -> BOr (gen_i (depth - 1), gen_i (depth - 1))
    | 8 -> BXor (gen_i (depth - 1), gen_i (depth - 1))
    | 9 -> Shl (gen_i (depth - 1), gen_i (depth - 1))
    | 10 -> LShr (gen_i (depth - 1), gen_i (depth - 1))
    | 11 -> AShr (gen_i (depth - 1), gen_i (depth - 1))
    | _ -> IIte (gen_b (depth - 1), gen_i (depth - 1), gen_i (depth - 1))

and gen_b depth =
  if depth <= 0 then
    if Random.bool () then BVar (Random.int 3) else BConst (Random.bool ())
  else
    match Random.int 11 with
    | 0 -> BVar (Random.int 3)
    | 1 -> BConst (Random.bool ())
    | 2 -> Lt (Random.bool (), gen_i (depth - 1), gen_i (depth - 1))
    | 3 -> Leq (Random.bool (), gen_i (depth - 1), gen_i (depth - 1))
    | 4 -> EqI (gen_i (depth - 1), gen_i (depth - 1))
    | 5 -> EqB (gen_b (depth - 1), gen_b (depth - 1))
    | 6 ->
        PtrEq
          ( (gen_i (depth - 1), gen_i (depth - 1)),
            (gen_i (depth - 1), gen_i (depth - 1)) )
    | 7 -> And (gen_b (depth - 1), gen_b (depth - 1))
    | 8 -> Or (gen_b (depth - 1), gen_b (depth - 1))
    | 9 -> Not (gen_b (depth - 1))
    | _ -> BIte (gen_b (depth - 1), gen_b (depth - 1), gen_b (depth - 1))

let rec si = function
  | IVar i -> Printf.sprintf "i%d" i
  | IConst n -> string_of_int n
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (si a) (si b)
  | Sub (a, b) -> Printf.sprintf "(%s - %s)" (si a) (si b)
  | Mul (a, b) -> Printf.sprintf "(%s * %s)" (si a) (si b)
  | Neg a -> Printf.sprintf "(-%s)" (si a)
  | BAnd (a, b) -> Printf.sprintf "(%s & %s)" (si a) (si b)
  | BOr (a, b) -> Printf.sprintf "(%s | %s)" (si a) (si b)
  | BXor (a, b) -> Printf.sprintf "(%s ^ %s)" (si a) (si b)
  | Shl (a, b) -> Printf.sprintf "(%s << %s)" (si a) (si b)
  | LShr (a, b) -> Printf.sprintf "(%s l>> %s)" (si a) (si b)
  | AShr (a, b) -> Printf.sprintf "(%s a>> %s)" (si a) (si b)
  | IIte (c, a, b) -> Printf.sprintf "(%s ? %s : %s)" (sb c) (si a) (si b)

and sb = function
  | BVar i -> Printf.sprintf "b%d" i
  | BConst b -> string_of_bool b
  | Lt (s, a, b) -> Printf.sprintf "(%s <%s %s)" (si a) (if s then "s" else "u") (si b)
  | Leq (s, a, b) -> Printf.sprintf "(%s <=%s %s)" (si a) (if s then "s" else "u") (si b)
  | EqI (a, b) -> Printf.sprintf "(%s == %s)" (si a) (si b)
  | EqB (a, b) -> Printf.sprintf "(%s ==b %s)" (sb a) (sb b)
  | PtrEq ((l1, o1), (l2, o2)) ->
      Printf.sprintf "(&(%s,%s) == &(%s,%s))" (si l1) (si o1) (si l2) (si o2)
  | And (a, b) -> Printf.sprintf "(%s && %s)" (sb a) (sb b)
  | Or (a, b) -> Printf.sprintf "(%s || %s)" (sb a) (sb b)
  | Not a -> Printf.sprintf "!(%s)" (sb a)
  | BIte (c, a, b) -> Printf.sprintf "(%s ? %s : %s)" (sb c) (sb a) (sb b)

(* {2 Symbolic builders} *)

module BS = struct
  let k n = S.BitVec.mk_masked w (Z.of_int n)

  let rec bi = function
    | IVar i -> S.mk_var (var i) (S.t_bv w)
    | IConst n -> k n
    | Add (a, b) -> S.BitVec.add (bi a) (bi b)
    | Sub (a, b) -> S.BitVec.sub (bi a) (bi b)
    | Mul (a, b) -> S.BitVec.mul (bi a) (bi b)
    | Neg a -> S.BitVec.neg (bi a)
    | BAnd (a, b) -> S.BitVec.and_ (bi a) (bi b)
    | BOr (a, b) -> S.BitVec.or_ (bi a) (bi b)
    | BXor (a, b) -> S.BitVec.xor (bi a) (bi b)
    | Shl (a, b) -> S.BitVec.shl (bi a) (bi b)
    | LShr (a, b) -> S.BitVec.lshr (bi a) (bi b)
    | AShr (a, b) -> S.BitVec.ashr (bi a) (bi b)
    | IIte (c, a, b) -> S.Bool.ite (bb c) (bi a) (bi b)

  and bb = function
    | BVar i -> S.mk_var (var (100 + i)) S.TBool
    | BConst b -> if b then S.Bool.v_true else S.Bool.v_false
    | Lt (s, a, b) -> S.BitVec.lt ~signed:s (bi a) (bi b)
    | Leq (s, a, b) -> S.BitVec.leq ~signed:s (bi a) (bi b)
    | EqI (a, b) -> S.Bool.sem_eq (bi a) (bi b)
    | EqB (a, b) -> S.Bool.sem_eq (bb a) (bb b)
    | PtrEq ((l1, o1), (l2, o2)) ->
        S.Bool.sem_eq (S.Ptr.mk (bi l1) (bi o1)) (S.Ptr.mk (bi l2) (bi o2))
    | And (a, b) -> S.Bool.and_ (bb a) (bb b)
    | Or (a, b) -> S.Bool.or_ (bb a) (bb b)
    | Not a -> S.Bool.not (bb a)
    | BIte (c, a, b) -> S.Bool.ite (bb c) (bb a) (bb b)
end

module BD = struct
  let k n = D.bv_mk w (Z.of_int n)

  let rec bi = function
    | IVar i -> D.mk_var (var i) (D.t_bv w)
    | IConst n -> k n
    | Add (a, b) -> D.bv_add false (bi a) (bi b)
    | Sub (a, b) -> D.bv_sub false (bi a) (bi b)
    | Mul (a, b) -> D.bv_mul false (bi a) (bi b)
    | Neg a -> D.bv_neg (bi a)
    | BAnd (a, b) -> D.bv_and (bi a) (bi b)
    | BOr (a, b) -> D.bv_or (bi a) (bi b)
    | BXor (a, b) -> D.bv_xor (bi a) (bi b)
    | Shl (a, b) -> D.bv_shl (bi a) (bi b)
    | LShr (a, b) -> D.bv_lshr (bi a) (bi b)
    | AShr (a, b) -> D.bv_ashr (bi a) (bi b)
    | IIte (c, a, b) -> D.ite (bb c) (bi a) (bi b)

  and bb = function
    | BVar i -> D.mk_var (var (100 + i)) D.TBool
    | BConst b -> if b then D.v_true else D.v_false
    | Lt (s, a, b) -> D.bv_lt s (bi a) (bi b)
    | Leq (s, a, b) -> D.bv_leq s (bi a) (bi b)
    | EqI (a, b) -> D.sem_eq (bi a) (bi b)
    | EqB (a, b) -> D.sem_eq (bb a) (bb b)
    | PtrEq ((l1, o1), (l2, o2)) ->
        D.sem_eq (D.ptr (bi l1) (bi o1)) (D.ptr (bi l2) (bi o2))
    | And (a, b) -> D.and_ (bb a) (bb b)
    | Or (a, b) -> D.or_ (bb a) (bb b)
    | Not a -> D.not (bb a)
    | BIte (c, a, b) -> D.ite (bb c) (bb a) (bb b)
end

(* {2 Concrete evaluation, normalised to a scalar} *)

type env = { iv : Z.t array; bv : bool array }

let s_eval env x =
  let v =
    SE.eval x ~eval_var:(fun _x v ty ->
        let i = Var.to_int v in
        if S.equal_ty ty S.TBool then
          if env.bv.(i - 100) then S.Bool.v_true else S.Bool.v_false
        else S.BitVec.mk w env.iv.(i))
  in
  match S.kind v with
  | S.BitVec z -> `I (mask z)
  | S.Bool b -> `B b
  | _ -> `Other

let d_eval env x =
  match
    D.eval x ~eval_var:(fun v ty ->
        let i = Var.to_int v in
        Some
          (if D.equal_ty ty D.TBool then
             if env.bv.(i - 100) then D.v_true else D.v_false
           else D.bv_mk w env.iv.(i)))
  with
  | None -> `Other
  | Some v -> (
      match D.kind v with
      | D.BitVec z -> `I (mask z)
      | D.Bool b -> `B b
      | _ -> `Other)

let res_eq a b =
  match (a, b) with
  | `I x, `I y -> Z.equal x y
  | `B x, `B y -> Bool.equal x y
  | `Other, `Other -> true
  | _ -> false

let pp_res = function
  | `I z -> "I " ^ Z.to_string z
  | `B b -> "B " ^ string_of_bool b
  | `Other -> "<other>"

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 20000 in
  (try Random.init (int_of_string Sys.argv.(2)) with _ -> Random.init 42);
  let mism = ref 0 and shown = ref 0 in
  for _ = 1 to n do
    let is_int = Random.bool () in
    let depth = 2 + Random.int 4 in
    let guard f = try `Ok (f ()) with Division_by_zero -> `Div0 in
    let sterm, dterm, skip, descr =
      if is_int then
        let e = gen_i depth in
        let s = guard (fun () -> BS.bi e) and d = guard (fun () -> BD.bi e) in
        ( (match s with `Ok t -> `T t | `Div0 -> `Bad),
          (match d with `Ok t -> `T t | `Div0 -> `Bad),
          (s = `Div0) <> (d = `Div0),
          si e )
      else
        let e = gen_b depth in
        let s = guard (fun () -> BS.bb e) and d = guard (fun () -> BD.bb e) in
        ( (match s with `Ok t -> `T t | `Div0 -> `Bad),
          (match d with `Ok t -> `T t | `Div0 -> `Bad),
          (s = `Div0) <> (d = `Div0),
          sb e )
    in
    if skip then (
      incr mism;
      if !shown < 25 then (
        incr shown;
        Printf.printf "MISMATCH build div0  expr=%s\n" descr))
    else
      match (sterm, dterm) with
      | `Bad, `Bad -> ()
      | `T st, `T dt ->
          for _ = 1 to 6 do
            let env =
              {
                iv = Array.init 3 (fun _ -> Z.of_int (Random.int 256));
                bv = Array.init 3 (fun _ -> Random.bool ());
              }
            in
            let sres = s_eval env st and dres = d_eval env dt in
            if not (res_eq sres dres) then (
              incr mism;
              if !shown < 25 then (
                incr shown;
                Printf.printf
                  "MISMATCH svalue=%s dsl=%s\n  expr  =%s\n  S-term=%s\n  \
                   D-term=%s\n"
                  (pp_res sres) (pp_res dres) descr
                  (Format.asprintf "%a" S.pp st)
                  (Format.asprintf "%a" D.pp dt)))
          done
      | _ -> ()
  done;
  if !mism = 0 then Printf.printf "ALL %d x6 RANDOM ENVS EQUIVALENT\n" n
  else (
    Printf.printf "\n%d MISMATCHES\n" !mism;
    exit 1)
