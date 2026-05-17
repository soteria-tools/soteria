(** A re-implementation of {!Svalue} written with the [%%svalue] DSL.

    Every algebraic simplification of the hand-written [svalue.ml] is
    transcribed, arm for arm, as a K-style rewrite rule, so [Svalue_dsl] is
    {b semantically equivalent} to {!Svalue} (proven by the differential test
    in [tests/svalue_dsl]).

    The hash-consed term type, the hash-consing machinery, commutative normal
    forms, the smart constructors, constant folding, the generic [mk_*]
    dispatchers and [eval] are all derived by the PPX. Only value-specific
    glue ([is_mod], [neg], the [Z.t] conveniences, [Infix], …) is hand-written
    — either in a [with {{ ... }}] block (so it lives inside the generated
    module) or after it. *)

open Soteria_std
open Hc
module Var = Symex.Var

(* OCaml-modulo (the fold backing [mod]). *)
let mod_z i1 i2 =
  let r = Z.( mod ) i1 i2 in
  if Z.lt r Z.zero then Z.add r i2 else r

[%%svalue
{|
  ty Bool sort sbool | Int sort sint

  sort sint    : Int  {{ [ `NonZero | `Zero ] }}
  sort nonzero : Int  {{ [ `NonZero ] }}
  sort sbool   : Bool {{ [ `Bool ] }}
  sort any            {{ [ `Bool | `NonZero | `Zero ] }}

  leaf    Var  : poly = Var.t
  literal Bool : Bool = bool   as of_bool
  literal Int  : Int  = Z.t    as int_z   print Z.pp_print

  with {{
    (* [is_mod v n]: is [v] provably a multiple of [n]? *)
    let rec is_mod v n =
      match v.node.kind with
      | Int i1 -> Z.equal (Z.( mod ) i1 n) Z.zero
      | Binop (Plus, v2, v3) -> is_mod v2 n && is_mod v3 n
      | Binop (Minus, v2, v3) -> is_mod v2 n && is_mod v3 n
      | Binop (Times, v2, v3) -> is_mod v2 n || is_mod v3 n
      | _ -> false
  }}

  (* {2 Booleans} *)

  op not : Bool -> Bool ! = Not {
    rule not true     ~> false
    rule not false    ~> true
    rule not (not a)  ~> a
    rule not (a < b)  ~> Leq b a
    rule not (a <= b) ~> Lt b a
    rule not (a || b) ~> And (not a) (not b)
    rule not (a && b) ~> Or (not a) (not b)
  }

  op and_ : Bool -> Bool -> Bool && = And {
    commutative
    rule a && a     ~> a
    rule false && _  ~> false
    rule true  && b  ~> b
  }

  op or_ : Bool -> Bool -> Bool || = Or {
    commutative
    rule true  || _  ~> true
    rule false || b  ~> b
  }

  op ite : Bool -> any -> any -> any = Ite {
    rule ite true  t e ~> t
    rule ite false t e ~> e
    rule ite g true  false ~> g
    rule ite g false true  ~> not g
    rule ite g false e ~> not g && e
    rule ite g true  e ~> g || e
    rule ite g t false ~> g && t
    rule ite g t true  ~> not g || t
    rule ite g t t ~> t
  }

  (* {2 Equality} *)

  op sem_eq : Int -> Int -> Bool == = Eq {
    commutative
    rule a == a ~> true
    rule (#x:Int)  == (#y:Int)  ~> {{ of_bool (Z.equal x y) }}
    rule (#x:Bool) == (#y:Bool) ~> {{ of_bool (Stdlib.( = ) x y) }}
    rule a == (a + c) ~> c == 0
    rule a == (c + a) ~> c == 0
    rule (a + c) == a ~> c == 0
    rule (c + a) == a ~> c == 0
    rule (p + q) == (p + s) ~> q == s
    rule (p + q) == (q + s) ~> p == s
    rule (p + q) == (s + p) ~> q == s
    rule (p + q) == (s + q) ~> p == s
    rule (a + #x) == #y ~> a == (#y - #x)
    rule (#x + a) == #y ~> a == (#y - #x)
    rule (z - #x) == #y ~> z == (#y + #x)
    rule (#x * a) == #y ~> {{ if Z.equal Z.zero x then of_bool (Z.equal Z.zero y)
                              else if Z.equal Z.zero (Z.rem y x) then sem_eq a (int_z (Z.div y x))
                              else v_false }}
    rule (a * #x) == #y ~> {{ if Z.equal Z.zero x then of_bool (Z.equal Z.zero y)
                              else if Z.equal Z.zero (Z.rem y x) then sem_eq a (int_z (Z.div y x))
                              else v_false }}
  }

  (* {2 Integers} *)

  op add : Int -> Int -> Int + = Plus {
    commutative
    fold Z.add
    identity 0
    rule (a + #i) + #j ~> a + (#i + #j)
    rule (#i + a) + #j ~> (#i + #j) + a
    rule #i + (a + #j) ~> (#i + #j) + a
    rule #i + (#j + a) ~> (#i + #j) + a
  }

  op sub : Int -> Int -> Int - = Minus {
    fold Z.sub
    rule a - 0 ~> a
    rule (Var a) - (Var b) ~> 0 when {{ Var.equal a b }}
    rule (#i - a) - #j ~> (#i - #j) - a
    rule (a - #i) - #j ~> a - (#i + #j)
    rule #i - (#j - a) ~> (#i - #j) + a
    rule #i - (a - #j) ~> (#i + #j) - a
    rule (x + y) - x ~> y
    rule (x + y) - y ~> x
    rule x - (x + y) ~> 0 - y
    rule x - (y + x) ~> 0 - y
  }

  op mul : Int -> Int -> Int * = Times {
    commutative
    absorbing 0
    identity 1
    fold Z.mul
  }

  op div : Int -> nonzero -> Int {{/}} = Div {
    fold Z.div
    rule div a 1 ~> a
    rule div (a * #i) #j ~> a when {{ Z.equal i j }}
    rule div (#i * a) #j ~> a when {{ Z.equal i j }}
    rule div #j (a * #i) ~> a when {{ Z.equal i j }}
    rule div #j (#i * a) ~> a when {{ Z.equal i j }}
  }

  op rem : Int -> nonzero -> Int {{rem}} = Rem {
    fold Z.rem
    rule rem a 1 ~> 0
    rule rem a #i ~> 0 when {{ is_mod a i }}
    rule rem (a * n) (b * m) ~> n * (rem a b) when {{ equal n m }}
    rule rem (n * a) (m * b) ~> n * (rem a b) when {{ equal n m }}
  }

  op mod_ : Int -> nonzero -> Int {{mod}} = Mod {
    fold mod_z
    rule mod_ a 1 ~> 0
    rule mod_ a #i ~> 0 when {{ is_mod a i }}
    rule mod_ (mod_ x #m1) #m2 ~> mod_ x #m2 when {{ Z.geq m1 m2 && Z.divisible m1 m2 }}
    rule mod_ (x + (mod_ l #m1)) #m2 ~> mod_ (x + l) #m2 when {{ Z.geq m1 m2 && Z.divisible m1 m2 }}
  }

  op lt : Int -> Int -> Bool < = Lt {
    fold Z.lt
    rule a < a ~> false
    rule a < (a + c) ~> 0 < c
    rule a < (c + a) ~> 0 < c
    rule (a + c) < a ~> c < 0
    rule (c + a) < a ~> c < 0
    rule (p + q) < (p + s) ~> q < s
    rule (p + q) < (q + s) ~> p < s
    rule (p + q) < (s + p) ~> q < s
    rule (p + q) < (s + q) ~> p < s
    rule (a + #x) < #y ~> a < (#y - #x)
    rule (#x + a) < #y ~> a < (#y - #x)
    rule #y < (a + #x) ~> (#y - #x) < a
    rule #y < (#x + a) ~> (#y - #x) < a
    rule (a - #x) < #y ~> a < (#y + #x)
    rule (#x - a) < #y ~> (#x - #y) < a
    rule #y < (a - #x) ~> (#y + #x) < a
    rule #y < (#x - a) ~> a < (#x - #y)
    rule #y < (#x * a) ~> {{ if Z.equal Z.zero x then of_bool (Z.lt y Z.zero)
                             else let op = if Z.divisible y x || Z.gt y Z.zero then lt else leq in
                               if Z.lt Z.zero x then op (int_z (Z.div y x)) a
                               else op a (int_z (Z.div y x)) }}
    rule #y < (a * #x) ~> {{ if Z.equal Z.zero x then of_bool (Z.lt y Z.zero)
                             else let op = if Z.divisible y x || Z.gt y Z.zero then lt else leq in
                               if Z.lt Z.zero x then op (int_z (Z.div y x)) a
                               else op a (int_z (Z.div y x)) }}
    rule (#x * a) < #y ~> {{ if Z.equal Z.zero x then of_bool (Z.lt Z.zero y)
                             else let op = if Z.divisible y x || Z.lt y Z.zero then lt else leq in
                               if Z.lt Z.zero x then op a (int_z (Z.div y x))
                               else op (int_z (Z.div y x)) a }}
    rule (a * #x) < #y ~> {{ if Z.equal Z.zero x then of_bool (Z.lt Z.zero y)
                             else let op = if Z.divisible y x || Z.lt y Z.zero then lt else leq in
                               if Z.lt Z.zero x then op a (int_z (Z.div y x))
                               else op (int_z (Z.div y x)) a }}
    rule (mod_ _ #x) < #y ~> true when {{ Z.leq x y }}
    rule (rem _ #x)  < #y ~> true when {{ Z.leq x y }}
    rule #y < (mod_ _ #x) ~> true when {{ Z.lt y (Z.neg (Z.abs x)) }}
    rule #y < (rem _ #x)  ~> true when {{ Z.lt y (Z.neg (Z.abs x)) }}
    rule n < (ite b t e) ~> {{ ite b (lt n t) (lt n e) }}
       when {{ (match n.node.kind with Int _ -> true | _ -> false) }}
    rule (ite b t e) < n ~> {{ ite b (lt t n) (lt e n) }}
       when {{ (match n.node.kind with Int _ -> true | _ -> false) }}
  }

  op leq : Int -> Int -> Bool <= = Leq {
    fold Z.leq
    rule a <= a ~> true
    rule a <= (a + c) ~> 0 <= c
    rule a <= (c + a) ~> 0 <= c
    rule (p + q) <= (p + s) ~> q <= s
    rule (p + q) <= (q + s) ~> p <= s
    rule (p + q) <= (s + p) ~> q <= s
    rule (p + q) <= (s + q) ~> p <= s
    rule (a + #x) <= #y ~> a <= (#y - #x)
    rule (#x + a) <= #y ~> a <= (#y - #x)
    rule #y <= (a + #x) ~> (#y - #x) <= a
    rule #y <= (#x + a) ~> (#y - #x) <= a
    rule (a - #x) <= #y ~> a <= (#y + #x)
    rule (#x - a) <= #y ~> (#x - #y) <= a
    rule #y <= (a - #x) ~> (#y + #x) <= a
    rule #y <= (#x - a) ~> a <= (#x - #y)
    rule #y <= (#x * a) ~> {{ if Z.equal Z.zero x then of_bool (Z.lt y Z.zero)
                              else let op = if Z.divisible y x || Z.lt y Z.zero then leq else lt in
                                if Z.lt Z.zero x then op (int_z (Z.div y x)) a
                                else op a (int_z (Z.div y x)) }}
    rule #y <= (a * #x) ~> {{ if Z.equal Z.zero x then of_bool (Z.lt y Z.zero)
                              else let op = if Z.divisible y x || Z.lt y Z.zero then leq else lt in
                                if Z.lt Z.zero x then op (int_z (Z.div y x)) a
                                else op a (int_z (Z.div y x)) }}
    rule (#x * a) <= #y ~> {{ if Z.equal Z.zero x then of_bool (Z.lt y Z.zero)
                              else let op = if Z.divisible y x || Z.gt y Z.zero then leq else lt in
                                if Z.lt Z.zero x then op a (int_z (Z.div y x))
                                else op (int_z (Z.div y x)) a }}
    rule (a * #x) <= #y ~> {{ if Z.equal Z.zero x then of_bool (Z.lt y Z.zero)
                              else let op = if Z.divisible y x || Z.gt y Z.zero then leq else lt in
                                if Z.lt Z.zero x then op a (int_z (Z.div y x))
                                else op (int_z (Z.div y x)) a }}
    rule (mod_ _ #x) <= #y ~> true when {{ Z.leq x y }}
    rule (rem _ #x)  <= #y ~> true when {{ Z.leq x y }}
    rule #y <= (rem _ #x)  ~> true when {{ Z.leq y (Z.neg (Z.abs x)) }}
    rule #y <= (mod_ _ _)  ~> true when {{ Z.leq y Z.zero }}
    rule n <= (ite b t e) ~> {{ ite b (leq n t) (leq n e) }}
       when {{ (match n.node.kind with Int _ -> true | _ -> false) }}
    rule (ite b t e) <= n ~> {{ ite b (leq t n) (leq e n) }}
       when {{ (match n.node.kind with Int _ -> true | _ -> false) }}
  }

  nop distinct = Distinct {{
    fun l ->
      let sure_neq a b =
        (Stdlib.not (equal_ty a.node.ty b.node.ty))
        ||
        match (a.node.kind, b.node.kind) with
        | Int a, Int b -> Stdlib.not (Z.equal a b)
        | Bool a, Bool b -> a <> b
        | _ -> false
      in
      match l with
      | [] | [ _ ] -> v_true
      | l ->
          let cross = Seq.self_cross_product (List.to_seq l) in
          if Seq.for_all (fun (a, b) -> sure_neq a b) cross then v_true
          else Nop (Distinct, l) <| TBool
  }}
|}]

(* {2 Hand-written conveniences (the value-specific glue)} *)

let int i = int_z (Z.of_int i)

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z") else int_z z

let nonzero x = if x = 0 then raise (Invalid_argument "nonzero") else int x
let zero = int_z Z.zero
let one = int_z Z.one
let neg v = match v.node.kind with Int i -> int_z (Z.neg i) | _ -> sub zero v
let conj l = List.fold_left and_ v_true l

let rec split_ands (sv : t) (f : t -> unit) : unit =
  match sv.node.kind with
  | Binop (And, s1, s2) ->
      split_ands s1 f;
      split_ands s2 f
  | _ -> f sv

let geq v1 v2 = leq v2 v1
let gt v1 v2 = lt v2 v1

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

let sure_neq a b =
  (Stdlib.not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Int a, Int b -> Stdlib.not (Z.equal a b)
  | Bool a, Bool b -> a <> b
  | _ -> false

module Infix = struct
  let int_z = int_z
  let int = int
  let ( ==@ ) = sem_eq
  let ( ==?@ ) = sem_eq_untyped
  let ( >@ ) = gt
  let ( >=@ ) = geq
  let ( <@ ) = lt
  let ( <=@ ) = leq
  let ( &&@ ) = and_
  let ( ||@ ) = or_
  let ( +@ ) = add
  let ( -@ ) = sub
  let ( ~- ) = neg
  let ( *@ ) = mul
  let ( /@ ) = div
  let ( %@ ) = mod_
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_nonzero = nonzero
    let[@inline] zero () = zero
    let[@inline] one () = one
  end
end
