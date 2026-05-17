(** AST for the [%%svalue { ... }] value-language DSL.

    The DSL lets one describe a hash-consed value language the way
    [tiny_values/svalue.ml] is written by hand: a [ty] enumeration, a set of
    leaf/literal kinds, and a set of operators each carrying K-style rewrite
    rules. From this description the PPX derives the hash-consed term type, the
    hash-consing machinery, commutative normal forms, the smart constructors
    (with all their algebraic simplifications), constant folding and the
    generic [eval]/[mk_*] dispatchers.

    Grammar (informal):

    {[
      ty Bool | Int

      leaf    Var  : poly = Var.t
      literal Bool : Bool = bool          as of_bool
      literal Int  : Int  = Z.t           as int_z   print Z.pp_print

      op not : Bool -> Bool "!" = Not {
        involutive
        rule not (a < b)   ~> b <= a
        rule not (a || b)  ~> not a && not b
      }

      op add : Int -> Int -> Int "+" = Plus {
        commutative
        fold Z.add
        identity 0
        rule (a + #i) + #j ~> a + (#i + #j)
        rule a + b ~> b  when {{ Svalue.equal a b |> fun _ -> false }}
      }
    ]}

    A lowercase identifier in a rule is a term-level pattern variable (binds a
    whole hash-consed [t]); [#x] is a literal-level variable (binds the payload
    of a literal kind, e.g. the [Z.t] inside [Int]); [_] is a wildcard;
    [true]/[false] and integer literals are literal constants; [{{ ... }}] is
    an OCaml escape hatch usable as a rule RHS or inside a [when] guard. *)

type loc = Ppxlib.location

(* Boxed [expression] / [core_type] coming straight from the OCaml parser, used
   for the escape hatch and for [fold]/[print] function references. *)
type ocaml_expr = Ppxlib.expression
type ocaml_type = Ppxlib.core_type

type ty_decl = {
  ty_variants : string list;
  ty_args : (string * ocaml_type list) list;
      (** variant -> constructor payload, e.g. [TSeq of ty], [TBitVector of int] *)
  ty_sorts : (string * string) list;  (** variant -> default phantom sort *)
  ty_loc : loc;
}

(* A phantom "sort" used to generate the typed wrapper, e.g.
   [sort sint : Int = {{ [ `NonZero | `Zero ] }}]. *)
type sort_decl = {
  sort_name : string;
  sort_base : string option;  (** runtime [ty] this sort refines *)
  sort_row : ocaml_type;  (** the polymorphic-variant row type *)
  sort_loc : loc;
}

type leaf_decl = {
  leaf_name : string;  (** OCaml constructor, e.g. [Var] *)
  leaf_ty : string option;  (** associated [ty] name, [None] for [poly] *)
  leaf_payload : ocaml_type;  (** OCaml type expression, e.g. [Var.t] *)
  leaf_loc : loc;
}

type literal_decl = {
  lit_name : string;  (** OCaml constructor, e.g. [Int] *)
  lit_ty : string;  (** associated [ty] name, e.g. [Int] *)
  lit_payload : ocaml_type;  (** OCaml payload type, e.g. [Z.t] *)
  lit_ctor : string option;  (** friendly constructor name, e.g. [int_z] *)
  lit_print : ocaml_expr option;  (** printer for the payload *)
  lit_loc : loc;
}

(* Pattern / rewrite expression tree, shared by rule LHS, RHS and guards.

   [EApp (name, params, args)]: [params] are the operator constructor's
   payload (e.g. the [signed] of [Lt of bool], written [lt[s] a b]); [args]
   are the value operands. [params] is [] for nullary constructors. *)
type expr =
  | EWild of loc
  | EVar of string * loc  (** term-level variable *)
  | ELit of string * string option * loc
      (** literal-level variable [#x] with optional [:Kind] annotation *)
  | EBool of bool * loc
  | EInt of int * loc
  | EApp of string * expr list * expr list * loc
  | EOcaml of ocaml_expr  (** [{{ ... }}] escape hatch *)

type guard = GCmp of [ `Eq | `Neq ] * expr * expr | GAnd of guard * guard | GOcaml of ocaml_expr

type rule = { r_lhs : expr; r_rhs : expr; r_guard : guard option; r_loc : loc }

type op_decl = {
  op_name : string;  (** smart-constructor / prefix name, e.g. [add] *)
  op_args : string list;  (** operand [ty] names *)
  op_ret : string;  (** result [ty] name *)
  op_symbol : string option;  (** infix / pretty symbol, e.g. ["+"] *)
  op_ctor : string;  (** OCaml constructor, e.g. [Plus] *)
  op_params : (string * ocaml_type) list;
      (** constructor payload, e.g. [Lt of bool] -> [("signed", bool)] *)
  op_commutative : bool;
  op_idempotent : bool;  (** [a op a ~> a] *)
  op_involutive : bool;  (** unary [f (f a) ~> a] *)
  op_identity : int option;  (** [a op <id> ~> a] *)
  op_absorbing : int option;  (** [<z> op _ ~> <z>] *)
  op_fold : ocaml_expr option;  (** constant-folding function *)
  op_rules : rule list;
  op_loc : loc;
}

(* n-ary operator (e.g. [Distinct]). Its smart constructor is supplied by hand
   under [op_name]; only the kind/module/dispatch wiring is derived. *)
type nop_decl = {
  nop_name : string;
  nop_ctor : string;
  nop_symbol : string option;
  nop_body : ocaml_expr option;  (** [{{ fun l -> ... }}] smart constructor *)
  nop_loc : loc;
}

type ocaml_struct = Ppxlib.structure

type decl =
  | DTy of ty_decl
  | DLeaf of leaf_decl
  | DLiteral of literal_decl
  | DOp of op_decl
  | DNop of nop_decl
  | DSort of sort_decl
  | DAux of ocaml_struct  (** [with {{ ... }}] in-module helper code *)

type program = decl list
