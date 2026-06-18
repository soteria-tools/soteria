open Logs.Import
open Soteria_std
module Var = Symex.Var

(** A witness of type equality between two value indices. Produced by [eq_ty]
    and used to recover the index of a hash-consed node without [Obj.magic]. *)
type ('a, 'b) eq = Equal : ('a, 'a) eq

(** {2 Value-kind indices}

    Symbolic values are indexed by a closed polymorphic-variant tag describing
    their kind. We name each tag once here and use the names everywhere (never
    the raw backticked tags); a value that may be one of several kinds uses a
    union of these, e.g. [ [ sbv | sfloat ] t ]. *)

type sbool = [ `Bool ]
type sbv = [ `Bv ]
type sfloat = [ `Float ]
type sloc = [ `Loc ]
type sptr = [ `Ptr ]
type 'a sseq = [ `List of 'a ]

(** Any C-like value: an integer/bitvector, a pointer, or a float. *)
type cval = [ sbv | sptr | sfloat ]

module FloatPrecision = struct
  type t = F16 | F32 | F64 | F128
  [@@deriving eq, show { with_path = false }, ord]

  let size = function F16 -> 16 | F32 -> 32 | F64 -> 64 | F128 -> 128

  let of_size = function
    | 16 -> F16
    | 32 -> F32
    | 64 -> F64
    | 128 -> F128
    | _ -> L.failwith "Invalid float size"
end

module FloatClass = struct
  type t = Normal | Subnormal | Zero | Infinite | NaN
  [@@deriving eq, show { with_path = false }, ord]

  let as_fpclass = function
    | Normal -> FP_normal
    | Subnormal -> FP_subnormal
    | Zero -> FP_zero
    | Infinite -> FP_infinite
    | NaN -> FP_nan
end

module RoundingMode = struct
  type t = NearestTiesToEven | NearestTiesToAway | Ceil | Floor | Truncate
  [@@deriving eq, show { with_path = false }, ord]
end

(* {2 Value kinds (phantom indices)}

   Symbolic values are indexed by an {e open} polymorphic-variant describing
   their kind: [`Bool], [`Bv] (bitvectors and integers), [`Float], [`Loc],
   [`Ptr] and [`List of _] (sequences). The indices are closed, so a node's
   index is fully determined; this is what lets the hash-cons table recover it
   via a type-equality witness ([eq_ty]) without [Obj.magic]. A value that may
   be one of several kinds is wrapped existentially (see [packed]) rather than
   given a union index. *)

type _ ty =
  | TBool : sbool ty
  | TFloat : FloatPrecision.t -> sfloat ty
  | TLoc : int -> sloc ty
  | TPointer : int -> sptr ty (* size of location and offset *)
  | TSeq : 'a ty -> 'a sseq ty
  | TBitVector : int -> sbv ty

(** An existentially-wrapped type, used where the index is irrelevant (e.g. the
    binders of an existential, or heterogeneous lists of types). *)
type packed_ty = PackedTy : 'a ty -> packed_ty

let rec pp_ty : type a. Format.formatter -> a ty -> unit =
 fun ft ty ->
  match ty with
  | TBool -> Fmt.string ft "TBool"
  | TFloat fp -> Fmt.pf ft "TFloat %a" FloatPrecision.pp fp
  | TLoc n -> Fmt.pf ft "TLoc %d" n
  | TPointer n -> Fmt.pf ft "TPointer %d" n
  | TSeq ty -> Fmt.pf ft "TSeq (%a)" pp_ty ty
  | TBitVector n -> Fmt.pf ft "TBitVector %d" n

(** Type-equality test for indices: returns a witness [Equal] when the two types
    are equal. Because the indices are closed, the witness is derivable; it is
    what lets the hash-cons table recover a node's index without [Obj.magic]. *)
let rec eq_ty : type a b. a ty -> b ty -> (a, b) eq option =
 fun t1 t2 ->
  match (t1, t2) with
  | TBool, TBool -> Some Equal
  | TFloat a, TFloat b -> if FloatPrecision.equal a b then Some Equal else None
  | TLoc a, TLoc b -> if Int.equal a b then Some Equal else None
  | TPointer a, TPointer b -> if Int.equal a b then Some Equal else None
  | TSeq a, TSeq b -> (
      match eq_ty a b with Some Equal -> Some Equal | None -> None)
  | TBitVector a, TBitVector b -> if Int.equal a b then Some Equal else None
  | (TBool | TFloat _ | TLoc _ | TPointer _ | TSeq _ | TBitVector _), _ -> None

let equal_ty : type a b. a ty -> b ty -> bool =
 fun t1 t2 -> match eq_ty t1 t2 with Some Equal -> true | None -> false

let ty_tag : type a. a ty -> int = function
  | TBool -> 0
  | TFloat _ -> 1
  | TLoc _ -> 2
  | TPointer _ -> 3
  | TSeq _ -> 4
  | TBitVector _ -> 5

let rec compare_ty : type a b. a ty -> b ty -> int =
 fun t1 t2 ->
  match (t1, t2) with
  | TBool, TBool -> 0
  | TFloat a, TFloat b -> FloatPrecision.compare a b
  | TLoc a, TLoc b -> Int.compare a b
  | TPointer a, TPointer b -> Int.compare a b
  | TSeq a, TSeq b -> compare_ty a b
  | TBitVector a, TBitVector b -> Int.compare a b
  | (TBool | TFloat _ | TLoc _ | TPointer _ | TSeq _ | TBitVector _), _ ->
      Int.compare (ty_tag t1) (ty_tag t2)

let t_bool = TBool
let t_float fp = TFloat fp
let t_f16 = t_float F16
let t_f32 = t_float F32
let t_f64 = t_float F64
let t_f128 = t_float F128
let t_loc n = TLoc n
let t_ptr n = TPointer n
let t_seq ty = TSeq ty
let t_bv n = TBitVector n
let is_float : type a. a ty -> bool = function TFloat _ -> true | _ -> false
let is_bv : type a. a ty -> bool = function TBitVector _ -> true | _ -> false

let precision_of_f : type a. a ty -> FloatPrecision.t = function
  | TFloat p -> p
  | ty -> L.failwith "Not a float: %a" pp_ty ty

let size_of : type a. a ty -> int = function
  | TBitVector n -> n
  | TPointer n -> n
  | TLoc n -> n
  | ty -> L.failwith "Not a bit value: %a" pp_ty ty

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  (* Unary operators are grouped by operand kind; each group is indexed by its
     result kind, so the corresponding [t_kind] constructor carries a concrete
     (non-existential) operand. *)

  (* operand is [sbv] *)
  type _ on_bv =
    | FloatOfBv :
        RoundingMode.t * bool * FloatPrecision.t
        -> sfloat on_bv (* signed * precision *)
    | FloatOfBvRaw : FloatPrecision.t -> sfloat on_bv
    | BvExtract : int * int -> sbv on_bv (* from idx (incl) * to idx (incl) *)
    | BvExtend : bool * int -> sbv on_bv (* signed * by N bits *)
    | BvNot : sbv on_bv
    | Neg : bool -> sbv on_bv
  (* is this negation overflow-checked (operand <> INT_MIN)? optims only *)

  (* operand is [sbool] *)
  type _ on_bool =
    | Not : sbool on_bool
    | BvOfBool : int -> sbv on_bool (* target bitvec size *)

  (* operand is [sfloat] *)
  type _ on_float =
    | BvOfFloat :
        RoundingMode.t * bool * int
        -> sbv on_float (* signed * target bitvec size *)
    | FAbs : sfloat on_float
    | FIs : FloatClass.t -> sbool on_float
    | FRound : RoundingMode.t -> sfloat on_float

  (* operand is [sptr] *)
  type _ on_ptr = GetPtrLoc : sloc on_ptr | GetPtrOfs : sbv on_ptr

  let equal_on_bv : type a b. a on_bv -> b on_bv -> bool =
   fun u1 u2 ->
    match (u1, u2) with
    | FloatOfBv (r1, s1, p1), FloatOfBv (r2, s2, p2) ->
        RoundingMode.equal r1 r2
        && Stdlib.( = ) s1 s2
        && FloatPrecision.equal p1 p2
    | FloatOfBvRaw a, FloatOfBvRaw b -> FloatPrecision.equal a b
    | BvExtract (f1, t1), BvExtract (f2, t2) ->
        Int.equal f1 f2 && Int.equal t1 t2
    | BvExtend (s1, b1), BvExtend (s2, b2) ->
        Stdlib.( = ) s1 s2 && Int.equal b1 b2
    | BvNot, BvNot -> true
    | Neg a, Neg b -> Stdlib.( = ) a b
    | ( (FloatOfBv _ | FloatOfBvRaw _ | BvExtract _ | BvExtend _ | BvNot | Neg _),
        _ ) ->
        false

  let equal_on_bool : type a b. a on_bool -> b on_bool -> bool =
   fun u1 u2 ->
    match (u1, u2) with
    | Not, Not -> true
    | BvOfBool a, BvOfBool b -> Int.equal a b
    | (Not | BvOfBool _), _ -> false

  let equal_on_float : type a b. a on_float -> b on_float -> bool =
   fun u1 u2 ->
    match (u1, u2) with
    | BvOfFloat (r1, s1, n1), BvOfFloat (r2, s2, n2) ->
        RoundingMode.equal r1 r2 && Stdlib.( = ) s1 s2 && Int.equal n1 n2
    | FAbs, FAbs -> true
    | FIs a, FIs b -> FloatClass.equal a b
    | FRound a, FRound b -> RoundingMode.equal a b
    | (BvOfFloat _ | FAbs | FIs _ | FRound _), _ -> false

  let equal_on_ptr : type a b. a on_ptr -> b on_ptr -> bool =
   fun u1 u2 ->
    match (u1, u2) with
    | GetPtrLoc, GetPtrLoc -> true
    | GetPtrOfs, GetPtrOfs -> true
    | (GetPtrLoc | GetPtrOfs), _ -> false

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")

  let pp_on_bv : type a. Format.formatter -> a on_bv -> unit =
   fun ft -> function
    | FloatOfBv (rm, signed, p) ->
        Fmt.pf ft "%abv2f[%a,%a]" pp_signed signed RoundingMode.pp rm
          FloatPrecision.pp p
    | FloatOfBvRaw p -> Fmt.pf ft "bv2f[%a]" FloatPrecision.pp p
    | BvExtract (from, to_) -> Fmt.pf ft "extract[%d-%d]" from to_
    | BvExtend (signed, by) -> Fmt.pf ft "extend[%a%d]" pp_signed signed by
    | BvNot -> Fmt.string ft "!bv"
    | Neg checked -> Fmt.pf ft "-%s" (if checked then "ck" else "")

  let pp_on_bool : type a. Format.formatter -> a on_bool -> unit =
   fun ft -> function
    | Not -> Fmt.string ft "!"
    | BvOfBool n -> Fmt.pf ft "b2bv[%d]" n

  let pp_on_float : type a. Format.formatter -> a on_float -> unit =
   fun ft -> function
    | BvOfFloat (rm, signed, n) ->
        Fmt.pf ft "f2%abv[%a,%d]" pp_signed signed RoundingMode.pp rm n
    | FAbs -> Fmt.string ft "abs."
    | FIs fc -> Fmt.pf ft "fis(%a)" FloatClass.pp fc
    | FRound mode -> Fmt.pf ft "fround(%a)" RoundingMode.pp mode

  let pp_on_ptr : type a. Format.formatter -> a on_ptr -> unit =
   fun ft -> function
    | GetPtrLoc -> Fmt.string ft "loc"
    | GetPtrOfs -> Fmt.string ft "ofs"
end

(** For an arithmetic operation, records the signedness(es) in which it is known
    not to overflow, and so behaves like exact integer arithmetic. This is used
    to justify simplifications. *)
type checked = { signed : bool; unsigned : bool }
[@@deriving eq, show { with_path = false }, ord]

let unchecked = { signed = false; unsigned = false }
let checked_both = { signed = true; unsigned = true }
let checked_signed = { signed = true; unsigned = false }
let checked_unsigned = { signed = false; unsigned = true }

let checked_of_signed signed =
  if signed then checked_signed else checked_unsigned

(** Whether [c] guarantees no overflow in the given signedness. *)
let checked_has ~signed c = if signed then c.signed else c.unsigned

(** Whether [c] guarantees no overflow in at least one signedness. *)
let is_checked c = c.signed || c.unsigned

(** The strongest flag justified by both [a] and [b], i.e. the intersection of
    their guarantees. Used when a simplification rebuilds an operation from two
    already-checked ones. [is_checked (checked_meet a b)] thus tells whether [a]
    and [b] share a signedness in which both are exact. *)
let checked_meet a b =
  { signed = a.signed && b.signed; unsigned = a.unsigned && b.unsigned }

module Binop = struct
  (* Binary operators are grouped by the kinds of their operands and result, so
     that the corresponding [t_kind] constructors carry concrete (i.e.
     non-existential) operands. Equality [Eq] is the only binary operation
     polymorphic in its operands, so it gets its own [t_kind] constructor. *)

  (* bitvector arithmetic & bitwise: [sbv -> sbv -> sbv]. [checked] / [bool]
     payloads record overflow-checkedness / signedness, as before. *)
  type arith =
    | Add of checked
    | Sub of checked
    | Mul of checked
    | Div of bool (* signed *)
    | Rem of bool (* signed *)
    | Mod
    | BvConcat
    | BitAnd
    | BitOr
    | BitXor
    | Shl
    | LShr
    | AShr
  [@@deriving eq]

  (* bitvector comparisons & overflow checks: [sbv -> sbv -> sbool] *)
  type cmp =
    | Lt of bool (* signed *)
    | Leq of bool (* signed *)
    | AddOvf of bool (* signed *)
    | SubOvf of bool (* signed *)
    | MulOvf of bool (* signed *)
  [@@deriving eq]

  (* float arithmetic: [sfloat -> sfloat -> sfloat] *)
  type farith = FAdd | FSub | FMul | FDiv | FRem [@@deriving eq]

  (* float comparisons: [sfloat -> sfloat -> sbool] *)
  type fcmp = FEq | FLeq | FLt [@@deriving eq]

  (* boolean: [sbool -> sbool -> sbool] *)
  type boolean = And | Or [@@deriving eq]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")

  let pp_checked ft = function
    | { signed = false; unsigned = false } -> ()
    | { signed = true; unsigned = false } -> Fmt.string ft "cks"
    | { signed = false; unsigned = true } -> Fmt.string ft "cku"
    | { signed = true; unsigned = true } -> Fmt.string ft "ck"

  let pp_arith ft = function
    | Add checked -> Fmt.pf ft "+%a" pp_checked checked
    | Sub checked -> Fmt.pf ft "-%a" pp_checked checked
    | Mul checked -> Fmt.pf ft "*%a" pp_checked checked
    | Div s -> Fmt.pf ft "/%a" pp_signed s
    | Rem s -> Fmt.pf ft "rem%a" pp_signed s
    | Mod -> Fmt.string ft "mod"
    | BvConcat -> Fmt.string ft "++"
    | BitAnd -> Fmt.string ft "&"
    | BitOr -> Fmt.string ft "|"
    | BitXor -> Fmt.string ft "^"
    | Shl -> Fmt.string ft "<<"
    | LShr -> Fmt.string ft "l>>"
    | AShr -> Fmt.string ft "a>>"

  let pp_cmp ft = function
    | Lt s -> Fmt.pf ft "<%a" pp_signed s
    | Leq s -> Fmt.pf ft "<=%a" pp_signed s
    | AddOvf s -> Fmt.pf ft "+%a_ovf" pp_signed s
    | SubOvf s -> Fmt.pf ft "-%a_ovf" pp_signed s
    | MulOvf s -> Fmt.pf ft "*%a_ovf" pp_signed s

  let pp_farith ft = function
    | FAdd -> Fmt.string ft "+."
    | FSub -> Fmt.string ft "-."
    | FMul -> Fmt.string ft "*."
    | FDiv -> Fmt.string ft "/."
    | FRem -> Fmt.string ft "rem."

  let pp_fcmp ft = function
    | FEq -> Fmt.string ft "==."
    | FLeq -> Fmt.string ft "<=."
    | FLt -> Fmt.string ft "<."

  let pp_boolean ft = function
    | And -> Fmt.string ft "&&"
    | Or -> Fmt.string ft "||"
end

type _ t_kind =
  | Var : Var.t -> 'a t_kind
  | Bool : bool -> sbool t_kind
  | Float : string -> sfloat t_kind
  | Ptr : sloc t * sbv t -> sptr t_kind
  | BitVec : Z.t -> sbv t_kind
  | LocLit : Z.t -> sloc t_kind
    (* A concrete location identifier (the null location is [LocLit 0]).
       Locations are bit-vector-shaped but kept distinct from [sbv] at the type
       level, so they have their own literal. *)
  | Seq : 'a t list -> 'a sseq t_kind
  | UnBv : 'o Unop.on_bv * sbv t -> 'o t_kind
  | UnBool : 'o Unop.on_bool * sbool t -> 'o t_kind
  | UnFloat : 'o Unop.on_float * sfloat t -> 'o t_kind
  | UnPtr : 'o Unop.on_ptr * sptr t -> 'o t_kind
  | BvArith : Binop.arith * sbv t * sbv t -> sbv t_kind
  | BvCmp : Binop.cmp * sbv t * sbv t -> sbool t_kind
  | FArith : Binop.farith * sfloat t * sfloat t -> sfloat t_kind
  | FCmp : Binop.fcmp * sfloat t * sfloat t -> sbool t_kind
  | BoolBin : Binop.boolean * sbool t * sbool t -> sbool t_kind
  | Eq : 'a t * 'a t -> sbool t_kind
  | Nop : Nop.t * 'a t list -> sbool t_kind
  | Ite : sbool t * 'a t * 'a t -> 'a t_kind
  | Exists : (Var.t * packed_ty) list * sbool t -> sbool t_kind

and 'a t_node = { kind : 'a t_kind; ty : 'a ty }

(* Our own hash-consed wrapper (we do not use the [Hc] library, because we need
   to recover a node's index via a type-equality witness rather than
   [Obj.magic]). [tag] is a process-unique identifier. *)
and 'a hash_consed = { node : 'a; tag : int }
and 'a t = 'a t_node hash_consed

(* An existentially-wrapped hash-consed node: the entries of the hash-cons
   table. *)
type packed = Packed : 'a t_node hash_consed -> packed

let[@inline] hash (t : 'a t) = t.tag
let[@inline] kind (t : 'a t) : 'a t_kind = t.node.kind
let[@inline] get_ty (t : 'a t) : 'a ty = t.node.ty
let[@inline] unique_tag (t : 'a t) = t.tag
let is_bool_ty : type a. a ty -> bool = function TBool -> true | _ -> false

let iter_vars (sv : 'a t) (f : Var.t * packed_ty -> unit) : unit =
  let rec aux : type a. ignore:Var.Set.t -> a t -> unit =
   fun ~ignore sv ->
    let aux' : type b. b t -> unit = fun sv -> aux ~ignore sv in
    match sv.node.kind with
    | Var v -> if Var.Set.mem v ignore then () else f (v, PackedTy sv.node.ty)
    | Bool _ -> ()
    | Float _ -> ()
    | BitVec _ -> ()
    | LocLit _ -> ()
    | Ptr (l, r) ->
        aux' l;
        aux' r
    | BvArith (_, l, r) ->
        aux' l;
        aux' r
    | BvCmp (_, l, r) ->
        aux' l;
        aux' r
    | FArith (_, l, r) ->
        aux' l;
        aux' r
    | FCmp (_, l, r) ->
        aux' l;
        aux' r
    | BoolBin (_, l, r) ->
        aux' l;
        aux' r
    | Eq (l, r) ->
        aux' l;
        aux' r
    | UnBv (_, sv) -> aux' sv
    | UnBool (_, sv) -> aux' sv
    | UnFloat (_, sv) -> aux' sv
    | UnPtr (_, sv) -> aux' sv
    | Nop (_, l) -> List.iter aux' l
    | Seq l -> List.iter aux' l
    | Ite (c, t, e) ->
        aux' c;
        aux' t;
        aux' e
    | Exists (vs, sv) ->
        let ignore =
          List.fold_left (fun ignore (v, _) -> Var.Set.add v ignore) ignore vs
        in
        aux ~ignore sv
  in
  aux ~ignore:Var.Set.empty sv

let rec pp : type a. Format.formatter -> a t -> unit =
 fun ft t ->
  let open Fmt in
  match t.node.kind with
  | Var v -> pf ft "V%a" Var.pp v
  | Bool b -> pf ft "%b" b
  | Float f -> pf ft "%sf" f
  | BitVec bv ->
      let size = size_of t.node.ty in
      if size mod 4 <> 0 then
        pf ft "0b%s" (Z.format ("0" ^ string_of_int size ^ "b") bv)
      else pf ft "0x%s" (Z.format ("0" ^ string_of_int (size / 4) ^ "x") bv)
  | LocLit z ->
      (* Render concrete locations exactly like the bitvectors they used to be
         (before locations became a distinct kind), to keep output stable. *)
      let size = size_of t.node.ty in
      if size mod 4 <> 0 then
        pf ft "0b%s" (Z.format ("0" ^ string_of_int size ^ "b") z)
      else pf ft "0x%s" (Z.format ("0" ^ string_of_int (size / 4) ^ "x") z)
  | Ptr (l, o) -> pf ft "&(%a, %a)" pp l pp o
  | Seq l -> pf ft "%a" (brackets (list ~sep:comma (fun ft x -> pp ft x))) l
  | Ite (c, t, e) -> pf ft "(%a ? %a : %a)" pp c pp t pp e
  | Exists (vs, v) ->
      let var_pp ft (v, PackedTy ty) = pf ft "V%a:%a" Var.pp v pp_ty ty in
      pf ft "∃ %a. %a" (list ~sep:comma var_pp) vs pp v
  | UnBool (Not, { node = { kind = Eq (v1, v2); _ }; _ }) ->
      pf ft "(%a != %a)" pp v1 pp v2
  | UnBv (op, v) -> pf ft "%a(%a)" Unop.pp_on_bv op pp v
  | UnBool (op, v) -> pf ft "%a(%a)" Unop.pp_on_bool op pp v
  | UnFloat (op, v) -> pf ft "%a(%a)" Unop.pp_on_float op pp v
  | UnPtr (op, v) -> pf ft "%a(%a)" Unop.pp_on_ptr op pp v
  | BvArith (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp_arith op pp v2
  | BvCmp (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp_cmp op pp v2
  | FArith (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp_farith op pp v2
  | FCmp (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp_fcmp op pp v2
  | BoolBin (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp_boolean op pp v2
  | Eq (v1, v2) ->
      pf ft "(%a == %a)" (fun ft x -> pp ft x) v1 (fun ft x -> pp ft x) v2
  | Nop (op, l) -> (
      let rec aux = function
        | acc, [] -> acc
        | Some l, { node = { kind = Var v; _ }; _ } :: rest ->
            aux (Some (Var.to_int v :: l), rest)
        | _, _ -> None
      in
      let range =
        aux (Some [], l)
        |> Option.bind (fun l ->
            let l = List.sort Int.compare l in
            let min = List.hd l in
            let max = List.hd @@ List.rev l in
            if max - min + 1 = List.length l then Some (min, max) else None)
      in
      match range with
      | Some (min, max) -> pf ft "%a(V|%d-%d|)" Nop.pp op min max
      | None ->
          pf ft "%a(%a)" Nop.pp op (list ~sep:comma (fun ft x -> pp ft x)) l)

let pp_full = pp

(* Equality and comparison are purely by the unique hash-consing tag, so they
   are heterogeneous in the value indices (two differently-typed values are
   simply never equal). This avoids spurious "would escape its scope" errors
   when comparing a value of a known type with one extracted from a GADT. *)
let[@inline] equal : 'a 'b. 'a t -> 'b t -> bool =
 fun a b -> Int.equal a.tag b.tag

let[@inline] compare : 'a 'b. 'a t -> 'b t -> int =
 fun a b -> Int.compare a.tag b.tag

let rec sure_neq : type a b. a t -> b t -> bool =
 fun a b ->
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | BitVec a, BitVec b -> not (Z.equal a b)
  | LocLit a, LocLit b -> not (Z.equal a b)
  | Bool a, Bool b -> a <> b
  | Ptr (la, oa), Ptr (lb, ob) -> sure_neq la lb || sure_neq oa ob
  | _ -> false

(* Structural equality of two nodes: types equal and kinds equal, with children
   compared by their (unique) hash-consing tag. Replaces the derived equality;
   used as the hash-consing equality. *)
let rec list_tag_equal : type a b. a t list -> b t list -> bool =
 fun l1 l2 ->
  match (l1, l2) with
  | [], [] -> true
  | x :: xs, y :: ys -> Int.equal x.tag y.tag && list_tag_equal xs ys
  | _ -> false

let equal_binder (v1, PackedTy t1) (v2, PackedTy t2) =
  Var.equal v1 v2 && equal_ty t1 t2

(* Structural equality of two kinds, compared by their children's unique
   hash-consing tags. Heterogeneous (the two kinds need not share an index):
   mismatched constructors fall through to [false], so this doubles as the
   hash-cons table's equality and avoids boxing a [(a, b) eq option] witness on
   every bucket comparison. The [-56] (unreachable-case) warning is suppressed:
   GADT exhaustiveness is imprecise here, so some same-constructor cases are
   wrongly flagged unreachable, but they are reachable at runtime and required
   for correctness. *)
let kind_eq : type a b. a t_kind -> b t_kind -> bool =
 fun k1 k2 ->
  match[@warning "-56"] (k1, k2) with
  | Var a, Var b -> Var.equal a b
  | Bool a, Bool b -> Stdlib.( = ) a b
  | Float a, Float b -> String.equal a b
  | Ptr (l1, o1), Ptr (l2, o2) ->
      Int.equal l1.tag l2.tag && Int.equal o1.tag o2.tag
  | BitVec a, BitVec b -> Z.equal a b
  | LocLit a, LocLit b -> Z.equal a b
  | Seq l1, Seq l2 -> list_tag_equal l1 l2
  | UnBv (op1, v1), UnBv (op2, v2) ->
      Unop.equal_on_bv op1 op2 && Int.equal v1.tag v2.tag
  | UnBool (op1, v1), UnBool (op2, v2) ->
      Unop.equal_on_bool op1 op2 && Int.equal v1.tag v2.tag
  | UnFloat (op1, v1), UnFloat (op2, v2) ->
      Unop.equal_on_float op1 op2 && Int.equal v1.tag v2.tag
  | UnPtr (op1, v1), UnPtr (op2, v2) ->
      Unop.equal_on_ptr op1 op2 && Int.equal v1.tag v2.tag
  | BvArith (op1, a1, b1), BvArith (op2, a2, b2) ->
      Binop.equal_arith op1 op2
      && Int.equal a1.tag a2.tag
      && Int.equal b1.tag b2.tag
  | BvCmp (op1, a1, b1), BvCmp (op2, a2, b2) ->
      Binop.equal_cmp op1 op2
      && Int.equal a1.tag a2.tag
      && Int.equal b1.tag b2.tag
  | FArith (op1, a1, b1), FArith (op2, a2, b2) ->
      Binop.equal_farith op1 op2
      && Int.equal a1.tag a2.tag
      && Int.equal b1.tag b2.tag
  | FCmp (op1, a1, b1), FCmp (op2, a2, b2) ->
      Binop.equal_fcmp op1 op2
      && Int.equal a1.tag a2.tag
      && Int.equal b1.tag b2.tag
  | BoolBin (op1, a1, b1), BoolBin (op2, a2, b2) ->
      Binop.equal_boolean op1 op2
      && Int.equal a1.tag a2.tag
      && Int.equal b1.tag b2.tag
  | Eq (a1, b1), Eq (a2, b2) ->
      Int.equal a1.tag a2.tag && Int.equal b1.tag b2.tag
  | Nop (op1, l1), Nop (op2, l2) -> Nop.equal op1 op2 && list_tag_equal l1 l2
  | Ite (c1, t1, e1), Ite (c2, t2, e2) ->
      Int.equal c1.tag c2.tag
      && Int.equal t1.tag t2.tag
      && Int.equal e1.tag e2.tag
  | Exists (vs1, b1), Exists (vs2, b2) ->
      Int.equal b1.tag b2.tag && List.equal equal_binder vs1 vs2
  | _ -> false

(* Node equality that yields a type-equality witness: the types must be equal
   (which gives [a = b]) and the kinds structurally equal. This witness is what
   lets the hash-cons table return a node at the queried index, magic-free. *)
let equal_node : type a b. a t_node -> b t_node -> (a, b) eq option =
 fun n1 n2 ->
  match eq_ty n1.ty n2.ty with
  | None -> None
  | Some Equal -> if kind_eq n1.kind n2.kind then Some Equal else None

let[@inline] combine h x = (h * 65599) + x

(* Avalanche finaliser (32-bit lowbias mix, fits in OCaml's 63-bit int). The
   bucket index uses the low bits of the hash, and the [combine] accumulator
   alone mixes them poorly for the dense, sequential children tags we produce —
   so without this, lookups in a large table degrade to long chains. *)
let[@inline] final h =
  let h = h lxor (h lsr 16) in
  let h = h * 0x45d9f3b in
  let h = h lxor (h lsr 16) in
  let h = h * 0x45d9f3b in
  h lxor (h lsr 16)

let rec hash_ty : type a. a ty -> int = function
  | TBool -> 1
  | TFloat fp -> combine 2 (Hashtbl.hash fp)
  | TLoc n -> combine 3 n
  | TPointer n -> combine 4 n
  | TSeq t -> combine 5 (hash_ty t)
  | TBitVector n -> combine 6 n

let hash_node : type a. a t_node -> int =
 fun { kind; ty } ->
  let hty = hash_ty ty in
  let seq h l = List.fold_left (fun h sv -> combine h sv.tag) h l in
  final
  @@
  match kind with
  | Var v -> combine (combine 1 (Var.hash v)) hty
  | Bool b -> combine (combine 2 (Bool.to_int b)) hty
  | Float f -> combine (combine 3 (Hashtbl.hash f)) hty
  | BitVec z -> combine (combine 4 (Z.hash z)) hty
  | LocLit z -> combine (combine 5 (Z.hash z)) hty
  | Ptr (l, r) -> combine (combine (combine 6 l.tag) r.tag) hty
  | Seq l -> combine (seq 7 l) hty
  | UnBv (op, v) -> combine (combine (combine 8 (Hashtbl.hash op)) v.tag) hty
  | UnBool (op, v) -> combine (combine (combine 9 (Hashtbl.hash op)) v.tag) hty
  | UnFloat (op, v) ->
      combine (combine (combine 10 (Hashtbl.hash op)) v.tag) hty
  | UnPtr (op, v) -> combine (combine (combine 11 (Hashtbl.hash op)) v.tag) hty
  | BvArith (op, l, r) ->
      combine (combine (combine (combine 12 (Hashtbl.hash op)) l.tag) r.tag) hty
  | BvCmp (op, l, r) ->
      combine (combine (combine (combine 13 (Hashtbl.hash op)) l.tag) r.tag) hty
  | FArith (op, l, r) ->
      combine (combine (combine (combine 14 (Hashtbl.hash op)) l.tag) r.tag) hty
  | FCmp (op, l, r) ->
      combine (combine (combine (combine 15 (Hashtbl.hash op)) l.tag) r.tag) hty
  | BoolBin (op, l, r) ->
      combine (combine (combine (combine 16 (Hashtbl.hash op)) l.tag) r.tag) hty
  | Eq (l, r) -> combine (combine (combine 17 l.tag) r.tag) hty
  | Nop (op, l) -> combine (seq (combine 18 (Hashtbl.hash op)) l) hty
  | Ite (c, t, e) ->
      combine (combine (combine (combine 19 c.tag) t.tag) e.tag) hty
  | Exists (vs, sv) ->
      let h =
        List.fold_left
          (fun h (v, PackedTy ty) ->
            combine (combine h (Var.hash v)) (hash_ty ty))
          20 vs
      in
      combine (combine h sv.tag) hty

(* A (strong) hash table of nodes, keyed by structure. We cannot use a weak/
   ephemeron table à la the [Hc] library: that keeps an entry's value alive only
   while its key (the node) is reachable, but our heterogeneous (GADT-indexed)
   nodes must be wrapped in a [packed] box to share one key type, and such a
   wrapper is not retained by anything once [hashcons] returns — so under GC it
   would be collected while the node is still live, silently breaking
   deduplication (and, with it, structural simplifications). The table is
   per-process and bounded by the run's distinct terms. *)
module Node_table = Hashtbl.Make (struct
  type t = packed

  (* Pure-bool structural equality: avoids boxing a [(a, b) eq option] witness
     on every bucket comparison. [equal_ty] settles the index, [kind_eq] (which
     is heterogeneous) the structure. *)
  let equal (Packed a) (Packed b) =
    equal_ty a.node.ty b.node.ty && kind_eq a.node.kind b.node.kind

  let hash (Packed a) = hash_node a.node
end)

let node_table : packed Node_table.t = Node_table.create 1023
let node_counter = ref 0

let hashcons : type a. a t_node -> a t_node hash_consed =
 fun node ->
  (* Build the candidate node (and its [packed] box) up front so the same box
     serves as both the lookup key and, on a miss, the stored entry — avoiding a
     throwaway probe allocation. On a hit we hand back the existing node and
     undo the counter bump, so tags are assigned exactly as if we only counted
     on misses. *)
  incr node_counter;
  let hc = { node; tag = !node_counter } in
  let key = Packed hc in
  match Node_table.find_opt node_table key with
  | Some (Packed found) -> (
      decr node_counter;
      match equal_node node found.node with
      | Some Equal -> found
      | None -> assert false)
  | None ->
      Node_table.replace node_table key key;
      hc

let ( <| ) : type a. a t_kind -> a ty -> a t =
 fun kind ty -> hashcons { kind; ty }

let mk_var v ty = Var v <| ty

(** We put commutative binary operators in some sort of normal form where the
    element with the smallest id is on the LHS, to increase cache hits. One
    helper per (commutative) operand group. *)
let mk_commut_arith op l r =
  if l.tag <= r.tag then BvArith (op, l, r) else BvArith (op, r, l)

let mk_commut_cmp op l r =
  if l.tag <= r.tag then BvCmp (op, l, r) else BvCmp (op, r, l)

let mk_commut_bool op l r =
  if l.tag <= r.tag then BoolBin (op, l, r) else BoolBin (op, r, l)

let mk_commut_farith op l r =
  if l.tag <= r.tag then FArith (op, l, r) else FArith (op, r, l)

let mk_commut_fcmp op l r =
  if l.tag <= r.tag then FCmp (op, l, r) else FCmp (op, r, l)

let mk_commut_eq : type a. a t -> a t -> sbool t_kind =
 fun l r -> if l.tag <= r.tag then Eq (l, r) else Eq (r, l)

(** We put commutative n-ary operators in some sort of normal form where
    elements are sorted by ud, to increase cache hits. If [idem] is true, will
    also remove duplicates. *)
let mk_commut_nop : type a. ?idem:bool -> Nop.t -> a t list -> sbool t_kind =
 fun ?(idem = true) op vs ->
  let sort = if idem then List.sort_uniq else List.sort in
  let vs = sort (fun l r -> Int.compare l.tag r.tag) vs in
  Nop (op, vs)

(** {2 Operator declarations} *)

module type Bool = sig
  val v_true : sbool t
  val v_false : sbool t
  val to_bool : 'a t -> bool option
  val of_bool : bool -> sbool t
  val and_ : sbool t -> sbool t -> sbool t
  val and_lazy : sbool t -> (unit -> sbool t) -> sbool t
  val or_ : sbool t -> sbool t -> sbool t
  val or_lazy : sbool t -> (unit -> sbool t) -> sbool t
  val conj : sbool t list -> sbool t
  val not : sbool t -> sbool t
  val split_ands : sbool t -> sbool t Iter.t
  val distinct : 'a t list -> sbool t
  val distinct_seq : 'a t Seq.t -> sbool t
  val ite : sbool t -> 'a t -> 'a t -> 'a t

  (** Do not use this directly when instantiating your own binders, use
      [exists_n] or its variants *)
  val mk_exists : (Var.t * packed_ty) list -> sbool t -> sbool t

  val exists_n :
    not_in:'a t -> packed_ty list -> (packed list -> sbool t) -> sbool t

  val exists_1 : not_in:'a t -> 'b ty -> ('b t -> sbool t) -> sbool t

  val exists_2 :
    not_in:'a t -> 'b ty -> 'c ty -> ('b t -> 'c t -> sbool t) -> sbool t

  val exists_3 :
    not_in:'a t ->
    'b ty ->
    'c ty ->
    'd ty ->
    ('b t -> 'c t -> 'd t -> sbool t) ->
    sbool t

  val sem_eq : 'a t -> 'a t -> sbool t
  val sem_eq_untyped : 'a t -> 'b t -> sbool t
end

module type BitVec = sig
  (* constructor *)
  val mk : int -> Z.t -> sbv t
  val mk_masked : int -> Z.t -> sbv t
  val mki : int -> int -> sbv t
  val zero : int -> sbv t
  val one : int -> sbv t
  val bv_to_z : bool -> int -> Z.t -> Z.t
  val to_z : 'a t -> Z.t option
  val msb_of : sbv t -> int

  (* arithmetic *)
  val add : ?checked:checked -> sbv t -> sbv t -> sbv t
  val sub : ?checked:checked -> sbv t -> sbv t -> sbv t
  val mul : ?checked:checked -> sbv t -> sbv t -> sbv t
  val div : signed:bool -> sbv t -> sbv t -> sbv t
  val rem : signed:bool -> sbv t -> sbv t -> sbv t
  val mod_ : sbv t -> sbv t -> sbv t
  val neg : ?checked:bool -> sbv t -> sbv t

  (* overflow checks *)
  val add_overflows : signed:bool -> sbv t -> sbv t -> sbool t
  val sub_overflows : signed:bool -> sbv t -> sbv t -> sbool t
  val mul_overflows : signed:bool -> sbv t -> sbv t -> sbool t
  val neg_overflows : sbv t -> sbool t

  (* inequalities *)
  val lt : signed:bool -> sbv t -> sbv t -> sbool t
  val leq : signed:bool -> sbv t -> sbv t -> sbool t
  val gt : signed:bool -> sbv t -> sbv t -> sbool t
  val geq : signed:bool -> sbv t -> sbv t -> sbool t

  (* bitvec manipulation *)
  val concat : sbv t -> sbv t -> sbv t
  val extend : signed:bool -> int -> sbv t -> sbv t
  val extract : int -> int -> sbv t -> sbv t

  (* bitwise operations *)
  val and_ : sbv t -> sbv t -> sbv t
  val or_ : sbv t -> sbv t -> sbv t
  val xor : sbv t -> sbv t -> sbv t
  val shl : sbv t -> sbv t -> sbv t
  val lshr : sbv t -> sbv t -> sbv t
  val ashr : sbv t -> sbv t -> sbv t
  val not : sbv t -> sbv t

  (* bool-bv conversions *)
  val of_bool : int -> sbool t -> sbv t
  val to_bool : sbv t -> sbool t
  val not_bool : sbv t -> sbv t

  (* float-bv conversions *)
  val of_float :
    rounding:RoundingMode.t -> signed:bool -> size:int -> sfloat t -> sbv t

  val to_float :
    rounding:RoundingMode.t ->
    signed:bool ->
    fp:FloatPrecision.t ->
    sbv t ->
    sfloat t

  val to_float_raw : sbv t -> sfloat t
end

module type Float = sig
  (* constructors *)
  val mk : FloatPrecision.t -> string -> sfloat t
  val f16 : float -> sfloat t
  val f32 : float -> sfloat t
  val f64 : float -> sfloat t
  val f128 : float -> sfloat t
  val like : sfloat t -> float -> sfloat t
  val fp_of : sfloat t -> FloatPrecision.t

  (* arithmetic *)
  val add : sfloat t -> sfloat t -> sfloat t
  val sub : sfloat t -> sfloat t -> sfloat t
  val mul : sfloat t -> sfloat t -> sfloat t
  val div : sfloat t -> sfloat t -> sfloat t
  val rem : sfloat t -> sfloat t -> sfloat t
  val abs : sfloat t -> sfloat t
  val neg : sfloat t -> sfloat t
  val round : RoundingMode.t -> sfloat t -> sfloat t

  (* comparisons *)
  val eq : sfloat t -> sfloat t -> sbool t
  val lt : sfloat t -> sfloat t -> sbool t
  val leq : sfloat t -> sfloat t -> sbool t
  val gt : sfloat t -> sfloat t -> sbool t
  val geq : sfloat t -> sfloat t -> sbool t

  (* classification *)
  val is_floatclass : FloatClass.t -> sfloat t -> sbool t
  val is_normal : sfloat t -> sbool t
  val is_subnormal : sfloat t -> sbool t
  val is_zero : sfloat t -> sbool t
  val is_infinite : sfloat t -> sbool t
  val is_nan : sfloat t -> sbool t
end

(** {2 Booleans} *)

module rec Bool : Bool = struct
  let v_true = Bool true <| TBool
  let v_false = Bool false <| TBool

  let[@inline] to_bool t =
    if equal t v_true then Some true
    else if equal t v_false then Some false
    else None

  let of_bool b =
    (* avoid re-alloc and re-hashconsing *)
    if b then v_true else v_false

  (* Recognize an upper-bound constraint [a < c] / [a <= c], returning the
     bounded value, the signedness, the original node, and the inclusive upper
     bound as an integer in that signedness. [as_lower_bound] is symmetric for
     [c < a] / [c <= a] (returning an inclusive lower bound). *)
  let as_upper_bound v =
    match v.node.kind with
    | BvCmp (Lt s, a, { node = { kind = BitVec c; _ }; _ }) ->
        let a = (a : sbv t) in
        Some (a, s, v, Z.pred (BitVec.bv_to_z s (size_of a.node.ty) c))
    | BvCmp (Leq s, a, { node = { kind = BitVec c; _ }; _ }) ->
        let a = (a : sbv t) in
        Some (a, s, v, BitVec.bv_to_z s (size_of a.node.ty) c)
    | _ -> None

  let as_lower_bound v =
    match v.node.kind with
    | BvCmp (Lt s, { node = { kind = BitVec c; _ }; _ }, a) ->
        let a = (a : sbv t) in
        Some (a, s, v, Z.succ (BitVec.bv_to_z s (size_of a.node.ty) c))
    | BvCmp (Leq s, { node = { kind = BitVec c; _ }; _ }, a) ->
        let a = (a : sbv t) in
        Some (a, s, v, BitVec.bv_to_z s (size_of a.node.ty) c)
    | _ -> None

  (* For two bounds on the same value (same signedness), [&&] keeps the tighter
     and [||] keeps the looser; either way the kept node already implies (resp.
     is implied by) the other, so we can drop it. Callers only invoke these once
     both operands have matched as upper (resp. lower) bounds, so the [None]
     cases are unreachable. *)
  let combine_upper_bounds ~keep_tighter v1 v2 =
    match (as_upper_bound v1, as_upper_bound v2) with
    | Some (_, _, _, u1), Some (_, _, _, u2) ->
        if Stdlib.( = ) (Z.leq u1 u2) keep_tighter then v1 else v2
    | _ -> assert false

  let combine_lower_bounds ~keep_tighter v1 v2 =
    match (as_lower_bound v1, as_lower_bound v2) with
    | Some (_, _, _, l1), Some (_, _, _, l2) ->
        if Stdlib.( = ) (Z.geq l1 l2) keep_tighter then v1 else v2
    | _ -> assert false

  (* Whether [boundv] (a bound on some [a]) is implied by [eqv] (an equality [a
     == k] with [k] constant), i.e. [k] satisfies the bound. *)
  let bound_implied_by_eq (boundv : sbool t) (eqv : sbool t) =
    (* [check a k] : the equality is [a == k]; does [boundv] (a bound on [a])
       allow [k]? [a] is kept at its (existential) operand type. *)
    let check : type x. x t -> Z.t -> bool =
     fun a k ->
      let k_in s = BitVec.bv_to_z s (size_of a.node.ty) k in
      (match as_upper_bound boundv with
        | Some (ba, s, _, u) -> equal ba a && Z.leq (k_in s) u
        | None -> false)
      ||
      match as_lower_bound boundv with
      | Some (ba, s, _, l) -> equal ba a && Z.geq (k_in s) l
      | None -> false
    in
    match eqv.node.kind with
    | Eq (a, { node = { kind = BitVec k; _ }; _ }) -> check a k
    | Eq ({ node = { kind = BitVec k; _ }; _ }, a) -> check a k
    | _ -> false

  let rec and_ v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v1 v2 -> v1
    | Bool false, _ | _, Bool false -> v_false
    | Bool true, _ -> v2
    | _, Bool true -> v1
    (* p && !p <=> false *)
    | _, UnBool (Not, p) when equal v1 p -> v_false
    | UnBool (Not, p), _ when equal v2 p -> v_false
    | Eq (l1, r1), Eq (l2, r2)
      when (equal l1 l2 && sure_neq r1 r2)
           || (equal l1 r2 && sure_neq r1 l2)
           || (equal r1 l2 && sure_neq l1 r2)
           || (equal r1 r2 && sure_neq l1 l2) ->
        v_false
    | ( Eq (bv1, { node = { kind = UnBv (BvExtract (s1, e1), x); _ }; _ }),
        Eq (bv2, { node = { kind = UnBv (BvExtract (s2, e2), y); _ }; _ }) )
      when equal x y && (e1 + 1 = s2 || e2 + 1 = s1) ->
        let bv, xy =
          if e1 + 1 = s2 then (BitVec.concat bv2 bv1, BitVec.extract s1 e2 x)
          else (BitVec.concat bv1 bv2, BitVec.extract s2 e1 x)
        in
        sem_eq bv xy
    (* two upper (resp. lower) bounds on the same value (e.g. [a < c1] && [a <=
       c2]) keep the tighter one *)
    | ( BvCmp ((Lt s1 | Leq s1), a1, { node = { kind = BitVec _; _ }; _ }),
        BvCmp ((Lt s2 | Leq s2), a2, { node = { kind = BitVec _; _ }; _ }) )
      when s1 = s2 && equal a1 a2 ->
        combine_upper_bounds ~keep_tighter:true v1 v2
    | ( BvCmp ((Lt s1 | Leq s1), { node = { kind = BitVec _; _ }; _ }, a1),
        BvCmp ((Lt s2 | Leq s2), { node = { kind = BitVec _; _ }; _ }, a2) )
      when s1 = s2 && equal a1 a2 ->
        combine_lower_bounds ~keep_tighter:true v1 v2
    | _ -> mk_commut_bool And v1 v2 <| TBool

  and or_ v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v1 v2 -> v1
    | Bool true, _ | _, Bool true -> v_true
    | Bool false, _ -> v2
    | _, Bool false -> v1
    (* p || !p <=> true *)
    | _, UnBool (Not, p) when equal v1 p -> v_true
    | UnBool (Not, p), _ when equal v2 p -> v_true
    | BvCmp (Lt s1, l1, r1), BvCmp (Lt s2, l2, r2)
      when s1 = s2 && equal l1 r2 && equal r1 l2 ->
        not (sem_eq l1 r1)
    | BvCmp (Lt s1, l1, r1), BvCmp (Leq s2, l2, r2)
    | BvCmp (Leq s1, l1, r1), BvCmp (Lt s2, l2, r2)
      when s1 = s2 && equal l1 r2 && equal r1 l2 ->
        v_true
    | BoolBin (Or, a, b), _ when equal a v2 || equal b v2 -> v1
    | _, BoolBin (Or, a, b) when equal v1 a || equal v1 b -> v2
    (* a bound absorbs an equality it already allows, e.g. [a < c] || [a ==
       0] *)
    | BvCmp ((Lt _ | Leq _), _, _), Eq (_, _) when bound_implied_by_eq v1 v2 ->
        v1
    | Eq (_, _), BvCmp ((Lt _ | Leq _), _, _) when bound_implied_by_eq v2 v1 ->
        v2
    (* two upper (resp. lower) bounds on the same value keep the looser one *)
    | ( BvCmp ((Lt s1 | Leq s1), a1, { node = { kind = BitVec _; _ }; _ }),
        BvCmp ((Lt s2 | Leq s2), a2, { node = { kind = BitVec _; _ }; _ }) )
      when s1 = s2 && equal a1 a2 ->
        combine_upper_bounds ~keep_tighter:false v1 v2
    | ( BvCmp ((Lt s1 | Leq s1), { node = { kind = BitVec _; _ }; _ }, a1),
        BvCmp ((Lt s2 | Leq s2), { node = { kind = BitVec _; _ }; _ }, a2) )
      when s1 = s2 && equal a1 a2 ->
        combine_lower_bounds ~keep_tighter:false v1 v2
    | _ -> mk_commut_bool Or v1 v2 <| TBool

  and not sv =
    if equal sv v_true then v_false
    else if equal sv v_false then v_true
    else
      match sv.node.kind with
      | UnBool (Not, sv) -> sv
      | BvCmp (Lt signed, v1, v2) -> BitVec.leq ~signed v2 v1
      | BvCmp (Leq signed, v1, v2) -> BitVec.lt ~signed v2 v1
      | BoolBin (Or, v1, v2) -> and_ (not v1) (not v2)
      | BoolBin (And, v1, v2) -> or_ (not v1) (not v2)
      | Eq (a, b) -> (
          (* not (k == x) where [k] is a 1-bit constant is [(1-k) == x] *)
          let one_bit : type x. x t -> x t -> sbool t option =
           fun p q ->
            match p.node with
            | { kind = BitVec bv; ty = TBitVector 1 } ->
                Some (sem_eq (BitVec.mk 1 Z.(one - bv)) q)
            | _ -> None
          in
          match one_bit a b with
          | Some r -> r
          | None -> (
              match one_bit b a with
              | Some r -> r
              | None -> UnBool (Not, sv) <| TBool))
      | Nop (Distinct, [ l; r ]) -> sem_eq l r
      | _ -> UnBool (Not, sv) <| TBool

  and ite : type a. sbool t -> a t -> a t -> a t =
   fun guard if_ else_ ->
    match (guard.node.kind, if_.node.kind, else_.node.kind) with
    | Bool true, _, _ -> if_
    | Bool false, _, _ -> else_
    | _, Bool true, Bool false -> guard
    | _, Bool false, Bool true -> not guard
    | _, Bool false, _ -> and_ (not guard) else_
    | _, Bool true, _ -> or_ guard else_
    | _, _, Bool false -> and_ guard if_
    | _, _, Bool true -> or_ (not guard) if_
    | _, BitVec o, BitVec z when Z.(equal o one) && Z.equal z Z.zero ->
        (* [if_] matched [BitVec], so [a = sbv] here. *)
        BitVec.of_bool (size_of if_.node.ty) guard
    | _ when equal if_ else_ -> if_
    | _ -> Ite (guard, if_, else_) <| if_.node.ty

  and sem_eq : type a. a t -> a t -> sbool t =
   fun v1 v2 ->
    match[@warning "-ambiguous-var-in-pattern-guard"]
      (v1.node.kind, v2.node.kind)
    with
    | _ when equal v1 v2 -> v_true
    | Bool b1, Bool b2 -> of_bool (b1 = b2)
    | Ptr (l1, o1), Ptr (l2, o2) -> and_ (sem_eq l1 l2) (sem_eq o1 o2)
    | BitVec b1, BitVec b2 -> of_bool (Z.equal b1 b2)
    | LocLit b1, LocLit b2 -> of_bool (Z.equal b1 b2)
    (* Arithmetics *)
    | BitVec _, UnBv (Neg _, v2) -> sem_eq (BitVec.neg v1) v2
    | UnBv (Neg _, v1), BitVec _ -> sem_eq v1 (BitVec.neg v2)
    | BitVec _, UnBv (BvNot, v2) -> sem_eq (BitVec.not v1) v2
    | UnBv (BvNot, v1), BitVec _ -> sem_eq v1 (BitVec.not v2)
    | BitVec _, BvArith (Add _, ({ node = { kind = BitVec _; _ }; _ } as l), r)
      ->
        sem_eq (BitVec.sub v1 l) r
    | BitVec _, BvArith (Add _, r, ({ node = { kind = BitVec _; _ }; _ } as l))
      ->
        sem_eq (BitVec.sub v1 l) r
    | BvArith (Add _, ({ node = { kind = BitVec _; _ }; _ } as l), r), BitVec _
      ->
        sem_eq (BitVec.sub v2 l) r
    | BvArith (Add _, r, ({ node = { kind = BitVec _; _ }; _ } as l)), BitVec _
      ->
        sem_eq (BitVec.sub v2 l) r
    | BitVec _, BvArith (Sub _, l, ({ node = { kind = BitVec _; _ }; _ } as r))
      ->
        sem_eq (BitVec.add v1 r) l
    | BitVec _, BvArith (Sub _, ({ node = { kind = BitVec _; _ }; _ } as l), r)
      ->
        sem_eq (BitVec.sub l v1) r
    | BvArith (Sub _, l, ({ node = { kind = BitVec _; _ }; _ } as r)), BitVec _
      ->
        sem_eq (BitVec.add v2 r) l
    | BvArith (Sub _, ({ node = { kind = BitVec _; _ }; _ } as l), r), BitVec _
      ->
        sem_eq (BitVec.sub l v2) r
    | _, BvArith (Add _, v2, { node = { kind = BitVec bv; _ }; _ })
      when equal v1 v2 ->
        of_bool Z.(equal bv zero)
    | _, BvArith (Add _, { node = { kind = BitVec bv; _ }; _ }, v2)
      when equal v1 v2 ->
        of_bool Z.(equal bv zero)
    | BvArith (Add _, { node = { kind = BitVec bv; _ }; _ }, v1), _
      when equal v1 v2 ->
        of_bool Z.(equal bv zero)
    | BvArith (Add _, v1, { node = { kind = BitVec bv; _ }; _ }), _
      when equal v1 v2 ->
        of_bool Z.(equal bv zero)
    | ( ( BvArith (Add _, ({ node = { kind = BitVec bv_l; _ }; _ } as l), y)
        | BvArith (Add _, y, ({ node = { kind = BitVec bv_l; _ }; _ } as l)) ),
        ( BvArith (Add _, ({ node = { kind = BitVec bv_r; _ }; _ } as r), x)
        | BvArith (Add _, x, ({ node = { kind = BitVec bv_r; _ }; _ } as r)) ) )
      ->
        (* y + l == x + r <=> y == x + (r - l) <=> y + (l - r) == x *)
        (* we pick the option that will make a positive constant (superstition) *)
        (* This rewrite holds modulo 2^n regardless of overflow, but the
           rebuilt sum carries no overflow guarantee, so it stays unchecked. *)
        if Z.geq bv_l bv_r then
          sem_eq x
            (BitVec.add ~checked:unchecked y
               (BitVec.sub ~checked:unchecked l r))
        else
          sem_eq y
            (BitVec.add ~checked:unchecked x
               (BitVec.sub ~checked:unchecked r l))
    | ( BitVec n,
        ( BvArith
            (Mul { unsigned = true; _ }, { node = { kind = BitVec m; _ }; _ }, x)
        | BvArith
            (Mul { unsigned = true; _ }, x, { node = { kind = BitVec m; _ }; _ })
          ) )
    | ( ( BvArith
            (Mul { unsigned = true; _ }, { node = { kind = BitVec m; _ }; _ }, x)
        | BvArith
            (Mul { unsigned = true; _ }, x, { node = { kind = BitVec m; _ }; _ })
          ),
        BitVec n ) ->
        if Z.(equal m zero) then of_bool (Z.equal n Z.zero)
        else if Z.(equal n zero) then sem_eq x (BitVec.zero (size_of x.node.ty))
        else if Z.(divisible n m) then
          sem_eq x (BitVec.mk (size_of x.node.ty) Z.(n / m))
        else v_false
    (* Cancelling a common factor [a] from [a*b == a*d] is only sound when [a]
       is odd (invertible modulo 2^n), or when both multiplications are
       overflow-checked (so they behave like exact integer arithmetic). *)
    | ( BvArith (Mul ck1, { node = { kind = BitVec a; _ }; _ }, b),
        BvArith (Mul ck2, { node = { kind = BitVec c; _ }; _ }, d) )
      when Z.(equal a c) && (Z.is_odd a || is_checked (checked_meet ck1 ck2)) ->
        sem_eq b d
    | ( BvArith (Mul ck1, b, { node = { kind = BitVec a; _ }; _ }),
        BvArith (Mul ck2, d, { node = { kind = BitVec c; _ }; _ }) )
      when Z.(equal a c) && (Z.is_odd a || is_checked (checked_meet ck1 ck2)) ->
        sem_eq b d
    | ( BvArith (Mul ck1, { node = { kind = BitVec a; _ }; _ }, b),
        BvArith (Mul ck2, d, { node = { kind = BitVec c; _ }; _ }) )
      when Z.(equal a c) && (Z.is_odd a || is_checked (checked_meet ck1 ck2)) ->
        sem_eq b d
    | ( BvArith (Mul ck1, b, { node = { kind = BitVec a; _ }; _ }),
        BvArith (Mul ck2, { node = { kind = BitVec c; _ }; _ }, d) )
      when Z.(equal a c) && (Z.is_odd a || is_checked (checked_meet ck1 ck2)) ->
        sem_eq b d (* Bitvectors *)
    (* 0 == L | R ==> 0 == L && 0 == R, splitting is better for the PC *)
    | (BitVec z, BvArith (BitOr, l, r) | BvArith (BitOr, l, r), BitVec z)
      when Z.equal z Z.zero ->
        let z = BitVec.zero (size_of v1.node.ty) in
        and_ (sem_eq l z) (sem_eq r z)
    (* for N == (M & X), if N has bits set that aren't in M, then it must be
       false, since the mask would unset them *)
    | BitVec n, BvArith (BitAnd, { node = { kind = BitVec mask; _ }; _ }, _)
    | BitVec n, BvArith (BitAnd, _, { node = { kind = BitVec mask; _ }; _ })
    | BvArith (BitAnd, { node = { kind = BitVec mask; _ }; _ }, _), BitVec n
    | BvArith (BitAnd, _, { node = { kind = BitVec mask; _ }; _ }), BitVec n
      when let sz = size_of v1.node.ty in
           let full_mask = Z.(pred (one lsl sz)) in
           let mask_not = Z.(lognot mask land full_mask) in
           Stdlib.not Z.(equal (n land mask_not) Z.zero) ->
        v_false
    | BitVec _, BvArith (BvConcat, l, r) -> sem_eq_concat_const v1 l r
    | BvArith (BvConcat, l, r), BitVec _ -> sem_eq_concat_const v2 l r
    | UnBv (BvExtend (false, by), bv), BitVec z
    | BitVec z, UnBv (BvExtend (false, by), bv) ->
        (* unsigned-extend(bv) == z: bv : sbv (BvExtend's operand is a bv) *)
        let extend_eq : sbv t -> int -> Z.t -> sbool t =
         fun bv by z ->
          let size_bv = size_of bv.node.ty in
          (* if any of the bits of z are set in [size_bv, size_bv+by), this
             cannot be true since they're extended to 0 *)
          let mask = Z.(pred (one lsl by) lsl size_bv) in
          if Stdlib.not Z.(equal (z land mask) Z.zero) then v_false
          else sem_eq bv (BitVec.mk size_bv z)
        in
        extend_eq bv by z
    (* ite(b, A::B, C::D) == l :: r <=>
     * ite(b, A, C) == l && ite(b, B, D) == r *)
    | Ite (b, t, e), BvArith (BvConcat, l, r) -> sem_eq_ite_concat b t e l r
    | BvArith (BvConcat, l, r), Ite (b, t, e) -> sem_eq_ite_concat b t e l r
    | BvArith (BvConcat, l1, r1), BvArith (BvConcat, l2, r2)
      when size_of l1.node.ty = size_of l2.node.ty ->
        and_ (sem_eq l1 l2) (sem_eq r1 r2)
    (* BvOfBool and If-then-elses *)
    | Ite (b, l, t), BitVec _ -> ite b (sem_eq l v2) (sem_eq t v2)
    | Ite (b, l, t), Bool _ -> ite b (sem_eq l v2) (sem_eq t v2)
    | BitVec _, Ite (b, l, t) -> ite b (sem_eq v1 l) (sem_eq v1 t)
    | Bool _, Ite (b, l, t) -> ite b (sem_eq v1 l) (sem_eq v1 t)
    | Bool false, _ -> not v2
    | _, Bool false -> not v1
    | Bool true, _ -> v2
    | _, Bool true -> v1
    | UnBool (BvOfBool _, b), UnBool (BvOfBool _, c) ->
        sem_eq (b : sbool t) (c : sbool t)
    | UnBool (Not, b), UnBool (Not, c) -> sem_eq (b : sbool t) (c : sbool t)
    | UnBool (BvOfBool _, b), BitVec z | BitVec z, UnBool (BvOfBool _, b) ->
        let b = (b : sbool t) in
        if Z.equal z Z.one then b
        else if Z.equal z Z.zero then not b
        else v_false
    | _ -> (
        match (v1.node.ty, v2.node.ty) with
        (* special case: for BVs, check if we can infer the most significant set
           bits and extract *)
        | TBitVector _, TBitVector _ ->
            let current_size = size_of v1.node.ty in
            let msb = max (BitVec.msb_of v1) (BitVec.msb_of v2) in
            if 0 <= msb && msb < current_size - 1 then
              let v1' = BitVec.extract 0 msb v1 in
              let v2' = BitVec.extract 0 msb v2 in
              sem_eq v1' v2'
            else (* regular sem_eq *)
              mk_commut_eq v1 v2 <| TBool
        | _ -> mk_commut_eq v1 v2 <| TBool)

  (* [z == l ++ r] for a constant [z], split per concat-half. *)
  and sem_eq_concat_const (z : sbv t) (l : sbv t) (r : sbv t) : sbool t =
    let size_r = size_of r.node.ty in
    let size_l = size_of l.node.ty in
    let z_r = BitVec.extract 0 (size_r - 1) z in
    let z_l = BitVec.extract size_r (size_r + size_l - 1) z in
    and_ (sem_eq l z_l) (sem_eq r z_r)

  (* [ite(b, t, e) == l ++ r] split per concat-half; [t]/[e] are bitvectors. *)
  and sem_eq_ite_concat (b : sbool t) (t : sbv t) (e : sbv t) (l : sbv t)
      (r : sbv t) : sbool t =
    let size_r = size_of r.node.ty in
    let size_l = size_of l.node.ty in
    let t_r = BitVec.extract 0 (size_r - 1) t in
    let t_l = BitVec.extract size_r (size_r + size_l - 1) t in
    let e_r = BitVec.extract 0 (size_r - 1) e in
    let e_l = BitVec.extract size_r (size_r + size_l - 1) e in
    and_ (sem_eq (ite b t_l e_l) l) (sem_eq (ite b t_r e_r) r)

  (* TODO: merge binders if the body is an exists *)
  let mk_exists binders body =
    let body_vars = Var.Hashset.of_iter (iter_vars body |> Iter.map fst) in
    let binders =
      List.filter (fun (v, _) -> Var.Hashset.mem body_vars v) binders
    in
    match binders with [] -> body | _ -> Exists (binders, body) <| TBool

  (* [base_var ~not_in] is a fresh variable index, chosen high to mark these as
     existentials and avoid clashing with variables of [not_in]. *)

  (** * [exists_n ~not_in tys mk] creates an existential with [length tys]
      variables of types [tys], that are not in [not_in], and with body created
      by [mk : t list -> t] which takes the created variables as input in the
      same order as [tys]. *)
  let base_var ~not_in =
    let max = ref 0 in
    iter_vars not_in (fun (v, _) -> max := Int.max !max (Var.to_int v));
    (* FIXME: Ideally, the not_in parameter would not be necessary. What we
       should be doing is creating variables with identifiers Int.max, Int.max -
       1, etc., then create the value, and then substitute those variables with
       ones that are not in the rest of the value. Unfortunately, this requires
       calling `Eval.eval`, but that creates a cycle... *)
    !max + 10_000

  let exists_n ~not_in tys mk =
    let base = base_var ~not_in in
    let binders =
      List.mapi
        (fun i (PackedTy ty) -> (Var.of_int (base + i), PackedTy ty))
        tys
    in
    let binders_vs =
      List.map (fun (v, PackedTy ty) -> Packed (mk_var v ty)) binders
    in
    let body = mk binders_vs in
    mk_exists binders body

  let exists_1 ~not_in ty mk =
    let v = Var.of_int (base_var ~not_in) in
    mk_exists [ (v, PackedTy ty) ] (mk (mk_var v ty))

  let exists_2 ~not_in ty1 ty2 mk =
    let base = base_var ~not_in in
    let v1 = Var.of_int base and v2 = Var.of_int (base + 1) in
    mk_exists
      [ (v1, PackedTy ty1); (v2, PackedTy ty2) ]
      (mk (mk_var v1 ty1) (mk_var v2 ty2))

  let exists_3 ~not_in ty1 ty2 ty3 mk =
    let base = base_var ~not_in in
    let v1 = Var.of_int base
    and v2 = Var.of_int (base + 1)
    and v3 = Var.of_int (base + 2) in
    mk_exists
      [ (v1, PackedTy ty1); (v2, PackedTy ty2); (v3, PackedTy ty3) ]
      (mk (mk_var v1 ty1) (mk_var v2 ty2) (mk_var v3 ty3))

  let sem_eq_untyped : type a b. a t -> b t -> sbool t =
   fun v1 v2 ->
    match eq_ty v1.node.ty v2.node.ty with
    | Some Equal -> sem_eq v1 v2
    | None -> v_false

  let and_lazy v1 v2 =
    match v1.node.kind with Bool false -> v_false | _ -> and_ v1 (v2 ())

  let or_lazy v1 v2 =
    match v1.node.kind with Bool true -> v_true | _ -> or_ v1 (v2 ())

  let conj l = List.fold_left and_ v_true l

  let rec split_ands (sv : sbool t) (f : sbool t -> unit) : unit =
    match sv.node.kind with
    | BoolBin (And, s1, s2) ->
        split_ands s1 f;
        split_ands s2 f
    | _ -> f sv

  let distinct_raw s ?l () =
    (* [Distinct l] when l is empty or of size 1 is always true *)
    match Seq.compare_length_with s 2 with
    | -1 -> v_true
    | _ -> (
        let cross_product = Seq.self_cross_product s in
        let rec aux seq =
          match seq () with
          | Seq.Nil -> Some true
          | Seq.Cons ((a, b), rest) ->
              if equal a b then Some false
              else if sure_neq a b then aux rest
              else None
        in
        let res = aux cross_product in
        match (res, l) with
        | Some true, _ -> v_true
        | Some false, _ -> v_false
        | None, Some l -> mk_commut_nop Distinct l <| TBool
        | None, None -> mk_commut_nop Distinct (List.of_seq s) <| TBool)

  let distinct_seq s = distinct_raw s ()
  let distinct l = distinct_raw (List.to_seq l) ~l ()
end

(** {2 Bit vectors} *)
and BitVec : BitVec = struct
  let mk n bv =
    assert (n > 0);
    assert (Z.(zero <= bv && bv < one lsl n));
    BitVec bv <| t_bv n

  (* Bitwidth -> [(1 lsl n) - 1] mask. Memoized to avoid re-allocating the mask
     bignum on each [mk_masked] call (expensive in pathological cases). *)
  let mask_cache : Z.t Array.t = Array.init 256 (fun n -> Z.(pred (one lsl n)))
  let mask_of_bits n = if n <= 255 then mask_cache.(n) else Z.(pred (one lsl n))

  let mk_masked n bv =
    let mask = mask_of_bits n in
    BitVec (Z.logand bv mask) <| t_bv n

  let mki n i = mk n (Z.of_int i)

  (* Index [n-1] holds the cached value for bitwidth [n]; we skip [n=0] because
     [mk] asserts [n > 0]. *)
  let zero_cache : sbv t Array.t = Array.init 256 (fun n -> mk (n + 1) Z.zero)
  let[@inline] zero n = if n <= 256 then zero_cache.(n - 1) else mk n Z.one
  let one_cache : sbv t Array.t = Array.init 256 (fun n -> mk (n + 1) Z.one)
  let[@inline] one n = if n <= 256 then one_cache.(n - 1) else mk n Z.zero

  (** [bv_to_z signed bits z] parses a BitVector [z], for a given bitwidth
      [bits], with [signed], into an integer. *)
  let bv_to_z signed bits z = if signed then Z.signed_extract z 0 bits else z

  let to_z : type a. a t -> Z.t option =
   fun v -> match v.node.kind with BitVec z -> Some z | _ -> None

  (** [max_for signed n] is the inclusive maximum for a bitvector of size [n]
      when it is [signed] *)
  let max_for signed n =
    let n = if signed then n - 1 else n in
    Z.(pred (one lsl n))

  (** [min_for signed n] is the inclusive minimum for a bitvector of size [n]
      when it is [signed] *)
  let min_for signed n =
    if signed then Z.(neg (one lsl Stdlib.( - ) n 1)) else Z.zero

  (** [is_right_mask z] is true if [z] is of the form [0*1+] *)
  let is_right_mask z = Z.(z > zero && popcount (succ z) = 1)

  (** [right_mask_size z] is, for a [z] of the form [0*1{n}], [n]. If [z] is not
      of the form [0*1{n}], the result is undefined, so use [is_right_mask]
      before. *)
  let right_mask_size z = Z.(log2 (succ z))

  (** [covers_bitwidth bits z] is true if [z] is of the form [1+] and covers the
      whole bitwidth of size [bits]. *)
  let covers_bitwidth bits z = is_right_mask z && right_mask_size z = bits

  let is_pow2 z = Z.(gt z zero && popcount z = 1)

  (** [lsb z] returns the least significant bit of z that is set, or 128 if z is
      0. *)
  let lsb z = if Z.equal z Z.zero then 128 else Z.(logand z (neg z) |> log2)

  let rec msb_of v =
    match v.node.kind with
    | BitVec z when Z.(z > zero) -> Z.log2 z
    | BitVec z when Z.(equal z zero) -> size_of v.node.ty - 1
    | BvArith (BitAnd, bv1, bv2) -> min (msb_of bv1) (msb_of bv2)
    | Ite (_, l, r) -> max (msb_of l) (msb_of r)
    | UnBv (BvExtend (false, __), v) -> msb_of v
    | _ -> size_of v.node.ty - 1

  let overflows ~signed n l r op =
    let minz = min_for signed n in
    let maxz = max_for signed n in
    let l = bv_to_z signed n l in
    let r = bv_to_z signed n r in
    let res = op l r in
    Z.Compare.(res < minz || res > maxz)

  (* Re-associating a checked add/sub folds two of its constants ([a], [b]) into
     a single one via [op]. The rebuilt operation can overflow even when the
     originals didn't (the fold wraps), so the [checked] flag only survives in a
     signedness where that constant fold does not itself overflow. *)
  let mask_checked_after_fold c a b op =
    match (a.node.kind, b.node.kind) with
    | BitVec za, BitVec zb ->
        let n = size_of a.node.ty in
        let keep ~signed =
          checked_has ~signed c && Stdlib.not (overflows ~signed n za zb op)
        in
        { signed = keep ~signed:true; unsigned = keep ~signed:false }
    | _ -> unchecked

  let ovf_check ~signed n l r op = Bool.of_bool @@ overflows ~signed n l r op

  let of_bool n b =
    if equal Bool.v_true b then one n
    else if equal Bool.v_false b then zero n
    else UnBool (BvOfBool n, b) <| TBitVector n

  let to_bool v =
    match v.node.kind with
    | BitVec z -> Bool.of_bool (Stdlib.not (Z.equal z Z.zero))
    | UnBool (BvOfBool _, sv') -> sv'
    | _ -> Bool.not (Bool.sem_eq v (zero (size_of v.node.ty)))

  let not_bool v =
    let n = size_of v.node.ty in
    match v.node.kind with
    | BitVec z -> if Z.equal z Z.zero then one n else zero n
    | UnBool (BvOfBool n, g) -> of_bool n (Bool.not g)
    | _ -> of_bool n (Bool.sem_eq v (zero n))

  let rec add ?(checked = unchecked) v1 v2 =
    assert (equal_ty v1.node.ty v2.node.ty);
    match[@warning "-ambiguous-var-in-pattern-guard"]
      (v1.node.kind, v2.node.kind)
    with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l + r)
    | UnBv (Neg _, v1), _ -> sub v2 v1
    | _, UnBv (Neg _, v2) -> sub v1 v2
    | _, BitVec z when Z.equal z Z.zero -> v1
    | BitVec z, _ when Z.equal z Z.zero -> v2
    | (BitVec z, UnBv (BvNot, v) | UnBv (BvNot, v), BitVec z)
      when Z.equal z Z.one ->
        neg v
    | BvArith (Add c, ({ node = { kind = BitVec _; _ }; _ } as c1), r), BitVec _
    | BvArith (Add c, r, ({ node = { kind = BitVec _; _ }; _ } as c1)), BitVec _
      ->
        let checked =
          mask_checked_after_fold (checked_meet checked c) c1 v2 Z.( + )
        in
        add ~checked (add c1 v2) r
    | BvArith (Sub c, l, ({ node = { kind = BitVec _; _ }; _ } as c1)), BitVec _
      ->
        let checked =
          mask_checked_after_fold (checked_meet checked c) v2 c1 Z.( - )
        in
        add ~checked l (sub v2 c1)
    | BvArith (Sub c, ({ node = { kind = BitVec _; _ }; _ } as c1), r), BitVec _
      ->
        let checked =
          mask_checked_after_fold (checked_meet checked c) c1 v2 Z.( + )
        in
        sub ~checked (add c1 v2) r
    | _, BvArith (Sub _, l, r) when equal r v1 -> l
    | BvArith (Sub _, l, r), _ when equal r v2 -> l
    | BvArith (Mul _, l1, r1), BvArith (Mul _, l2, r2)
      when equal l1 l2 || equal l1 r2 || equal r1 l2 || equal r1 r2 ->
        if equal l1 l2 then mul ~checked l1 (add ~checked r1 r2)
        else if equal l1 r2 then mul ~checked l1 (add ~checked r1 l2)
        else if equal r1 l2 then mul ~checked r1 (add ~checked l1 r2)
        else mul ~checked r1 (add ~checked l1 l2)
    | ( BvArith (Mul ck1, ({ node = { kind = BitVec l1; _ }; _ } as v_l1), r1),
        BvArith (Mul ck2, ({ node = { kind = BitVec l2; _ }; _ } as v_l2), r2) )
    | ( BvArith (Mul ck1, r1, ({ node = { kind = BitVec l1; _ }; _ } as v_l1)),
        BvArith (Mul ck2, ({ node = { kind = BitVec l2; _ }; _ } as v_l2), r2) )
    | ( BvArith (Mul ck1, ({ node = { kind = BitVec l1; _ }; _ } as v_l1), r1),
        BvArith (Mul ck2, r2, ({ node = { kind = BitVec l2; _ }; _ } as v_l2)) )
    | ( BvArith (Mul ck1, r1, ({ node = { kind = BitVec l1; _ }; _ } as v_l1)),
        BvArith (Mul ck2, r2, ({ node = { kind = BitVec l2; _ }; _ } as v_l2)) )
      when is_checked (checked_meet (checked_meet checked ck1) ck2)
           && (Z.divisible l1 l2 || Z.divisible l2 l1) ->
        let checked = checked_meet (checked_meet checked ck1) ck2 in
        if Z.divisible l2 l1 then
          let common = mk (size_of v1.node.ty) (Z.div l2 l1) in
          mul ~checked v_l1 (add ~checked r1 (mul ~checked common r2))
        else
          let common = mk (size_of v1.node.ty) (Z.div l1 l2) in
          mul ~checked v_l2 (add ~checked r2 (mul ~checked common r1))
    | Ite (b, l, r), BitVec x | BitVec x, Ite (b, l, r) ->
        (* only propagate down ites if we know it's concrete *)
        let n = size_of v1.node.ty in
        let x = mk n x in
        Bool.ite b (add ~checked l x) (add ~checked r x)
    | _ -> mk_commut_arith (Add checked) v1 v2 <| v1.node.ty

  and sub ?(checked = unchecked) v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l - r)
    | _, BitVec z when Z.equal z Z.zero -> v1
    | BitVec z, _ when Z.equal z Z.zero ->
        (* 0 - x = -x; the subtraction overflows (signed) iff x = INT_MIN, so a
           signed-checked subtraction yields a signed-checked negation. *)
        neg ~checked:checked.signed v2
    | _, _ when equal v1 v2 -> zero (size_of v1.node.ty)
    (* BAD PERF:!!!! *)
    | _, UnBv (Neg _, v2) -> add v1 v2
    | BvArith (Sub c, ({ node = { kind = BitVec _; _ }; _ } as c1), s), BitVec _
      ->
        let checked =
          mask_checked_after_fold (checked_meet c checked) c1 v2 Z.( - )
        in
        sub ~checked (sub c1 v2) s
    | BvArith (Sub c, s, ({ node = { kind = BitVec _; _ }; _ } as c1)), BitVec _
      ->
        let checked =
          mask_checked_after_fold (checked_meet c checked) c1 v2 Z.( + )
        in
        sub ~checked s (add c1 v2)
    | BitVec _, BvArith (Add c, ({ node = { kind = BitVec _; _ }; _ } as r), l)
    | BitVec _, BvArith (Add c, l, ({ node = { kind = BitVec _; _ }; _ } as r))
      ->
        let checked =
          mask_checked_after_fold (checked_meet c checked) v1 r Z.( - )
        in
        sub ~checked (sub v1 r) l
    | ( BvArith (Add c, ({ node = { kind = BitVec bv1; _ }; _ } as r), l),
        BitVec bv2 )
    | ( BvArith (Add c, l, ({ node = { kind = BitVec bv1; _ }; _ } as r)),
        BitVec bv2 ) ->
        (* if bv1 < bv2 there would be an overflow which causes problems since
           the operation can't be deemed checked anymore. *)
        if Z.lt bv1 bv2 then
          let checked =
            mask_checked_after_fold (checked_meet c checked) v2 r Z.( - )
          in
          sub ~checked l (neg (sub r v2))
        else
          let checked =
            mask_checked_after_fold (checked_meet c checked) r v2 Z.( - )
          in
          add ~checked l (sub r v2)
    | BvArith (Add _, l, r), _ when equal l v2 -> r
    | BvArith (Add _, l, r), _ when equal r v2 -> l
    | BvArith (Add _, l1, r1), BvArith (Add _, l2, r2) when equal l1 l2 ->
        sub ~checked r1 r2
    | _l, BvArith (Sub _, l', r) when equal v1 l' -> r
    (* only propagate down ites if we know it's concrete *)
    | Ite (b, l, r), BitVec _ -> Bool.ite b (sub l v2) (sub r v2)
    | BitVec _, Ite (b, l, r) -> Bool.ite b (sub v1 l) (sub v1 r)
    | UnBool (BvOfBool n, b), BitVec _ -> Bool.ite b (sub (one n) v2) (neg v2)
    | BitVec _, UnBool (BvOfBool n, b) -> Bool.ite b (sub v1 (one n)) v1
    | _ -> BvArith (Sub checked, v1, v2) <| v1.node.ty

  and neg ?(checked = false) v =
    let n = size_of v.node.ty in
    match v.node.kind with
    | BitVec bv -> mk_masked n Z.(neg bv)
    | UnBv (Neg _, v) -> v
    | Ite (b, l, r) -> Bool.ite b (neg ~checked l) (neg ~checked r)
    | UnBool (BvOfBool n, b) -> Bool.ite b (neg (one n)) (zero n)
    | _ -> UnBv (Neg checked, v) <| v.node.ty

  (** [mod_ v1 v2] is the signed remainder of [v1 / v2], which takes the sign of
      the divisor [v2] if [signed]. For an unsigned version, use
      [rem ~signed:false]. *)
  and mod_ v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let size = size_of v1.node.ty in
        let l = bv_to_z true size l in
        let r = bv_to_z true size r in
        let res = Z.(l mod r) in
        let res =
          if Z.(res < zero) && Stdlib.not Z.(r < zero) then Z.(res + r)
          else if Z.(res >= zero) && Z.(r < zero) then Z.(res + r)
          else res
        in
        mk_masked size res
    | _ -> BvArith (Mod, v1, v2) <| v1.node.ty

  (** [rem ~signed v1 v2] is the remainder of [v1 / v2], which takes the sign of
      the dividend [v1] if [signed]. *)
  and rem ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let size = size_of v1.node.ty in
        let l = bv_to_z signed size l in
        let r = bv_to_z signed size r in
        mk_masked size Z.(l mod r)
    | BitVec z, _ when Z.equal Z.zero z -> zero (size_of v1.node.ty)
    | _, BitVec r when Stdlib.not signed && Z.(equal r one) ->
        zero (size_of v1.node.ty)
    | _, BitVec r when Stdlib.not signed && is_pow2 r ->
        let size = size_of v1.node.ty in
        let bitwidth = Z.log2 r in
        let lower = extract 0 (bitwidth - 1) v1 in
        extend ~signed:false (size - bitwidth) lower
    | BvArith (Add _, { node = { kind = BitVec l; _ }; _ }, r), BitVec d
      when Stdlib.not signed && Z.(equal l d) ->
        rem ~signed r v2
    | BvArith (Add _, r, { node = { kind = BitVec l; _ }; _ }), BitVec d
      when Stdlib.not signed && Z.(equal l d) ->
        rem ~signed r v2
    | ( BvArith (Rem false, r, ({ node = { kind = BitVec r1; _ }; _ } as v_r1)),
        BitVec r2 )
      when Stdlib.not signed
           && Z.(equal zero (rem r1 r2) || equal zero (rem r2 r1)) ->
        let rhs = if Z.(equal (min r1 r2) r1) then v_r1 else v2 in
        rem ~signed r rhs
    | _ -> BvArith (Rem signed, v1, v2) <| v1.node.ty

  and not (v : sbv t) =
    match v.node.kind with
    | BitVec bv ->
        let n = size_of v.node.ty in
        mk_masked n Z.(lognot bv)
    | Ite (b, l, r) -> Bool.ite b (not l) (not r)
    | _ -> UnBv (BvNot, v) <| v.node.ty

  and and_ v1 v2 =
    let n = size_of v1.node.ty in
    assert (n == size_of v2.node.ty);
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk n Z.(logand l r)
    | BitVec mask, _ when Z.(equal mask zero) -> v1
    | _, BitVec mask when Z.(equal mask zero) -> v2
    | BitVec mask, _ when covers_bitwidth n mask -> v2
    | _, BitVec mask when covers_bitwidth n mask -> v1
    (* For (x >> s) & m, the mask is irrelevant if it entirely covers [bitsize -
       s] *)
    | ( (BvArith (LShr, _, { node = { kind = BitVec shift; _ }; _ }) as base),
        BitVec mask )
    | ( BitVec mask,
        (BvArith (LShr, _, { node = { kind = BitVec shift; _ }; _ }) as base) )
      when let shift_i = Z.to_int shift in
           shift_i >= 0
           && shift_i < n
           &&
           let bitwidth = n - shift_i in
           let low_mask = Z.(pred (one lsl bitwidth)) in
           Z.(equal (mask land low_mask) low_mask) ->
        base <| t_bv n
    | BitVec _, Ite (b, l, r) -> Bool.ite b (and_ v1 l) (and_ v1 r)
    | Ite (b, l, r), BitVec _ -> Bool.ite b (and_ l v2) (and_ r v2)
    | BitVec m1, BvArith (BitAnd, x, { node = { kind = BitVec m2; _ }; _ })
    | BitVec m1, BvArith (BitAnd, { node = { kind = BitVec m2; _ }; _ }, x)
    | BvArith (BitAnd, x, { node = { kind = BitVec m2; _ }; _ }), BitVec m1
    | BvArith (BitAnd, { node = { kind = BitVec m2; _ }; _ }, x), BitVec m1 ->
        and_ x (mk n (Z.logand m1 m2))
    (* collapse M & (N | (P & X)) into (M & N) | (M & P & X) where M, N and P
       concrete *)
    | ( (BitVec _ as m),
        BvArith
          ( BitOr,
            ({ node = { kind = BitVec _; _ }; _ } as n),
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, ({ node = { kind = BitVec _; _ }; _ } as p), x);
                  _;
                };
              _;
            } ) )
    | ( (BitVec _ as m),
        BvArith
          ( BitOr,
            ({ node = { kind = BitVec _; _ }; _ } as n),
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, x, ({ node = { kind = BitVec _; _ }; _ } as p));
                  _;
                };
              _;
            } ) )
    | ( (BitVec _ as m),
        BvArith
          ( BitOr,
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, ({ node = { kind = BitVec _; _ }; _ } as p), x);
                  _;
                };
              _;
            },
            ({ node = { kind = BitVec _; _ }; _ } as n) ) )
    | ( (BitVec _ as m),
        BvArith
          ( BitOr,
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, x, ({ node = { kind = BitVec _; _ }; _ } as p));
                  _;
                };
              _;
            },
            ({ node = { kind = BitVec _; _ }; _ } as n) ) )
    | ( BvArith
          ( BitOr,
            ({ node = { kind = BitVec _; _ }; _ } as n),
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, ({ node = { kind = BitVec _; _ }; _ } as p), x);
                  _;
                };
              _;
            } ),
        (BitVec _ as m) )
    | ( BvArith
          ( BitOr,
            ({ node = { kind = BitVec _; _ }; _ } as n),
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, x, ({ node = { kind = BitVec _; _ }; _ } as p));
                  _;
                };
              _;
            } ),
        (BitVec _ as m) )
    | ( BvArith
          ( BitOr,
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, ({ node = { kind = BitVec _; _ }; _ } as p), x);
                  _;
                };
              _;
            },
            ({ node = { kind = BitVec _; _ }; _ } as n) ),
        (BitVec _ as m) )
    | ( BvArith
          ( BitOr,
            {
              node =
                {
                  kind =
                    BvArith
                      (BitAnd, x, ({ node = { kind = BitVec _; _ }; _ } as p));
                  _;
                };
              _;
            },
            ({ node = { kind = BitVec _; _ }; _ } as n) ),
        (BitVec _ as m) ) ->
        let ty = v1.node.ty in
        let m = m <| ty in
        or_ (and_ m n) (and_ m p |> and_ x)
    (* M & (N | X) *)
    | BitVec m_and, BvArith (BitOr, x, { node = { kind = BitVec m_or; _ }; _ })
    | BitVec m_and, BvArith (BitOr, { node = { kind = BitVec m_or; _ }; _ }, x)
    | BvArith (BitOr, x, { node = { kind = BitVec m_or; _ }; _ }), BitVec m_and
    | BvArith (BitOr, { node = { kind = BitVec m_or; _ }; _ }, x), BitVec m_and
      ->
        let overlap = Z.logand m_and m_or in
        if Z.equal overlap m_and then
          (* M & (N | X) when M & N == M ==> M (all bits already set by the
             or) *)
          mk n m_and
        else if Z.equal overlap Z.zero then
          (* M & (N | X) when M & N == 0 ==> M & X (no bits set by the or) *)
          and_ x (mk n m_and)
        else (* give up *)
          mk_commut_arith BitAnd v1 v2 <| t_bv n
    (* if it's a right mask, it's usually beneficial to propagate it *)
    | (BitVec mask, BvArith (BitAnd, l, r) | BvArith (BitAnd, l, r), BitVec mask)
      when is_right_mask mask ->
        let n = size_of v1.node.ty in
        let mask = mk n mask in
        and_ (and_ mask l) (and_ mask r)
    | BitVec o, UnBool (BvOfBool _, _) when Z.equal o Z.one -> v2
    | UnBool (BvOfBool _, _), BitVec o when Z.equal o Z.one -> v1
    | UnBool (BvOfBool _, b1), UnBool (BvOfBool _, b2) ->
        of_bool n (Bool.and_ b1 b2)
    | ( Ite (b1, l1, { node = { kind = BitVec r1; _ }; _ }),
        Ite (b2, l2, { node = { kind = BitVec r2; _ }; _ }) )
      when Z.(equal r1 zero) && Z.(equal r2 zero) ->
        let n = size_of v1.node.ty in
        Bool.ite (Bool.and_ b1 b2) (and_ l1 l2) (zero n)
    | _, _ -> mk_commut_arith BitAnd v1 v2 <| t_bv n

  and or_ v1 v2 =
    match[@warning "-ambiguous-var-in-pattern-guard"]
      (v1.node.kind, v2.node.kind)
    with
    | BitVec l, BitVec r ->
        let n = size_of v1.node.ty in
        mk n Z.(l lor r)
    | BitVec z, _ when Z.equal z Z.zero -> v2
    | _, BitVec z when Z.equal z Z.zero -> v1
    | _, _ when equal v1 v2 -> v1
    (* m1 | (x & m2) where m2 is a subset of m1's bits means the & is
       redundant *)
    | BitVec m1, BvArith (BitAnd, _, { node = { kind = BitVec m2; _ }; _ })
    | BitVec m1, BvArith (BitAnd, { node = { kind = BitVec m2; _ }; _ }, _)
    | BvArith (BitAnd, _, { node = { kind = BitVec m2; _ }; _ }), BitVec m1
    | BvArith (BitAnd, { node = { kind = BitVec m2; _ }; _ }, _), BitVec m1
      when Z.(equal (logand m1 m2) m2) ->
        mk (size_of v1.node.ty) m1
    | BitVec m1, BvArith (BitOr, x, { node = { kind = BitVec m2; _ }; _ })
    | BitVec m1, BvArith (BitOr, { node = { kind = BitVec m2; _ }; _ }, x)
    | BvArith (BitOr, x, { node = { kind = BitVec m2; _ }; _ }), BitVec m1
    | BvArith (BitOr, { node = { kind = BitVec m2; _ }; _ }, x), BitVec m1 ->
        or_ x (mk (size_of v1.node.ty) (Z.logor m1 m2))
    (* 0x0..0X..X | (0x0..0Y..Y << N) when N = |X..X| ==> 0x0..0Y..YX..X *)
    | ( UnBv (BvExtend (false, nx), base),
        BvArith
          ( Shl,
            { node = { kind = UnBv (BvExtend (false, _), tail); _ }; _ },
            { node = { kind = BitVec shift; _ }; _ } ) )
      when Z.to_int shift = size_of base.node.ty ->
        let tail_size = size_of tail.node.ty in
        if nx = tail_size then concat tail base
        else if nx > tail_size then
          let new_base = concat tail base in
          extend ~signed:false (nx - tail_size) new_base
        else
          let new_tail = extract 0 (nx - 1) tail in
          concat new_tail base
    | UnBool (BvOfBool n, b1), UnBool (BvOfBool _, b2) ->
        of_bool n (Bool.or_ b1 b2)
    | _ -> mk_commut_arith BitOr v1 v2 <| v1.node.ty

  and xor v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let n = size_of v1.node.ty in
        mk n Z.(l lxor r)
    | BitVec z, _ when Z.equal z Z.zero -> v2
    | _, BitVec z when Z.equal z Z.zero -> v1
    | UnBool (BvOfBool n, b1), UnBool (BvOfBool _, b2) ->
        of_bool n (Bool.not (Bool.sem_eq b1 b2))
    | _ -> mk_commut_arith BitXor v1 v2 <| v1.node.ty

  (** [extract from_ to_ v] returns a bitvector covering bits from index
      [from_], to index [to_], inclusive. [0 <= from_ <= to_ < size_of v] must
      hold. *)
  and extract from_ to_ v =
    let prev_size = size_of v.node.ty in
    assert (0 <= from_ && from_ <= to_ && to_ < prev_size);
    let size = to_ - from_ + 1 in
    match v.node.kind with
    | BitVec bv -> mk_masked size Z.(bv asr from_)
    | _ when from_ = 0 && to_ = prev_size - 1 -> v
    | BvArith (((BitAnd | BitOr | BitXor) as bop), v1, v2) -> (
        let v1 = extract from_ to_ v1 in
        let v2 = extract from_ to_ v2 in
        match bop with
        | BitAnd -> and_ v1 v2
        | BitOr -> or_ v1 v2
        | BitXor -> xor v1 v2
        | _ -> L.failwith "unreachable binop")
    | BvArith (Shl, v1, { node = { kind = BitVec x; _ }; _ }) ->
        (* extract[from_, to_](v1 << x) *)
        let shift = Z.to_int x in
        if from_ >= shift then
          (* All extracted bits come from the original v1, shifted *)
          extract (from_ - shift) (to_ - shift) v1
        else if to_ < shift then
          (* All extracted bits are zeros introduced by the shift *)
          zero size
        else
          (* Some bits are zeros, some are from v1 *)
          (* bits [from_, shift-1] are 0, bits [shift, to_] come from v1[0, to_-shift] *)
          let high_part = extract 0 (to_ - shift) v1 in
          let low_zeros = zero (shift - from_) in
          concat high_part low_zeros
    | BvArith (LShr, v1, { node = { kind = BitVec x; _ }; _ }) ->
        (* extract[from_, to_](v1 >> x) *)
        (* After right shift by x, bit i of result = bit (i+x) of original if i+x < prev_size, else 0 *)
        let shift = Z.to_int x in
        if from_ + shift >= prev_size then
          (* All extracted bits are zeros introduced by the shift *)
          zero size
        else if to_ + shift < prev_size then
          (* All extracted bits come from the original v1, shifted *)
          extract (from_ + shift) (to_ + shift) v1
        else
          (* Some bits are from v1, some are zeros *)
          (* bits [from_, prev_size-shift-1] come from v1[from_+shift, prev_size-1] *)
          (* bits [prev_size-shift, to_] are 0 *)
          let low_part = extract (from_ + shift) (prev_size - 1) v1 in
          let high_zeros = zero (to_ - (prev_size - shift - 1)) in
          concat high_zeros low_part
    | Ite (b, l, r) ->
        let l = extract from_ to_ l in
        let r = extract from_ to_ r in
        Bool.ite b l r
    | UnBv (BvExtend (false, by), _) when from_ >= prev_size - by ->
        (* zero extension, and we're extracting only the extended bits *)
        zero size
    | UnBv (BvExtend (true, by), v) when from_ >= prev_size - by && from_ = to_
      ->
        (* sign extension, and we're extracting an extended bit: msb of the prev
           value *)
        let prev_size = size_of v.node.ty in
        extract (prev_size - 1) (prev_size - 1) v
    | UnBv (BvExtend (signed, _), v) when from_ = 0 ->
        (* extracting from the beginning of an extended value *)
        let orig_size = size_of v.node.ty in
        if to_ = orig_size - 1 then
          (* extracting exactly the original bits *)
          v
        else if to_ < orig_size then
          (* extracting subset of original bits *)
          extract from_ to_ v
        else
          (* we can reduce the extend *)
          extend ~signed (to_ - orig_size + 1) v
    | UnBv (BvExtend (_, by), v) when to_ <= prev_size - by - 1 ->
        (* extracting from original bits *)
        extract from_ to_ v
    | UnBv (BvExtract (prev_from_, _), v) ->
        extract (prev_from_ + from_) (prev_from_ + to_) v
    | BvArith (BvConcat, l, r) ->
        let size_r = size_of r.node.ty in
        if from_ >= size_r then extract (from_ - size_r) (to_ - size_r) l
        else if to_ < size_r then extract from_ to_ r
        else
          let r' = extract from_ (size_r - 1) r in
          let l' = extract 0 (to_ - size_r) l in
          concat l' r'
    | BvArith (Add _, l, r) when from_ = 0 ->
        let l_low = extract from_ to_ l in
        let r_low = extract from_ to_ r in
        add l_low r_low
    | BvArith (Add _, { node = { kind = BitVec n; _ }; _ }, x) when to_ < lsb n
      ->
        extract from_ to_ x
    | BvArith (Add _, x, { node = { kind = BitVec n; _ }; _ }) when to_ < lsb n
      ->
        extract from_ to_ x
    | BvArith (Mul _, { node = { kind = BitVec n; _ }; _ }, _)
      when is_pow2 n && to_ < Z.log2 n ->
        zero size
    | BvArith (Mul _, l, r) when from_ = 0 ->
        (* i think the extraction is necessarily unchecked? *)
        let l_low = extract from_ to_ l in
        let r_low = extract from_ to_ r in
        mul ~checked:unchecked l_low r_low
    | BvArith (Rem false, l, { node = { kind = BitVec n; _ }; _ })
      when from_ = 0 && Z.log2 n < to_ ->
        (* extract[0,N](X % M) when M < 2^M can be pushed down *)
        let l = extract from_ to_ l in
        rem ~signed:false l (mk (to_ + 1) n)
    | _ -> UnBv (BvExtract (from_, to_), v) <| t_bv size

  and extend ~signed extend_by v =
    assert (is_bv v.node.ty);
    let n = size_of v.node.ty in
    let to_ = n + extend_by in
    assert (extend_by > 0);
    match v.node.kind with
    | BitVec bv ->
        if signed then
          (* Sign extend: replicate the MSB *)
          mk_masked to_ (Z.signed_extract bv 0 n)
        else
          (* Zero extend: do nothing *)
          mk to_ bv
    | UnBv (BvExtend (prev_signed, prev_by), v) when prev_signed = signed ->
        (* combine extensions *)
        extend ~signed (prev_by + extend_by) v
    | Ite (b, l, r) ->
        let l = extend ~signed extend_by l in
        let r = extend ~signed extend_by r in
        Bool.ite b l r
    (* can't extend if signed && n == 1, as it should be all 1s *)
    | UnBool (BvOfBool n, b) when Stdlib.not signed || n > 1 -> of_bool to_ b
    (* | UnBv (BvExtract (_, t), v)
      when Stdlib.not signed && to_ = size_of v.node.ty && t = to_ - 1 ->
        (* extend[N](extract[L-N, L-1] X) == X >> N, where L = size_of X *)
        lshr v (mki to_ extend_by) *)
    (* unlike with extract, we don't want to propagate extend within the expression for &, |, ^,
         as that will require a more expensive bit-blasting. *)
    (* We also note the following reduction is *not valid*, as some upper bits may be set;
         e.g. given i2bv[3](8) = 0b000, extend[1](i2bv[3](8)) = 0b0000, whereas
              i2bv[4](8) = 0b1000
      | Unop (BvOfInt, v) -> Unop (BvOfInt, v) <| t_bv signed to_ *)
    | _ -> UnBv (BvExtend (signed, extend_by), v) <| t_bv to_

  (** [concat v1 v2] for [v1] of size [n] and [v2] of size [m] is a bitvector of
      size [n + m] where the first [m] bits are [v2] and the following [n] are
      [v1] *)
  and concat v1 v2 =
    let n1 = size_of v1.node.ty in
    let n2 = size_of v2.node.ty in
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (n1 + n2) Z.(r + shift_left l n2)
    | UnBv (BvExtract (from1, to1), v1), UnBv (BvExtract (from2, to2), v2)
      when to2 + 1 = from1 && equal v1 v2 ->
        extract from2 to1 v1
    (* safeguard: to avoid infinite loops, we keep bv-concats of extracts
       left-dominant *)
    | ( UnBv (BvExtract _, _),
        BvArith
          ( BvConcat,
            { node = { kind = UnBv (BvExtract _, _); _ }; _ },
            { node = { kind = UnBv (BvExtract _, _); _ }; _ } ) ) ->
        BvArith (BvConcat, v1, v2) <| t_bv (n1 + n2)
    (* We re-order (extract A ++ (extract B ++ X)) to ((extract A ++ extract B)
       ++ X) *)
    | ( UnBv (BvExtract _, x),
        BvArith
          ( BvConcat,
            ({ node = { kind = UnBv (BvExtract _, y); _ }; _ } as left),
            right ) )
      when equal x y ->
        concat (concat v1 left) right
    (* We re-order ((X ++ extract A) ++ extract B) to (X ++ (extract A ++
       extract B)) *)
    | ( BvArith
          ( BvConcat,
            left,
            ({ node = { kind = UnBv (BvExtract _, x); _ }; _ } as right) ),
        UnBv (BvExtract _, y) )
      when equal x y ->
        concat left (concat right v2)
    | Ite (b1, l1, r1), Ite (b2, l2, r2) when equal b1 b2 ->
        Bool.ite b1 (concat l1 l2) (concat r1 r2)
    | _, _ -> BvArith (BvConcat, v1, v2) <| t_bv (n1 + n2)

  and shl v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l lsl to_int r)
    | _, BitVec s when Z.equal s Z.zero -> v1
    | _, BitVec s when Z.geq s (Z.of_int (size_of v1.node.ty)) ->
        zero (size_of v1.node.ty)
    | BvArith (Shl, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
        let n = size_of v1.node.ty in
        shl v (mk n Z.(s1 + s2))
    | BvArith (LShr, x, { node = { kind = BitVec sr; _ }; _ }), BitVec sl ->
        if Z.leq sl sr then
          (* (x >> s1) << s2 where s2 < s1 = x >> (s1 - s2) & (mask with lower
             bits cleared) *)
          let n = size_of v1.node.ty in
          let mask = Z.(lognot (pred (one lsl to_int sl))) in
          and_ (lshr x (mk n Z.(sr - sl))) (mk_masked n mask)
        else
          (* (x >> s1) << s2 where s2 > s1 = x << (s2 - s1) & (mask with lower
             bits cleared) *)
          let n = size_of v1.node.ty in
          let shift = Z.(sl - sr) in
          let mask = Z.(lognot (pred (one lsl to_int sr))) in
          shl (and_ x (mk_masked n mask)) (mk n shift)
    | BvArith (BitAnd, x, { node = { kind = BitVec mask; _ }; _ }), BitVec s
    | BvArith (BitAnd, { node = { kind = BitVec mask; _ }; _ }, x), BitVec s ->
        (* (x & mask) << s = (x << s) & (mask << s) *)
        let n = size_of v1.node.ty in
        let shifted_mask = Z.(mask lsl to_int s) in
        and_ (shl x v2) (mk_masked n shifted_mask)
    | BvArith (BitOr, x, { node = { kind = BitVec mask; _ }; _ }), BitVec s
    | BvArith (BitOr, { node = { kind = BitVec mask; _ }; _ }, x), BitVec s ->
        (* (x | mask) << s = (x << s) | (mask << s) *)
        let n = size_of v1.node.ty in
        let shifted_mask = Z.(mask lsl to_int s) in
        or_ (shl x v2) (mk_masked n shifted_mask)
    | _ -> BvArith (Shl, v1, v2) <| v1.node.ty

  and lshr v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l asr to_int r)
    | _, BitVec s when Z.equal s Z.zero -> v1
    | _, BitVec s when Z.geq s (Z.of_int (size_of v1.node.ty)) ->
        zero (size_of v1.node.ty)
    | BvArith (LShr, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
        let n = size_of v1.node.ty in
        lshr v (mk n Z.(s1 + s2))
    | BvArith (BitAnd, x, { node = { kind = BitVec mask; _ }; _ }), BitVec s
    | BvArith (BitAnd, { node = { kind = BitVec mask; _ }; _ }, x), BitVec s ->
        (* (x & mask) >> s = (x >> s) & (mask >> s) *)
        let n = size_of v1.node.ty in
        let shifted_mask = Z.(mask asr to_int s) in
        and_ (lshr x v2) (mk n shifted_mask)
    | BvArith (BitOr, x, { node = { kind = BitVec mask; _ }; _ }), BitVec s
    | BvArith (BitOr, { node = { kind = BitVec mask; _ }; _ }, x), BitVec s ->
        (* (x | mask) >> s = (x >> s) | (mask >> s) *)
        let n = size_of v1.node.ty in
        let shifted_mask = Z.(mask asr to_int s) in
        or_ (lshr x v2) (mk n shifted_mask)
    | _ -> BvArith (LShr, v1, v2) <| v1.node.ty

  and ashr v1 v2 =
    let size = size_of v1.node.ty in
    let size_z = Z.of_int (size_of v1.node.ty) in
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let n = size_of v1.node.ty in
        mk_masked n Z.(Z.signed_extract l 0 n asr to_int r)
    | _, BitVec s when Z.equal s Z.zero -> v1
    | _, BitVec s when Z.geq s size_z ->
        ashr v1 (mk_masked size (Z.pred size_z))
    | BvArith (AShr, v, { node = { kind = BitVec s1; _ }; _ }), BitVec s2 ->
        ashr v (mk size Z.(s1 + s2))
    | _ -> BvArith (AShr, v1, v2) <| v1.node.ty

  and mul ?(checked = unchecked) v1 v2 =
    assert (equal_ty v1.node.ty v2.node.ty);
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> mk_masked (size_of v1.node.ty) Z.(l * r)
    | _, BitVec z when Z.equal z Z.one -> v1
    | BitVec z, _ when Z.equal z Z.one -> v2
    | _, BitVec z when Z.equal z Z.zero -> zero (size_of v1.node.ty)
    | BitVec z, _ when Z.equal z Z.zero -> zero (size_of v1.node.ty)
    (* c * (-x) = (-c) * x (holds mod 2^n); with a checked negation (x <>
       INT_MIN) and c <> INT_MIN, the signed no-overflow guarantee carries over
       to the rebuilt product. *)
    | (BitVec c, UnBv (Neg true, x) | UnBv (Neg true, x), BitVec c)
      when Stdlib.not
             (Z.equal
                (bv_to_z true (size_of v1.node.ty) c)
                (min_for true (size_of v1.node.ty))) ->
        let n = size_of v1.node.ty in
        mul ~checked:(checked_meet checked checked_signed) (neg (mk n c)) x
    | ( ( BvArith (Mul ckm, x, { node = { kind = BitVec n; _ }; _ })
        | BvArith (Mul ckm, { node = { kind = BitVec n; _ }; _ }, x) ),
        BitVec m )
    | ( BitVec m,
        ( BvArith (Mul ckm, { node = { kind = BitVec n; _ }; _ }, x)
        | BvArith (Mul ckm, x, { node = { kind = BitVec n; _ }; _ }) ) )
      when is_checked (checked_meet checked ckm) ->
        mul ~checked:(checked_meet checked ckm) x
          (mk_masked (size_of v1.node.ty) Z.(n * m))
    (* only propagate down ites if we know it's concrete *)
    | Ite (b, l, r), BitVec x | BitVec x, Ite (b, l, r) ->
        let n = size_of v1.node.ty in
        let x = mk n x in
        Bool.ite b (mul l x) (mul r x)
    | _ -> mk_commut_arith (Mul checked) v1 v2 <| v1.node.ty

  let rec div ~signed v1 v2 =
    assert (equal_ty v1.node.ty v2.node.ty);
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        let size = size_of v1.node.ty in
        let l = bv_to_z signed size l in
        let r = bv_to_z signed size r in
        let res = Z.(l / r) in
        mk_masked size res
    | _, BitVec r when Z.equal r Z.one -> v1
    (* this case shouldn't happen but it avoids conflicts for the next two
       patterns *)
    | ( BvArith
          ( Mul checked,
            ({ node = { kind = BitVec _; _ }; _ } as l),
            ({ node = { kind = BitVec _; _ }; _ } as r) ),
        BitVec _ ) ->
        div ~signed (mul ~checked l r) v2
    | ( BvArith
          (Mul { unsigned = true; _ }, { node = { kind = BitVec n; _ }; _ }, x),
        BitVec d )
    | ( BvArith
          (Mul { unsigned = true; _ }, x, { node = { kind = BitVec n; _ }; _ }),
        BitVec d )
      when Stdlib.not signed && Z.(divisible n d) ->
        (* (x * n) / d = x * (n / d) when n % d == 0 *)
        mul ~checked:checked_unsigned x (mk (size_of v1.node.ty) Z.(n / d))
    | ( BvArith
          (Mul { unsigned = true; _ }, { node = { kind = BitVec n; _ }; _ }, x),
        BitVec d )
    | ( BvArith
          (Mul { unsigned = true; _ }, x, { node = { kind = BitVec n; _ }; _ }),
        BitVec d )
      when Stdlib.not signed && Z.(divisible d n) ->
        (* (x * n) / d = x / (d / n) when d % n == 0 *)
        let divisor = Z.(d / n) in
        div ~signed x (mk (size_of v1.node.ty) divisor)
    | BvArith (Div s, x, { node = { kind = BitVec n; _ }; _ }), BitVec d
      when s = signed
           && (Stdlib.not @@ overflows ~signed (size_of v1.node.ty) n d Z.mul)
      ->
        (* (x / n) / d = x / (n * d) (if n * d doesn't overflow) *)
        div ~signed x (mk (size_of v1.node.ty) Z.(n * d))
    | UnBv (BvExtend (false, by), x), BitVec z
      when Stdlib.not signed && msb_of v2 < size_of x.node.ty ->
        (* extend[uN](X) / N when msb(N) < size(X) <=> extend[uN](X / N) *)
        let v2 = mk (size_of x.node.ty) z in
        extend ~signed:false by (div ~signed x v2)
    | _ -> BvArith (Div signed, v1, v2) <| v1.node.ty

  (* Whether [v] is an addition, subtraction or multiplication involving a
     constant that is known not to overflow when interpreted as unsigned. The
     unsigned [lt]/[leq] cases can reduce such operations, so a signed
     comparison against one is worth rewriting to unsigned. *)
  let is_checked_unsigned_op v =
    match v.node.kind with
    | BvArith ((Add c | Sub c | Mul c), { node = { kind = BitVec _; _ }; _ }, _)
    | BvArith ((Add c | Sub c | Mul c), _, { node = { kind = BitVec _; _ }; _ })
      ->
        c.unsigned
    | _ -> false

  (* An inclusive upper bound on the unsigned value of [v], derived from its
     most significant possibly-set bit (e.g. [x & 0b11 <= 3]). Conservative: an
     unknown value bounds to the full width's maximum. *)
  let unsigned_ub v = Z.(pred (one lsl Stdlib.(msb_of v + 1)))

  (* When rewriting a checked comparison [y + l <op> x + r] into one with a
     single symbolic sum (e.g. [y <op> x + (r - l)]), that rebuilt sum is only
     guaranteed not to overflow when the joined constant [d] lies between [0]
     and [base] (then [v + d] sits between the in-range values [v] and [v +
     base]). [base] and [d] are the signed values of the constants. *)
  let const_keeps_in_range ~base d =
    Z.leq (Z.min Z.zero base) d && Z.leq d (Z.max Z.zero base)

  let rec lt ~signed v1 v2 =
    assert (equal_ty v1.node.ty v2.node.ty);
    let bits = size_of v1.node.ty in
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r ->
        Bool.of_bool @@ Z.lt (bv_to_z signed bits l) (bv_to_z signed bits r)
    | _ when equal v1 v2 -> Bool.v_false
    (* -a < -b <=> b < a, when neither negation overflows *)
    | UnBv (Neg true, a), UnBv (Neg true, b) when signed -> lt ~signed b a
    (* -a < c <=> -c < a, when -a doesn't overflow and c <> INT_MIN *)
    | UnBv (Neg true, a), BitVec c
      when signed
           && Stdlib.not (Z.equal (bv_to_z signed bits c) (min_for signed bits))
      ->
        lt ~signed (neg v2) a
    (* c < -a <=> a < -c, when -a doesn't overflow and c <> INT_MIN *)
    | BitVec c, UnBv (Neg true, a)
      when signed
           && Stdlib.not (Z.equal (bv_to_z signed bits c) (min_for signed bits))
      ->
        lt ~signed a (neg v1)
    | ( BitVec bv_v1,
        ( BvArith
            (Add checked, ({ node = { kind = BitVec bv_r; _ }; _ } as r), x)
        | BvArith
            (Add checked, x, ({ node = { kind = BitVec bv_r; _ }; _ } as r)) ) )
      when checked_has ~signed checked ->
        if Stdlib.not signed && Z.lt bv_v1 bv_r then Bool.v_true
        else if overflows ~signed bits bv_v1 bv_r Z.( - ) then
          BvCmp (Lt signed, v1, v2) <| TBool
        else lt ~signed (sub ~checked:(checked_of_signed signed) v1 r) x
    | ( ( BvArith
            (Add checked, ({ node = { kind = BitVec bv_l; _ }; _ } as l), x)
        | BvArith
            (Add checked, x, ({ node = { kind = BitVec bv_l; _ }; _ } as l)) ),
        BitVec bv_v2 )
      when checked_has ~signed checked ->
        if Stdlib.not signed && Z.lt bv_v2 bv_l then Bool.v_false
        else if overflows ~signed bits bv_v2 bv_l Z.( - ) then
          BvCmp (Lt signed, v1, v2) <| TBool
        else lt ~signed x (sub ~checked:(checked_of_signed signed) v2 l)
    | _, BvArith (Add checked, v2, v2')
      when checked_has ~signed checked && (equal v1 v2 || equal v1 v2') ->
        (* a < a + b when + doesn't overflow is equivalent to 0 < b *)
        let b = if equal v1 v2 then v2' else v2 in
        lt ~signed (zero bits) b
    | BvArith (Add checked, v1, v1'), _
      when checked_has ~signed checked && (equal v2 v1 || equal v2 v1') ->
        (* a + b < a when + doesn't overflow is equivalent to b < 0 *)
        let b = if equal v2 v1 then v1' else v1 in
        lt ~signed b (zero bits)
    | ( ( BvArith
            (Add checked_l, ({ node = { kind = BitVec bv_l; _ }; _ } as l), y)
        | BvArith
            (Add checked_l, y, ({ node = { kind = BitVec bv_l; _ }; _ } as l))
          ),
        ( BvArith
            (Add checked_r, ({ node = { kind = BitVec bv_r; _ }; _ } as r), x)
        | BvArith
            (Add checked_r, x, ({ node = { kind = BitVec bv_r; _ }; _ } as r))
          ) )
      when checked_has ~signed checked_l && checked_has ~signed checked_r ->
        (* y + l < x + r <=> y + (l - r) < x <=> y < x + (r - l); only sound
           when the rebuilt symbolic sum is known not to overflow. *)
        let int_l = bv_to_z signed bits bv_l in
        let int_r = bv_to_z signed bits bv_r in
        let chk = checked_of_signed signed in
        if const_keeps_in_range ~base:int_l Z.(int_l - int_r) then
          lt ~signed (add ~checked:chk y (sub ~checked:chk l r)) x
        else if const_keeps_in_range ~base:int_r Z.(int_r - int_l) then
          lt ~signed y (add ~checked:chk x (sub ~checked:chk r l))
        else BvCmp (Lt signed, v1, v2) <| TBool
    | _, BitVec x when Stdlib.not signed && Z.(equal x one) ->
        (* unsigned x < 1 is x == 0 *)
        Bool.sem_eq v1 (zero bits)
    (* x < ite(b, 1, 0)
     * => ite(b, x < 1, x < 0)
     * => ite(b, x = 0, false)
     * => b && x = 0 *)
    | _, UnBool (BvOfBool n, b) when Stdlib.not signed ->
        Bool.and_ b (Bool.sem_eq v1 (zero n))
    | Ite (b, l, r), _ -> Bool.ite b (lt ~signed l v2) (lt ~signed r v2)
    | _, Ite (b, l, r) -> Bool.ite b (lt ~signed v1 l) (lt ~signed v1 r)
    | _, BitVec x
      when signed && Z.(equal x zero) && Stdlib.not (is_checked_unsigned_op v1)
      ->
        let lt_zero v =
          BvCmp (Lt signed, v, zero (size_of v.node.ty)) <| TBool
        in
        let not_eq_0 v = Bool.not (Bool.sem_eq v1 (zero (size_of v.node.ty))) in
        (* this function returns if this node is negative if we can tell, and
           otherwise the node that represents the sign bit *)
        let rec aux_lt_zero v =
          match v.node.kind with
          | UnBv (BvExtend (true, _), v) -> aux_lt_zero v
          | UnBv (BvExtend (false, _), _) -> Bool.v_false
          | BvArith (Mod, _, r) -> Bool.and_ (aux_lt_zero r) (not_eq_0 v)
          | BvArith (Rem true, l, _) -> Bool.and_ (aux_lt_zero l) (not_eq_0 v)
          | BvArith (Div true, l, r) ->
              Bool.and_ (not_eq_0 v)
                (Bool.not (Bool.sem_eq (aux_lt_zero l) (aux_lt_zero r)))
          | BvArith (BvConcat, l, _) -> aux_lt_zero l
          | UnBv (BvNot, v) -> Bool.not (aux_lt_zero v)
          | UnBool (BvOfBool n, _) when n > 1 -> Bool.v_false
          | Ite (_, l, r) ->
              let pos_l = aux_lt_zero l in
              let pos_r = aux_lt_zero r in
              if pos_l = pos_r then pos_l else lt_zero v
          | _ -> lt_zero v
        in
        aux_lt_zero v1
    | BitVec x, _ when Z.equal (bv_to_z signed bits x) (max_for signed bits) ->
        Bool.v_false
    | _, BitVec x when Z.equal (bv_to_z signed bits x) (min_for signed bits) ->
        Bool.v_false
    | BitVec x, _ when Z.equal (bv_to_z signed bits x) (min_for signed bits) ->
        Bool.not (Bool.sem_eq v1 v2)
    | _, BitVec x when Z.equal (bv_to_z signed bits x) (max_for signed bits) ->
        Bool.not (Bool.sem_eq v1 v2)
    | ( BitVec c2,
        ( BvArith (Mul checked, x, ({ node = { kind = BitVec c1; _ }; _ } as v2))
        | BvArith (Mul checked, ({ node = { kind = BitVec c1; _ }; _ } as v2), x)
          ) )
      when checked_has ~signed checked ->
        (* PROOF FOR c2 < x * c1
         * (assert (and
         *   (not (bvsmulo x c1))
         *   (not (= c1 #x00))
         *   (let
         *     (
         *       (c1neg (bvslt c1 #x00))
         *       (c2neg (bvslt c2 #x00))
         *       (divs (= (bvsrem c2 c1) #x00)))
         *     (not
         *       (= (bvslt c2 (bvmul x c1))
         *         (ite (or divs (not c2neg))
         *           (ite c1neg
         *             (or (and (= c1 #xff) (= c2 #x80))
         *               (bvslt x (bvsdiv c2 c1)))
         *             (bvslt (bvsdiv c2 c1) x))
         *           (ite c1neg
         *             (bvsle x (bvsdiv c2 c1))
         *             (bvsle (bvsdiv c2 c1) x)))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        (* be careful bc c1 = v2 and c2 = v1 in this case *)
        if Z.divisible c2 c1 || Z.geq c2 Z.zero then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_true
            else lt ~signed x (div ~signed v1 v2)
          else lt ~signed (div ~signed v1 v2) x
        else if Z.lt c1 Z.zero then leq ~signed x (div ~signed v1 v2)
        else leq ~signed (div ~signed v1 v2) x
    | ( ( BvArith (Mul checked, x, ({ node = { kind = BitVec c1; _ }; _ } as v1))
        | BvArith (Mul checked, ({ node = { kind = BitVec c1; _ }; _ } as v1), x)
          ),
        BitVec c2 )
      when checked_has ~signed checked ->
        (* PROOF FOR : x * c1 < c2
         * (assert (and
         *   (not (bvsmulo x c1))
         *   (not (= c1 #x00))
         *   (let
         *     (
         *       (c1neg (bvslt c1 #x00))
         *       (c2neg (bvslt c2 #x00))
         *       (divs (= (bvsrem c2 c1) #x00)))
         *     (not
         *       (= (bvslt (bvmul x c1) c2)
         *         (ite (or divs c2neg)
         *           (ite c1neg
         *             (and (not (and (= c1 #xff) (= c2 #x80)))
         *               (bvslt (bvsdiv c2 c1) x))
         *             (bvslt x (bvsdiv c2 c1)))
         *           (ite c1neg
         *             (bvsle (bvsdiv c2 c1) x)
         *             (bvsle x (bvsdiv c2 c1))))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        if Z.divisible c2 c1 || Z.lt c2 Z.zero then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_false
            else lt ~signed (div ~signed v2 v1) x
          else lt ~signed x (div ~signed v2 v1)
        else if Z.lt c1 Z.zero then leq ~signed (div ~signed v2 v1) x
        else leq ~signed x (div ~signed v2 v1)
    | BvArith (Mul checked_l, l1, r1), BvArith (Mul checked_r, l2, r2)
      when checked_has ~signed checked_l && checked_has ~signed checked_r ->
        (* Can only cancel common factor if it's provably non-zero *)
        let is_nonzero v = sure_neq v (zero (size_of v.node.ty)) in
        if equal l1 l2 && is_nonzero l1 then lt ~signed r1 r2
        else if equal l1 r2 && is_nonzero l1 then lt ~signed r1 l2
        else if equal r1 l2 && is_nonzero r1 then lt ~signed l1 r2
        else if equal r1 r2 && is_nonzero r1 then lt ~signed l1 l2
        else BvCmp (Lt signed, v1, v2) <| TBool
    | ( BitVec bv_v1,
        BvArith (Sub checked, x, ({ node = { kind = BitVec bv_k; _ }; _ } as k))
      )
      when checked_has ~signed checked ->
        (* v1 < x - k <=> v1 + k < x (when v1 + k doesn't overflow) *)
        if overflows ~signed bits bv_v1 bv_k Z.( + ) then
          if Stdlib.not signed then Bool.v_false
          else BvCmp (Lt signed, v1, v2) <| TBool
        else lt ~signed (add ~checked:(checked_of_signed signed) v1 k) x
    | ( BitVec bv_v1,
        BvArith (Sub checked, ({ node = { kind = BitVec bv_k; _ }; _ } as k), x)
      )
      when checked_has ~signed checked ->
        (* v1 < k - x <=> x < k - v1 (when k - v1 doesn't overflow) *)
        if overflows ~signed bits bv_k bv_v1 Z.( - ) then
          if Stdlib.not signed then Bool.v_false
          else BvCmp (Lt signed, v1, v2) <| TBool
        else lt ~signed x (sub ~checked:(checked_of_signed signed) k v1)
    | ( BvArith (Sub checked, x, ({ node = { kind = BitVec bv_k; _ }; _ } as k)),
        BitVec bv_v2 )
      when checked_has ~signed checked ->
        (* x - k < v2 <=> x < v2 + k (when v2 + k doesn't overflow) *)
        if overflows ~signed bits bv_v2 bv_k Z.( + ) then
          if Stdlib.not signed then Bool.v_true
          else BvCmp (Lt signed, v1, v2) <| TBool
        else lt ~signed x (add ~checked:(checked_of_signed signed) v2 k)
    | ( BvArith (Sub checked, ({ node = { kind = BitVec bv_k; _ }; _ } as k), x),
        BitVec bv_v2 )
      when checked_has ~signed checked ->
        (* k - x < v2 <=> k - v2 < x (when k - v2 doesn't overflow) *)
        if overflows ~signed bits bv_k bv_v2 Z.( - ) then
          if Stdlib.not signed then Bool.v_true
          else BvCmp (Lt signed, v1, v2) <| TBool
        else lt ~signed (sub ~checked:(checked_of_signed signed) k v2) x
    (* v1 <u c is true when v1's value can't reach c *)
    | _, BitVec c when Stdlib.not signed && Z.lt (unsigned_ub v1) c ->
        Bool.v_true
    (* c <u v2 is false when v2's value can't exceed c *)
    | BitVec c, _ when Stdlib.not signed && Z.leq (unsigned_ub v2) c ->
        Bool.v_false
    | BitVec c, _ when signed && is_checked_unsigned_op v2 ->
        signed_to_unsigned_cmp ~is_leq:false ~c_on_left:true c v1 v2
    | _, BitVec c when signed && is_checked_unsigned_op v1 ->
        signed_to_unsigned_cmp ~is_leq:false ~c_on_left:false c v1 v2
    | _ -> BvCmp (Lt signed, v1, v2) <| TBool

  and leq ~signed v1 v2 =
    assert (equal_ty v1.node.ty v2.node.ty);
    let bits = size_of v1.node.ty in
    match (v1.node.kind, v2.node.kind) with
    | _ when equal v1 v2 -> Bool.v_true
    | BitVec l, BitVec r ->
        Bool.of_bool @@ Z.leq (bv_to_z signed bits l) (bv_to_z signed bits r)
    (* -a <= -b <=> b <= a, when neither negation overflows *)
    | UnBv (Neg true, a), UnBv (Neg true, b) when signed -> leq ~signed b a
    (* -a <= c <=> -c <= a, when -a doesn't overflow and c <> INT_MIN *)
    | UnBv (Neg true, a), BitVec c
      when signed
           && Stdlib.not (Z.equal (bv_to_z signed bits c) (min_for signed bits))
      ->
        leq ~signed (neg v2) a
    (* c <= -a <=> a <= -c, when -a doesn't overflow and c <> INT_MIN *)
    | BitVec c, UnBv (Neg true, a)
      when signed
           && Stdlib.not (Z.equal (bv_to_z signed bits c) (min_for signed bits))
      ->
        leq ~signed a (neg v1)
    | ( BitVec bv_v1,
        ( BvArith
            (Add checked, ({ node = { kind = BitVec bv_r; _ }; _ } as r), x)
        | BvArith
            (Add checked, x, ({ node = { kind = BitVec bv_r; _ }; _ } as r)) ) )
      when checked_has ~signed checked ->
        if Stdlib.not signed && Z.lt bv_v1 bv_r then Bool.v_true
        else if overflows ~signed bits bv_v1 bv_r Z.( - ) then
          BvCmp (Leq signed, v1, v2) <| TBool
        else leq ~signed (sub ~checked:(checked_of_signed signed) v1 r) x
    | ( ( BvArith
            (Add checked, ({ node = { kind = BitVec bv_l; _ }; _ } as l), x)
        | BvArith
            (Add checked, x, ({ node = { kind = BitVec bv_l; _ }; _ } as l)) ),
        BitVec bv_v2 )
      when checked_has ~signed checked ->
        if Stdlib.not signed && Z.lt bv_v2 bv_l then Bool.v_false
        else if overflows ~signed bits bv_v2 bv_l Z.( - ) then
          BvCmp (Leq signed, v1, v2) <| TBool
        else leq ~signed x (sub ~checked:(checked_of_signed signed) v2 l)
    | ( ( BvArith
            (Add checked_l, ({ node = { kind = BitVec bv_l; _ }; _ } as l), y)
        | BvArith
            (Add checked_l, y, ({ node = { kind = BitVec bv_l; _ }; _ } as l))
          ),
        ( BvArith
            (Add checked_r, ({ node = { kind = BitVec bv_r; _ }; _ } as r), x)
        | BvArith
            (Add checked_r, x, ({ node = { kind = BitVec bv_r; _ }; _ } as r))
          ) )
      when checked_has ~signed checked_l && checked_has ~signed checked_r ->
        (* y + l <= x + r <=> y + (l - r) <= x <=> y <= x + (r - l); only sound
           when the rebuilt symbolic sum is known not to overflow. *)
        let int_l = bv_to_z signed bits bv_l in
        let int_r = bv_to_z signed bits bv_r in
        let chk = checked_of_signed signed in
        if const_keeps_in_range ~base:int_l Z.(int_l - int_r) then
          leq ~signed (add ~checked:chk y (sub ~checked:chk l r)) x
        else if const_keeps_in_range ~base:int_r Z.(int_r - int_l) then
          leq ~signed y (add ~checked:chk x (sub ~checked:chk r l))
        else BvCmp (Leq signed, v1, v2) <| TBool
    | _, BvArith (Add checked, v2, v2')
      when checked_has ~signed checked && (equal v1 v2 || equal v1 v2') ->
        (* a <= b + a when + doesn't overflow is equivalent to 0 <= b *)
        let b = if equal v1 v2 then v2' else v2 in
        leq ~signed (zero bits) b
    | BvArith (Add checked, v1, v1'), _
      when checked_has ~signed checked && (equal v2 v1 || equal v2 v1') ->
        (* a + b <= a when + doesn't overflow is equivalent to b <= 0 *)
        let b = if equal v2 v1 then v1' else v1 in
        leq ~signed b (zero bits)
    | BitVec x, _ when Z.equal (bv_to_z signed bits x) (min_for signed bits) ->
        Bool.v_true
    | _, BitVec x when Z.equal (bv_to_z signed bits x) (max_for signed bits) ->
        Bool.v_true
    | ( BitVec c2,
        ( BvArith (Mul checked, x, ({ node = { kind = BitVec c1; _ }; _ } as v2))
        | BvArith (Mul checked, ({ node = { kind = BitVec c1; _ }; _ } as v2), x)
          ) )
      when checked_has ~signed checked ->
        (* PROOF FOR : c2 <= x * c1
         * (assert (and
         *   (not (bvsmulo x c1))
         *   (not (= c1 #x00))
         *   (let
         *     (
         *       (c1neg (bvslt c1 #x00))
         *       (c2neg (bvslt c2 #x00))
         *       (divs (= (bvsrem c2 c1) #x00)))
         *       (not
         *         (= (bvsle c2 (bvmul x c1) )
         *           (ite divs
         *             (ite c1neg
         *               (or (and (= c1 #xff) (= c2 #x80))
         *                 (bvsle x (bvsdiv c2 c1)))
         *               (bvsle (bvsdiv c2 c1) x))
         *             (ite c1neg
         *               (ite c2neg
         *                 (bvsle x (bvsdiv c2 c1))
         *                 (bvslt x (bvsdiv c2 c1)))
         *               (ite c2neg
         *                 (bvsle (bvsdiv c2 c1) x)
         *                 (bvslt (bvsdiv c2 c1) x))))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        (* be careful bc c1 = v2 and c2 = v1 in this case *)
        if Z.divisible c2 c1 then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_true
            else leq ~signed x (div ~signed v1 v2)
          else leq ~signed (div ~signed v1 v2) x
        else if Z.lt c1 Z.zero then
          if Z.lt c2 Z.zero then leq ~signed x (div ~signed v1 v2)
          else lt ~signed x (div ~signed v1 v2)
        else if Z.lt c2 Z.zero then leq ~signed (div ~signed v1 v2) x
        else lt ~signed (div ~signed v1 v2) x
    | ( ( BvArith (Mul checked, x, ({ node = { kind = BitVec c1; _ }; _ } as v1))
        | BvArith (Mul checked, ({ node = { kind = BitVec c1; _ }; _ } as v1), x)
          ),
        BitVec c2 )
      when checked_has ~signed checked ->
        (* PROOF FOR : x * c1 <= c2
         * (assert (and
         *   (not (bvsmulo x c1))
         *   (not (= c1 #x00))
         *   (let
         *     (
         *       (c1neg (bvslt c1 #x00))
         *       (c2neg (bvslt c2 #x00))
         *       (divs (= (bvsrem c2 c1) #x00)))
         *     (not
         *       (= (bvsle (bvmul x c1) c2)
         *         (ite divs
         *           (ite c1neg
         *             (and (not (and (= c1 #xff) (= c2 #x80)))
         *               (bvsle (bvsdiv c2 c1) x))
         *             (bvsle x (bvsdiv c2 c1)))
         *           (ite c1neg
         *             (ite c2neg
         *               (bvslt (bvsdiv c2 c1) x)
         *               (bvsle (bvsdiv c2 c1) x))
         *             (ite c2neg
         *               (bvslt x (bvsdiv c2 c1))
         *               (bvsle x (bvsdiv c2 c1)))))))))) *)
        let c1 = bv_to_z signed bits c1 in
        let c2 = bv_to_z signed bits c2 in
        if Z.divisible c2 c1 then
          if Z.lt c1 Z.zero then
            if
              signed
              && Z.equal c1 (Z.of_int (-1))
              && Z.equal c2 (min_for signed bits)
            then Bool.v_false
            else leq ~signed (div ~signed v2 v1) x
          else leq ~signed x (div ~signed v2 v1)
        else if Z.lt c1 Z.zero then
          if Z.lt c2 Z.zero then lt ~signed (div ~signed v2 v1) x
          else leq ~signed (div ~signed v2 v1) x
        else if Z.lt c2 Z.zero then lt ~signed x (div ~signed v2 v1)
        else leq ~signed x (div ~signed v2 v1)
    | BvArith (Mul checked_l, l1, r1), BvArith (Mul checked_r, l2, r2)
      when checked_has ~signed checked_l && checked_has ~signed checked_r ->
        (* Can only cancel common factor if it's provably non-zero *)
        let is_nonzero v = sure_neq v (zero (size_of v.node.ty)) in
        if equal l1 l2 && is_nonzero l1 then leq ~signed r1 r2
        else if equal l1 r2 && is_nonzero l1 then leq ~signed r1 l2
        else if equal r1 l2 && is_nonzero r1 then leq ~signed l1 r2
        else if equal r1 r2 && is_nonzero r1 then leq ~signed l1 l2
        else BvCmp (Leq signed, v1, v2) <| TBool
    | BvArith (Div false, _, { node = { kind = BitVec d; _ }; _ }), BitVec n
      when Stdlib.not signed && Z.(gt (mul n d) (max_for false bits)) ->
        Bool.v_true
    | Ite (b, l, r), BitVec _ ->
        Bool.ite b (leq ~signed l v2) (leq ~signed r v2)
    | BitVec _, Ite (b, l, r) ->
        Bool.ite b (leq ~signed v1 l) (leq ~signed v1 r)
    | ( BitVec bv_v1,
        BvArith (Sub checked, x, ({ node = { kind = BitVec bv_k; _ }; _ } as k))
      )
      when checked_has ~signed checked ->
        (* v1 <= x - k <=> v1 + k <= x (when v1 + k doesn't overflow) *)
        if overflows ~signed bits bv_v1 bv_k Z.( + ) then
          if Stdlib.not signed then Bool.v_false
          else BvCmp (Leq signed, v1, v2) <| TBool
        else leq ~signed (add ~checked:(checked_of_signed signed) v1 k) x
    | ( BitVec bv_v1,
        BvArith (Sub checked, ({ node = { kind = BitVec bv_k; _ }; _ } as k), x)
      )
      when checked_has ~signed checked ->
        (* v1 <= k - x <=> x <= k - v1 (when k - v1 doesn't overflow) *)
        if overflows ~signed bits bv_k bv_v1 Z.( - ) then
          if Stdlib.not signed then Bool.v_false
          else BvCmp (Leq signed, v1, v2) <| TBool
        else leq ~signed x (sub ~checked:(checked_of_signed signed) k v1)
    | ( BvArith (Sub checked, x, ({ node = { kind = BitVec bv_k; _ }; _ } as k)),
        BitVec bv_v2 )
      when checked_has ~signed checked ->
        (* x - k <= v2 <=> x <= v2 + k (when v2 + k doesn't overflow) *)
        if overflows ~signed bits bv_v2 bv_k Z.( + ) then
          if Stdlib.not signed then Bool.v_true
          else BvCmp (Leq signed, v1, v2) <| TBool
        else leq ~signed x (add ~checked:(checked_of_signed signed) v2 k)
    | ( BvArith (Sub checked, ({ node = { kind = BitVec bv_k; _ }; _ } as k), x),
        BitVec bv_v2 )
      when checked_has ~signed checked ->
        (* k - x <= v2 <=> k - v2 <= x (when k - v2 doesn't overflow) *)
        if overflows ~signed bits bv_k bv_v2 Z.( - ) then
          if Stdlib.not signed then Bool.v_true
          else BvCmp (Leq signed, v1, v2) <| TBool
        else leq ~signed (sub ~checked:(checked_of_signed signed) k v2) x
    (* v1 <=u c is true when v1's value can't exceed c *)
    | _, BitVec c when Stdlib.not signed && Z.leq (unsigned_ub v1) c ->
        Bool.v_true
    (* c <=u v2 is false when v2's value can't reach c *)
    | BitVec c, _ when Stdlib.not signed && Z.lt (unsigned_ub v2) c ->
        Bool.v_false
    | BitVec c, _ when signed && is_checked_unsigned_op v2 ->
        signed_to_unsigned_cmp ~is_leq:true ~c_on_left:true c v1 v2
    | _, BitVec c when signed && is_checked_unsigned_op v1 ->
        signed_to_unsigned_cmp ~is_leq:true ~c_on_left:false c v1 v2
    | _ -> BvCmp (Leq signed, v1, v2) <| TBool

  and signed_to_unsigned_cmp ~is_leq ~c_on_left c v1 v2 =
    (* Rewrites a signed comparison [v1 R v2] (with [R] being [<=] when
       [is_leq], else [<]) where the concrete operand [c] is on the [c_on_left]
       side and the other operand is a checked-unsigned operation, into an
       equivalent unsigned formula. This lets the checked-unsigned reductions
       fire. Splitting the non-constant operand [X] at the sign threshold [s =
       2^(bits-1)] (so [X <u s] is its non-negative half), and with [c_cmp] the
       same comparison taken unsigned: - [c] on the left: [c R X <=> c_cmp && X
       <u s] if [c >=s 0] [c R X <=> X <u s || c_cmp] if [c <s 0] - [c] on the
       right: [X R c <=> c_cmp || s <=u X] if [c >=s 0] [X R c <=> s <=u X &&
       c_cmp] if [c <s 0] *)
    let bits = size_of v1.node.ty in
    let sign_bit = mk bits Z.(one lsl Stdlib.( - ) bits 1) in
    let c_cmp =
      if is_leq then leq ~signed:false v1 v2 else lt ~signed:false v1 v2
    in
    let nonneg = Z.geq (bv_to_z true bits c) Z.zero in
    if c_on_left then
      let in_pos = lt ~signed:false v2 sign_bit in
      if nonneg then Bool.and_ c_cmp in_pos else Bool.or_ in_pos c_cmp
    else
      let in_neg = leq ~signed:false sign_bit v1 in
      if nonneg then Bool.or_ c_cmp in_neg else Bool.and_ in_neg c_cmp

  let gt ~signed v1 v2 = lt ~signed v2 v1
  let geq ~signed v1 v2 = leq ~signed v2 v1

  let add_overflows ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> ovf_check ~signed (size_of v1.node.ty) l r Z.( + )
    | BitVec z, _ when Z.equal z Z.zero -> Bool.v_false
    | _, BitVec z when Z.equal z Z.zero -> Bool.v_false
    | _ when size_of v1.node.ty == 1 ->
        let one = one 1 in
        Bool.and_ (Bool.sem_eq v1 one) (Bool.sem_eq v2 one)
    | BitVec z, _ when Stdlib.not signed ->
        let n = size_of v1.node.ty in
        let m = max_for signed n in
        gt ~signed v2 (mk n Z.(m - z))
    | _, BitVec z when Stdlib.not signed ->
        let n = size_of v1.node.ty in
        let m = max_for signed n in
        gt ~signed v1 (mk n Z.(m - z))
    | (BitVec z, x | x, BitVec z) when signed ->
        let x = if x == v1.node.kind then v1 else v2 in
        let n = size_of v1.node.ty in
        let z = bv_to_z signed n z in
        if Z.gt z Z.zero then
          (* z > 0 so overflows if max - z < x *)
          let max = max_for signed n in
          gt ~signed x (mk n (Z.sub max z))
        else
          (* z < 0 so overflows if x < min - z *)
          let min = min_for signed n in
          lt ~signed x (mk_masked n (Z.sub min z))
    | UnBool (BvOfBool n, b1), UnBool (BvOfBool _, b2) ->
        if signed && n == 2 then
          (* Signed addition of two booleans of size 2 overflows iff they are
             both true *)
          Bool.and_ b1 b2
        else Bool.v_false
    | UnBool (BvOfBool _, b), other | other, UnBool (BvOfBool _, b) ->
        (* ite(b, 1, 0) + x only overflows if b && x == max *)
        let n = size_of v1.node.ty in
        let max = max_for signed n in
        let other = other <| t_bv n in
        Bool.and_ b (Bool.sem_eq other (mk n max))
    | _ -> mk_commut_cmp (AddOvf signed) v1 v2 <| TBool

  let mul_overflows ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> ovf_check ~signed (size_of v1.node.ty) l r Z.( * )
    | _ when signed && size_of v1.node.ty == 1 ->
        (* We need to special-case size one because the other simplifications
           will mess with that case, and it's pretty easy to simplify. *)
        let one = one 1 in
        Bool.and_ (Bool.sem_eq v1 one) (Bool.sem_eq v2 one)
    | _
      when if signed then msb_of v1 + msb_of v2 < size_of v1.node.ty - 2
           else msb_of v1 + msb_of v2 < size_of v1.node.ty - 1 ->
        Bool.v_false
    | BitVec z, x | x, BitVec z ->
        (* z is a known constant *)
        if Z.equal z Z.zero || Z.equal z Z.one then Bool.v_false
        else
          let n = size_of v1.node.ty in
          let z = bv_to_z signed n z in
          if signed then
            (* For signed overflow, the correct condition is: z * x overflows
               iff x < min_x or x > max_x, where min_x = ceil((-2^(n-1))/z),
               max_x = floor((2^(n-1)-1)/z) for z > 0, and swapped for z < 0. *)
            let min_val = Z.neg (Z.shift_left Z.one (n - 1)) in
            let max_val = Z.pred (Z.shift_left Z.one (n - 1)) in
            if Z.equal z (Z.of_int (-1)) then
              (* z = -1: only overflows when x = MIN_VALUE *)
              Bool.sem_eq (x <| v1.node.ty) (mk_masked n min_val)
            else
              let min_x, max_x =
                if Z.gt z Z.zero then (* z > 0 *)
                  let min_x = Z.(min_val / z) in
                  let max_x = Z.(max_val / z) in
                  (min_x, max_x)
                else (* z < 0 *)
                  let min_x = Z.(max_val / z) in
                  let max_x = Z.(min_val / z) in
                  (min_x, max_x)
              in
              Bool.or_
                (lt ~signed (x <| v1.node.ty) (mk_masked n min_x))
                (gt ~signed (x <| v1.node.ty) (mk_masked n max_x))
          else
            (* For unsigned overflow, * z * x overflows iff x > floor((2^n - 1)
               / z) *)
            let maxn = Z.pred (Z.shift_left Z.one n) in
            let bound = Z.(maxn / z) in
            gt ~signed (x <| v1.node.ty) (mk n bound)
    (* `x * (y / x)` cannot overflow with unsigned ops *)
    | _, BvArith (Div false, _, v2) when Stdlib.not signed && equal v1 v2 ->
        Bool.v_false
    | BvArith (Div false, _, v1), _ when Stdlib.not signed && equal v1 v2 ->
        Bool.v_false
    | _ -> mk_commut_cmp (MulOvf signed) v1 v2 <| TBool

  let neg_overflows v =
    let n = size_of v.node.ty in
    Bool.sem_eq (mk_masked n (min_for true n)) v

  let sub_overflows ~signed v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | BitVec l, BitVec r -> ovf_check ~signed (size_of v1.node.ty) l r Z.( - )
    | _ when equal v1 v2 -> Bool.v_false
    | _ ->
        if Stdlib.not signed then lt ~signed v1 v2
        else BvCmp (SubOvf signed, v1, v2) <| TBool

  let of_float ~rounding ~signed ~size v =
    UnFloat (BvOfFloat (rounding, signed, size), v) <| t_bv size

  let to_float ~rounding ~signed ~fp v =
    UnBv (FloatOfBv (rounding, signed, fp), v) <| t_float fp

  let to_float_raw v =
    let fp = FloatPrecision.of_size (size_of v.node.ty) in
    UnBv (FloatOfBvRaw fp, v) <| t_float fp
end

(** {2 Floating point} *)
and Float : Float = struct
  let f2str = Stdlib.Float.to_string
  let str2f = Stdlib.Float.of_string
  let mk fp f = Float f <| t_float fp
  let mk_f fp f = Float (f2str f) <| t_float fp
  let like v f = Float (f2str f) <| v.node.ty
  let fp_of (v : sfloat t) = match v.node.ty with TFloat fp -> fp
  let f16 f = mk_f F16 f
  let f32 f = mk_f F32 f
  let f64 f = mk_f F64 f
  let f128 f = mk_f F128 f

  let[@inline] is_floatclass fc =
   fun sv ->
    match sv.node.kind with
    | Float f ->
        Bool.of_bool (FloatClass.as_fpclass fc = classify_float (str2f f))
    | _ -> UnFloat (FIs fc, sv) <| TBool

  let is_normal = is_floatclass Normal
  let is_subnormal = is_floatclass Subnormal
  let is_infinite = is_floatclass Infinite
  let is_nan = is_floatclass NaN
  let is_zero = is_floatclass Zero

  let eq v1 v2 =
    if equal v1 v2 then Bool.not (is_nan v1)
    else mk_commut_fcmp FEq v1 v2 <| TBool

  let lt v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Float f1, Float f2 -> Bool.of_bool (str2f f1 < str2f f2)
    | _ -> FCmp (FLt, v1, v2) <| TBool

  let leq v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Float f1, Float f2 -> Bool.of_bool (str2f f1 <= str2f f2)
    | _ -> FCmp (FLeq, v1, v2) <| TBool

  let gt v1 v2 = lt v2 v1
  let geq v1 v2 = leq v2 v1
  let add v1 v2 = mk_commut_farith FAdd v1 v2 <| v1.node.ty
  let sub v1 v2 = FArith (FSub, v1, v2) <| v1.node.ty
  let div v1 v2 = FArith (FDiv, v1, v2) <| v1.node.ty
  let mul v1 v2 = mk_commut_farith FMul v1 v2 <| v1.node.ty
  let rem v1 v2 = FArith (FRem, v1, v2) <| v1.node.ty

  let abs v =
    match v.node.kind with
    | UnFloat (FAbs, _) -> v
    | _ -> UnFloat (FAbs, v) <| v.node.ty

  let neg v =
    let fp = fp_of v in
    FArith (FSub, mk fp "0.0", v) <| v.node.ty

  let round rm sv = UnFloat (FRound rm, sv) <| sv.node.ty
end

(** {2 Pointers} *)

module Ptr = struct
  let mk l o =
    assert (size_of l.node.ty = size_of o.node.ty);
    Ptr (l, o) <| TPointer (size_of o.node.ty)

  let loc p =
    match p.node.kind with
    | Ptr (l, _) -> l
    | _ -> UnPtr (GetPtrLoc, p) <| TLoc (size_of p.node.ty)

  let null_loc n = LocLit Z.zero <| TLoc n
  let is_null_loc l = Bool.sem_eq l (null_loc (size_of l.node.ty))
  let loc_of_z n z = LocLit z <| TLoc n
  let loc_of_int n i = loc_of_z n (Z.of_int i)

  let ofs p =
    match p.node.kind with
    | Ptr (_, o) -> o
    | _ ->
        let n = size_of p.node.ty in
        UnPtr (GetPtrOfs, p) <| TBitVector n

  let decompose p =
    match p.node.kind with Ptr (l, o) -> (l, o) | _ -> (loc p, ofs p)

  let add_ofs p o =
    let loc, ofs = decompose p in
    mk loc (BitVec.add ofs o)

  let null n = mk (null_loc n) (BitVec.zero n)
  let is_null p = Bool.sem_eq p (null (size_of p.node.ty))
  let is_at_null_loc p = is_null_loc (loc p)
end

(** {2 Sequences} *)

module SSeq = struct
  let mk ~seq_ty l = Seq l <| seq_ty

  let inner_ty : type a. a sseq ty -> a ty =
   fun ty -> match ty with TSeq ty -> ty
end

(** {2 General constructors} *)

(** {2 Infix operators} *)

module Infix = struct
  let bv_z = BitVec.mk
  let ptr = Ptr.mk
  let seq = SSeq.mk
  let ( ==@ ) = Bool.sem_eq
  let ( ==?@ ) = Bool.sem_eq_untyped
  let ( &&@ ) = Bool.and_
  let ( ||@ ) = Bool.or_
  let ( >@ ) = BitVec.gt ~signed:false
  let ( >=@ ) = BitVec.geq ~signed:false
  let ( <@ ) = BitVec.lt ~signed:false
  let ( <=@ ) = BitVec.leq ~signed:false
  let ( >$@ ) = BitVec.gt ~signed:true
  let ( >=$@ ) = BitVec.geq ~signed:true
  let ( <$@ ) = BitVec.lt ~signed:true
  let ( <=$@ ) = BitVec.leq ~signed:true
  let ( +@ ) = BitVec.add ~checked:unchecked
  let ( -@ ) = BitVec.sub ~checked:unchecked
  let ( ~- ) v = BitVec.neg v
  let ( *@ ) = BitVec.mul ~checked:unchecked
  let ( /@ ) = BitVec.div ~signed:false
  let ( /$@ ) = BitVec.div ~signed:true
  let ( %@ ) = BitVec.rem ~signed:false
  let ( %$@ ) = BitVec.rem ~signed:true
  let ( <<@ ) = BitVec.shl
  let ( >>@ ) = BitVec.lshr
  let ( >>>@ ) = BitVec.ashr
  let ( ^@ ) = BitVec.xor
  let ( &@ ) = BitVec.and_
  let ( |@ ) = BitVec.or_
  let ( ==.@ ) = Float.eq
  let ( >.@ ) = Float.gt
  let ( >=.@ ) = Float.geq
  let ( <.@ ) = Float.lt
  let ( <=.@ ) = Float.leq
  let ( +.@ ) = Float.add
  let ( -.@ ) = Float.sub
  let ( *.@ ) = Float.mul
  let ( /.@ ) = Float.div
end
