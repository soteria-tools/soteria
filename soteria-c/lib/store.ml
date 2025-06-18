include Symbol_std.Map

type ctype = Cerb_frontend.Ctype.ctype

let pp_ctype = Fmt_ail.pp_ty

type binding_kind =
  | Stackptr of Typed.T.sptr Typed.t
  | Value of Typed.T.cval Typed.t
  | Uninit
[@@deriving show { with_path = false }]

type binding = { kind : binding_kind; ty : ctype }
[@@deriving show { with_path = false }]

type nonrec t = binding t

let reserve sym ty =
  let binding = { kind = Uninit; ty } in
  add sym binding

let add_value sym value ty t =
  let binding = { kind = Value value; ty } in
  add sym binding t

let add_stackptr sym sptr ty t =
  let binding = { kind = Stackptr sptr; ty } in
  add sym binding t

let declare_value sym value t =
  update sym
    (function
      | None -> failwith "Store: Assigning unknown symbol?"
      | Some { kind = _; ty } -> Some { kind = Value value; ty })
    t

let pp : t Fmt.t = Fmt.Dump.iter_bindings iter Fmt.nop Fmt_ail.pp_sym pp_binding
