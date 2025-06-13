include Stdlib.Map.Make (Ail_helpers.Symbol_std)

type ctype = Cerb_frontend.Ctype.ctype

let pp_ctype = Fmt_ail.pp_ty

type binding_kind =
  | Stackptr of Typed.T.sptr Typed.t
  | Value of Typed.T.cval Typed.t
  | Uninit
[@@deriving show { with_path = false }]

type binding = { kind : binding_kind option; ty : ctype }
[@@deriving show { with_path = false }]

type nonrec t = binding t

let reserve sym ty =
  let binding = { kind = None; ty } in
  add sym binding

let add_value sym value ty t =
  let kind = Some (Value value) in
  let binding = { kind; ty } in
  add sym binding t

let add_stackptr sym sptr ty t =
  let kind = Some (Stackptr sptr) in
  let binding = { kind; ty } in
  add sym binding t

let declare sym kind t =
  update sym
    (function
      | None -> failwith "Store: Assigning unknown symbol?"
      | Some { kind = Some _; _ } -> failwith "Store: Redefining symbol?"
      | Some { kind = None; ty } ->
          let kind = Some kind in
          Some { kind; ty })
    t

let declare_value sym value t = declare sym (Value value) t
let declare_uninit sym t = declare sym Uninit t
let pp : t Fmt.t = Fmt.Dump.iter_bindings iter Fmt.nop Fmt_ail.pp_sym pp_binding
