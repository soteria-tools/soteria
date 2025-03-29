(* This could be parametric on an integer type and Node type. But is there realy a reason ? *)

open Csymex.Syntax
open Typed
open Typed.Infix
open Typed.Syntax
open Csymex
module Ctype = Cerb_frontend.Ctype

include
  Bfa_symex.Tree_block.Make (L) (Csymex)
    (struct
      module Symex = Csymex
      module Syntax = Typed.Infix

      type sint = Typed.T.sint

      let zero = 0s

      module Leaf = struct
        type t = { value : T.cval Typed.t; ty : Ctype.ctype }
        type out_ty = T.cval
        type ty = Ctype.ctype

        let pp ft t =
          let open Fmt in
          pf ft "%a : %a" Typed.ppa t.value Fmt_ail.pp_ty t.ty

        let pp_ty = Fmt_ail.pp_ty

        let size_of ty =
          match Layout.layout_of ty with
          | Some layout -> Typed.int @@ layout.size
          | None -> Fmt.failwith "Unhandled type: %a" Fmt_ail.pp_ty ty

        let split_at at node =
          Fmt.kstr not_impl "Splitting %a at %a" pp node Typed.ppa at

        let init value ty = { value; ty }

        let decode ty { value; ty = tyw } =
          if Ctype.ctypeEqual ty tyw then Result.ok value
          else
            Fmt.kstr not_impl "Type mismatch when decoding value: %a vs %a"
              Fmt_ail.pp_ty ty Fmt_ail.pp_ty tyw

        let to_zeros _ = 0s
        let is_zeros { value; _ } = [ value ==@ 0s ]
        let constrs ty = Option.get @@ Layout.constraints ty

        type serialized = {
          ty : Ctype.ctype; [@printer Fmt_ail.pp_ty]
          v : T.cval Typed.t;
        }
        [@@deriving show { with_path = false }]

        let serialize { ty; value } = { ty; v = value }
        let deserialize { v; _ } = return v
        let ty_of { ty; _ } = ty
        let iter_vars { v; _ } f = Typed.iter_vars v f

        let nondet_serialize ty =
          let+ v = Layout.nondet_c_ty ty in
          { ty; v }

        let subst_serialized f { v; ty } =
          let v = Typed.subst f v in
          { ty; v }
      end
    end)
