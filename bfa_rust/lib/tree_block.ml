(* This could be parametric on an integer type and Node type. But is there realy a reason ? *)

open Rustsymex.Syntax
open Typed
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Charon

include
  Bfa_symex.Tree_block.Make (L) (Rustsymex)
    (struct
      module Symex = Rustsymex
      module Syntax = Typed.Infix

      type sint = Typed.T.sint

      let zero = 0s

      module Leaf = struct
        type t = { value : T.cval Typed.t; ty : Values.literal_type }
        type out_ty = T.cval
        type ty = Values.literal_type

        let pp ft t =
          let open Fmt in
          pf ft "%a : %s" Typed.ppa t.value (Charon_util.lit_to_string t.ty)

        let pp_ty fmt ty = Fmt.pf fmt "%s" (Charon_util.lit_to_string ty)
        let size_of ty = Typed.int @@ Layout.size_of_literal_ty ty

        let split_at at node =
          Fmt.kstr not_impl "Splitting %a at %a" pp node Typed.ppa at

        let init value ty = { value; ty }

        let decode ty { value; ty = tyw } =
          if Values.equal_literal_type ty tyw then Result.ok value
          else
            match Layout.constraints ty with
            | Some constrs ->
                if%sat Typed.conj (constrs value) then Result.ok value
                else Result.error `UBTransmute
            | None ->
                Fmt.kstr not_impl
                  "Couldn't convert type, contraints unknown for %s -> %s"
                  (Charon_util.lit_to_string ty)
                  (Charon_util.lit_to_string tyw)

        let to_zeros _ = 0s
        let is_zeros { value; _ } = [ value ==@ 0s ]
        let constrs ty = Option.get @@ Layout.constraints ty

        type serialized = {
          ty : Types.literal_type; [@printer Types.pp_literal_type]
          v : T.cval Typed.t;
        }
        [@@deriving show { with_path = false }]

        let serialize { ty; value } = { ty; v = value }
        let deserialize { v; _ } = return v
        let ty_of { ty; _ } = ty
        let iter_vars { v; _ } f = Typed.iter_vars v f

        let nondet_serialize ty =
          let+ v = Layout.nondet_literal_ty ty in
          { ty; v }

        let subst_serialized f { v; ty } =
          let v = Typed.subst f v in
          { ty; v }
      end
    end)
