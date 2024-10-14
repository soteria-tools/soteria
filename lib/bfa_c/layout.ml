open Cerb_frontend.Ctype

module Archi = struct
  let word_size = 8
end

let is_int (Ctype (_, ty)) =
  match ty with Basic (Integer _) -> true | _ -> false

let size_of_int_ty (int_ty : integerType) =
  Cerb_frontend.Ocaml_implementation.DefaultImpl.impl.sizeof_ity int_ty

let size_of (Ctype (_, ty)) =
  match ty with Basic (Integer inty) -> size_of_int_ty inty | _ -> None

let size_of_s ty =
  match size_of ty with
  | Some size -> Csymex.return (Svalue.int size)
  | None ->
      Fmt.kstr Csymex.not_impl "Cannot yet compute size of type %a"
        Fmt_ail.pp_ty ty

let int_constraints (int_ty : integerType) =
  let open Svalue.Infix in
  let open Syntaxes.Option in
  match int_ty with
  | Char -> Some (fun x -> [ Svalue.zero #<= x; x #< (Svalue.int 256) ])
  | Bool -> Some (fun x -> [ Svalue.zero #<= x; x #< (Svalue.int 2) ])
  | Signed _ ->
      let+ size = size_of_int_ty int_ty in
      let min = Z.neg (Z.shift_left Z.one ((size * 8) - 1)) in
      let max = Z.pred (Z.shift_left Z.one ((size * 8) - 1)) in
      fun x -> [ (Svalue.int_z min) #<= x; x #<= (Svalue.int_z max) ]
  | Unsigned _ ->
      let+ size = size_of_int_ty int_ty in
      let max = Z.pred (Z.shift_left Z.one (size * 8)) in
      fun x -> [ Svalue.zero #<= x; x #<= (Svalue.int_z max) ]
  | _ -> None
