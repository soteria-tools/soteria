open Charon
open Typed.Syntax

module Archi = struct
  let word_size = 8
end

type layout = {
  size : int;
  align : int; (* members_ofs : (CF.Symbol.identifier * int) Array.t; *)
}

let is_int : Types.ty -> bool = function
  | TLiteral (TInteger _) -> true
  | _ -> false

let size_of_int_ty : Types.integer_type -> int = function
  | I128 | U128 -> 16
  | I64 | U64 -> 8
  | I32 | U32 -> 4
  | I16 | U16 -> 2
  | I8 | U8 -> 1
  | Isize | Usize -> Archi.word_size

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_int_ty : Types.integer_type -> int = size_of_int_ty

let layout_of : Types.ty -> layout option = function
  | TLiteral (TInteger inty) ->
      let size = size_of_int_ty inty in
      let align = align_of_int_ty inty in
      Some { size; align }
  | ty ->
      L.debug (fun m -> m "Cannot compute layout of %a" Types.pp_ty ty);
      None

let size_of_s ty =
  match layout_of ty with
  | Some { size; _ } -> Rustsymex.return (Typed.int size)
  | None ->
      Fmt.kstr Rustsymex.not_impl "Cannot yet compute size of type %a"
        Types.pp_ty ty

let int_constraints (int_ty : Types.integer_type) =
  let open Typed.Infix in
  match int_ty with
  | I128 | I64 | I32 | I16 | I8 | Isize ->
      let size = size_of_int_ty int_ty in
      let min = Z.neg (Z.shift_left Z.one ((size * 8) - 1)) in
      let max = Z.pred (Z.shift_left Z.one ((size * 8) - 1)) in
      fun x -> [ Typed.int_z min <=@ x; x <=@ Typed.int_z max ]
  | U128 | U64 | U32 | U16 | U8 | Usize ->
      let size = size_of_int_ty int_ty in
      let max = Z.pred (Z.shift_left Z.one (size * 8)) in
      fun x -> [ 0s <=@ x; x <=@ Typed.int_z max ]

let constraints :
    Types.ty -> (Typed.T.cval Typed.t -> Typed.T.sbool Typed.t list) option =
  let open Typed.Infix in
  function
  | TNever -> Some (fun x -> [ x ==@ 0s ])
  | TRawPtr _ -> Some (fun _ -> [])
  | TLiteral (TInteger ity) ->
      let constrs = int_constraints ity in
      Some
        (fun x ->
          match Typed.cast_checked x Typed.t_int with
          | None -> [ Typed.v_false ]
          | Some x -> constrs x)
  | ty ->
      L.info (fun m ->
          m "No constraints implemented for type %a" Types.pp_ty ty);
      None

let nondet_ty : Types.ty -> Typed.T.cval Typed.t Rustsymex.t =
  let open Rustsymex.Syntax in
  function
  | TNever -> Rustsymex.return 0s
  | TRawPtr _ ->
      let* loc = Rustsymex.nondet Typed.t_loc in
      let* ofs = Rustsymex.nondet Typed.t_int in
      Rustsymex.return (Typed.Ptr.mk loc ofs)
  | TLiteral (TInteger ity) ->
      let constrs = int_constraints ity in
      let+ res = Rustsymex.nondet ~constrs Typed.t_int in
      (res :> Typed.T.cval Typed.t)
  | ty ->
      Rustsymex.not_impl
        (Fmt.str "nondet_ty: unsupported type %a" Types.pp_ty ty)
