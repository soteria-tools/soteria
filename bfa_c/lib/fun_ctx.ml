module Bidirectional_map = struct
  module SMap = Map.Make (Ail_helpers.Symbol_std)
  module IMap = Map.Make (Z)

  type t = { s_to_i : Z.t SMap.t; i_to_s : Ail_helpers.Symbol_std.t IMap.t }

  let empty = { s_to_i = SMap.empty; i_to_s = IMap.empty }

  let add s i map =
    let s_to_i = SMap.add s i map.s_to_i in
    let i_to_s = IMap.add i s map.i_to_s in
    { s_to_i; i_to_s }

  let get_loc_id s map = SMap.find_opt s map.s_to_i
  let get_sym i map = IMap.find_opt i map.i_to_s
end

(* FIXME: This is slightly off because we could have a location that is already assigned.
          This will cause an unsoundness when testing pointer equality with other objects.
          But, if you do this, you already have a bug! We will merely signal the wrong bug.
          In any case, we will fix this when we have native support for disjoint addresses.
*)
type t = { counter : Z.t; bmap : Bidirectional_map.t }

let empty = { counter = Z.zero; bmap = Bidirectional_map.empty }

let declare_fn sym t =
  let counter = t.counter in
  let bmap = Bidirectional_map.add sym counter t.bmap in
  { counter = Z.add counter Z.one; bmap }

let of_linked_program (prog : Ail_tys.linked_program) =
  ListLabels.fold_left prog.sigma.function_definitions ~init:empty
    ~f:(fun ctx (sym, _) -> declare_fn sym ctx)

let decay_fn_sym sym t =
  Bidirectional_map.get_loc_id sym t.bmap
  |> Option.map (fun z ->
         let loc = Svalue.Ptr.loc_of_z z in
         let loc : Typed.T.sloc Typed.t = Typed.type_ loc in
         loc)
  |> Csymex.of_opt_not_impl ~msg:"Function has not been declared!"

let get_sym sv t =
  let res =
    match Typed.kind sv with
    | Svalue.Int z -> Bidirectional_map.get_sym z t.bmap
    | _ -> None
  in
  Csymex.of_opt_not_impl ~msg:"Could not resolve function" res
