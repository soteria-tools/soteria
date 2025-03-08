open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
module T = Typed.T

let unit = Types.TAdt (TTuple, TypesUtils.empty_generic_args)

let std_type_map =
  [ ("core::fmt::Arguments", unit) ]
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

let clean_name =
  List.filter @@ function Types.PeIdent ("<'_>", _) -> false | _ -> true

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

let type_eval ~(crate : UllbcAst.crate) (t : Types.type_decl) =
  let ctx = NameMatcher.ctx_from_crate crate in
  (* Unfortunately, some type names end with a region identifier,
     like "core::fmt::Arguments::<'_>". NameMatcher doesn't allow "<'_>" in the
     pattern, so we manually filter it out beforehand. *)
  let name = clean_name t.item_meta.name in
  NameMatcherMap.find_opt ctx match_config name std_type_map
