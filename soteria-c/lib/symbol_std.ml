(*** More standard interface to [Symbol], making it nicer for 
   using with Stdlib functors *)

open Cerb_frontend.Symbol

module SELF = struct
  type t = Cerb_frontend.Symbol.sym

  let equal = equal_sym
  let compare = compare_sym
  let hash = Hashtbl.hash
  let pp = Fmt.of_to_string Cerb_frontend.Pp_symbol.to_string
end

include SELF

let pp_sym_hum ft sym =
  match sym with
  | Cerb_frontend.Symbol.Symbol (_digest, _i, SD_Id id) -> Fmt.string ft id
  | _ -> pp ft sym

module Set = Set.Make (SELF)
module Map = Map.Make (SELF)
