(*** More standard interface to [Symbol], making it nicer for using with Stdlib
  functors *)

open Cerb_frontend.Symbol

module SELF = struct
  type t = Cerb_frontend.Symbol.sym

  let equal = equal_sym
  let compare = compare_sym
  let hash = Hashtbl.hash

  let pp ft (Symbol (_, n, sd)) =
    let pp_id = Soteria.Logs.Printers.pp_unstable ~name:"id" Fmt.int in
    match sd with
    | SD_Id str | SD_ObjectAddress str | SD_FunArgValue str ->
        Fmt.pf ft "%s_%a" str pp_id n
    | _ -> Fmt.pf ft "a_%a" pp_id n

  let show = Fmt.to_to_string pp
end

include SELF

let pp_sym_hum ft sym =
  match sym with
  | Cerb_frontend.Symbol.Symbol (_digest, _i, SD_Id id) -> Fmt.string ft id
  | _ -> pp ft sym

module Set = Set.Make (SELF)
module Map = Map.Make (SELF)
