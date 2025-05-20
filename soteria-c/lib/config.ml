type t = {
  solver_timeout : int option; [@default None]
  dump_unsupported_file : string option; [@default None]
  dump_smt_file : string option; [@default None]
  auto_include_path : string;
      [@default
        match Auto_include_site.Sites.includes with
        | [] -> "."
        | [ x ] -> x
        | _ -> failwith "Multiple auto-include paths found"]
  z3_path : string; [@default "z3"]
  no_ignore_parse_failures : bool; [@default false]
  no_ignore_duplicate_symbols : bool; [@default false]
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
