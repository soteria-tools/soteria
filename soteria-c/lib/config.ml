type t = {
  solver_timeout : int option; [@default None]
  dump_unsupported_file : string option; [@default None]
  dump_smt_file : string option; [@default None]
  auto_include_path : string;
      [@default List.nth Auto_include_site.Sites.includes 0]
  z3_path : string; [@default "z3"]
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
