type t = {
  dump_unsupported_file : string option; [@default None]
  auto_include_path : string;
      [@default
        match Auto_include_site.Sites.includes with
        | [] -> "."
        | [ x ] -> x
        | _ -> failwith "Multiple auto-include paths found"]
  no_ignore_parse_failures : bool; [@default false]
  no_ignore_duplicate_symbols : bool; [@default false]
  parse_only : bool; [@default false]
  dump_summaries_file : string option; [@default None]
  show_manifest_summaries : bool; [@default false]
  alloc_cannot_fail : bool; [@default false]
  use_cerb_headers : bool; [@default false]
  infinite_fuel : bool; [@default false] (* TODO: find a better interface. *)
  cbmc_compat : bool; [@default false]
  havoc_undefined_funs : bool; [@default false]
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
