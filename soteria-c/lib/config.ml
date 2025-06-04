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
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
