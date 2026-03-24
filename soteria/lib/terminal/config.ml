type t = {
  compact : bool;
      [@docs Soteria_std.Cmdliner_helpers.Sections.output]
      [@name [ "compact" ]]
      [@env "SOTERIA_COMPACT_DIAGNOSTICS"]
      [@make.default false]
      (** Make diagnostic outputs compact.*)
}
[@@deriving subliner, make]

let default = make ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Terminal" ~default ()
let set_and_lock config = set_and_lock config
