type t = {
  no_color : bool;
      [@names [ "no-color"; "no-colour" ]]
      [@env "NO_COLOR"]
      [@make.default false]
      (** Disables coloured output. *)
  hide_unstable : bool;
      [@names [ "hide-unstable"; "diffable" ]]
      [@env "HIDE_UNSTABLE"]
      [@make.default false]
      (** Do not display unstable values like durations (e.g. for diffing
          purposes). *)
  compact : bool;
      [@name [ "compact" ]]
      [@env "SOTERIA_COMPACT_DIAGNOSTICS"]
      [@make.default false]
      (** Make diagnostic outputs compact.*)
}
[@@deriving subliner, make]

let default = make ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Terminal" ~default ()

let set_and_lock config =
  set_and_lock config;
  Profile.init ~no_color:config.no_color ()

let no_color () = (get ()).no_color
let compact () = (get ()).compact
