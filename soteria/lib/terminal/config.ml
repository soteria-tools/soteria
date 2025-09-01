type t = {
  no_color : bool;
      [@names [ "no-color"; "no-colour" ]]
      [@env "NO_COLOR"]
      [@make.default false]
      (** Disables coloured output *)
  compact : bool;
      [@name [ "compact" ]]
      [@env "SOTERIA_COMPACT_DIAGNOSTICS"]
      [@make.default false]
      (** Make diagnostic outputs compact.*)
}
[@@deriving subliner, make]

let default = make ()

let set, get, lock =
  let current_config = ref default in
  let locked = ref false in
  let lock () = locked := true in
  let set_config config =
    if !locked then failwith "Terminal configuration cannot be changed anymore";
    current_config := config
  in
  let current_config () = !current_config in
  (set_config, current_config, lock)

let set_and_lock config =
  set config;
  lock ();
  Profile.init ~no_color:config.no_color ()

let no_color () = (get ()).no_color
let compact () = (get ()).compact
