type t = { no_color : bool; compact : bool }

let set, get, lock =
  let current_config = ref { no_color = false; compact = false } in
  let locked = ref false in
  let lock () = locked := true in
  let set_config config =
    if !locked then failwith "Logging configuration cannot be changed anymore";
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
