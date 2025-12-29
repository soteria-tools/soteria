type t = {
  output_stats : string option;
      [@names [ "output-stats"; "dump-stats"; "stats" ]]
      [@env "SOTERIA_OUTPUT_STATS"]
      (** If stats should be output. If the value is "stdout", prints the stats
          to stdout; otherwise, stores them as JSON in the specified file. *)
}
[@@deriving make, subliner]

let default = make ()

let set, get, lock =
  let current_config = ref default in
  let locked = ref false in
  let lock () = locked := true in
  let set_config config =
    if !locked then failwith "Stats configuration cannot be changed anymore";
    current_config := config
  in
  let current_config () = !current_config in
  (set_config, current_config, lock)

let set_and_lock config =
  set config;
  lock ()
