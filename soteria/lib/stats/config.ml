type t = {
  output_stats : string option;
      [@names [ "output-stats"; "dump-stats"; "stats" ]]
      [@env "SOTERIA_OUTPUT_STATS"]
      (** If stats should be output. If the value is "stdout", prints the stats
          to stdout; otherwise, stores them as JSON in the specified file. *)
}
[@@deriving make, subliner]

let default = make ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Stats" ~default ()
