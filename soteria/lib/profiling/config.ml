type t = {
  flamegraphs_dir : string option;
      [@names [ "flamegraphs-dir"; "flamegraph-dir" ]]
      [@env "SOTERIA_FLAMEGRAPHS_DIR"]
}
[@@deriving make, subliner]

let default = make ()

let get, set_and_lock =
  Soteria_std.Write_once.make ~name:"Profiling config" ~default ()
