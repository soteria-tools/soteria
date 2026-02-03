type t = {
  flamegraphs_folder : string option;
      [@names [ "flamegraphs-folder" ]] [@env "SOTERIA_FLAMEGRAPHS_FOLDER"]
}
[@@deriving make, subliner]

let default = make ()

let get, set_and_lock =
  Soteria_std.Write_once.make ~name:"Profiling config" ~default ()
