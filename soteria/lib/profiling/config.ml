type t = {
  record_flamegraph : bool;
      [@names [ "record-flamegraph"; "flamegraph" ]]
      [@env "SOTERIA_PROFILING_ENABLED"]
      [@make.default false]
}
[@@deriving make, subliner]

let default = make ()

let get, set_and_lock =
  Soteria_std.Write_once.make ~name:"Profiling config" ~default ()
