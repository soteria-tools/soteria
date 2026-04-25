type t = {
  flamegraphs : string option;
      [@names [ "flamegraphs" ]]
      [@env "SOTERIA_FLAMEGRAPHS"]
      [@docs "PROFILING OPTIONS"]
      (** Specify the folder in which the flamegraphs will be saved. *)
}
[@@deriving make, subliner]

let default = make ()

let get, set_and_lock =
  Soteria_std.Write_once.make ~name:"Profiling config" ~default ()
