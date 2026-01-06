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

(** Whether we are currently in a test environment ([dune test]), the PWD is set
    to the string "$TESTCASE_ROOT". This can be used to avoid printing unstable
    values automatically, instead of passing [--hide-unstable] to all tests
    manually. *)
let in_test_environment =
  let v =
    lazy
      (Option.is_some (Sys.getenv_opt "INSIDE_DUNE")
      && Option.is_some (Sys.getenv_opt "BUILD_PATH_PREFIX_MAP"))
  in
  fun () -> Lazy.force v

let set, get, lock =
  let current_config = ref default in
  let locked = ref false in
  let lock () = locked := true in
  let set_config config =
    if !locked then failwith "Terminal configuration cannot be changed anymore";
    let config =
      if in_test_environment () then { config with hide_unstable = true }
      else config
    in
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
