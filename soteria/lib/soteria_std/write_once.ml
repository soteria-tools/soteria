(** Write-once configuration cell.

    Creates a mutable reference that can be set once and locked, after which any
    further modification raises [Failure].

    [make ~default ()] returns a pair [(get, set_and_lock)] where:
    - [get ()] returns the current value
    - [set_and_lock v] sets the value to [v] and locks the cell *)
let make ~name ~default () =
  let current_config = ref default in
  let locked = ref false in

  let set_config config =
    if !locked then
      Fmt.failwith "%s configuration cannot be changed anymore" name;
    current_config := config
  in
  let get () = !current_config in
  let set_and_lock config =
    set_config config;
    locked := true
  in
  (get, set_and_lock)
