let to_string = Htmlit.El.to_string ~doctype:false
let log_msg_class = "log-msg"

let header =
  Format.sprintf
    {|
  <!DOCTYPE html>
  <html lang="en">

  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
  |}
  ^ [%blob "assets/soteria_logs.css"]
  ^ {|
    </style>
    <script>
  |}
  ^ [%blob "assets/soteria_logs.js"]
  ^ {|
    </script>
    <title>Symex Log</title>
  </head>

  <body>
    <h1>Symex Log</h1>
    <header>
      <input id="search-input" type="text" placeholder="Search...">
      <div id="filters"></div>
    </header>
  |}

let footer = {|
</body>
</html>
  |}

let log_msg ?(siblings = []) ?(attrs = []) ?(inline = false) msg =
  let cons = if inline then Htmlit.El.span else Htmlit.El.div in
  Htmlit.(cons ~at:(At.class' log_msg_class :: attrs) (El.txt msg :: siblings))

let section_opening ~is_branch =
  if is_branch then {|<details class="is-branch">|} else {|<details>|}

let section_closing = {|</details>|}

let section_title title_txt =
  Htmlit.El.(summary [ log_msg ~inline:true title_txt ]) |> to_string

let message level str =
  let time = Unix.gettimeofday () in
  let t = Unix.localtime time in
  let t =
    Fmt.str "%02d:%02d:%02d.%03d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec
      (int_of_float (time *. 1000.0) mod 1000)
  in
  let time = Htmlit.(El.span ~at:[ At.class' "timestamp" ] [ El.txt t ]) in
  to_string
  @@ log_msg
       ~attrs:[ Htmlit.At.class' (Level.to_string level) ]
       ~siblings:[ time ] str
