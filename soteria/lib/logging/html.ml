let to_string = Htmlit.El.to_string ~doctype:false
let log_msg_class = "log-msg"

let header =
  {|
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <style>
        .log-msg {
          font-family: monospace;
          white-space: pre;
        }
      </style>
      <title>Symex Log</title>
    </head>
    <body>
      <h1>Symex Log</h1>
  |}

let footer = {|
      </body>
      </html>
  |}

let log_msg ?(inline = false) msg =
  let cons = if inline then Htmlit.El.span else Htmlit.El.div in
  Htmlit.(cons ~at:[ At.class' log_msg_class ] [ El.txt msg ])

let section_opening = {|<details>|}
let section_closing = {|</details>|}

let section_title title_txt =
  Htmlit.El.(summary [ log_msg ~inline:true title_txt ]) |> to_string

let message _level str = to_string (log_msg str)
