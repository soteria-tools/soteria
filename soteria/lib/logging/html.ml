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
      white-space: pre-wrap;
      margin: 2px;
      padding: 2px;
      border-radius: 4px;
      position: relative;
    }

    summary {
      font-style: italic;
      cursor: pointer;
    }

    details {
      margin: 0.5em 0;
      padding: 0.5em;
      border: 1px solid #ccc;
      border-radius: 5px;
    }

    .timestamp {
      position: absolute;
      top: 0;
      right: 0;
      background: #ffffff80;
      color: #000;
      font-weight: normal;
      font-size: 0.8em;
    }
    .timestamp:hover {
      opacity: 0;
    }

    .log-msg.branch {
      font-style: italic;
      color: #888;
    }

    .TRACE {
      background-color: #c4c4c4;
    }
    .DEBUG {
      background-color: #dee;
      color: #777;
    }
    .INFO {
      background-color: #fff;
    }
    .WARN {
      background-color: #ffeb3b;
    }
    .ERROR {
      background-color: #ed3e3e;
      font-weight: bold;
    }
  </style>
  <script>
    function summaryToMsg(elem) {
      if (elem.tagName == "SUMMARY") {
        const div = document.createElement("div");
        div.className = "log-msg branch";
        for (let child of elem.childNodes) {
          div.appendChild(child);
        }
        elem.parentNode.replaceChild(div, elem);
      }
    }

    function flattenDirectDetails(element) {
      // Recurse first
      for (let child of element.children) {
        flattenDirectDetails(child);
      }

      // Will not be undefined iff there is a single <details> tag
      let detailChild = undefined;
      for (let child of element.children) {
        if (child.tagName == "DETAILS" && !child.hasClass("is-branch")) {
          if (detailChild == undefined) {
            detailChild = child;
          } else {
            detailChild = undefined;
            break;
          }
        }
      }

      if (detailChild == undefined) {
        return;
      }
      while (detailChild.firstChild) {
        summaryToMsg(detailChild.firstChild);
        element.insertBefore(detailChild.firstChild, detailChild);
      }

      element.removeChild(detailChild);
    }

    // Start from the body
    document.addEventListener('DOMContentLoaded', () => { flattenDirectDetails(document.body) });
  </script>
  <title>Symex Log</title>
</head>

<body>
  <h1>Symex Log</h1>
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
