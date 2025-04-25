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
    }

    details {
      margin: 0.5em 0;
      padding: 0.5em;
      border: 1px solid #ccc;
      border-radius: 5px;
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
        div.className = "log-msg";
        div.innerText = elem.innerText;
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
        if (child.tagName == "DETAILS") {
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

let log_msg ?(attrs = []) ?(inline = false) msg =
  let cons = if inline then Htmlit.El.span else Htmlit.El.div in
  Htmlit.(cons ~at:(At.class' log_msg_class :: attrs) [ El.txt msg ])

let section_opening ~is_branch =
  if is_branch then {|<details class="is-branch">|} else {|<details>|}

let section_closing = {|</details>|}

let section_title title_txt =
  Htmlit.El.(summary [ log_msg ~inline:true title_txt ]) |> to_string

let message level str =
  to_string @@ log_msg ~attrs:[ Htmlit.At.class' (Level.to_string level) ] str
