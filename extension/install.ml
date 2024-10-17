open Import

let url =
  let computed = ref None in
  fun () ->
    match !computed with
    | Some url -> url
    | None ->
        (* We might have a complex computation involving github requests at some point.
           We only do it the first time. *)
        let url =
          "https://www.doc.ic.ac.uk/~sja3417/artifact/macos-latest-package.zip"
        in
        computed := Some url;
        url
