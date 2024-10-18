include
  [%js:
  val download_and_extract : string -> string -> unit Promise.t
  [@@js.global "downloadAndExtract"]

  val rename : string -> string -> unit Promise.t [@@js.global "fsRename"]
  val make_executable : string -> unit Promise.t [@@js.global "makeExec"]]

let download_and_extract url dest =
  Logging.debug "Downloading %s to %s" url dest;
  download_and_extract url dest

let rename src dest =
  Logging.debug "Renaming %s to %s" src dest;
  rename src dest

let make_executable path =
  Logging.debug "Making %s executable" path;
  make_executable path
