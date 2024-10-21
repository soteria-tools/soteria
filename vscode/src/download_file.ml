module String = struct
  type t = string [@@js]
end

module Result = Interop.Js.Result (Interop.Js.Unit) (String)

include
  [%js:
  val download_and_extract :
    progress:Vscode.Progress.t -> string -> string -> Result.t Promise.t
  [@@js.global "downloadAndExtract"]

  val rename : string -> string -> Result.t Promise.t [@@js.global "fsRename"]
  val make_executable : string -> Result.t Promise.t [@@js.global "makeExec"]
  val dl_archive_name : string [@@js.global "dlArchiveName"]]

let download_and_extract ~progress url dest =
  Logging.debug "Downloading %s to %s" url dest;
  download_and_extract ~progress url dest

let rename src dest =
  Logging.debug "Renaming %s to %s" src dest;
  rename src dest

let make_executable path =
  Logging.debug "Making %s executable" path;
  make_executable path
