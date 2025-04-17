let prefixed key = "soteria." ^ key

module Commands = struct
  let restart_server = prefixed "server.restart"
  let toggle_debug_mode = prefixed "server.toggleDebugMode"
end
