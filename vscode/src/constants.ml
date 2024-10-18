let prefixed key = "bfa." ^ key

module Commands = struct
  let restart_server = prefixed "server.restart"
end
