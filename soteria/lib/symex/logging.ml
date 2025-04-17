let src = Logs.Src.create "Soteria.Symex"
let info m = Logs.info ~src m
let warn m = Logs.warn ~src m
let err m = Logs.err ~src m
let debug m = Logs.debug ~src m
let app m = Logs.app ~src m
