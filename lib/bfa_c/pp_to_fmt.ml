let f (pprinter : 'a -> PPrint.document) : 'a Fmt.t =
 fun ft a ->
  let buffer = Buffer.create 1024 in
  PPrint.ToBuffer.pretty 0.5 80 buffer (pprinter a);
  Fmt.pf ft "%s" (Buffer.contents buffer)
